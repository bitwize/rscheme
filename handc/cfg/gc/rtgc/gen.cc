#include <iostream.h>
#include <assert.h>

#include <rtgc/gc.hh>
#include <rtgc/objmgr.hh>
#ifdef INLINES
#include <rtgc/inlines.hh>
#endif
// This file contains all of the generational routines that can't be inlined.
// See the file gen.hh for more information, and the file gen.ci for the
// routines which can be inlined.

//**********************************************************************
// generation::generation
// This routine contructs the generation class object. This routine must be
// called from garbage_collector::garbage_collector before allocating
// the first heap object. Gen_num is the number of the generation, and passed 
// to the color_set class.

generation::generation(int gen_num){
   this_generation = gen_num;
   for(int i = 0;i < NUM_SIZE_CLASSES; i++){
      color_sets[i] = new color_set(gen_num,i);
      if(color_sets[i] == 0){
	 cerr << "Sorry, couldn't get enough heap space" << endl;
	 exit(0);
      }
   }
}

//***************************************************************************
// generation::init
//
// This routine initializes the generation object before any garbage collection
// has started.  Currently, this routine simply calls init on each of its
// size class objects.

void generation::init(int i) {
   stored_into_list_head = 0;
   stored_into_list_tail = 0;
   no_more_work = 0;
   current_shade_color = INIT_SHADING_COLOR;

   // Tell the client to reset its roots.

   // We use the root scanning routine through scan_root object, to
   // make an illusion that we have multiple scannsers. This should
   // be changed in future.

   // The generation number i is used for an ID for the root set
   // for this generation.
   root.init(i);
   
   root.abstract_stable_root_reset();
   root.abstract_quasistable_root_reset();
   root.abstract_unstable_root_reset();
}


//**************************************************************************
// generation::blacken_grays
//
// trace this generation and blacken grays in this generation.
// assume that at the time that this routine is called, some grays 
// has been setup by the tracing functions.
//
// This function returns 1 if it used up its time allotment but has not
// yet finished blackening all of the grays.  It returns 0 if there are
// no more grays to blacken.

int generation::blacken_grays(void) {
   UINT_32 work_done;

   if (gc.incremental_tracing_state.Done == true)
   {
      // If the last time this routine was called, we finished tracing
      // an entire object, then this time, we must start looking for
      // a new object to trace.  If we didn't finish with an object,
      // then just resume where we left off.

      gc.incremental_tracing_state.current_size_class_being_traced = 
	 NUM_SIZE_CLASSES-1;
   }

   do {
      for(/* no initialization */;
	  gc.incremental_tracing_state.current_size_class_being_traced >= 0;
	  gc.incremental_tracing_state.current_size_class_being_traced--)
      {
	 // color_set::blacken_next_gray returns 1 if there was a gray to
	 // blacken (even if it couldn't finish blackening it), and a 0
	 // otherwise.
 
	 work_done = get_work_left_to_do();
	 while(get_color_set(gc.incremental_tracing_state.current_size_class_being_traced)->blacken_next_gray(work_done,get_work_left_to_do()))
	 {
	    if (increment_work(work_done) <= 0)
	    {
	       // quit early
	       return(1);  // we have completed one increment of work
	    }
	 }
      }
      gc.incremental_tracing_state.current_size_class_being_traced = 
	 NUM_SIZE_CLASSES - 1;
   } while(any_more_grays());
   return(0);
}

#ifdef GENERATIONAL
//**************************************************************************
// generation::blacken_or_promote_grays
//
// trace this generation and blacken grays in this generation.
// assume that at the time that this routine is called, some grays 
// has been setup by the tracing functions.
//
int generation::blacken_or_promote_grays(void) {
   UINT_32 work_done;

   if (gc.incremental_tracing_state.Done == true)
   {
      // If the last time this routine was called, we finished tracing
      // an entire object, then this time, we must start looking for
      // a new object to trace.  If we didn't finish with an object,
      // then just resume where we left off.

      gc.incremental_tracing_state.current_size_class_being_traced = 
	 NUM_SIZE_CLASSES-1;
   }

   do {
      for(/* no initialization */;
	  gc.incremental_tracing_state.current_size_class_being_traced >= 0;
	  gc.incremental_tracing_state.current_size_class_being_traced--)
      {
	 // color_set::blacken_next_gray returns 1 if there was a gray to
	 // blacken (even if it couldn't finish blackening it), and a 0
	 // otherwise.
 
	 work_done = get_work_left_to_do();
	 while(get_color_set(gc.incremental_tracing_state.current_size_class_being_traced)->blacken_or_promote_next_gray(work_done,get_work_left_to_do()))
	 {
	    if (increment_work(work_done) <= 0)
	    {
	       // quit early
	       return(1);  // we have completed one increment of work
	    }
	 }
      }
      gc.incremental_tracing_state.current_size_class_being_traced = 
	 NUM_SIZE_CLASSES - 1;
   } while(any_more_grays());
   return(0);
}
#endif


//**********************************************************************
// generation::trace_single_generation
//
// This routine is used for the non-generational rtgc.  It traces
// a single generation (in this case, there is only one generation).
// If it finishes tracing this generation within its time limit,
// it returns 1. Otherwise, it returns 0.
//
// This routine is called from garbage_collector::collect and implements
// the tracing algorithm.  The algorithm is as follows:
//
//  First, scan the root set incrementally.  For each pointer from the    
//  stack, if the object pointed to is not shaded, then gray it.  Next,   
//  see if graying this pointer created any more gray objects.  If so,
//  then blacken those grays.  If this completes one increment of work
//  (If the collector runs out of time) then just return.  Otherwise 
//  proceed with the next root pointer.
//                                                                        
//  Next, the stored_into_list is incrementally scanned and each object   
//  pointed to by this list is grayed (if it is not already shaded) and   
//  some collection work is performed.  If this completes an increment of 
//  work then just return.  Otherwise proceed with the next element in    
//  the stored_into_list.                                                 
//                                                                        
//  Finally, if there is no more work to do, then scan the stack one      
//  more time to see if we are done (this time the stack scan is atomic). 
//  If any more objects need to be traced, they are traced.  If this can  
//  be completed within one increment of work then we are done and a      
//  gc flip is performed.  Otherwise, just return.                        
//                                                                        
//  The root set consists of every pointer in the pointer_stack, which    
//  is a stack that parallels the activation stack, but which is          
//  initialized with the global variables, and only contains pointers     
//  into the garbage collected heap                                       

int generation::trace_single_generation(void){

   // This sets the work amount to be done in this increment.
   set_work_left_to_do(gc_scheduler.get_work_per_increment(0));
   done_tracing = 0;

#ifdef SNAPSHOT_WRITE_BARRIER
   if(gc.first_collection_increment) {
      scan_root_atomic();
      gc.first_collection_increment = false;
      return(0);
   } else {

      // First, see if there already exist any grays.
      // If so, traverse them first

      if ((gc.incremental_tracing_state.Done == false) || any_more_grays())
      {
	 // Since there is work left to do, do some collecting.
	 
	 // generation::blacken_grays() will return 1 if it is done with an
	 // increment of collecing (i.e. it is out of time), and zero
	 // if there are no more grays to blacken.
	 
	 if (blacken_grays()) {
	    // We ran out of time before the collection finished, so return 0.
	    return 0;
	 }
      }

      done_tracing = 1;
      return(1);
   }
      
#else // Not a snapshot write barrier

   // First, see if there already exist any grays.  If so, traverse them first

   if (any_more_grays()) {

      // Since there is work left to do, do some collecting.
      
      // generation::blacken_grays() will return 1 if it is done with an
      // increment of collecing (i.e. it is out of time), and zero
      // if there are no more grays to blacken.
      
      if (blacken_grays()) {
	 // We ran out of time before the collection finished, so return 0.
	 return 0;
      }
   }
   
   // Next, scan the root set.  The routine scan_stack will search down the
   // runtime stack, and if it finds a pointer to any white object, it will
   // gray that object and return 1.  If it scans the entire stack and has
   // found no pointers to white objects, it returns 0.  Note that the stack
   // is scanned incrementally, returning after the first white object is
   // grayed.  Once the stack has been scanned entirely, it is not scanned
   // again until termination (see code below).

   while(scan_root()) {

      // Since there is work left to do, do some collecting.
      // generation::blacken_grays() will return 1 if it is done with an
      // increment of collecing (i.e. it is out of time), and zero
      // if there are no more grays to blacken
      
      if (blacken_grays()) {
	 return 0;
      }
   }

#ifndef HARD_REAL_TIME // In the hard real-time version, pointer stores
                       // that might violate the tri-color invariant
                       // cause the immediate graying the the left-hand
                       // object.  Thus, there is no stored into list.

   // gray an object from the to do list.

   // generation::get_ptr_stored_into_list() will return a pointer to a
   // location which might contain a pointer to an object which might need
   // to be grayed.  If there aren't any more pointers in the stored into
   // list, then generation::get_ptr_stored_into_list() will return 0.  
   // cast_and_deref_ptr is a user supplied function (see gcclient.h for
   // more information) that takes the pointer to the location that might
   // hold a pointer to an object, dereferences this first pointer, if there
   // is a pointer at that memory location, it casts that pointer into a
   // valid C pointer, and returns it.  It returns the C pointer NULL if
   // there isn't a valid pointer at that location.

   pos_ptr_addr temp;

   while ((temp = get_ptr_stored_into_list())!=0) {
      gc_obj_addr ptr;
      gc_object_base *object;

      // We have gotten the next stored into location.
      // First, since we have a pointer to the location that was
      // stored into, and we want to know about the object pointed
      // to by the *pointer* stored *in* that location, we cast
      // and dereference this pointer.  If we get back something other
      // than NULL, then we have a valid C pointer to some object that
      // we might have to gray.

      if((ptr = cast_and_deref_ptr(temp)) != NULL) {

	 // First, we derive the header of this rval object.  There
	 // are two ways of doing this depending on whether the language
	 // system allows derived pointers.  One just subtracts off
	 // the language level header, and the other does a virtual
	 // function call to compute the offset for this object.

#ifdef NO_DERIVED_POINTERS
	 object = (gc_object_base *)(((char *)ptr)-sizeof(gc_object_base));

#ifndef NDEBUG
	 // In the debugging case, we derive the start of the object
	 // anyway just to make sure that there *really* aren't
	 // any derived pointers
	 object_manager *om = gc.get_object_manager(ptr);
	 gc_object_base *object1 = om->find_start_of_object(ptr);
	 assert(object == object1);
#endif //NDEBUG

#else // derived pointers
	 object_manager *om = gc.get_object_manager(ptr);
	 object = om->find_start_of_object(ptr);
#endif // derived pointers

	 // Next, determine if the rval object is already shaded.
	 if(!object->is_shaded()) {

	    // If the rval object is not shaded, shade it and do
	    // some more tracing.

#ifdef NO_DERIVED_POINTERS
	    object->get_containing_list()->gray_this_object(object);
#else // derived pointers
	    // In this case, we already had to look up the object
	    // manager, so don't do it again.  Call a special case
	    // version of get_containing_list().

	    object->get_containing_list(om)->gray_this_object(object);
#endif // derived pointers

	    // generation::blacken_grays() will return 1 if it is
	    // done with an increment of collecing (out of time) and
	    // 0 if there were no more grays.

	    if (blacken_grays()) {
	       return 0;
	    }
	 }
      }
   }

#endif // not HARD_REAL_TIME

#ifdef INCREMENTAL_UPDATE_WRITE_BARRIER

   // Finally, if there is no more work to do, then scan the stack one
   // more time to see if we are done (this time the stack scan is atomic).

   // scan_root_atomic will return 1 if it grayed any objects,
   // otherwise it will return 0.

   if (scan_root_atomic()) {

      // generations.blacken_grays will return 1 if it is done with an
      // increment of collecting, and 0 if it finished collecting before
      // the increment was completed.
      
      if (blacken_grays()) {
	 return 0;
      }
      // It is still possible to terminate here.  In this case, we tried
      // to terminate by doing an atomic stack scan, but found some more
      // gray objects.  However, we were able to blacken them all within
      // our alloted time limit so we terminate.
   }
#endif

   done_tracing = 1;

   // reset all of the incremental root scanning pointers for the next
   // complete gc cycle.
   root.abstract_stable_root_reset();
   root.abstract_quasistable_root_reset();
   root.abstract_unstable_root_reset();

   return 1;
#endif // Not a snapshot write barrier
}


#ifdef GENERATIONAL 
//**********************************************************************
// Generation::trace_youngest_generation
//  The algorithm is as follows:
//
//  First, scan the root set incrementally.  For each pointer from the
//  stack, if the object pointed to is not shaded, then gray it.  Next,
//  see if there are any grays.  If so, then blacken those grays.  If
//  this completes one increment of work then just return.  Otherwise
//  proceed with the next root pointer.
//
//
//   Next, the igp_list and stored_into_list are incrementally scanned
// and each object pointed to by this list is grayed (if it is not
// already shaded) and some tracing work is performed.  If this completes
// an increment of work then just return.  Otherwise proceed with the
// next element in the igp_list and stored_into_list.
//
//
//  Finally, if there is no more work to do, then scan the stack one
//  more time to see if we are done (this time the stack scan is atomic).
//  If any more objects need to be traced, they are traced.  If this can
//  be completed within one increment of work then we are done and 1 is
//  returned. Usually 0 is returned.

int generation::trace_youngest_generation(void){
   pos_ptr_addr temp;

   // This sets the work amount to be done in this increment.
   set_work_left_to_do(gc_scheduler.get_work_per_increment(0));
   done_tracing = 0;

   // If the work amount to be done is zero, return.li
   if(get_work_left_to_do() <= 0){
      return(0);
   }

   // First, see if there already exist any grays.  If so, traverse them first

   if ((gc.incremental_tracing_state.Done == false) || any_more_grays()) {

      // Since there is work left to do, do some tracing.
      // generations.blacken_or_promote_grays
      // will return 1 if it is done with an
      // increment of tracing.
      
      if (blacken_or_promote_grays()) {
	 // Tracing is not done, so return 0.
	 return(0);
      }
   }
   
   // Next, scan the root set.  The routine scan_root will search down the
   // root set, and if it finds a pointer to any white object, it will
   // gray that object and return 1.  If it scans the entire root set and has
   // found no pointers to white objects, it returns 0.  Note that the root
   // set is scanned incrementally, returning after the first white object is
   // grayed.  Once the root set has been scanned entirely, it is not scanned
   // again until finalization (see code below).

   while(scan_root()) {
      // Since there is work left to do, do some tracing.

      // blacken_or_promote_grays() will return 1 if it is done with an
      // increment of tracing.
      if (blacken_or_promote_grays()) {
	 // Tracing is not done, so return 0.
	 return(0);
      }
   }

   // Next, we scan the intergeneration pointers from the older generations.
   // If it finds a white object in this generation, it grays it and returns
   // 1. If there are no such objects, return 0
   while(gc.get_igpm()->scan_igp_list(this_generation)){
      // generations::blacken_or_promote_grays
      // will return 1 if it is done with an
      // increment of tracing.

      if (blacken_or_promote_grays()) {
	 // Tracing is not done, so return 0.
	 return(0);
      }
   }

   // gray an object from the to do list.

   // get_ptr_stored_into_list will return a pointer to a location which
   // might contain a pointer to an object which might need to be grayed.
   // If there aren't any more pointers in the stored into list, then
   // get_ptr_stored_into_list will return 0.  cast_and_deref_ptr is a user
   // supplied function (see gcclient.h for more information) that takes the
   // pointer to the location that might hold a pointer to an object,
   // dereferences this first pointer, if there is a pointer at that memory
   // location, it casts that pointer into a valid C pointer, and returns it.
   // It returns the C pointer NULL if there isn't a valid pointer at that
   // location.

   while ((temp = get_ptr_stored_into_list())!=0) {
      gc_obj_addr ptr;
      gc_object_base *object;

      // We have gotten the next stored into location.
      // First, since we have a pointer to the location that was
      // stored into, and we want to know about the object pointed
      // to by the *pointer* stored *in* that location, we cast
      // and dereference this pointer.  If we get back something other
      // than NULL, then we have a valid C pointer to some object that
      // we might have to gray.

      if((ptr = cast_and_deref_ptr(temp)) != NULL) {
	 // First, we derive the header of this rval object.  There
	 // are two ways of doing this depending on whether the language
	 // system allows derived pointers.  One just subtracts off
	 // the language level header, and the other does a virtual
	 // function call.
#ifdef NO_DERIVED_POINTERS
	 object = (gc_object_base *)(((char *)ptr)-sizeof(gc_object_base));
#ifndef NDEBUG
	 object_manager *om = gc.get_object_manager(ptr);
	 gc_object_base *object1 = om->find_start_of_object(ptr);
	 assert(object == object1);
#endif //NDEBUG
#else
	 object_manager *om = gc.get_object_manager(ptr);
	 object = om->find_start_of_object(ptr);
#endif
	 // Next, determine if the rval object is already shaded and if
	 // it is in this generation.
	 // The pointed object was in this generation when the write
	 // barrier enlisted the pointer, but it might have been traced and
	 // promoted by now. In that case, we have to do nothing. If the
	 // pointed object is promoted as a black, trivially unnecessary.
	 // If it was promoted as a white, the one-step older generation is
	 // in the first tracing phase, so we have to go through a whole
	 // GC cycle of this generation at least one more time, and all
	 // pointers from this generation are found at that time.

	 if(!object->is_shaded() && object->get_gen_num() == 0){

	    // If the rval object is a white in this generation,
	    // shade it and do some more tracing (by calling
	    // generations::blacken_or_promote_grays).
#ifdef NO_DERIVED_POINTERS
	    object->get_containing_list()->gray_this_object(object);
#else	    
	    object->get_containing_list(om)->gray_this_object(object);
#endif

	    // generations::blacken_or_promote_grays
	    // will return 1 if it is done with an
	    // increment of tracing.

	    if (blacken_or_promote_grays()) {
	       // Trace is not done, so return 0.
	       return(0);
	    }
	 }
      }
   }

   // Finally, if there is no more work to do, then scan the root set one
   // more time to see if we are done (this time the root set scan is atomic).

   // scan_root_atomic will return 1 if it grayed any objects,
   // otherwise it will return 0.

   if (scan_root_atomic()) {

      // generations.blacken_or_promote_grays
      // will return 1 if it is done with an increment of
      // tracing, and 0 if it finished tracing before the increment was
      // completed.

      if (blacken_or_promote_grays()) {
	 // Tracing is not done, so return 0.
	 return(0);
      }
   }


   // Tracing may add some igp pointer from the older generation, when it
   // promotes an object, but the object pointed by the igp must be grayed
   // right away.
   assert(!gc.get_igpm()->scan_igp_list(this_generation));

   // The tracing is done, set the flag, increment the one-step older
   // generation's gc cycle counter and return 1.
   done_tracing = 1;

   root.abstract_stable_root_reset();
   root.abstract_quasistable_root_reset();
   root.abstract_unstable_root_reset();

   // If the one-step older generation is in the second phase,
   if(gc.get_gen(1)->tracing_phase_counter != 0){
      // Increment the GC cycle counter.
      gc.get_gen(1)->tracing_phase_counter++;
   }
   // 1 represents the tracing is this generation is done.
   return(1);
}

//**********************************************************************
//  Generation::trace_mid_generation
//  The algorithm is as follows:
//
//  First, scan the root set incrementally.  For each pointer from the
//  stack, if the object pointed to is not shaded, then gray it.  Next,
//  see if there are any grays.  If so, then blacken those grays.  If
//  this completes one increment of work then just return.  Otherwise
//  proceed with the next root pointer.
//
//   Next, the igp_list and stored_into_list are incrementally scanned
// and each object pointed to by this list is grayed (if it is not
// already shaded) and some tracing work is performed.  If this completes
// an increment of work then just return.  Otherwise proceed with the
// next element in the igp_list and stored_into_list.
//
// When we come back to this tracer, we blacken objects grayed by the
// write barrier and younger tracers. If there is no gray objects, then
// check the stored_into_list if there is a new entry. If there are some,
// do some blacking. Even if we have nothing to do, we have to keep
// returning without termination flag until the younger tracers finishes
// one whole GC cycle.
// 
// If there is no gray object to blacken, and if there is no untraced
// entry in the stored_into_list, scan the stack one more time
// atomically. If we find no objects to be traced, reset counters and
// return 1 as the termination flag. If we find some, we have to do some
// more tracing.
//
int generation::trace_mid_generation(void){
   pos_ptr_addr temp;

   // This sets the work amount to be done in this increment.
   set_work_left_to_do(gc_scheduler.get_work_per_increment(this_generation));

   done_tracing = 0;

   // If no_more_work is on, it means we have no more objects to scan and 
   // we can terminated this GC cycle only if the younger generation has
   // terminated its GC cycle. Since we have a chance to terminate if it is
   // on, we keep working. 
   if(get_work_left_to_do() <= 0 && !no_more_work){
      return(0);
   }else{
      no_more_work = 0;
   }

   // First, see if there already exist any grays.  If so, traverse them first

   if (any_more_grays()) {

      // Since there is work left to do, do some tracing.
      // generations.blacken_or_promote_grays
      // will return 1 if it is done with an
      // increment of tracing.
      
      if (blacken_or_promote_grays()) {
	 // Tracing is not done, so return 0.
	 return(0);
      }
   }
   
   // Next, scan the root set.  The routine scan_root will search down the
   // root set, and if it finds a pointer to any white object, it will
   // gray that object and return 1.  If it scans the entire root set and has
   // found no pointers to white objects, it returns 0.  Note that the root
   // set is scanned incrementally, returning after the first white object is
   // grayed.  Once the root set has been scanned entirely, it is not scanned
   // again until finalization (see code below).

   while(scan_root()) {
      // Since there is work left to do, do some tracing.

      // blacken_or_promote_grays() will return 1 if it is done with an
      // increment of tracing.
      if (blacken_or_promote_grays()) {
	 // Tracing is not done, so return 0.
	 return(0);
      }
   }

   // Next, we scan the intergeneration pointers from the older generations.
   // If it finds a white object in this generation, it grays it and returns
   // 1. If there are no such objects, return 0
   while(gc.get_igpm()->scan_igp_list(this_generation)){
      // generations::blaken_or_promote_grays
      // will return 1 if it is done with an
      // increment of tracing.

      if (blacken_or_promote_grays()) {
	 // Tracing is not done, so return 0.
	 return(0);
      }
   }

   // gray an object from the to do list.

   // get_ptr_stored_into_list will return a pointer to a location which
   // might contain a pointer to an object which might need to be grayed.
   // If there aren't any more pointers in the stored into list, then
   // get_ptr_stored_into_list will return 0.  cast_and_deref_ptr is a user
   // supplied function (see gcclient.h for more information) that takes the
   // pointer to the location that might hold a pointer to an object,
   // dereferences this first pointer, if there is a pointer at that memory
   // location, it casts that pointer into a valid C pointer, and returns it.
   // It returns the C pointer NULL if there isn't a valid pointer at that
   // location.

   while ((temp = get_ptr_stored_into_list())!=0) {
      gc_obj_addr ptr;
      gc_object_base *object;

      // We have gotten the next stored into location.
      // First, since we have a pointer to the location that was
      // stored into, and we want to know about the object pointed
      // to by the *pointer* stored *in* that location, we cast
      // and dereference this pointer.  If we get back something other
      // than NULL, then we have a valid C pointer to some object that
      // we might have to gray.

      if((ptr = cast_and_deref_ptr(temp)) != NULL) {
	 // First, we derive the header of this rval object.  There
	 // are two ways of doing this depending on whether the language
	 // system allows derived pointers.  One just subtracts off
	 // the language level header, and the other does a virtual
	 // function call.
#ifdef NO_DERIVED_POINTERS
	 object = (gc_object_base *)(((char *)ptr)-sizeof(gc_object_base));
#ifndef NDEBUG
	 object_manager *om = gc.get_object_manager(ptr);
	 gc_object_base *object1 = om->find_start_of_object(ptr);
	 assert(object == object1);
#endif //NDEBUG
#else
	 object_manager *om = gc.get_object_manager(ptr);
	 object = om->find_start_of_object(ptr);
#endif
	 // Next, determine if the rval object is already shaded and if
	 // it is in this generation.
	 // The pointed object was in this generation when the write
	 // barrier enlisted the pointer, but it might have been traced and
	 // promoted by now. In that case, we have to do nothing. If the
	 // pointed object is promoted as a black, trivially unnecessary.
	 // If it was promoted as a white, the one-step older generation is
	 // in the first tracing phase, so we have to go through a whole
	 // GC cycle of this generation at least one more time, and all
	 // pointers from this generation are found at that time.

	 if(!object->is_shaded() && object->get_gen_num() == this_generation)
	 {
	    // If the rval object is a white in this generation,
	    // shade it and do some more tracing (by calling
	    // generations::blacken_or_promote_grays).
#ifdef NO_DERIVED_POINTERS
	    object->get_containing_list()->gray_this_object(object);
#else
	    object->get_containing_list(om)->gray_this_object(object);
#endif

	    // generations::blacken_or_promote_grays
	    // will return 1 if it is done with an
	    // increment of tracing.

	    if (blacken_or_promote_grays()) {
	       // Trace is not done, so return 0.
	       return(0);
	    }
	 }
      }
   }

   if(tracing_phase_counter == 0){
      // tracing_phase_counter == 0 means we're in the first tracing phase. 
      // Since reaching here means we have traced all objects reachable from
      // the root set (since we are in the oldest generation), we set the GC
      // cycle counter on. tracing_phase_counter == 1 means we are in a
      // drop-in cycle. We trace the whole younger generations
      // when tracing_phase_counter == 2. If tracing_phase_counter is
      // larger than or equal to 3, we have gone through at least one whole
      // GC cycle, so we can go on to the finalization. Otherwise, we have
      // to wait for the gc cycle of the one-step younger generation to end.
      if(gc.get_gen(this_generation - 1)->tracing_is_done()){
	 tracing_phase_counter = 2;
      }else{
	 tracing_phase_counter = 1;
      }    
      return(0);
   }else if(tracing_phase_counter < 3){
      return(0);
   }

   //#ifdef ALLOCATE_WHITE    
   // We synchronize the end of a GC cycle to that of younger generations.
   // Note: This is required only for white allocation, but the current
   // implementation, especially gc_flip requires this syncronization.
   if(!gc.get_gen(this_generation - 1)->tracing_is_done()){
      // This variable is for avoiding the awkward GC termination
      // failure. If the control reaches here, it means no more work
      // to be done except for an atomic scan of root set, but it
      // cannot terminate the GC cycle only because the younger
      // generatin GC hasn't terminated. If this flag is on, GC for
      // this generation keeps reaching here and checking whether
      // the younger generation has finished.
      no_more_work = 1;
      return(0);
   }
   //#endif     
   
   // Finally, if there is no more work to do, then scan the root set one
   // more time to see if we are done (this time the root set scan is atomic).

   // scan_root_atomic will return 1 if it grayed any objects,
   // otherwise it will return 0.

   if (scan_root_atomic()) {

      // generations.blacken_or_promote_grays
      // will return 1 if it is done with an increment of
      // tracing, and 0 if it finished tracing before the increment was
      // completed.

      if (blacken_or_promote_grays()) {
	 // Tracing is not done, so return 0.
	 return(0);
      }
   }

   // Tracing may add some igp pointer from the older generation, when it
   // promotes an object, but the object pointed by the igp must be grayed
   // at the time.
   assert(!gc.get_igpm()->scan_igp_list(this_generation));

   // The tracing is done, set the flag, increment the one-step older
   // generation's gc cycle counter and return 1.
   done_tracing = 1;
   tracing_phase_counter = 0;
   
   root.abstract_stable_root_reset();
   root.abstract_quasistable_root_reset();
   root.abstract_unstable_root_reset();

   // If the one-step older generation is in the second phase,
   if(gc.get_gen(this_generation + 1)->tracing_phase_counter != 0){
      // Increment the GC cycle counter.
      gc.get_gen(this_generation + 1)->tracing_phase_counter++;
   }
   // 1 represents the tracing is this generation is done.
   return(1);
}


//************************************************************************
//  Generation::trace_oldest_generation
//  The algorithm is as follows:
//
//  First, scan the root set incrementally.  For each pointer from the
//  stack, if the object pointed to is not shaded, then gray it.  Next,
//  see if there are any grays.  If so, then blacken those grays.  If
//  this completes one increment of work then just return.  Otherwise
//  proceed with the next root pointer.
//
//  Next the stored_into_list is incrementally scanned and each object
// pointe by this list is grayed (if it is not already shaded) and some
// tracing work is performed. If this completes an increment of work then
// just return. Otherwise proceed with the next element in the
// stored_into_list.
//
// After tracing all objects from root set, stored_into_list, we go on to
// the next phase of tracing. We set the tracing_phase to a positive
// number and return this time. After this, write_barrier and the younger
// generational tracers grays the white objects pointed by younger black
// objects.
//
// When we come back to this tracer, we blacken objects grayed by the
// write barrier and younger tracers. If there is no gray objects, then
// check the stored_into_list if there is a new entry. If there are some,
// do some blacking. Even if we have nothing to do, we have to keep
// returning without termination flag until the younger tracers finishes
// one whole GC cycle.
//
// If there is no gray object to blacken, we check the tracing phase.
// If we are in the first phase, we turn on the phase counter. The
// phase counter indicates how many GC cycle of one-step younger
// generation we have gone through, since we turn on the counter. If the
// phase counter shows that we have gone through at least one whole gc 
// cycle of one step younger generation, so now we are ready to finish
// the GC cycle.
//
// If there is no gray object to blacken, and if there is no untraced
// entry in the stored_into_list, scan the stack one more time
// atomically. If we find no objects to be traced, reset counters and
// return 1 as the termination flag. If we find some, we have to do some
// more tracing.
//
int generation::trace_oldest_generation(void){
   pos_ptr_addr temp;

   // This sets the work amount to be done in this increment.
   set_work_left_to_do(gc_scheduler
		       .get_work_per_increment(NUMBER_OF_GENERATIONS - 1));

   done_tracing = 0;
   
   // If the work amount to be done is zero, return.
   // If no_more_work is on, it means we have no more objects to scan and 
   // we can terminated this GC cycle only if the younger generation has
   // terminated its GC cycle. Since we have a chance to terminate if it is
   // on, we keep working. 
   if(get_work_left_to_do() <= 0 && !no_more_work){
      return(0);
   }else{
      no_more_work = 0;
   }

   // First, see if there already exist any grays.  If so, traverse them first

   if (any_more_grays()) {

      // Since there is work left to do, do some tracing.
      // generations.trace will return 1 if it is done with an
      // increment of tracing.
      
      if (blacken_grays()) {
	 // Tracing is not done, so return 0.
	 return(0);
      }
   }
   
   // Next, scan the root set.  The routine scan_root will search down the
   // root set, and if it finds a pointer to any white object, it will
   // gray that object and return 1.  If it scans the entire root set and has
   // found no pointers to white objects, it returns 0.  Note that the root
   // set is scanned incrementally, returning after the first white object is
   // grayed.  Once the root set has been scanned entirely, it is not scanned
   // again until finalization (see code below).

   while(scan_root()) {
      // Since there is work left to do, do some tracing.

      // Trace() will return 1 if it is done with an
      // increment of tracing.
      if (blacken_grays()) {
	 // Tracing is not done, so return 0.
	 return(0);
      }
   }

   // gray an object from the to do list.

   // get_ptr_stored_into_list will return a pointer to a location which
   // might contain a pointer to an object which might need to be grayed.
   // If there aren't any more pointers in the stored into list, then
   // get_ptr_stored_into_list will return 0.  cast_and_deref_ptr is a user
   // supplied function (see gcclient.h for more information) that takes the
   // pointer to the location that might hold a pointer to an object,
   // dereferences this first pointer, if there is a pointer at that memory
   // location, it casts that pointer into a valid C pointer, and returns it.
   // It returns the C pointer NULL if there isn't a valid pointer at that
   // location.

   while ((temp = get_ptr_stored_into_list())!=0) {
      gc_obj_addr ptr;
      gc_object_base *object;

      // We have gotten the next stored into location.
      // First, since we have a pointer to the location that was
      // stored into, and we want to know about the object pointed
      // to by the *pointer* stored *in* that location, we cast
      // and dereference this pointer.  If we get back something other
      // than NULL, then we have a valid C pointer to some object that
      // we might have to gray.

      if((ptr = cast_and_deref_ptr(temp)) != NULL) {
	 // First, we derive the header of this rval object.  There
	 // are two ways of doing this depending on whether the language
	 // system allows derived pointers.  One just subtracts off
	 // the language level header, and the other does a virtual
	 // function call.
#ifdef NO_DERIVED_POINTERS
	 object = (gc_object_base *)(((char *)ptr)-sizeof(gc_object_base));
#ifndef NDEBUG
	 object_manager *om = gc.get_object_manager(ptr);
	 gc_object_base *object1 = om->find_start_of_object(ptr);
	 assert(object == object1);
#endif //NDEBUG
#else
	 object_manager *om = gc.get_object_manager(ptr);
	 object = om->find_start_of_object(ptr);
#endif
	 // Next, determine if the rval object is already shaded.
	 // The pointed object should be in this generation. The value of
	 // the pointer variable may have been changed to point to an
	 // younger object, and after that, the young pointed object may
	 // die early before the enlisted pointer is referenced.
	 if(!object->is_shaded() && object->get_gen_num()==this_generation){
	    // If the rval object is not shaded, shade it and do
	    // some more tracing (by calling generations::trace).
#ifdef NO_DERIVED_POINTERS
	    object->get_containing_list()->gray_this_object(object);
#else
	    object->get_containing_list(om)->gray_this_object(object);
#endif

	    // generations::trace will return 1 if it is done with an
	    // increment of tracing.

	    if (blacken_grays()) {
	       // tracing is not done, so return 0.
	       return(0);
	    }
	 }
      }
   }

   if(tracing_phase_counter == 0){
      // tracing_phase == 0 means we're in the first tracing phase. 
      // Since reaching here means we have traced all objects reachable from
      // the root set (since we are in the oldest generation), we set the GC
      // cycle counter on. tracing_phase == 1 means we are in a drop-in
      // cycle. We trace the whole younger generations when tracing_phase ==
      // 2. If tracing_phase is larger than or equal to 3, we have gone
      // through at least one whole GC cycle, so we can go on to the
      // finalization.
      if(gc.get_gen(this_generation - 1)->tracing_is_done()){
	 tracing_phase_counter = 2;
      }else{
	 tracing_phase_counter = 1;
      }    
      return(0);
   }else if(tracing_phase_counter < 3){
      return(0);
   }

   //#ifdef ALLOCATE_WHITE    
   // We synchronize the end of a GC cycle to that of younger generations.
   // Note: This is required only for white allocation, but the current
   // implementation, especially gc_flip requires this syncronization.
   if(!gc.get_gen(this_generation - 1)->tracing_is_done()){
      // This variable is for avoiding the awkward GC termination
      // failure. If the control reaches here, it means no more work
      // to be done except for an atomic scan of root set, but it
      // cannot terminate the GC cycle only because the younger
      // generatin GC hasn't terminated. If this flag is on, GC for
      // this generation keeps reaching here and checking whether
      // the younger generation has finished.
      no_more_work = 1;
      return(0);
   }
   //#endif     
   
   // Finally, if there is no more work to do, then scan the root set one
   // more time to see if we are done (this time the stack root is atomic).

   // scan_root_atomic will return 1 if it grayed any objects,
   // otherwise it will return 0.

   if (scan_root_atomic()) {

      // generation::trace will return 1 if it is done with an increment of
      // work, and 0 if it finished tracing before the increment was
      // completed.

      if (blacken_grays()) {
	 // Garbage collection is not done, so return 0.
	 return(0);
      }
   }

   done_tracing = 1;
   tracing_phase_counter = 0;

   root.abstract_stable_root_reset();
   root.abstract_quasistable_root_reset();
   root.abstract_unstable_root_reset();

   // The collection is done, so return 1.  
   return(1);
}
#endif // GENERATIONAL




//***************************************************************************
// generation::any_more_work_to_do
//
// This routine searches all of the size classes of this generations
// looking for any gray objects.  If any gray objects are found, then
// this routine returns 1 signifying that there is still some work to be
// done.  0 is returned if there were no gray objects found.
//
// This routine is mainly called from the single generation full collect
// function.
int generation::any_more_work_to_do(void)
{
   // If we are not done collecting, the of course there is some more
   // work left to do.
   if (!done_tracing) {
      return(1);
   } else {
      // If there are into stored into object, then there is work left
      // to do.
      if (stored_into_list_head != stored_into_list_tail) {
	 return(1);
      } else {
	 // Finally, look for some more grays.
	 return(any_more_grays());
      }
   }
}


#ifndef INLINES
#include <rtgc/gen1.ci>
#include <rtgc/gen2.ci>
#endif
