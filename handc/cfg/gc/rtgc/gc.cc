#include <values.h>
#include <iostream.h>
#include <assert.h>

#include <rtgc/objmgr.hh>
#include <rtgc/gcclient.h>
#include <rtgc/allocator.hh>
#include <rtgc/gc.hh>
#ifdef SPTR_CONTAINS_OBJ_HDR
#include <rtgc/sp.hh>
#endif
#ifdef INLINES
#include <rtgc/inlines.hh>
#endif
#ifdef DEBUG_LEVEL2
#include <rtgc/pointers.hh>
#endif
#include <rtgc/scheduler.hh>

// FILE: gc.cc
//
// This file contains the garbage collector code that for what ever reason
// can't be inlined.  All of the inlineable code is in the file gc.ci
// and the description for the classes used at this level of the garbage
// collector are in the file gc.hh.

// The variable gc is an instance of the garbage collector.
// The idea being that you can have several instances of the garbage
// collector active in memory at one time, each managing their own heap.
// Unfortunately, on a single processor system, there is only one root set,
// and this is likely to cause problems if there were two instances of the
// garbage collector in use.  Also, some places in the code make use of
// the variable gc.  So, since they expect there to be only one garbage
// collector named gc, there may be problems trying to expand the system
// to use more than one garbage collector.

#ifdef HEAP_ALLOCATED_GC
garbage_collector* gcptr = new garbage_collector;
garbage_collector& gc = *gcptr; // an instance of the garbage collector
#else
garbage_collector gc;
#endif

// For efficiency *only*, these two variables have been placed into registers.

#ifdef __GNUC__
register object_manager* *object_managers=gc.object_managers_array;
#else
object_manager* *object_managers=gc.object_managers_array;
#endif

//**************************************************************************
// heap_round
//
// This routine takes a pointer, and returns a derived pointer which points
// the the start of the next page (as defined by PAGE_MASK in the file
// config.hh) of memory.  This routine is used so that all heap pages 
// fall on even logical page boundries (power of 2 boundries).

char *heap_round(void *start_of_heap) 
{
   return((char *)(((char *)start_of_heap) + 
		   GC_PAGE_SIZE - 
		   (((unsigned int)start_of_heap) & PAGE_MASK)));
}



//***************************************************************************
// profile_write_barrier
//
// This routine profiles things about the write barrier like how many
// time the lval is one color and the rval is another color, and how many
// time the lval is a root.
// 
// If you are using the garbage collector for a language without derived
// pointers (NO_DERIVED_POINTERS is defined) then pass the lobject and
// robject to this routine.  If not, then just pass NULL in for those arguments
// and this routine will compute the lobject and robject for you from the
// lvalue and rvalue respectively.

#ifdef  PROFILE_WRITE_BARRIER
void garbage_collector::profile_write_barrier(pos_ptr_addr lvalue,
					      gc_obj_addr lobject,
					      gc_obj_addr rvalue,
					      gc_obj_addr robject)
{

#ifdef GC_TURN_OFF_ABLE
   // The garbage collector only gets turned off if we are allocating black.
   if(!gc_turned_off) {
#endif

      // first count the root cases.
      
      if (is_root(lvalue)) {
	 if(is_root(pos_ptr_addr(rvalue))) {
	    both_root++;
	 } else {
	    lval_root_rval_not++;
	 }
      } else {
	 if(is_root(pos_ptr_addr(rvalue))) {
	    assert(rvalue == NULL);
	    rval_root_lval_not++;
	 } else {
	    neither_root++;
	 }
      }
#ifdef GC_TURN_OFF_ABLE
   } else {
      num_times_gc_turned_off++;
   }
#endif

   // Next check for blackness and whiteness of the lval and rval.
   
   object_manager *rval_object_manager;
   object_manager *lval_object_manager;
   
#ifdef GC_TURN_OFF_ABLE
   // The garbage collector only gets turned off if we are allocating black.
   if(!gc_turned_off) {
#endif
      
      // We don\'t record stores into the stack.
      // These are handled when we scan the stack.
      
#ifndef NO_ROOT_WRITE_BARRIER
      // If the lvalue is a root, then it is shaded and we are done.
      if (!is_root(lvalue)) {
#endif
	 // In this case, we know that the lvalue is not a root.
	 if(!is_root(pos_ptr_addr(rvalue))) {


#ifdef NO_DERIVED_POINTERS
	    gc_object_base *start_of_rval_object = robject;		
#else
	    // In this case, we know that the rvalue is not a root.
	    // derive the start of the object
	    rval_object_manager =
	       get_object_manager_NOT_ROOT((void *)rvalue);
	    
	    gc_object_base *start_of_rval_object = 
	       rval_object_manager->find_start_of_object((void *)rvalue);
#endif

#ifdef NO_DERIVED_POINTERS
	    gc_object_base *start_of_lval_object = lobject;
#else
	    lval_object_manager = get_object_manager_NOT_ROOT(lvalue);		
	    gc_object_base *start_of_lval_object = 
	       lval_object_manager->find_start_of_object((void *)lvalue);
#endif
	    
	    if (start_of_rval_object->is_shaded()) {
	       lval_object_manager = get_object_manager_NOT_ROOT(lvalue);
	       
	       if (start_of_lval_object->is_shaded()) {
		  both_shaded++;
	       } else {
		  rval_shaded_lval_not++;
	       }
	    } else {
	       if (start_of_lval_object->is_shaded()) {
		  lval_shaded_rval_not++;
	       } else {
		  both_not_shaded++;
	       }
	    }
	 }
#ifndef NO_ROOT_WRITE_BARRIER
      }
#endif
#ifdef GC_TURN_OFF_ABLE
   }
#endif
}
#endif //  PROFILE_WRITE_BARRIER    


gc_obj_addr garbage_collector::next_not_known_free_object(void)
{
   if (not_known_free_object_iterator_generation_number >= NUMBER_OF_GENERATIONS)
   {
      return(NULL);
   } else {
      gc_obj_addr next_object =
	 gc.get_gen(not_known_free_object_iterator_generation_number)
	    ->get_color_set(not_known_free_object_iterator_colorset_number)
	       ->next_not_known_free_object();
      while (next_object == NULL)
      {
	 not_known_free_object_iterator_colorset_number++;
	 if(not_known_free_object_iterator_colorset_number == NUM_SIZE_CLASSES)
	 {
	    not_known_free_object_iterator_colorset_number = 0;
	    not_known_free_object_iterator_generation_number++;
	    if(not_known_free_object_iterator_generation_number == 
	       NUMBER_OF_GENERATIONS) 
	    {
	       return(NULL);
	    } else {
	       gc.get_gen(not_known_free_object_iterator_generation_number)
		  ->get_color_set(not_known_free_object_iterator_colorset_number)
		     ->reset_not_known_free_object_iterator();
	       next_object = 
		  gc.get_gen(not_known_free_object_iterator_generation_number)
		     ->get_color_set(not_known_free_object_iterator_colorset_number)
			->next_not_known_free_object();
	    }
	 } else {
	    gc.get_gen(not_known_free_object_iterator_generation_number)
	       ->get_color_set(not_known_free_object_iterator_colorset_number)
		  ->reset_not_known_free_object_iterator();
	    next_object = 
	       gc.get_gen(not_known_free_object_iterator_generation_number)
		  ->get_color_set(not_known_free_object_iterator_colorset_number)
		     ->next_not_known_free_object();
	 }
      }
      return(next_object);
   }
}


gc_obj_addr garbage_collector::next_dead_object(void)
{
   if (dead_object_iterator_generation_number >= NUMBER_OF_GENERATIONS)
      {
	 return(NULL);
      } else {
	 gc_obj_addr next_object =
	    gc.get_gen(dead_object_iterator_generation_number)
	       ->get_color_set(dead_object_iterator_colorset_number)
		  ->next_dead_object();
	 while (next_object == NULL)
	    {
	       dead_object_iterator_colorset_number++;
	       if(dead_object_iterator_colorset_number == NUM_SIZE_CLASSES)
		  {
		     dead_object_iterator_colorset_number = 0;
		     dead_object_iterator_generation_number++;
		     if(dead_object_iterator_generation_number == 
			NUMBER_OF_GENERATIONS) 
			{
			   return(NULL);
			} else {
			   gc.get_gen(dead_object_iterator_generation_number)
			      ->get_color_set(dead_object_iterator_colorset_number)
				 ->reset_dead_object_iterator();
			   next_object = 
			      gc.get_gen(dead_object_iterator_generation_number)
				 ->get_color_set(dead_object_iterator_colorset_number)
				    ->next_dead_object();
			}
		  } else {
		     gc.get_gen(dead_object_iterator_generation_number)
			->get_color_set(dead_object_iterator_colorset_number)
			   ->reset_dead_object_iterator();
		     next_object = 
			gc.get_gen(dead_object_iterator_generation_number)
			   ->get_color_set(dead_object_iterator_colorset_number)
			      ->next_dead_object();
		  }
	    }
	 return(next_object);
      }
}



//***************************************************************************
// garbage_collector::gc_flip
//
// The following routine is something of a mess, because the final version
// of the collector which insures hard realtime constraints hasn't yet
// been settled on.
//
// This routine is called after tracing of all live objects is complete.
// It tells every size class to collect its garbage, and then computes
// a throttle setting for the next garbage collector cycle.

void garbage_collector::gc_flip(void)
{
   INT_32 i,j;

//   cout << "flip" << endl;
//   safe_for_is_obj_dead();

#ifdef GC_MONITOR
   monitor();
#endif

#ifdef DEBUG_LEVEL2
   consistency_check();
   check_soundness();
#endif
   
#ifdef SNAPSHOT_WRITE_BARRIER
   first_collection_increment = true;
#endif

   // call the registered gc_flip callback function
   (*gc_flip_callback_function)();

#ifdef GC_TURN_OFF_ABLE
   // The garbage collector only gets turned off if we are allocating black.
   // This happens only in the non generational black allocation. 
   gc_turned_off = 0; // turn the garbage collector back on.
#endif
   for(i=0;i<NUMBER_OF_GENERATIONS;i++) {
      // If the ith generation's gc tracing is not done, we don't
      // reclaim the objects in this generation.
      if(!get_gen(i)->tracing_is_done()){
	 continue;
      }

      for(j=0;
	  j<NUM_SIZE_CLASSES;
	  j++)
	 {
	    // Change the white objects into free objects in ith generation.
	    // Don't move the reclaimed garbage to the youngest generation.
	    // They are move to the youngest generation when no more free
	    // objects are available in the younger generations.
	    gc.get_gen(i)->get_color_set(j)->reclaim_garbage();

	    //	      MAX_LIVE / 
	    //	      ((((gc.get_gen(0)->get_color_set(j)->number_of_objects *
	    //		  gc.get_gen(0)->get_color_set(j)->size) - 
	    //		 MAX_LIVE) / 2.0) -
	    // do I really need this fudge factor?
	    //	       AMOUNT_TO_ALLOCATE_PER_INCREMENT);
	    //       gc.get_gen(0)->get_color_set(j)->number_of_free = 
	    //	      ((gc.get_gen(0)->get_color_set(j)->number_of_objects - 
	    //		(MAX_LIVE / gc.get_gen(0)->get_color_set(j)->size))/2) -
	    //		(AMOUNT_TO_ALLOCATE_PER_INCREMENT / 
	    //		 gc.get_gen(0)->get_color_set(j)->size);

//	   
//	       // Check if there is at least some minimum amount of free
//	       // memory left in the system.  If not, then snarf some more.
//	       if (gc.get_gen(i)->get_color_set(j)->number_of_objects != 0) {
//		  if ((gc.get_gen(i)->get_color_set(j)->number_of_non_black
//		       * (1<<gc.get_gen(i)->get_color_set(j)->size_class))
//		      < (NUM_PAGES_TO_SNARF_AT_A_TIME * GC_PAGE_SIZE))
//		     {
//			gc.get_gen(i)->get_color_set(j)->snarf_more_memory();
//		     }
//	       }

	 }
#ifdef GENERATIONAL
      get_gen(i)->toggle_current_shade_color();
      // Switch the igp lists containing the ipg from ith generation.
      if(i != 0){
	 get_igpm()->switch_igp_lists(i);
      }
      // Reset the index of the igp lists containing the igps into ith
      // generation.
      if(i != NUMBER_OF_GENERATIONS - 1){
	 get_igpm()->reset_igp_lists(i);
      }

      gc_scheduler.reset_at_gc_flip();
#else
      gc.toggle_current_shade_color();
#endif

#ifdef TEST_FRAGMENTATION
      test_fragmentation(i);
#endif
   }

   // Set the global variable allocation_color to the appropriate color.
#ifdef GENERATIONAL

#ifdef ALLOCATE_BLACK
   allocation_color = get_gen(0)->get_current_shade_color();
#else 
   allocation_color = !get_gen(0)->get_current_shade_color();
#endif

#else // non-generational case

#ifdef ALLOCATE_BLACK
   allocation_color = gc.get_current_shade_color();
#else 
   allocation_color = !gc.get_current_shade_color();
#endif

#endif // ifdef GENERATIONAL
}

#ifdef TEST_FRAGMENTATION
garbage_collector::test_fragmentation(int i){
   for(INT_32 l=0;l<NUM_SIZE_CLASSES;l++) {
      gc.get_gen(i)->get_color_set(l)->num_pages = 0;
      for(INT_32 n=0;n<(GC_PAGE_SIZE/32);n++) {
	 gc.get_gen(i)->get_color_set(l)->num_pages_with_n_objects[n]
	    = 0;
      }
   }
   INT_32 page_count=0;
   INT_32 page_empty = 0;
   INT_32 page_full = 0;
   INT_32 page_25 = 0;
   INT_32 page_50 = 0;
   INT_32 page_75 = 0;
   INT_32 page_100 = 0;
   for(INT_32 obj_num=1;obj_num < NUM_HEAP_PAGES;obj_num++) {
      object_manager *om = gc.object_managers_array[obj_num];
      if(om != NULL) {
	 assert((om->size_class >=0) && (om->size_class < NUM_SIZE_CLASSES));
	 if (om->num_objects_on_page == -1) { // large object
	    /* for now, don\'t count large objects in the fragmentation counts
	       INT_32 number_of_pages = ((1 << om->size_class)/GC_PAGE_SIZE);
	    cout << "num pages is " << number_of_pages << endl;
	    if (om->num_objects_in_use != 0) { // first page of large object
	       cout << "object is in use" << endl;
	       INT_32 number_of_pages = ((1 << om->size_class)/GC_PAGE_SIZE);
	       gc.get_gen(i)->get_color_set(om->size_class)->num_pages += number_of_pages;
	       page_count += number_of_pages;
	       obj_num += number_of_pages;
	       page_full += number_of_pages;
	    } else { // first page of now empty large object
	       cout << "object is not in use" << endl;
	       gc.get_gen(i)->get_color_set(om->size_class)->num_pages += number_of_pages;
	       page_count += number_of_pages;
	       obj_num += number_of_pages;
	       page_empty += number_of_pages;
	    }
	    */
	 } else {  // page of small objects
	    gc.get_gen(i)->get_color_set(om->size_class)->num_pages++;
	    page_count++;
	    assert((om->num_objects_in_use < GC_PAGE_SIZE) && (om->num_objects_in_use >= 0));
	    gc.get_gen(i)->get_color_set(om->size_class)->num_pages_with_n_objects[om->num_objects_in_use]++;
	    if(om->num_objects_in_use == 0) {
	       page_empty++;
	    } else if ((om->num_objects_in_use > 0) &&
		       (om->num_objects_in_use <= (.25 * om->num_objects_on_page)))
	       {
		  page_25++;
	       } else if ((om->num_objects_in_use > (.25 * om->num_objects_on_page)) &&
			  (om->num_objects_in_use <= (.50 * om->num_objects_on_page)))
		  {
		     page_50++;
		  } else if ((om->num_objects_in_use > (.50 * om->num_objects_on_page)) &&
			     (om->num_objects_in_use <= (.75 * om->num_objects_on_page)))
		     {
			page_75++;
		     } else if ((om->num_objects_in_use > (.75 * om->num_objects_on_page)) &&
				(om->num_objects_in_use < (1.0 * om->num_objects_on_page)))
			{
			   page_100++;
			} else if (om->num_objects_in_use == om->num_objects_on_page) {
			   page_full++;
			}
	 }
	 om->num_objects_in_use = 0;
	 assert((om->size_class >=0) && (om->size_class < NUM_SIZE_CLASSES));
      }
   }
   fprintf(gc.fragmentation_out,"%d ",page_count);
   fprintf(gc.fragmentation_out,"%d ",page_empty);
   fprintf(gc.fragmentation_out,"%d ",page_empty+page_25);
   fprintf(gc.fragmentation_out,"%d ",page_empty+page_25+page_50);
   fprintf(gc.fragmentation_out,"%d ",page_empty+page_25+page_50+page_75);
   fprintf(gc.fragmentation_out,"%d ",page_empty+page_25+page_50+page_75+page_100);
   fprintf(gc.fragmentation_out,"%d\n",page_empty+page_25+page_50+page_75+page_100+page_full);
   //	fprintf(gc.fragmentation_out,"%d %d %d %d %d %d %d\n",page_count,page_empty,page_25,page_50,page_75,page_100,page_full);

   //	fprintf(gc.fragmentation_out,"(GC %d\n",flip_number++);
   //	for (l=0;l<NUM_SIZE_CLASSES;l++) {
   //	    fprintf(gc.fragmentation_out,"    (%d %d",l,gc.get_gen(i)->get_color_set(l)->num_pages);
   //	    for(INT_32 n=0;n<(GC_PAGE_SIZE/32);n++) {
   //		fprintf(gc.fragmentation_out," %d ",gc.get_gen(i)->get_color_set(l)->num_pages_with_n_objects[n]);
   //	    }
   //	    fprintf(gc.fragmentation_out,")\n");
   //	}
   //	fprintf(gc.fragmentation_out,")\n");
   fflush(gc.fragmentation_out);
   //		if (om->num_objects_on_page == -1) {
   //		    amount_in_use += GC_PAGE_SIZE;
   //		    cout << "page " << obj_num << " " << flush;
   //		    cout << "100% in use" << endl;
   //		} else {
   //		    amount_alloced += GC_PAGE_SIZE;
   //		    amount_in_use += ((INT_32 ) (GC_PAGE_SIZE 
   //		                  * (((float)om->num_objects_in_use) / 
   //				     ((float)om->num_objects_on_page))));
   //		    amount_in_use -= ((int) (0.25 * (GC_PAGE_SIZE 
   //						     * (((float)om->num_objects_in_use) / 
   //							((float)om->num_objects_on_page)))));
   //
   //		}
   //		om->num_objects_in_use = 0;
   //	    }
   //	}
   //	fprintf(gc.fragmentation_out,"%f\n",((float) amount_in_use) / ((float) amount_alloced));

}
#endif  TEST_FRAGMENTATION

// *************************************************************************
// garbage_collector::collect                                            
//                                                                       
// This routine is the top level of the garbage collector.               
// It figures out which generation needs to be collected, and then       
// calls the garbage collectors for that generation and all generations  
// younger than that generation                                          
//
// This routine returns 1 if a collection cycle finished, and 0 if only 
// an increment finished.
//                                                                       
//NOTE:  This routine should *not* be inline, otherwise every class defined
//       in the system will get the entire garbage collector as part of
//       its new operator.

int garbage_collector::collect(void) {

#ifdef ZEN_TIMER
   gc_increment_timer.timer_on();
#endif
//   cout << "collecting" << endl;

#if NUMBER_OF_GENERATIONS == 1  

   if(!get_gen(0)->trace_single_generation()){
      // If the tracing was not finished in this increment,
      // trace_single_generation returns 0, so we return to the calling
      // function. Otherwise, go on to the following sentences since
      // we must have finished the gc cycle.
#ifdef ZEN_TIMER
      gc_increment_timer.timer_off();
#endif
      return(0);
   }

#else // if we have multiple generations

   get_gen(0)->trace_youngest_generation();

   for(int i = 1; i < NUMBER_OF_GENERATIONS - 1; i++) {
      get_gen(i)->trace_mid_generation();
   }
   
   get_gen(NUMBER_OF_GENERATIONS - 1)->trace_oldest_generation(); 
   
   // The end of the older GC cycles are synchronized to that of younger
   // generations in the current implementaion. So, we call the gc_flip()
   // only when the youngest GC cycle finishes. Otherwise, we simply return.
   // (Of course, the end of the youngest GC cycle does not mean those of
   // older generations.)
   // 
   // Note: This may change in future, especially in black allocate, where
   // the synchronization is not required.
   
   if(!get_gen(0)->tracing_is_done()){
#ifdef ZEN_TIMER
      gc_increment_timer.timer_off();
#endif
      return(0);
   }

#endif // Multiple generations

#ifdef GC_TURN_OFF_ABLE    

   // turn off gc until a size class has allocated all of its memory.
   // We only want to do this when we are allocating without space
   // constraints and only when allocating black.
   gc_turned_off = 1;

#else 

   // if we finish garbage collecting, we immediately flip.
   gc.gc_flip();

#endif
#ifdef DEBUG_LEVEL2
   consistency_check();
#endif
#ifdef ZEN_TIMER
   gc_increment_timer.timer_off();
#endif
   return(1);
}


// the global variable gc_initialized is initialized to 0 (by default by the
// operating system).  While the garbage collector is being initialized, 
// various gc routines look at the value of this variable.  If it is 0,
// then they know that they are being initialized.  It is set to non zero
// after initialization, so that these routines know that they are being
// called in the normal course of operation.  The main use of this feature
// is to determine if a pointer is a global variable.

int gc_initialized;

//****************************************************************************
//* garbage_collector::init                                                  *
//*                                                                          *
//* This routine should be called as the first line of main for any program  *
//* that needs to run under garbage collection.  It sets a flag that tells   *
//* the system that the first line of main has executed, and that any smart  *
//* pointers created after this point must not be in global static variables.*

void garbage_collector::init(int argc, const char **argv)
{
   cout << VERSION_STRING << endl;

   gc_initialized = 1; // we are done initing the garbage collectors.  No
   // global static PTR_Any variables will be seen from
   // this point on

   init_gcclient(argc,argv);

   // Resetting GC informations in each generation.
   for(int i = 0; i < NUMBER_OF_GENERATIONS ; i++){
      get_gen(i)->init(i);
   }


   // This routine initializes the table of the pointers to the colorsets
   // for the allocation.
   gc_allocation_table_initialize();

   // Set the global variable allocation_color to the appropriate color.
#ifdef GENERATIONAL

#ifdef ALLOCATE_BLACK
   allocation_color = get_gen(0)->get_current_shade_color(); 
#else 
   allocation_color = !get_gen(0)->get_current_shade_color(); 
#endif

#else // non-generational case

#ifdef ALLOCATE_BLACK
   allocation_color = gc.get_current_shade_color(); 
#else 
   allocation_color = !gc.get_current_shade_color(); 
#endif

#endif // ifdef GENERATIONAL

   // amount_allocated is a timer for when we need to do an increment of
   // collection work.  It is initialized to the maximum amount that can
   // be allocated before on increment of collection needs to be performed.
   // This variable is then decremented by the number of bytes allocated
   // at each call to new.  After an increment of collection, it is reset
   // to this initial value.
   amount_allocated = AMOUNT_TO_ALLOCATE_PER_INCREMENT;
}

//***************************************************************************
//* garbage_collector::garbage_collector                                    *
//*                                                                         *
//* This is the constructor for instances of the garbage collector.         *
//* It doesn't do anything particularly out of the ordinary.                *

garbage_collector::garbage_collector():
#ifdef ZEN_TIMER
    gc_increment_timer("gc_increment.time"),
    write_barrier_timer("write_barrier.time"),
    gray_timer("gray.time"),
#endif
    first_collection_increment(true)
{
   // Before allocating the heap, we want to allocate garbage collector
   // objects, like each generation including and the igp manager

   for(int i = 0; i < NUMBER_OF_GENERATIONS; i++){
      generations[i] = new generation(i);
   }

#ifdef GENERATIONAL    
   igpm = new igp_manager;
#endif

#ifndef GENERATIONAL
   // only in the non-generational version of the collector, is the
   // current shade color stored in the gc object.
   current_shade_color = INIT_SHADING_COLOR;
#endif

   // Note: The next lines of code just give us some bounds on the heap.
   // Only for the hard real-time collector do we use these lines to get
   // any actual memory for the system.
#ifdef HARD_REAL_TIME
   // When compiling for hard real-time, we want to preallocate the entire
   // heap.  So we sbrk the entire heap here.
   start_of_heap = heap_round(sbrk(NUM_HEAP_PAGES*GC_PAGE_SIZE));
   end_of_heap = ((char *)start_of_heap)+(NUM_HEAP_PAGES-1)*GC_PAGE_SIZE;

#else
   // When compiling for soft real-time, we don't care about preallocating
   // the entire heap.  So we just mark the begining of the heap by allocating
   // one page.
   start_of_heap = heap_round(sbrk(GC_PAGE_SIZE));
   end_of_heap = ((char *)start_of_heap)+(NUM_HEAP_PAGES-1)*GC_PAGE_SIZE;
#endif



   assert(start_of_heap);
   if(!start_of_heap) {
   cerr << "Sorry, couldn't get enough heap space" << endl;
   exit(0);
}

   gc_flip_callback_function =  gc_flip_callback_function_default;

   // Alloc and delete count are for debugging only.
   alloc_count = 0;
   delete_count = 0;
#ifdef TEST_FRAGMENTATION
   fragmentation_out = fopen("frag.dat","w");
   //    fprintf(fragmentation_out,"(\n");
#endif
#ifdef TEST_LIFETIME_DIST
   lifetime_file = fopen("lifetime.dat","w");
#endif

#ifndef HARD_REAL_TIME
   first_sbrk_pointer_index = 0;
   middle_sbrk_pointer_index = 0;
   last_sbrk_pointer_index = 0;
#endif

   char *environment_string = getenv("THROTTLE_SETTING");
   float environment_throttle_setting ;
   if (environment_string != NULL) {
      environment_throttle_setting = atof(environment_string);
   } else {
      environment_throttle_setting = DEFAULT_THROTTLE_SETTING;
   }
   cout << "throttle setting is " << environment_throttle_setting << endl;
   
   // an initial throttle setting.
   gc_scheduler.set_throttle_setting(environment_throttle_setting); 
   
   // get the current start of the heap.
   assert(((int)start_of_heap) != -1);
   assert(end_of_heap > start_of_heap);  // incase some bobo changes the decl
   // ordering on these two variables.
   
   // set stack and global object manager
   the_root_object_manager = new object_manager(0,0);
   
   high_water_mark = ((char *)start_of_heap);
   
#ifdef PROFILE_WRITE_BARRIER
   num_times_gc_turned_off=0;
   
   both_root=0;
   lval_root_rval_not=0;
   rval_root_lval_not=0;
   neither_root=0;
   both_shaded=0;
   rval_shaded_lval_not=0;
   lval_shaded_rval_not=0;
   both_not_shaded=0;
#endif
   
#ifdef PROFILE_PAUSES
   write_barrier_min_pause = MAXINT;
   write_barrier_max_pause = 0;
   gc_safe_point_min_pause = MAXINT;
   gc_safe_point_max_pause = 0;
#endif
   
}


garbage_collector::~garbage_collector()
{

#ifdef ZEN_TIMER
   cout << endl << "timing for the garbage collector" << endl;
   gc_increment_timer.timer_print();
   cout << endl << "timing for the write barrier" << endl;
   write_barrier_timer.timer_print();
   cout << endl << "timing for the gray_this_object routine" << endl;
   gray_timer.timer_print();
   cout << endl;
#endif // zen_timer

#ifdef TEST_FRAGMENTATION
   //    fprintf(fragmentation_out,")\n");
   fclose(fragmentation_out);
#endif

#ifdef TEST_LIFETIME_DIST
   fclose(lifetime_file);
   cout << "alloc count is " << alloc_count << endl;
   cout << "delete count is " << delete_count << endl;
#endif

#ifdef PROFILE_WRITE_BARRIER
   cout << "number of times the gc was turned off "
      << num_times_gc_turned_off << endl;
   cout << "both roots " << both_root << endl;
   cout << "lval is root, rval is not " << lval_root_rval_not << endl;
   cout << "rval is root, lval is not " << rval_root_lval_not << endl;
   cout << "neither are roots " << neither_root << endl;
   cout << "both are shaded " << both_shaded << endl;
   cout << "rval is shaded lval is not " << rval_shaded_lval_not << endl;
   cout << "lval is shaded rval is not " << lval_shaded_rval_not << endl;
   cout << "neither is shaded " << both_not_shaded << endl;
#endif

#ifdef PROFILE_PAUSES
   cout << "minimum write barrier pause was " << write_barrier_min_pause
      << " microseconds" << endl;
   cout << "maximum write barrier pause was " << write_barrier_max_pause
      << " microseconds" << endl;
#endif

   cout << "heap used " << (unsigned int)sbrk(0)-(unsigned int)start_of_heap 
        << " bytes" << endl;
}


//****************************************************************************
// count_white & count_black
//
// The next two routines (count_white & count_black) are used for profiling
// information only, and have no use in the running system.

void count_white(void)
{
   static INT_32 whites = 0;
   whites++;
}

void count_black(void)
{
   static INT_32 blacks = 0;
   blacks++;
}


// This function is the default function that is called everytime a
// gc_flip happens.  It is a noop function.

void gc_flip_callback_function_default(void)
{};


#ifdef DEBUG_LEVEL2
void garbage_collector::check_soundness(void){
   gc_object_base *ptr;

   cout << "Begin checking soundness..." ;

   for(int i = 0; i < NUMBER_OF_GENERATIONS; i++){
      for(int j = 0; j < NUM_SIZE_CLASSES; j++){
	 if(get_gen(i)->tracing_is_done()){
	    assert(get_gen(i)->get_color_set(j)->gray
		   == get_gen(i)->get_color_set(j)->scan);
	 }else if(i > 0){
	    for(ptr = get_gen(i)->get_color_set(j)->gray;
		ptr != get_gen(i)->get_color_set(j)->scan;
		ptr = ptr->get_next()){
	       assert(ptr->is_shaded());
	       follow_pointers_check_from_non_black(ptr);
	    }
	 }
#ifdef ALLOCATE_BLACK	    
	 for(ptr = get_gen(i)->get_color_set(j)->scan;
	     ptr != get_gen(i)->get_color_set(j)->free;
	     ptr = ptr->get_next()){
	    assert(ptr->is_shaded());
	    follow_pointers_check_from_black(ptr);
	 }

	 for(ptr = get_gen(i)->get_color_set(j)->white;
	     ptr != get_gen(i)->get_color_set(j)->tail;
	     ptr = ptr->get_next()){
	    assert(!ptr->is_shaded());
	    if(i > 0){
	       follow_pointers_check_from_non_black(ptr);
	    }
	 }
#endif //  ALLOCATE_BLACK	     
#ifdef ALLOCATE_WHITE	    
	 for(ptr = get_gen(i)->get_color_set(j)->scan;
	     ptr != get_gen(i)->get_color_set(j)->white;
	     ptr = ptr->get_next()){
	    assert(ptr->is_shaded());
	    follow_pointers_check_from_black(ptr);
	 }
	 for(ptr = get_gen(i)->get_color_set(j)->white;
	     ptr != get_gen(i)->get_color_set(j)->free;
	     ptr = ptr->get_next()){
	    assert(!ptr->is_shaded());
	    if(i > 0){
	       follow_pointers_check_from_non_black(ptr);
	    }
	 }
#endif
      }
   }
   cout << "Done\n";
}

void garbage_collector::shade_check_from_black(pos_ptr_addr ptr_addr,
					       gc_obj_addr ptr_val)
{
   gc_object_base *pointing = pointing_object;
   gc_object_base *pointed;
#ifdef NO_DERIVED_POINTERS
   pointed = (gc_object_base *)(((char *)ptr_val)-sizeof(gc_object_base));
#else
   object_manager *om = gc.get_object_manager(ptr_val);
   pointed = om->find_start_of_object(ptr_val);
#endif
   
   if(pointing->get_gen_num() > pointed->get_gen_num()){
      if(gc.get_gen(pointed->get_gen_num())->tracing_is_done()){
	 assert(pointed->is_shaded());
      }
   }else if(pointing->get_gen_num() == pointed->get_gen_num()){
      if(gc.get_gen(pointed->get_gen_num())->tracing_is_done()){
	 assert(pointed->is_shaded());
      }
   }else{
      if(gc.get_gen(pointed->get_gen_num())->get_tracing_phase() >= 3){
	 assert(pointed->is_shaded());
      }
   }
}

// If you know that pointing object is in the youngest generaion, you
// don't have to call this function to check the soundness.
void garbage_collector::shade_check_from_non_black(pos_ptr_addr ptr_addr,
						   gc_obj_addr ptr_val)
{
   gc_object_base *pointing = pointing_object;
   gc_object_base *pointed;
#ifdef NO_DERIVED_POINTERS
   pointed = (gc_object_base *)(((char *)ptr_val)-sizeof(gc_object_base));
#else
   object_manager *om = gc.get_object_manager(ptr_val);
   pointed = om->find_start_of_object(ptr_val);
#endif

   if(pointing->get_gen_num() > pointed->get_gen_num()
      && gc.get_gen(pointed->get_gen_num())->tracing_is_done()){
      assert(pointed->is_shaded());
   }
}

void garbage_collector::consistency_check(void){
   gc_object_base *obj,*prev;
   color_set *cset;

   cout << "Begin checking consistency..." ;
   for(int i = 0; i < NUMBER_OF_GENERATIONS; i++){
      for(int j = 0; j < NUM_SIZE_CLASSES; j++){
	 cset = get_gen(i)->get_color_set(j);
	 prev = NULL;
	 for(obj = cset->gray;
	     obj != cset->scan;
	     prev=obj,obj=obj->get_next()){
	    assert(obj->is_shaded());
	    assert(obj->get_gen_num() == i);
	    assert(prev == obj->get_previous());
	 }
#ifdef ALLOCATE_BLACK
	 // Now obj == cset->scan must hold.
	 for(; obj != cset->free; prev=obj,obj=obj->get_next()){
	    assert(obj->is_shaded());
	    assert(obj->get_gen_num() == i);
	    assert(prev == obj->get_previous());
	 }
	 // Now obj == cset->free
	 for(; obj != cset->white; prev=obj,obj=obj->get_next()){
	    assert(prev == obj->get_previous());
	 }
	 // Now obj == cset->white
	 for(; obj != cset->tail; prev=obj,obj=obj->get_next()){
	    assert(!obj->is_shaded());
	    assert(obj->get_gen_num() == i);
	    assert(prev == obj->get_previous());
	 }	    
#endif  ALLOCATE_BLACK
#ifdef ALLOCATE_WHITE
	 // Now obj == cset->scan must hold.
	 for(; obj != cset->white; prev=obj,obj=obj->get_next()){
	    assert(obj->is_shaded());
	    assert(obj->get_gen_num() == i);
	    assert(prev == obj->get_previous());
	 }
	 // Now obj == cset->white
	 for(; obj != cset->free; prev=obj,obj=obj->get_next()){
	    assert(!obj->is_shaded());
	    assert(obj->get_gen_num() == i);
	    assert(prev == obj->get_previous());
	 }	    
	 // Now obj == cset->free
	 for(; obj != cset->free; prev=obj,obj=obj->get_next()){
	    assert(prev == obj->get_previous());
	 }
#endif  ALLOCATE_WHITE
      }
   }
   cout << "Done\n" ;
}
#endif
#ifdef GC_MONITOR
void garbage_collector::monitor(void){
   int count;
   cout << "\tobjects\t\tnon blacks\tfree objects\tdone tracing\n"; 
   
   for(int i = 0; i < NUMBER_OF_GENERATIONS; i++){
      cout << "\t";
      count = 0;
      for(int j = 0; j < NUM_SIZE_CLASSES; j++) {
	 count += get_gen(i)->get_color_set(j)->get_num_of_objects();
      }
      cout << count << "\t\t";

      count = 0;
      for(j = 0; j < NUM_SIZE_CLASSES; j++) {
	 count += get_gen(i)->get_color_set(j)->get_num_of_non_black();
      }
      cout << count << "\t\t";	

      count = 0;
      for(j = 0; j < NUM_SIZE_CLASSES; j++) {
	 count += get_gen(i)->get_color_set(j)->get_num_of_free();
      }
      cout << count << "\t\t";	

      cout << get_gen(i)->tracing_is_done() << "\n";
   }
}
#endif // GC_MONITOR

//**********************************************************************
//* is_valid_object
//*
//* This routine takes a pointer to an object (at the garbage collector
//* level and checks to see if it is a valid object.  It does this by
//* deriving where the start of the object *should* be.  If the two 
//* pointers are different, then it is not a vaild object.  Otherwise, it
//* looks in the list that supposedly contains this object to see if it
//* exists.  If it is found, then true is returned, otherwise it is not
//* a valid object, and false is returned.

bool garbage_collector::is_valid_object(gc_object_base *object)
{
   object_manager *om = gc.get_object_manager((void *)object);
   gc_object_base *start_of_object = om->find_start_of_object(object);

   if (start_of_object != object) {
      // This is not a valid object
      return(false);
   }

   gc_object_base *head = start_of_object->get_containing_list()->get_gray();
   gc_object_base *tail = start_of_object->get_containing_list()->get_tail();

   while(head != tail) {
      if (head == object) {
	 // found the object, and it is a valid pointer to an object
	 return(true);
      }
      head = head->get_next();
   }
   // didn't find the object, so it must not be a valid pointer to an object
   return(false);
}


#ifndef INLINES
#include <rtgc/gc1.ci>
#include <rtgc/gc2.ci>
#endif
