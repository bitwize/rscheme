// Revised:  16 Nov 1993    Mike Hewett
//
// Added:     colorset::allocate_one_object()
// Modified:  colorset::snarf_more_memory to call allocate_one_object()
//
// ---------------------------------------------------------------
// FILE:  colorset.cc
//
// This file contains those functions which are part of the color_set class
// (see the file color_set.hh for a description) that can't be inlined
// (some because they are too large, and some because we take a pointer to
// them).

#include <assert.h>
#include <unistd.h>
#include <iostream.h>
#include <rtgc/gcserver.h>
#include <rtgc/objmgr.hh>
#include <rtgc/colorset.hh>
#ifdef INLINES
#include <rtgc/inlines.hh>
#endif

//***********************************************************************
// color_set::color_set
// This contructor assumes that color_set is called from the youngest colorset
// to the older. 

color_set::color_set(int gen_num, int size_class_num) {
   number_of_objects = 0;
   number_of_non_black = 0;
   number_of_free = 0;
   tail_object.set_next(NULL);
   number_of_objects_per_snarf = 0;
   number_of_snarfed_objects_allocated = number_of_objects_per_snarf + 1;


// initialize the color set pointers

   gray =     &head_object;
   scan =     &separator_object;
#ifdef ALLOCATE_BLACK
   free =     &separator_object;
#else // ALLOCATE_WHITE
   free =     &tail_object;
#endif
   old_free = &tail_object;
   white =    &separator_object;
   tail =     &tail_object;

   tail_object.set_previous(&separator_object);
   tail_object.set_next(NULL);
   separator_object.set_previous(&head_object);
   separator_object.set_next(&tail_object);
   head_object.set_previous(NULL);
   head_object.set_next(&separator_object);

   size_class = size_class_num;

   // Currently, different object managers are allocated for color_sets with
   // the same size in different generations. It should be the same, if it
   // can. However, at this stage, gc object is still under construction, and 
   // it is not easy to access different generation using get_gen() or
   // get_color_set(). Maybe, the friendship should be given to this class
   // from gc and generation classes.

   my_object_manager = new object_manager(0,size_class);

   UINT_32 object_size = 1 << size_class;
   // This size is set to the maximum object size including the header.
   size = object_size;
   my_generation = gen_num;
}


//****************************************************************
//* color_set::reclaim_or_allocate_objects_when_no_free 
//*
//* This function is called when there are no free objects in the colorset.
//* If the gc is turned off, it calls gc_flip to reclaim any garbage objects
//* that have been found by the garbage collector.  If there are no garbage
//* objects (or if the garbage collector was not turned off), then it 
//* fetches more memory from the system.

void color_set::reclaim_or_allocate_objects_when_no_free(void)
{ 

#ifdef GC_TURN_OFF_ABLE
   // The garbage collector only gets turned off if we are allocating
   // black with a single generation.  GC is turned off after finishing
   // tracing and before doing a gc flip. So if there are no free
   // objects left, first flip the GC and turn white objects into free.
   // Hopefully, this will provide more free objects for allocation

   if(gc_turned_off) {
      gc.gc_flip();
      amount_allocated = AMOUNT_TO_ALLOCATE_PER_INCREMENT;
   }
#endif

#ifdef GENERATIONAL    
   for(int i = 1; i < NUMBER_OF_GENERATIONS; i++){
      // Check if any older generations have any free objects that can
      // be used.  If move_old_free_to_young_free returns a non zero value,
      // then it found some objects that we can use.  We just return.
      if(gc.get_gen(i)->get_color_set(size_class)->move_old_free_to_young_free())
      {
	 return;
      }
   }
#endif
   // Check if there is at least some minimum amount of free memory left
   // in the system.  If not, then snarf some more.
   if (number_of_free < 1){
      snarf_more_memory();
   }
}


#ifdef ALLOCATE_BLACK
//***************************************************************************
// color_set::reclaim_garbage
//
// This routine reclaims the garbage memory for the system.
// The memory is reclaimed in an order such that the next memory
// to be allocated is memory that has never been shaded, followed by
// memory that was black, but has just now become garbage, followed
// by the memory that had been reclaimed on previous cycles of the system.
//
// This routine does not move free objects to another generation, even if the
// system is generational. It simply reclaim garbage white into free list, and
// olf free objects stays there. When allocator finds no more free objects is
// available in the younger generations, it calls fetch_old_free() and move
// the old free objects to the youngest generation for allocation.
//
// This is the version for the collector configured to allocate black.

void color_set::reclaim_garbage()
{	
   assert(scan->get_previous() == gray);

#ifndef NDEBUG

   // traverse the newly found garbage objects just to make sure there
   // aren't any live objects in there.  Used for debugging only.

   for(gc_object_base* tmp = white->get_next();
       tmp != tail;
       tmp = tmp->get_next())
   {
      assert(!tmp->is_shaded());
   }
#endif

   if (white->get_next() != tail)       // There are garbage objects
   {
      // reclaim the garbage.
      white->get_next()->set_previous(free->get_previous());
      tail->get_previous()->set_next(free);
      free->get_previous()->set_next(white->get_next());
      free->set_previous(tail->get_previous());

      if (scan == free)  // There are no black objects
      {  
	 scan = white->get_next();
      }
      free = white->get_next();

      white->set_next(tail);
      tail->set_previous(white);
   }

   // whiten the live objects.
   
   if (scan != free)   // there are live (black) objects
   {

      assert(white->get_next() == tail);
      assert(tail->get_previous() == white);

      scan->get_previous()->set_next(free);
      free->get_previous()->set_next(tail);
      tail->set_previous(free->get_previous());
      free->set_previous(scan->get_previous());
      scan->set_previous(white);
      white->set_next(scan);
      scan = free;
   }

   // Now the number of free objects is the number of the non black objects
   // at the end of the former GC cycle. And the number of non black objects
   // is the number of all objects.

   number_of_free = number_of_non_black;
   number_of_non_black = number_of_objects;

#ifndef NDEBUG
   INT_32 count = 0;
   gc_object_base* temp;

   for(temp = free; temp != white; temp = temp->get_next())
   {
      count++;
   }
   assert(count == number_of_free);
#endif

}
#endif // ALLOCATE_BLACK


#ifdef ALLOCATE_WHITE
//****************************************************************************
// color_set::reclaim_garbage
//
// This routine reclaims the garbage memory for the system.
// The memory is reclaimed in an order such that the next memory
// to be allocated is memory that has never been shaded, followed by
// memory that was black, but has just now become garbage, followed
// by the memory that had been reclaimed on previous cycles of the system.
//
// This is the version for allocating objects white.

void color_set::reclaim_garbage()
{	
   assert(gray->get_next() == scan); // we should be done scanning
   
   gc_object_base *temp_head, *temp_tail;

   if (white->get_next() != free) // make sure there are some garbage objects.
   {
      if (white->get_next() != old_free) // There are was black now garbage
      {                                  // objects.
	 if (old_free != free)  // There are some never been shaded objects
	 {
	    // switch the position of the was_black_now_garbage with the
	    // position of the never_shaded objects

	    temp_head = white->get_next();
	    temp_tail = old_free->get_previous();
	    
	    // Splice out the was black now garbage objects.
	    
	    white->set_next(old_free);
	    old_free->set_previous(white);
	    
	    // Splice the was black now garbage into the free list.
	    
	    temp_head->set_previous(free->get_previous());
	    temp_tail->set_next(free);
	    free->get_previous()->set_next(temp_head);
	    free->set_previous(temp_tail);
	 }
      }
   }

#ifndef NDEBUG

   // traverse the newly found garbage objects just to make sure there
   // aren't any live objects in there.  Used for debugging only.
   
   for(gc_object_base *temp = white->get_next();
       temp!=free;
       temp=temp->get_next())
   {
      assert(!temp->is_shaded());
   }

#endif

   free = white->get_next();
   old_free = free;

   // whiten the live objects.
   
   if (gray->get_next() != white)  // There are some live objects to whiten
   {
      temp_head = scan;
      temp_tail = white->get_previous();
      
      // Splice out the live objects;
      
      gray->set_next(white);
      white->set_previous(gray);

      // Splice the live objects into the white set

      temp_head->set_previous(white);
      temp_tail->set_next(free);
      white->set_next(temp_head);
      free->set_previous(temp_tail);
      scan = white;
   }

   // Now the number of free objects is the number of the non black objects
   // at the end of the former GC cycle. And the number of non black objects
   // is the number of all objects.

   number_of_free = number_of_non_black;
   number_of_non_black = number_of_objects;

#ifndef NDEBUG
   INT_32 count = 0;
   for(gc_object_base *temp= free;temp!= tail;temp=temp->get_next()){
      count++;
   }
   assert(count == number_of_free);
#endif
}
#endif // ALLOCATE_WHITE


#ifdef GENERATIONAL
// **********************************************************************
// move_old_free_to_young_free
// 
// This routine moves free objects from an older generation to a younger
// generation.  This routine is only called when there are no free objects
// in the younger generation.
//
// If any objects are moved, this function returns 1, otherwise it returns
// 0

#ifdef ALLOCATE_BLACK
int color_set::move_old_free_to_young_free(void){
   if(free == white)
   {
      // If there are no free objects, do nothing and return 0.
      return(0);
   } else {
      gc_object_base *free_head,*free_tail;

      free_head = free;
      free_tail = white->get_previous();
      free->get_previous()->set_next(white);
      white->set_previous(free->get_previous());

      if(free == scan){
	 scan = white;
      }
      free = white;

      int num = number_of_free;
      number_of_objects -= number_of_free;
      number_of_non_black -= number_of_free;
      number_of_free = 0;
      gc.get_gen(0)->get_color_set(size_class)
	 ->insert_objects_into_free_set(free_head,free_tail,num);
      return(1);
   }
}


#else // ALLOCATE_WHITE
int color_set::move_old_free_to_young_free(void){
   if(free == tail)
      return(0);
   else{
      gc_object_base *free_head,*free_tail;
      free_head = free;
      free_tail = tail->get_previous();
      if(free != gray){ //If there are some objects before free.
	 free->get_previous()->set_next(tail);
	 tail->set_previous(free->get_previous());
      }else{ // We have no objects before free.
	 tail->set_previous(NULL);
      }
      if(free == old_free){
	 if(free == white){
	    if(free == scan){
	       if(free == gray){
		  gray = tail;
	       }
	       scan = tail;
	    }
	    white = tail;
	 }
	 old_free = tail;
      }
      free = tail;
      int num = number_of_free;
      number_of_objects -= number_of_free;
      number_of_non_black -= number_of_free;
      number_of_free = 0;
      gc.get_gen(0)->get_color_set(size_class)
	 ->insert_objects_into_free_set(free_head,free_tail,num);
      return(1);
   }
}
#endif // White allocation
#endif // Generational collection

#ifdef GENERATIONAL
//**********************************************************************
// insert_objects_into_free_set
// 
// This routine inserts a list of objects into the free set.  It should
// be passed pointers to the head and tail of this list as well as the 
// number of objects in that list.
//
// This routine assumes that the free list is empty when it is called.

#ifdef ALLOCATE_BLACK
void color_set::insert_objects_into_free_set(gc_object_base *free_head,
					     gc_object_base *free_tail,
					     int number_of_inserted)
{
   assert(free == white);
   assert(number_of_free == 0);

   free_head->set_previous(free->get_previous());
   free->get_previous()->set_next(free_head);
   free_tail->set_next(free);
   free->set_previous(free_tail);
   if (scan == free) { // There are no black objects
      scan = free_head;
   }
   free = free_head;

   number_of_objects += number_of_inserted;
   number_of_non_black += number_of_inserted;
   number_of_free = number_of_inserted;
}
#endif // ALLOCATE_BLACK


#ifdef ALLOCATE_WHITE
void color_set::insert_objects_into_free_set(gc_object_base *free_head,
					     gc_object_base *free_tail,
					     int number_of_inserted)
{
   assert(free == tail);
   assert(number_of_free == 0);

   // splice in the free objects
   free_head->set_previous(tail->get_previous());
   free_tail->set_next(tail);
   tail->get_previous()->set_next(free_head);
   tail->set_previous(free_tail);
   free = free_head;
   if (old_free == tail) {
      old_free = free;
   }

   number_of_objects += number_of_inserted;
   number_of_non_black += number_of_inserted;
   number_of_free = number_of_inserted;
}
#endif // ALLOCATE_WHITE
#endif // GENERATIONAL

#ifdef DEBUG_LEVEL2
//***********************************************************************
// color_set::sanity_check
//
// This routine is included for debugging purposes.  Any time you
// want to make sure that the pointers into the color set are in the
// correct order, you simply call this routine, and it will print
// out the relative positions of the pointers.

void color_set::sanity_check() {
   INT_32 flag = 0;
   gc_object_base *footmp;

   cout << hex << " gray:" << gray << " scan:" << scan << " white:" << white
	<< " old_free:" << old_free << " free:" << free << " tail:" << tail 
	<< dec << endl;
   for(footmp = gray;footmp!=NULL;footmp=footmp->get_next()) {
      if (footmp == white){
	 cout << "white" << endl;
	 flag = 1;
      }
      if (footmp == gray) {
	 cout << "gray" << endl;
	 flag = 1;
      }
      if (footmp == scan) {
	 cout << "scan" << endl;
	 flag = 1;
      }
      if (footmp == old_free) {
	 cout << "old_free" << endl;
	 flag = 1;
      }
      if (footmp == free) {
	 cout << "free" << endl;
	 flag = 1;
      }
      if (footmp == tail) {
	 cout << "tail" << endl;
	 flag = 1;
      }
      if (flag) cout << "-------------------" << endl;
      flag = 0;
   }
   cout << "exiting sanity check" << endl;
}
#endif DEBUG_LEVEL2


//***********************************************************************
// color_set::allocate_one_object
// 
// This routine allocates space from a free page for one object
// by linking the object into the appropriate freelist.

void color_set::allocate_one_object(UINT_32 object_size)
{
   gc_object_base *cur_obj, *start;

   number_of_non_black++;
   number_of_free++;
   
   // Start is set to the (one) object allocated.
   start = (gc_object_base *)local_high_water_mark;

   // create the new object here
   cur_obj = (gc_object_base *) local_high_water_mark;

   // increment memory area to next object
   local_high_water_mark += object_size;

#ifndef HARD_REAL_TIME
   assert((local_high_water_mark-1) <= max_incremental_memory_extent);
#endif

   cur_obj->set_containing_list(this);

#ifdef GENERATIONAL
   cur_obj->set_gen_num(0);
   cur_obj->set_step(0);
#endif

   number_of_objects++;

   cur_obj->set_next(free);
   cur_obj->set_previous(free->get_previous());
   free->get_previous()->set_next(cur_obj);
   free->set_previous(cur_obj);

#ifdef ALLOCATE_BLACK

   if (scan == free) { // No black objects
      scan = cur_obj;
   }
#else // ALLOCATE_WHITE
   if (old_free == free) {
      old_free = cur_obj;
   }
#endif

   free = cur_obj;

}  // end of colorset::allocate_more_memory


//***********************************************************************
// color_set::snarf_more_memory
// 
// This routine allocates more memory from the operating system and places
// it into the appropriate size class.  
// It allocates NUM_PAGES_TO_SNARF_AT_A_TIME pages worth of objects.

void color_set::snarf_more_memory()
{
   UINT_32 i;
   char *memory_area;
   UINT_32 object_size = 1 << size_class;
   INT_32 num_objects_snarfed;

   if (object_size > GC_PAGE_SIZE) {
      if (object_size > NUM_PAGES_TO_SNARF_AT_A_TIME * GC_PAGE_SIZE) {
	 // We are allocating a large object and that object takes more
	 // pages than NUM_PAGES_TO_SNARF_AT_A_TIME
	 num_objects_snarfed = 1;
	 number_of_objects_per_snarf = num_objects_snarfed;
	 number_of_snarfed_objects_allocated = 0;
#ifdef HARD_REAL_TIME
	 local_high_water_mark = gc.high_water_mark;
	 // object_size should be a power of 2
	 // the starting point from which to alloc 
	 memory_area = gc.high_water_mark; 
	 gc.high_water_mark += object_size;
#else  // Non HARD_REAL_TIME
	 // we need to sbrk one extra page because heap_round is going
	 // to round the pointer up to the start of the next logical page.
	 void *sbrk_pointer = sbrk(GC_PAGE_SIZE+object_size);
	 if (sbrk_pointer == (void *)-1) {
	    cerr << "System heap exhausted.  Can not sbrk additional memory." << endl;
	    abort();
	 }

#ifndef NDEBUG
	 max_incremental_memory_extent = ((char *)sbrk_pointer) + GC_PAGE_SIZE+object_size;
#endif // NDEBUG

	 gc.record_sbrk_pointer(sbrk_pointer);
	 local_high_water_mark = heap_round(sbrk_pointer);
	 memory_area = local_high_water_mark;
#endif // not HARD_REAL_TIME

	 for(i=0; i < (object_size / GC_PAGE_SIZE); i++) {
	    // set the object manager for the pages used for this
	    // large object.  The object manager is a 
	    // multi_page object manager.
	    gc.set_object_manager(memory_area+(i*GC_PAGE_SIZE),
				  new object_manager(i,
						     size_class));
	 }

#ifdef TEST_FRAGMENTATION
	 object_manager *om =
	    (gc.get_object_manager(memory_area));
	 om->num_objects_on_page = 1;
#endif	// TEST_FRAGMENTATION

      } else {
	 // We are allocating large objects, but they fit in the amount
	 // of memory that we are snarfing.
	 num_objects_snarfed = NUM_PAGES_TO_SNARF_AT_A_TIME*GC_PAGE_SIZE 
	    / object_size;
	 number_of_objects_per_snarf = num_objects_snarfed;
	 number_of_snarfed_objects_allocated = 0;

#ifdef HARD_REAL_TIME
	 local_high_water_mark = gc.high_water_mark;
	 memory_area = gc.high_water_mark;
	 gc.high_water_mark += NUM_PAGES_TO_SNARF_AT_A_TIME*GC_PAGE_SIZE;
#else // NON-HARD_REAL_TIME
	 // we need to sbrk one extra page because heap_round is going
	 // to round the pointer up to the start of the next logical page.
	 void *sbrk_pointer = 
	    sbrk((NUM_PAGES_TO_SNARF_AT_A_TIME+1) * GC_PAGE_SIZE);
	 if (sbrk_pointer == (void *)-1) {
	    cerr << "System heap exhausted.  Can not sbrk additional memory." << endl;
	    abort();
	 }

#ifndef NDEBUG
	 max_incremental_memory_extent = ((char *)sbrk_pointer) + (NUM_PAGES_TO_SNARF_AT_A_TIME+1) * GC_PAGE_SIZE;
#endif

	 gc.record_sbrk_pointer(sbrk_pointer);
	 local_high_water_mark = heap_round(sbrk_pointer);
	 memory_area = local_high_water_mark;
#endif // NON-HARD_REAL_TIME

	 for(i=0;i<NUM_PAGES_TO_SNARF_AT_A_TIME;i++) {
	    // set the object manager for the pages used for these
	    // objects.
	    gc.set_object_manager(memory_area+(i*GC_PAGE_SIZE),
				  new object_manager(i%(object_size/GC_PAGE_SIZE),
						     size_class));
	 }

#ifdef TEST_FRAGMENTATION
	 object_manager *om =
	    (gc.get_object_manager(memory_area));
	 om->num_objects_on_page = 1;
#endif	    

      }
   } else {
      // If we don't have any empty space left on the pages we
      //    previously allocated, allocate some more pages.
      //    Otherwise, carve one more object out of the space
      //    previously allocated.

      if (number_of_snarfed_objects_allocated >= number_of_objects_per_snarf)
      {
	 // We are allocating small objects.
	 // figure out how many small objects to allocate
	 num_objects_snarfed = NUM_PAGES_TO_SNARF_AT_A_TIME*GC_PAGE_SIZE 
	    / object_size;

#ifdef HARD_REAL_TIME
	 local_high_water_mark = gc.high_water_mark;
	 memory_area = gc.high_water_mark;
	 gc.high_water_mark += NUM_PAGES_TO_SNARF_AT_A_TIME*GC_PAGE_SIZE;
#else // NON-HARD_REAL_TIME
	 // we need to sbrk one extra page because heap_round is going
	 // to round the pointer up to the start of the next logical page.
	 void *sbrk_pointer = 
	    sbrk((NUM_PAGES_TO_SNARF_AT_A_TIME+1) * GC_PAGE_SIZE);
	 if (sbrk_pointer == (void *)-1) {
	    cerr << "System heap exhausted.  Can not sbrk additional memory." << endl;
	    abort();
	 }

#ifndef NDEBUG
	 max_incremental_memory_extent = ((char *)sbrk_pointer) + (NUM_PAGES_TO_SNARF_AT_A_TIME+1) * GC_PAGE_SIZE;
#endif
	 gc.record_sbrk_pointer(sbrk_pointer);
	 local_high_water_mark = heap_round(sbrk_pointer);
	 memory_area = local_high_water_mark;
#endif

	 for(i=0;i<NUM_PAGES_TO_SNARF_AT_A_TIME;i++) {
	    // set the object manager for the pages used for these
	    // large objects.  The object manager is a 
	    // multi_page object manager
	    gc.set_object_manager(memory_area+(i*GC_PAGE_SIZE),
				  my_object_manager);
	 }
	 number_of_objects_per_snarf = num_objects_snarfed;
	 number_of_snarfed_objects_allocated = 0;
      }
   }
   number_of_snarfed_objects_allocated++;
   allocate_one_object(object_size);
}


#ifdef DEBUG_LEVEL2
//***********************************************************************
// color_set::in_which_color_list
//
// This routine is included for debugging purposes. It looks for the object
// in this color set.
// 
// The return value 1 means it is in black
//                  2                gray    
//                  3                white
//                  4                free
// 0 means the object is not in this list.
int color_set::in_which_color_list(gc_object_base *obj) {
   gc_object_base *ptr;
   int flag = 0;
   int i;

   for(i= 0,ptr = gray; ;ptr=ptr->get_next(),i++) {
      if (ptr == gray) {
	 flag = 2;
      }
      if (ptr == scan){
	 flag = 1;
      }
#ifdef ALLOCATE_BLACK
      if (ptr == free) {
	 flag = 4;
      }
      if (ptr == white) {
	 flag = 3;
      }
#else
      if (ptr == white) {
	 flag = 3;
      }
      if (ptr == free) {
	 flag = 4;
      }
#endif
      if (ptr == tail) {
	 flag = 0;
	 break;
      }
      if (ptr == obj){
	 break;
      }
   }
   cout << "Object position: " << i << "\n";
   return(flag);
}
#endif // 

#ifndef INLINES
#include <rtgc/colorset1.ci>
#include <rtgc/colorset2.ci>
#endif
