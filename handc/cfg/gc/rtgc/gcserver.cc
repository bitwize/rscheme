// This file conditionally includes the inline definitions of the gcserver
// routines (for the case where we don't want inline functions (debugging))

#include <assert.h>
#ifndef NDEBUG
#include <iostream.h>
#endif
#include <rtgc/gcserver.h>
#include <rtgc/gc.hh>
#include <rtgc/objmgr.hh>

#ifdef INLINES
#include <rtgc/inlines.hh>
#endif //INLINES

#ifdef FOREIGN
#include <rtgc/gcserver1.ci>
#include <rtgc/gcserver2.ci>
#else
#ifndef INLINES
#include <rtgc/gcserver1.ci>
#include <rtgc/gcserver2.ci>
#endif
#endif // FOREIGN

#undef EXTERNAL_LINK_TYPE
#ifdef FOREIGN
#define EXTERNAL_LINK_TYPE extern "C"
#else
#define EXTERNAL_LINK_TYPE
#endif

/* This variable shows how much memory we can allocate before beginning the
   next GC increment. This variable counts down from the total that can be
   allocated in an increment to 0. The increment job should be done before
   it reaches 0. */
int amount_allocated; 

/* If this variabel is on, the incremental garbage collection is off, so
   write barrier can also be turned off. GC_TURN_OFF_ABLE can be defined only in
   single generation black allocation.*/
#ifdef GC_TURN_OFF_ABLE
int gc_turned_off;
#endif

// gc_full_collect performs a complete garbage collection.  If the marking 
// process is underway when this routine is called, it completes the       
// collection from the point that it has already reached (in other words,  
// it does not start the marking process over from the begining).          
// It then does a second full collect so that it can get any floating 
// garbage (garbage that became garbage after it was traced and marked
// live).

#include <limits.h>

EXTERNAL_LINK_TYPE void gc_full_collect(void)
{
   gc_scheduler.set_full_throttle();
   while(!gc.get_gen(NUMBER_OF_GENERATIONS - 1)->tracing_is_done()) {

#ifdef GC_TURN_OFF_ABLE
      // If the garbage collector has been turned off, turn it back on
      // again before doing a full collection.
      gc_turned_off = 0;
#endif
      // Work_left_to_do is reset to work_per_increment in every
      // gc.collect() call.
      gc.collect();
   }
   // Second time to collect floating black objects.
   while(!gc.get_gen(NUMBER_OF_GENERATIONS - 1)->tracing_is_done()) {
#ifdef GC_TURN_OFF_ABLE
      // If the garbage collector has been turned off, turn it back on
      // again before doing a full collection.
      gc_turned_off = 0;
#endif
      // Work_left_to_do is reset to work_per_increment in every
      // gc.collect() call.
      gc.collect();
   }
   gc_scheduler.unset_full_throttle();

}

EXTERNAL_LINK_TYPE const void* const start_of_heap(void)
{
   return(gc.start_of_heap);
}

EXTERNAL_LINK_TYPE const void* const end_of_heap(void)
{
   return(gc.end_of_heap);
}

EXTERNAL_LINK_TYPE void reset_not_known_free_object_iterator(void)
{
   gc.reset_not_known_free_object_iterator();
}

EXTERNAL_LINK_TYPE gc_obj_addr next_not_known_free_object(void)
{
   return(gc.next_not_known_free_object());
}

EXTERNAL_LINK_TYPE void reset_dead_object_iterator(void)
{
   gc.reset_dead_object_iterator();
}

EXTERNAL_LINK_TYPE gc_obj_addr next_dead_object(void)
{
   return(gc.next_dead_object());
}

EXTERNAL_LINK_TYPE void register_dead_object_callback(void (*function_pointer)())
{
   gc.register_dead_object_callback(function_pointer);
}

EXTERNAL_LINK_TYPE void set_dead_flag(gc_obj_addr the_object)
{
   object_manager *om = gc.get_object_manager(the_object);
   gc_object_base *start_of_object = om->find_start_of_object(the_object);
   start_of_object->set_dead_flag(char(1));
}

EXTERNAL_LINK_TYPE void clear_dead_flag(gc_obj_addr the_object)
{
   object_manager *om = gc.get_object_manager(the_object);
   gc_object_base *start_of_object = om->find_start_of_object(the_object);
   start_of_object->set_dead_flag(char(0));
}

EXTERNAL_LINK_TYPE INT_32 get_dead_flag(gc_obj_addr the_object)
{
   object_manager *om = gc.get_object_manager(the_object);
   gc_object_base *start_of_object = om->find_start_of_object(the_object);
   return(int(start_of_object->get_dead_flag()));
}

