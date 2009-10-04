#ifndef STATES_H
#define STATES_H

// This file contains definitions of the state classes used to implement
// co-routines in the garbage collector.  As of this time, this is only
// needed for incremental scanning of large objects, so there is only one
// such state class defined.  In the future, if more are needed, then they 
// should be put here.

// The basic idea is that rather than using any local variables, all state
// variables will be captured in an instance of one of these state classes
// which is allocated statically.  When the to-be-continued routines are
// called, they will reference this state variable, and they
// can resume from where they left off by using this state information.

// A special variable called Done is set to true when the computation
// completes.  That way, just before the computation is resumed, it can
// check if this variable is set to true.  If so, then it can initialize
// the state to the next initial value.  Otherwise, it can just call the
// to-be-resumed funcitons and they will resume from where they left off.

// If we had real co-routines, none of this would be necessary.

struct gc_tracing_state {
   bool Done;
   gc_object_base *object;
   gc_obj_addr ptr_to_client_object;
   INT_32 current_size_class_being_traced;

   gc_tracing_state() { Done = true;}
};

#endif
