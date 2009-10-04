#ifndef GENERATION_HH
#define GENERATION_HH

// This file contains prototypes for the generational code for the garbage
// collector.  Since the collector is not currently generational, these
// routines are implemented such that there is only 1 generation.

#include <rtgc/config.hh>
#include <rtgc/colorset.hh>
#include <rtgc/scanroot.hh>
#include <rtgc/scheduler.hh>

class generation {
#ifdef GENERATIONAL
    friend double scheduling_manager::get_max_table_occupancy_ratio(int);
#endif

// ********** Variables ***************
  protected:
    color_set *color_sets[NUM_SIZE_CLASSES];// The color sets for this
                                            //generation
    char current_shade_color;  // The current color used to shade objects
                               // in this generation.

    // Note: This should be implemented as a constant. In order to do so, we
    // have to implement a default constructor and a member initializer for
    // the class generation whose instances are defined as array. 
    int this_generation;  // The generation number.            

    int done_tracing;     // set to true when tracing of this generation
	                  // is finished.
    int tracing_phase_counter; // If it is 0, the tracing is in the 1st phase.
                          // If it is positive, the tracing is in the 2nd
                          // phase. The value represents how many gc cycles
                          // it's spent in the 2nd phase. 1 represents it
                          // is in the drop in cycle. 2 represents the first
                          // whole GC cycle.
                          // Since the youngest generation has no phase,  
                          // it is always set to 0.

    int work_left_to_do;   // work left to do for this generation
                           // on this gc increment.
                           // The work amount is calculated by scheduling
                           // manager class.

    int no_more_work;      // This variable is controling the termination
                           // If there is no more work to do in non youngest
                           // generation, it will be on. Used only in
                           // trace_mid_generation() and trace_old_generation()

    // This is the glue class object to show the root set scanner to the
    // GC tracer, as if we have multiple scanners for generations.
    abstract_root_set root; // Abstract root set scanning object.

// ******* STORED_INTO_LIST
    // The stored into list is a queue of pointers to locations that have been
    // stored into.  The convention is that the tail will always point at the
    // next location into which we can save a value, and the head will always
    // point at the location from which we can get the next value.  The queue
    // is empty when tail == head, and full when tail+1 mod size == head.

    int stored_into_list_head; // The stored_into_list is implemented as a
                               // queue, and this is the head.

    int stored_into_list_tail; // This is the tail.

    // This is a list of pointers that still need to be blackened.
    pos_ptr_addr stored_into_list[STORED_INTO_LIST_SIZE];

                           
// ********** Member Functions***********
// ****** Contructors
  public:
    generation(int gen); // Each constuctor is called with its generation
    // number. In it, we allocate color_set objects and
    // object_managers if necessary. 
    void init(int i);   // initialize the memory for this generation.


// ********* Color set 
    color_set *get_color_set(int sizclass){return(color_sets[sizclass]);}

// **** Colors
    // Determine if an object is shaded in this generation
    LINK_TYPE int is_shaded(gc_object_base *object);

    // returns the color currently being used to shade objects.
    LINK_TYPE colors get_current_shade_color(void);

    // Switches the shading color for this generation
    void toggle_current_shade_color(void);
    
// ******** Tracing    
    int trace_single_generation(void); // Trace this generation
    int trace_youngest_generation(void); // Trace this generation assuming
                                         // it is youngest. 
    int trace_mid_generation(void); // Trace this generation assuming
                                    // it is not youngest nor oldest. 
    int trace_oldest_generation(void); // Trace this generation assuming
                                         // it is oldest. 
    int any_more_grays(void);  // returns true if this generation contains
                               // any gray objects.
    int blacken_grays(void); // Traces from the gray objects and blacken them.

    // Traces from the gray objects and blacken or promote them. This 
    // routine is used to scan the non-oldest generation in the generational
    // version.
    int blacken_or_promote_grays(void);
    
    // Promote the object into the color_set of the size_class in this
    // generation.
    void promote_into(gc_object_base *object, int size_class);

    // A boolean function that determines if there is any more work to do in
    // the current garbage collection cycle.  A return value of 0 means that
    // the current garbage collection cycle is finished. This function is
    // mainly used from single generation full_collect().
    int any_more_work_to_do(void);

    int get_tracing_phase(void) {return tracing_phase_counter;}

    int tracing_is_done(void){return done_tracing;}

    void gc_flip(void); // post (pre) processing after a gc cycle.


    LINK_TYPE int scan_root(void);   // Scans the root and grays all objects
                           // pointed to from the stack and
                           // returns the pointer to one.
                           // If there's no more object to
                           // gray, it returns NULL. Scanning is done
                           // over GC increments.
    

    LINK_TYPE int scan_root_atomic(void); // scans the root atomically for
                                          // gc termination.

// ******* Work amount control
    // set_work_left_to_do sets the counter which holds the amount of work
    // that should be performed for a garbage collection increment to the
    // value of its parameter.  Usually, this will be the value of the
    // throttle setting times the amount to allocate per increment.
    void set_work_left_to_do(int work);

    // Get the current value of work_left_to_do.
    inline int get_work_left_to_do(void){return work_left_to_do;}

    // This routine increments the amount of work done so far during this
    // garbage colleciton increment.  This is accomplished by decrementing the
    // amount of available work left to do.
    inline int increment_work(int work_units){
	return(work_left_to_do -= work_units);
    }
	
//******** Pointer Stored Into List
    // Call this to register a pointer as gray (so that it is rescanned later).
    // Note, we actually pass a pointer to the location holding the pointer
    // so that if the pointer changes, we will get the new value when 
    // rescanning the location.
    void add_ptr_stored_into_list(pos_ptr_addr ptr);
    
    // Removes a pointer from the pointer to do list, so that the object it
    // points to can be grayed.
    pos_ptr_addr get_ptr_stored_into_list(void); 
    


// ******** IGPS
// intergenerational pointers (or igp's) are pointers from one generation
// to some younger generation.  The method add_intergenerational_pointer adds
// an igp to the igp list.

    void add_intergenerational_pointer(gc_object_base *);
    
// The method remove_intergenerational_pointer removes a particular igp from
//  the igp list.

// NOTE: for now, this routine is not implemented.  It seems a whole lot easier
// if we simply rebuild the igp list from scratch on every collection of a
// particular generation, and treat the igp list conservatively.  So, it might
// contain pointers that are no longer actually live.  If this turns out
// to be a bad idea, then we can implement this method.
    
    void remove_intergenerational_pointer(gc_object_base *) {}
    

// ***** Debugging
    friend void break_point();  // used for debugging only

};


#endif
