#ifndef GARBAGE_COLLECTOR_HH
#define GARBAGE_COLLECTOR_HH

// This file contains class definition information for the garbage collector
// class.  The garbage collector contains generations (defined in the
// file gen.hh and each generation contains color_sets which are defined
// in colorset.hh).

#include <stddef.h>
#include <stdio.h>
#include <assert.h>


#include <rtgc/config.hh> // contains garbage collector compile time parameters
#include <rtgc/gcclient.h>// contains prototypes for language system defined 
// routines.
#include <rtgc/gcserver.h>   // contains prototypes for the language system
                             // interface as provided by the gc.
#include <rtgc/gen.hh>       // contains code for generational collection.
#include <rtgc/igps.hh>      // Intergenerational pointer manager  
#include <rtgc/states.h>

#ifdef ZEN_TIMER
#include <rtgc/zentimer.h>
#endif

#undef LINK_TYPE
#ifdef INLINES
#define LINK_TYPE inline 
#else
#define LINK_TYPE
#endif

// The function heap_round takes a memory location, and rounds it up to 
// the next logical page boundry.  It is used to insure that the garbage
// collector's heap memory is always aligned on logical page boundries.
// It is wise to allocate 1 more page than is needed so that heap_round
// can cut off part of the memory passed to it to round up to the next
// logical page boundry.

char *heap_round(void *start_of_heap);


extern int gc_initialized;  // C++ sets this variable to 0 upon startup,
// before any code is executed.  The routine
// init() sets it to 1.  The methods that define
// global pointers find out that they are global
// by checking if this variable is 0 (global)
// or 1 (stack).

// The class garbage_collector holds all of the methods and state data
// for the garbage collector top level routines.  It contains an array
// of pointers to generation objects which handle the code for generations,
// and each generation holds an array of pointers to color sets
// which hold the lists for the segregated storage managing.

class garbage_collector {
 public:
#ifdef ZEN_TIMER
   zen_timer gc_increment_timer;
   zen_timer write_barrier_timer;
   zen_timer gray_timer;
#endif
   gc_tracing_state incremental_tracing_state;

 private:
 friend const void *const start_of_heap(void);
 friend const void *const end_of_heap(void);
   
// ************ Variable Section ****************
   
 protected:   
// ***** Generations
   // For now, generations are defined here before start_of_heap. The c++
   // calls the constructor for its members in the order of definition
   // regardless of the order or explicite specification of member initializer
   // generations.generation() is called before setting start_of_heap.
   // In the future, generations may be a big object whose size is not power
   // of two. In that case, we should move the definition of generations
   // after start_of_heap, or make it an array of pointers.

// In a generational collector, this holds the generation objects.
   generation *generations[NUMBER_OF_GENERATIONS]; 

#ifdef GENERATIONAL
// the pointer to the igp list manager
   igp_manager *igpm;
#endif //GENERATIONAL

#ifndef GENERATIONAL
   // If we are not using the generational version of the collector, then
   // there is only one current shade color, and we save it here in the
   // garbage collection object
   int current_shade_color;
 public:
   int get_current_shade_color(void) {return(current_shade_color);}
   void toggle_current_shade_color(void)
   {
      current_shade_color = !current_shade_color;
   }
   INT_32 is_shaded(gc_object_base *object)
   {
      return(object->get_color() == current_shade_color);
   }
 protected:
#endif

#ifdef GENERATIONAL
 public:
   bool is_shaded(gc_object_base *object)
   {
      return(gc.generations[object->get_gen_num()]->is_shaded(object));
   }
 protected:
#endif

// **** Memory management

#ifndef NDEBUG
// This public makes start_of_heap and end_of_heap visible to some assertions.
 public:
#endif
   // The next two variables *must* be declared in this order.
   // this is because of brain-dead restrictions on the order of
   // initialization of objects in C++.
   const char* start_of_heap;  // A ptr to the start of the heap.
   const char* end_of_heap;    // A ptr to the end of the largest extent
                               // to which the heap can grow.

   // The root object manager is an object manager that is used for
   // every root object.
   object_manager* the_root_object_manager;

 public:
   bool first_collection_increment;

   // the object manager knows how to do things like shade objects within
   // its page, derive pointers to the start of objects from interior
   // pointers, and whether objects are already shaded.
   object_manager* object_managers_array[NUM_HEAP_PAGES]; 

   int alloc_count;
   int delete_count;

   char *high_water_mark;     // The highest location allocated to the heap
#ifndef HARD_REAL_TIME
// ****** sbrk pointer recording.
   void *sbrk_pointer_list[SBRK_POINTER_LIST_SIZE];
   int first_sbrk_pointer_index;
   int middle_sbrk_pointer_index;
   int last_sbrk_pointer_index;
#endif

// ****** Finalization Stuff
   // The next two variables are used by the live object iterator to
   // indicate which generation and colorset we are currently iterating
   // through.
   int not_known_free_object_iterator_colorset_number;
   int not_known_free_object_iterator_generation_number;

   // The next two variables are used by the dead object iterator to
   // indicate which generation and colorset we are currently iteraging
   // through.
   int dead_object_iterator_colorset_number;
   int dead_object_iterator_generation_number;

   // We set the function that will be called just before each gc_flip().
   void (*gc_flip_callback_function)();

// ****** Benchmarking Stuff
#ifdef TEST_FRAGMENTATION
   FILE *fragmentation_out;
#endif
#ifdef TEST_LIFETIME_DIST
   FILE *lifetime_file;
#endif


// ******************* Member functions *********************
 public:
// ***** Contructors and Initializer

   garbage_collector();  // initializes the garbage collector.
   ~garbage_collector(); // closes the fragmentation log file.
   void init(int argc, const char **argv);
   // preallocates memory and initializes the collector.

// **** Generations
   LINK_TYPE generation *get_gen(int generation_number){
      return(generations[generation_number]);
   }

// ***** Tracing information
   int collect(void);  // Does one increment of collection

   void gc_flip(void); // post (pre) processing after a gc cycle.

#ifdef GENERATIONAL
   igp_manager *get_igpm(void){return igpm;} 
#endif

// ***** Write Barriers
   // The write barrier is used to maintain the tri-color invariant.
   // any function with the suffix NDP assumes that the pointers
   // to lobject and robject are the same as returned by gc_alloc
   // (the allocator for the garbage collector).
   // This also implies that no NDP function should be called for any
   // pointer which might point into the root set.
   // for a more complete description of these functions, see the
   // file gcserver.h

   LINK_TYPE void snapshot_write_barrier(pos_ptr_addr lvalue);
   LINK_TYPE void incremental_update_write_barrier(gc_obj_addr lobject,
						   pos_ptr_addr lvalue,
						   gc_obj_addr new_robject);
   LINK_TYPE void incremental_update_wb_lval_black(gc_obj_addr lobject,
						   pos_ptr_addr lvalue,
						   gc_obj_addr new_robject);
   LINK_TYPE void incremental_update_wb_rval_white(gc_obj_addr lobject,
						   pos_ptr_addr lvalue,
						   gc_obj_addr new_robject);

#ifdef GENERATIONAL    
   void write_barrier_rval_black(pos_ptr_addr lvalue);
   void write_barrier_rval_black_NDP(gc_obj_addr lobject,
				     pos_ptr_addr lvalue);
#endif // GENERATIONAL    
#ifdef   PROFILE_WRITE_BARRIER
   void garbage_collector::profile_write_barrier(pos_ptr_addr lvalue,
						 gc_obj_addr lobject,
						 gc_obj_addr rvalue,
						 gc_obj_addr robject);
#endif

   bool is_valid_object(gc_object_base *object);

// *********** Memory Management
   // set_object_manager defines the object manager for a particular page of
   // the garbage collector's heap.
   void set_object_manager(const char* const memory_location,
			   object_manager* manager); 
   

   // get_object_manager return the object manager corresponding to a
   // particular pointer location
   LINK_TYPE object_manager *get_object_manager(const void *memory_location);

   // Return the object manager corresponding to a particular pointer location
   // NB: The user of this routine is guaranteeing that the pointer points
   // into the heap.
   LINK_TYPE object_manager *get_object_manager_NOT_ROOT(const void *);
   

   // is_on_heap tests if the memory location actually points to a heap
   // object.
   LINK_TYPE int garbage_collector::is_on_heap(gc_obj_addr memory_location);

// ****** Sbrk pointer record functions.
#ifndef HARD_REAL_TIME
   // This function will start recording pointers to objects sbrked
   // to form the heap.
   void record_new_sbrk_pointers(void);

   // This function will delete old objects who's pointers have been
   // recorded by the above function.
   void delete_old_sbrk_pointers(void);

   // This function records one pointer so that delete_old_sbrk_pointers
   // can delete it when called.
   void record_sbrk_pointer(void* ptr);
#endif
   
// ******* Finalization functions
   // The object iterator functions allow you to iterate through the
   // objects in the garbage collector's color_set lists.  Live objects
   // that are either black or gray, and dead objects are white objects.
   // No objects on any free list are returned by any of these routines.

   LINK_TYPE void reset_not_known_free_object_iterator(void);

   gc_obj_addr next_not_known_free_object(void);

   LINK_TYPE void reset_dead_object_iterator(void);

   gc_obj_addr next_dead_object(void);

   // This function registers a function with the garbage collector that
   // will be called just before each gc_flip(), ie.e it will be called
   // just before dead objects are placed into the free list.  The
   // default (should no function be registered) will be a noop.

   LINK_TYPE void register_dead_object_callback(void (*fp)());

#ifdef DEBUG_LEVEL2
   // Debugging routines.
   void check_soundness(void);
   static void shade_check_from_black(pos_ptr_addr ptr_addr,
				      gc_obj_addr pointed);
   static void shade_check_from_non_black(pos_ptr_addr ptr_addr,
					  gc_obj_addr pointed);
   void consistency_check(void);
#endif
#ifdef GC_MONITOR
   void monitor(void);
#endif

#ifdef  PROFILE_WRITE_BARRIER
 public:
   int num_times_gc_turned_off;
   int both_root;
   int lval_root_rval_not;
   int rval_root_lval_not;
   int neither_root;
   int both_shaded;
   int rval_shaded_lval_not;
   int lval_shaded_rval_not;
   int both_not_shaded;
   long write_barrier_min_pause;
   long write_barrier_max_pause;
   long gc_safe_point_min_pause;
   long gc_safe_point_max_pause;
#endif
};


// This function is the default function that is called everytime a
// gc_flip happens.  It is a noop function.

void gc_flip_callback_function_default(void);


#ifdef HEAP_ALLOCATED_GC
extern garbage_collector& gc; // the instance of the garbage collector
#else
extern garbage_collector gc; // the instance of the garbage collector
#endif

// A pointer to the start of the array of object managers.
extern object_manager* *object_managers;

#include <rtgc/gcclient.h>

#endif
