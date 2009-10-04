#ifndef COLORSET_HH
#define COLORSET_HH
// Revised:  16 Nov 1993    Mike Hewett
//
// Added:     color_set data members:  
//              unsigned int number_of_objects_per_snarf
//              unsigned int number_of_snarfed_objects_allocated 
//              char *       local_high_water_mark
//
//            color_set member functions:
//              allocate_one_object(unsigned int object_size);
//
// ---------------------------------------------------------------

// This file contains code specific to the implementation details of the
// particular collection algorithm being used.
//
// It implements the segragated storage scheme using Baker like 
// color sets to implement the implicit reclaimation part of the
// collector.

#include <assert.h>
#ifndef NDEBUG
#include <iostream.h>
#endif
#include <rtgc/config.hh>
#include <rtgc/gcserver.h>

#undef LINK_TYPE
#ifdef INLINES
#define LINK_TYPE inline 
#else
#define LINK_TYPE
#endif

// Forward declarations
class PTR_Any;
class color_set;
class garbage_collector;

extern garbage_collector gc;


// There are only two colors, shaded and non shaded. Both black and gray are
// shaded. There is no way to determine whethere black or gray just looking
// at the object. The difference is simply the position in the list.
// Also, there is no fixed color, like shaded or non-shaded. Each time, GC
// cycle finishes, the shaded color is changed. In other words, if 1 is
// shaded in this cycle, it will mean non shaded in the next cycle. So,
// we don't have to change the color of objects when we whiten blacks.
// In generational version, the shading color may be different depending on
// the generation. 
// In future, we want to use bool as the definition of colors, instead of int. 

const int INIT_SHADING_COLOR=0;   // Another color is !0.
typedef int colors;


class object_manager;

// This class implements the hidden headers that must be on every garbage
// collected object.  It basically implements the doubly linked list
// nature of the color set, as well as the generation, step and shaded
// information.

class gc_object_base {
// ***** Variable sections.
 protected:
   // pointers to the previous and next color set object in the
   // color set list
   gc_object_base *previous_, *next_;

#ifndef SMALL_GC_HEADERS
   char color;  // The current color of the object


   char generation;  // What generation the object belongs to


   char step;        // What step within a generation

// The dead flag is *only* for use with debugging.  It is up to the
// client code to set this flag.  The garbage collector does *not* set
// this flag in normal usage.  Usually, the client code will set this
// flag in every object that it gets from the dead object iterator
// (see gcserveri.ci) after the end of each collection.
   char dead_flag;  // If this flag is set, then the garbage collector
   // thinks that the object is really dead.

   // The next field is a pointer to the color set that contains an object.
   // This is used so that when we find a pointer to an object we can tell
   // that object to gray it self.  I don't like this design because it uses
   // an extra word in every object allocated.  Try to come up with a better
   // design.  One possible idea is to code the size class of the object.
   // This would only take up one byte.  The size class, along with the
   // object's generation would tell us which color set it is in.  The
   // down side is that the code to find the object's color set would be
   // somewhat less efficient than the pointer reference that is now needed.
   // I should make the access to this field a method so that we can change
   // the access and test the impact both ways.
   color_set *containing_list;
#endif

// ******* Member funcitons.
// ****** Contructors and Init
 public:
   gc_object_base ()
   { 
      // We assume that gc_objects are at least 8 bytes long.
      // Since a gc_object can have 0 useable bytes (to support
      // languages like C++ that can have pointers to 0 byte objects)
      // we assert that the size of the header for a gc_object
      // is at least 8 bytes in size.
#ifndef NDEBUG
      if(!(sizeof(class gc_object_base) >= 8)){
	 cerr << "error: assertion failed at " << __LINE__ << " of " << __FILE__ << endl;
	 exit(3);
      }
#endif

   }

// ******* Basic access functions.
// ******* Linking pointers
   // We use some of the lower bits of the previous_ and next_ pointers
   // to hold additional garbage collector information.
   // If the flag SMALL_GC_HEADERS is defined, then we use the following
   // bits:

   // previous_ bit 0: the color
   // previous_ bit 1: the step
   // previous_ bit 2: the dead flag
   // next_ bits 0 & 1: the generation

   // Note: we are assuming that all garbage collection objects are at
   // least 8 bytes large (the size of the header).  So, we know that they
   // will always be aligned on 8 byte boundaries.  We can thus use the lower
   // 3 bits of the next_ and prevous_ pointers without loosing any
   // information.

#ifndef SMALL_GC_HEADERS
   LINK_TYPE gc_object_base *get_previous(void)
   {
      return(previous_);
   }
   LINK_TYPE void set_previous(gc_object_base *pointer)
   {
      previous_ = pointer;
   }

   LINK_TYPE gc_object_base *get_next(void)
   {
      return(next_);
   }
   LINK_TYPE void set_next(gc_object_base *pointer)
   {
      next_ = pointer;
   }
#else
#define COLOR_SET_POINTER_MASK 0xfffffffc
   LINK_TYPE gc_object_base *get_previous()
   {
      return((gc_object_base *)
	     (((unsigned)previous_) & COLOR_SET_POINTER_MASK));
   }
   LINK_TYPE void set_previous(gc_object_base *pointer)
   {
      assert((unsigned(pointer) & (~COLOR_SET_POINTER_MASK)) == 0);
      previous_ = ((gc_object_base *)
		   ((((unsigned)previous_) & (0x3)) |
		    ((unsigned)pointer)));
   }

   LINK_TYPE gc_object_base *get_next()
   {
      return((gc_object_base *)
	     (((unsigned)next_) & COLOR_SET_POINTER_MASK));
   }
   LINK_TYPE void set_next(gc_object_base *pointer)
   {
      assert((unsigned(pointer) & (~COLOR_SET_POINTER_MASK)) == 0);
      next_ = ((gc_object_base *)
	       ((((unsigned)next_) & (0x3)) |
		((unsigned)pointer)));
   }
#endif


// ******** Colors
#ifndef SMALL_GC_HEADERS
   // methods to get and set the color of the object
   LINK_TYPE void set_color(colors c) {color = (char)c;}
   LINK_TYPE colors get_color(void) {return (colors)color;}
#else
#define COLOR_BIT_MASK (WORD_OF_ONE - 1)
   LINK_TYPE void set_color(colors c) 
   {
      assert(int(c) >=0 && int(c) <= 1); // assumes one bit of color
      // The color information goes into bit 0 of previous
      previous_ = (gc_object_base *)((((unsigned) previous_) &
				      COLOR_BIT_MASK) |
				     ((unsigned)c));
   }
   LINK_TYPE colors get_color(void)
   {
      return((colors) (((unsigned) previous_) &
		       (0x1)));
   }
#endif // SMALL_GC_HEADERS			 


// ********** Dead Flag
#ifndef SMALL_GC_HEADERS

// The dead flag is *only* for use with debugging.  It is up to the
// client code to set this flag.  The garbage collector does *not* set
// this flag in normal usage.  Usually, the client code will set this
// flag in every object that it gets from the dead object iterator
// (see gcserveri.ci) after the end of each collection.

   // methods to get and set the dead flag of the object
   LINK_TYPE void set_dead_flag(char df) {dead_flag = df;}
   LINK_TYPE char get_dead_flag(void) {return dead_flag;}
#else
#define DEAD_FLAG_BIT_MASK (WORD_OF_ONE - 4)
   LINK_TYPE void set_dead_flag(char df) 
   {
      assert(int(df) == 0 || int(df) == 1); // assumes one bit for the dead
	                                      // flag.
      // The dead flag information goes into bit 2 of previous_
      previous_ = (gc_object_base *)((((unsigned) previous_) &
				      DEAD_FLAG_BIT_MASK) |
				     ((unsigned)df));
   }
   LINK_TYPE char get_dead_flag(void)
   {
      return((char) (((unsigned) previous_) &
		     (0x4)));
   }
#endif // SMALL_GC_HEADERS			 

// ************** Generation *****************
// methods to get and set the generation of the object.
#ifndef SMALL_GC_HEADERS

#ifdef GENERATIONAL
   LINK_TYPE void set_gen_num(int generation_number)
   {
      generation = (char)generation_number;
   }
#else
   LINK_TYPE void set_gen_num(int)
   {
      // do nothing
   }
#endif


#ifdef GENERATIONAL
   LINK_TYPE int get_gen_num(void)
   {
      return (int)generation;
   }
#else
   LINK_TYPE int get_gen_num(void)
   {
      return(0); // if we are not using generational collection, then
                 // all objects are in the 0ith generation.
   }
#endif


#else // small gc header case

#define GENERATION_NUMBER_MASK 0xfffffffc

#ifdef GENERATIONAL
   LINK_TYPE void set_gen_num(int generation_number)
   {
      // assume two bits for the generation number (i.e. max 4 generations)
      // The generation info goes in bits 0 & 1 of next
      assert(generation_number >= 0 && generation_number <= 3);
      next_ = (gc_object_base *)((((unsigned) next_) &
				  GENERATION_NUMBER_MASK) |
				 unsigned(generation_number));
   }
#else
   LINK_TYPE void set_gen_num(int)
   {
      /* do nothing */
   }
#endif

   LINK_TYPE int get_gen_num(void)
   {
#ifdef GENERATIONAL
      return((int) (((unsigned) next_) &
		    (~GENERATION_NUMBER_MASK)));
#else
      return(0); // if we are not using generational collection, then
      // all objects are in the 0ith generation.
#endif
   }
#endif // SMALL_GC_HEADERS


// ******* Step *************
// methods to get and set the step of the object.

#ifndef SMALL_GC_HEADERS

#ifdef GENERATIONAL
   LINK_TYPE void set_step(int step_number)
   {
      step = (char)step_number;
   }
#else
   LINK_TYPE void set_step(int)
   {
      // do nothing
   }
#endif


   LINK_TYPE int get_step(void)
   {
#ifdef GENERATIONAL
      return(int)step;
#else
      return(0); // if we are not using generational collection, then
      // all objects are in the 0ith step.
#endif // GENERATIONAL
   }


// This functions determines the target object is well matured to be
// promoted, or not. The object should be shaded and younger generation,
// otherwise it may return a false result.
   LINK_TYPE int to_be_promoted(void){
#ifdef GENERATIONAL	
      assert(is_shaded());
      assert(get_gen_num() != NUMBER_OF_GENERATIONS - 1);
      return(step >= NUMBER_OF_STEPS);
#else // GENERATIONAL	
      return(0);
#endif // GENERATIONAL	
   }

   LINK_TYPE void increment_step(void)
   {
#ifdef	GENERATIONAL
      assert(get_gen_num() <= NUMBER_OF_GENERATIONS - 1);
      step++;
#endif // GENERATIONAL
   }

#else //SMALL_GC_HEADERS
#define STEP_NUMBER_MASK 0xfffffffd
#ifdef GENERATIONAL
   LINK_TYPE void set_step(int step_number)
   {
      // assume one bits for the step number (i.e. max 2 steps)
      // The step info goes into bit 1 of previous.
      assert(step_number >= 0 && step_number <= 1);
      previous_ = (gc_object_base *)((((unsigned) previous_) &
				      STEP_NUMBER_MASK) |
				     (unsigned(step_number) << 1));
   }
#else
   LINK_TYPE void set_step(int)
   {
      // do nothing
   }
#endif

   LINK_TYPE int get_step(void)
   {
#ifdef GENERATIONAL
//	assert(is_shaded());
//	assert(get_gen_num() != NUMBER_OF_GENERATIONS - 1);
      return((int) ((((unsigned) previous_) &
		     (~STEP_NUMBER_MASK)) >> 1));
#else
      return(0); // if we are not using generational collection, then
      // all objects are in the 0ith step.
#endif
   }
   LINK_TYPE int to_be_promoted(void){
#ifdef GENERATIONAL	
      return (get_step() == 0);
#else //  GENERATIONAL	
      return(0);
#endif // GENERATIONAL	
   }
   LINK_TYPE void increment_step(void){
#ifdef GENERATIONAL
      previous_ = (gc_object_base*)((unsigned)previous_ ^ ~STEP_NUMBER_MASK);
#endif	
   }
#endif // SMALL_GC_HEADERS

// Check whether it is shaded or not. It gets its belonging generation
// and takes some time, so if you know the containing generation, you
// should not use this member function.
   LINK_TYPE int is_shaded();

// ****** Containing list access ********* 
#ifndef SMALL_GC_HEADERS    	
   void set_containing_list(color_set *cs)
   {
      containing_list = cs;
   }
#else
   void set_containing_list(color_set *)
   {
      // In the small gc header, there is no containing_list pointer.
   }
#endif

   LINK_TYPE color_set *get_containing_list(void);

   // Call this version if you already have a pointer to the object
   // manager for the object.
   LINK_TYPE color_set *get_containing_list(object_manager *om);

};


class color_set {
// *********** Variables **************

 protected:
   // This size is set to the maximum object size including the header.
   int size;

   // size_class is the size class number in which this object belongs.
   // currently, this is the log2 of the total object size (including the
   // header), but this is likely to change.
   int size_class;

   // This shows the generation to which this color set belongs to.
   int my_generation;
   
   object_manager *my_object_manager;

// When allocating new objects black, the list is laid out in the
// following form:
//
// [] GRAY BLACK FREE [] WHITE []
//  ^      ^     ^    ^        ^
//  G      S     F    W        T
//
// Where G is the gray (head) pointer
//       S is the scan pointer
//       F is the free pointer
//       W is the white pointer
//       T is the tail pointer
//       [] is a special object that takes no space and is only
//          included to make some of the routines slightly faster.
//
// With the sets of objects determined by the following pointer pairs:
//
// GRAY:  gray(exclusive) to scan(exclusive)
// BLACK: scan(inclusive) to free(exclusive)
// FREE:  free(inclusive) to white(exclusive)
// WHITE: white(exclusive) to tail(exclusive)
//
// Note that this design keeps any pointers from every pointing at any
// objects in the gray or white sets.  With this, code in the write
// barrier that grays objects can do so unconditionally.
//
// When allocating new objects white, the list is laid out in the
// following form:
//
// [] GRAY BLACK [] WHITE FREE []
//  ^      ^      ^   ^   ^    ^
//  G      S      W   OF  F    T
//
// Where the pointers are the same as above with OF being the old free pointer.
//
// With the sets of objects determined by the following pointer pairs:
//
// GRAY:  gray(inclusive) to scan(exclusive)
// BLACK: scan(inclusive) to white(exclusive)
// FREE:  free(inclusive) to tail(exclusive)
// WHITE: white(exclusive) to free(exclusive)
// Was black, Now garbage: white(exclusive) to old_free(exclusive)
// Never been shaded: old_free(inclusive) to free(exclusive)
//
// Note, the algorithms in the garbage collector all assume that there
// is always one free object on the free list, and when the free list is
// down to one object, this is detected as no free objects.   (this makes
// the allocation routines much more efficient.  This free object contains
// no actual space (it is a member of the color_set class), so the list
// incures no overhead for this optimization.
//
// Note, the algorithms in the garbage collector all assume that no pointers
// point at any of the white or gray objects.  This makes the write barrier
// for the hard real-time code much more efficient; They can simply move an
// object from the white set to the gray set without having to worry about
// there being any pointers pointing to that object.

#ifdef DEBUG_LEVEL2
 public:
#endif
   gc_object_base *gray;
   gc_object_base *scan;
   gc_object_base *white;
   gc_object_base *free;
   gc_object_base *tail;
   
   // old_free is used only when allocating WHITE to
   // segragate never been shaded from was black now is garbage.
   gc_object_base *old_free; 
   
   // This is the tail object.  Its purpose is to allow for some optimizations
   // in the color set routines.  These optimizations always assume that
   // there will be at least one object in every size class, and that that
   // object will never be allocated.  So, we do not need any actual memory
   // behind that object.  We just allocate the header for the object
   // here in the size class.

   gc_object_base tail_object;
   
   // This is the head object.  Its purpose is to allow for some optimizations
   // in the write barrier routines for the hard real-time collector.
   // The basic idea is to design the white set so that no pointers point
   // to any objects in that set.  By doing this, an object can be moved
   // unconditionally to the black set (without worrying about moving the
   // set boundry pointers with that object).  We need the head object so
   // that in degenerate cases (where some sets are empty), the pointers
   // that seperate the sets have something to point to.
   
   gc_object_base head_object;
   
   // This is an object who`s purpose is to seperate the white set from the
   // free set by at least one object.  By doing this, we can always
   // unconditionally remove objects from the white set without having
   // to worry about there being set pointers pointing to any of them.

   gc_object_base separator_object;

   char * local_high_water_mark;  // for incremental page chopping.

   // This variable is used to put a bounds on the current page
   // that is being carved up incrementally to add more objects
   // to the garbage collector`s heap.

   char *max_incremental_memory_extent; // For debugging only!

   int max_live;
 public:    
   int number_of_free; // Ideally, gc must be done by the time this is 0.
   int number_of_objects;             // The number of objects of this size
                                       // class
   // Number_of_non_black is the sum of the numbers of white, free and gray
   // objects in the color set. After a gc_flip, this value is equal to 
   // the number of free objects, because all white object become free, and
   // there must not be any gray objects at the end of gc cycle.
   int number_of_non_black;
 protected:
   int number_of_objects_per_snarf;   // amount of space allocated at once.
   int number_of_snarfed_objects_allocated;

   // The object iterators return each live and dead object respectively.
   // The live object iterator iterates through the black and gray objects,
   // and the dead object iterator iterats through the white objects.
   // The next two variables are pointers into these two lists.

   gc_object_base *not_known_free_object_iterator;
   gc_object_base *dead_object_iterator;


// ************* Member functions ******************
 public:
// ***** Contructors and Init
   // This routine sets up the size class doubly linked list and the private
   // data members.
   void init(int gen_num, int size_class);

   // This is the constructor for color sets.  It sets up the color set
   // pointers.
   color_set(int gen_num, int size_class);

// **** color sets
   gc_object_base *get_free(void){return(free);}
   gc_object_base *get_white(void){return(white);}
   gc_object_base *get_gray(void){return(gray);}
   gc_object_base *get_black(void){return(scan);}
   gc_object_base *get_tail(void){return(&tail_object);}

   LINK_TYPE void insert_as_black(gc_object_base *object);
   LINK_TYPE void insert_as_white(gc_object_base *object);    


   // This function inserts a list of objects into the free set.
   // It assumes that this set is empty

   void insert_objects_into_free_set(gc_object_base *free_head,
				     gc_object_base *free_tail,
				     int number_of_inserted);

// **** Static information
   int get_num_of_objects(void){return(number_of_objects);}
   int get_num_of_non_black(void){return(number_of_non_black);}
   int get_num_of_free(void){return(number_of_free);}

// **** Allocation 
   // This routine returns one object of the size represented by this 
   // size class.

   gc_object_base* allocate(void);


   //This routine prepares more objects for allocation when no free is left.

   void reclaim_or_allocate_objects_when_no_free(void);


   // Move free objects from the older generations to the younger generations.
   // This function assumes that there are no free objects in the youngest
   // generaiton.
   // If any objects are moved, this function returns 1, otherwise it returns
   // 0

   int move_old_free_to_young_free(void);


   // allocates one object from snarfed memory.

   void allocate_one_object(UINT_32 object_size);


   // snarfs more memory for this size class from the OS.

   void snarf_more_memory(void);


// ***** Tracing
   // This routine returns the amount of work that should be charged 
   // against an allocation of this object.
   int get_work(void) {return size;}
   int get_size(void) {return size;}
   int get_gen_of_this_set(void) {return my_generation;}

   // returns true if there are any gray objects in this size class, 
   // false otherwise.
   LINK_TYPE int any_grays(void);


   // This routine blackens (blackens the object and grays its children) the
   // next object in the gray set (the object at the scan pointer).
   // blacken's one gray, returns 0 when no grays are left
   LINK_TYPE int blacken_next_gray(UINT_32& work_done, UINT_32 work_left_to_do); 

   // This routine blackens (blackens the object and grays its children) the
   // next object in the gray set (the object at the scan pointer).
   // If necessary, it promotes the object and do the necessary jobs.
   // If there is no gray, returns 0.
   LINK_TYPE int blacken_or_promote_next_gray(UINT_32& work_done, UINT_32 work_left_to_do);

   // This routine is a wrapper function that finds the object manager
   // for the object pointed to by the the argument and has that
   // object manager gray that object.
   LINK_TYPE static void gray_a_white(pos_ptr_addr ptr_addr,
			    gc_obj_addr ptr_val);

   // This routine expects to be passed a pointer to an object which it will
   // gray.
   LINK_TYPE void gray_this_object(gc_object_base *object);


// ***** Reclamation
   // this routine implicitly collects the garbage after all live data has
   // been marked
   void reclaim_garbage();

// ***** Interators
   // These functions implement the object iterators.  They return NULL
   // when they are called after the last object is the appropriate set
   // has been returned.  It is assumed that no garbage collector 
   // activity occurs between the call to the reset functions and the
   // calls to the next...object functions.

   LINK_TYPE void reset_not_known_free_object_iterator(void);
   LINK_TYPE gc_obj_addr next_not_known_free_object(void);
   LINK_TYPE void reset_dead_object_iterator(void);
   LINK_TYPE gc_obj_addr next_dead_object(void);


#ifdef DEBUG_LEVEL2
   // sanity_check() is a debugging routine, and serves no purpose in the
   // finished garbage collector.
   void sanity_check();
   
   // in_which_color_list scans this color_set and returns which part of
   // list the obj belongs to.
   int in_which_color_list(gc_object_base *obj);

#endif //DEBUG_LEVEL2

#ifdef TEST_FRAGMENTATION
 public:
   int num_pages;
   int num_pages_with_n_objects[PAGE_SIZE];
#endif
};  // end of class color_set


#endif
