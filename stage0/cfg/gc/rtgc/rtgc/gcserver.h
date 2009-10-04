#ifndef gcserver_h
#define gcserver_h


#include <stdlib.h>
#include <rtgc/config.hh>

/* This file defines prototypes for function that the garbage collector is */
/* compelled to supply.                                                    */
/*                                                                         */
/* First is the function init_gc.  It should be called before any lines    */
/* of the application program run.  This routine is provided so that the   */
/* application can have use of the routine main.  Note that in some        */
/* implementations of C++, if the garbage collector is to be linked to a   */
/* non-C++ client, the main must still be in C++.  In this case, I         */
/* suggest that the C++ main consist of two lines: init_gc(); and a call   */
/* to the client main (which must them be renamed).                        */
/*                                                                         */
/* Next are the write barrier functions.  Every time a pointer value is    */
/* modified, the client application is compelled to call one of these      */
/* functions.  Usually this is done for every assignment statement.        */
/* However, since the stack is scanned atomically at collection            */
/* termination, no write barrier call need be made for the assignment to   */
/* any root variables.                                                     */
/*                                                                         */
/* The allocation routine is called gc_alloc.  It is called to get some    */
/* memory from the garbage collected heap.  It does some book keeping in   */
/* addition to allocating memory (see the expanded documentation below).   */
/* If you think your compiler or runtime system can do a better job of     */
/* keeping this information (through compiler optimizations) then there    */
/* is a version that does no book keep ing (gc_alloc_nb).  If you use      */
/* gc_alloc_nb, then you must use gc_safe_point_nb, and not                */
/* gc_safe_point.                                                          */
/*                                                                         */
/* The safe point routines are hints to the garbage collector that it is   */
/* safe to perform garbage collection.  These routines are called          */
/* gc_safe_point, and gc_safe_point_nb.  No collection will be done at     */
/* any other time than an safe point, but not all calls to gc_safe_point   */
/* preform collection work.  The argument headroom is the maximum amount   */
/* of memory that can be allocated before the next call to a               */
/* gc_safe_point.                                                          */
/*                                                                         */
/* When locating garbage collected pointers within an object, the          */
/* language system must call gc_next_object and pass each embedded         */
/* pointer.  This is essentially a callback to the garbage collector.      */
/*                                                                         */
/* Finally, we have two routines which return the bounds of the heap:      */
/* start_of_heap, and end_of_heap.  These are provided for language        */
/* systems that have a hard time finding roots (systems without compiler   */
/* cooperation for example).                                               */



/* The macro LINK_TYPE is used to tell the compiler whether to compile
   with inlines, without inlines, or for non-c++ linkage.  See the Makefile
   more information about these options.
*/

#include <rtgc/linktype.h>

/* gc_obj_addr is a C pointer to an object that was allocated
   by the garbage collector. */

typedef void *gc_obj_addr;

/* untyped_addr is a C pointer to an object.  This is used as the lvalue
   of assignments.  The address is untyped because in some languages (C), the
   lvalue might be the middle of an object, and not the header of the object.
*/

typedef void *untyped_addr;

/* The file langtype.h is responsible for defining the type pos_ptr_addr
   (see below for more information on this type), and any other information
   the client might want to include (for inclusion into gcclient.h -- the
   client code for the garbage collector).
*/

#include <rtgc/langtype.h>


/*
 Note: Even though this file is named gcserver.h (and its companion file is
       gcclient.h, we mean server and client in the sence of the server
       being the garbage collector providing functions to the client code
       which is the language system using the garbage collector.  There
       are no rpc's going on.

*/


/* This variable shows how much memory we can allocate before beginning the
   next GC increment. */

extern int amount_allocated; 

/* This variable is set to true when gc is turned off. GC_TURN_OFF_ABLE
   can be true only in single generation black allocation.*/

#ifdef GC_TURN_OFF_ABLE
extern int gc_turned_off;
#endif

/* gc_alloc returns one garbage collectable object of size "size".
   gc_alloc records the number of bytes allocated so that gc_safe_point
   will know when to do an increment of collection. */

LINK_TYPE  gc_obj_addr gc_alloc(size_t size);


/* gc_alloc_nb (no bookkeeping) does not record the number of bytes
   allocated.  If calls are made to gc_alloc_nb, then gc_safe_point_nb
   *must* be called for the safe points, and the number of bytes
   allocated must be passed to it.  This function is only useful if
   the compiler can do some optimizations that make it cheaper for
   the language system to record this information than for the garbage
   collector.  */

LINK_TYPE gc_obj_addr gc_alloc_nb(size_t size);


/* The safe point routines are hints to the garbage collector that it is
   safe to perform garbage collection.  No collection will be done at
   any time other than a safe point, but not all calls to gc_safe_point
   preform colleciton work.  The parameter "headroom" is the maximum amount
   of memory that can be allocated before the next call to a gc_safe_point.
   If the amount allocated since the end of the last increment plus the head
   room is larger than AMOUNT_TO_ALLOCATE_PER_INCREMENT, an increment of gc
   work is performed. */

LINK_TYPE void gc_safe_point(size_t headroom);


/* This is a non-bookkeeping version of gc_alloc_safe_point, that is, the GC
   does not calculate the amount allocated. The client has to pass the
   number of bytes allocated since the last GC increment and the maximum
   amount of memory that could be allocated before the next call of
   the gc safe point function. */

LINK_TYPE void gc_safe_point_nb(size_t number_bytes_allocated,
				size_t headroom);

/* When locating garbage collected pointers within an object, the language
   system must call gc_next_object passing a pointer to the embedded pointer.
   This is essentially a callback to the garbage collector.

PARAMETERS:

   ptr_addr should be a pointer to the location in the object being traced,
   which could hold a pointer (or the NULL value).
   ptr_to_object is a pointer to the top of the object.  This should be the
                 same pointer that was returned to the language system by
		 the function gc_new().

*/

LINK_TYPE void gc_next_object(pos_ptr_addr ptr_addr,
			      gc_obj_addr ptr_to_object);


/* This function will derive the start of the object pointed to by ptr */

LINK_TYPE gc_obj_addr find_start_of_object(const void *ptr);


/*
 WRITE BARRIER FUNCTIONS

 The following functions implement the write barrier used by the
 garbage collector to track mutations to the graph of user objects.
 Every time the client code modifies a pointer value in a client
 object, the client code MUST call one of the supplied write barrier
 functions, EVEN IF THE NEW VALUE ISN'T A VALID POINTER.

 The choice of which function to call depends on what the client code
 knows about the object being mutated.  Note, in all calls, you are
 passing a pointer to the location into which the pointer value is
 being stored as the lvalue, a pointer to the top of the lvalue object
 as lobject, and a pointer to the top of the new robject as the
 new_robject.  If you are using a language in which pointers can point
 inside of an object, then call the routine:

 find_start_of_object(void *ptr);

 to find the start of the lobject and new_robject before calling the
 write barrier.

 Also note that the value actually stored in a word of memory may not
 be the bits that are passed as "robject".  For example, in Scheme,
 the lower bits of the pointer might be tag information.  These bits
 would need to be masked away before the pointer can be accessed.  In
 some write barriers, it might be necessary to examine the actual
 object pointed to by the bits in the lvalue.  The function
 cast_and_deref_ptr() (declared in gcclient.h and defined by the
 language implementor) is used to extract the actual pointer and
 dereference it.

 I introduce some type names here to help distinguish things that are
 pointers to objects (gc_obj_addr) and things that are pointers to
 small pieces of memory (words) which [may] contain pointers (the
 pos_ptr_addr type, defined in "langtype.h")

 In C++, sometimes these are blurred, but in the absence of derived
 pointers (ie, pointers to the inside of an object that is really
 (also) a pointer to an object), the two should be carefully
 distinguished.

 In particular, note that the "lvalue" is passed as a pointer to the
 word of memory that WILL CONTAIN a representation of the ACTUAL
 POINTER which is "robject"

 NOTE: It is important that the write barrier functions be called *BEFORE*
 the old pointer is overwritten with the new pointer.
*/


/* If nothing is known about the lval or the rval, then call: */

LINK_TYPE 
void write_barrier(gc_obj_addr lobject,
		   pos_ptr_addr lvalue,
		   gc_obj_addr new_robject);

    
/* If the lval is known to be an object that is newly allocated (there has
   been no chance for garbage collection), then call: */

LINK_TYPE
void write_barrier_lval_fresh(gc_obj_addr lobject,
                              pos_ptr_addr lvalue,
			      gc_obj_addr new_robject);

    
/* If the lval is known to be an object that is newly allocated (there has
   been no chance for garbage collection), and this is the initializing write
   then call: */

LINK_TYPE
void write_barrier_lval_init(gc_obj_addr lobject,
                             pos_ptr_addr lvalue,
			     gc_obj_addr new_robject);

    
/* If the lval is being written over with something that isn't a pointer
   to an object, then call: */

LINK_TYPE
void write_barrier_lval_clobber(gc_obj_addr lobject,
				pos_ptr_addr lvalue);

						  
/* If the rval is known to be a pointer to a fresh (allocated without any
   intervening calls to gc_safe_point) object call:  */

LINK_TYPE 
void write_barrier_rval_fresh(gc_obj_addr lobject,
			      pos_ptr_addr lvalue,
			      gc_obj_addr new_robject);


/* If the lval and rval are known to be fresh, then call: */

LINK_TYPE
void write_barrier_lval_fresh_rval_fresh(gc_obj_addr lobject,
                                         pos_ptr_addr lvalue,
					 gc_obj_addr robject);
    

/* If the lval is an initializing write, and the rval is known to be fresh,
   then call: */

LINK_TYPE
void write_barrier_lval_init_rval_fresh(gc_obj_addr lobject,
					pos_ptr_addr lvalue,
					gc_obj_addr robject);
    


/* Init_gc must be called before any lines of the application program run. */

EXTERNAL_LINK_TYPE void init_gc(int argc, const char **);


/* gc_full_collect performs a complete garbage collection.  If the marking */
/* process is underway when this routine is called, it completes the       */
/* collection from the point that it has already reached (in other words,  */
/* it does not start the marking process over from the begining).          */

EXTERNAL_LINK_TYPE void gc_full_collect(void);

/* start_of_heap returns the start of the garbage collected heap. */

EXTERNAL_LINK_TYPE const void *const start_of_heap(void);


/* end_of_heap returns the end of the garbage collected heap. */

EXTERNAL_LINK_TYPE const void *const end_of_heap(void);

/* The next functions have been implemented so that language systems can get */
/* at objects in the garbage collector's free list for various reasons.      */
/* These reasons include debugging, and object finalization.                 */

/* N.B.  These functions only return the correct values if next...object()   */
/* is called immediately after the call to reset...object_iterator().  Any   */
/* other garbage collector functions called between these two calls could    */
/* cause next...object() to return an incorrect value.                       */
/* Also, It only makes sence to use the iterators immediately after a        */
/* call to gc_full_collect                                                   */

/* Resets the iterator to the first live object in any of the garbage */
/* collector's color_set lists.                                       */

EXTERNAL_LINK_TYPE void reset_not_known_free_object_iterator(void);

/* Returns a pointer to the next live object in the garbage collector's  */
/* color_set lists.  This pointer will be the same pointer that was      */
/* returned by gc_alloc().  The routine will return NULL after a pointer */
/* to the last object has been returned.                                 */

EXTERNAL_LINK_TYPE gc_obj_addr next_not_known_free_object(void);

/* Resets the iterator to the first dead object in any of the garbage */
/* collector's color_set lists.                                       */

EXTERNAL_LINK_TYPE void reset_dead_object_iterator(void);

/* This is the same as next_live_object except that it returns a pointer     */
/* to the next dead object.  Objects on the free list are considered neither */
/* live nor dead, and hence, no pointer to them is returned.                 */

EXTERNAL_LINK_TYPE gc_obj_addr next_dead_object(void);

/* This function registers a function with the garbage collector that */
/* will be called just before each gc_flip(), i.e. it will be called  */
/* just before dead objects are placed into the free list.  The       */
/* default (should no function be registered) will be a noop.         */
/* To reset the function back to the default, just call this function */
/* with NULL as the argument.                                         */

EXTERNAL_LINK_TYPE void register_dead_object_callback(void (*fp)());

/*  set_dead_flag sets a flag on the object that is passed to it saying  */
/*  that the object is really dead.  The usual case for this flag will   */
/*  be to help with debugging.  The user will call this function on      */
/*  every object that is reaturned by next_dead_object.  Later, the      */
/*  client code can assert on this value when if access an object to     */
/*  make sure the garbage collector isn't prematurely reclaiming objects */
      
EXTERNAL_LINK_TYPE void set_dead_flag(gc_obj_addr the_object);
EXTERNAL_LINK_TYPE void clear_dead_flag(gc_obj_addr the_object);
EXTERNAL_LINK_TYPE INT_32 get_dead_flag(gc_obj_addr the_object);

/*  The function is_object_live will return true if the object is not
    known to be dead, and false otherwise. */

EXTERNAL_LINK_TYPE bool is_object_dead(gc_obj_addr obj);

/* Call gc member functions. */
EXTERNAL_LINK_TYPE gc_obj_addr call_gc_new_object(INT_32 size_class);
#endif
