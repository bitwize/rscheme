#ifndef gcclient_h
#define gcclient_h

/* This file contains prototypes for functions that the client code is     */
/* compelled to supply.  The garbage collector will call these functions.  */

/* The macro LINK_TYPE is used to conditionally compile the code   */
/* either for inlining, no inlining, or foreign function calling. */

#undef LINK_TYPE
#ifdef __cplusplus

#ifdef FOREIGN
#define LINK_TYPE extern "C"
#else

#ifdef INLINES
#define LINK_TYPE inline
#else
#define LINK_TYPE 
#endif

#endif /* of ifdef FOREIGN */

#else /* not compiling with c++ compiler */
#define LINK_TYPE
#endif /* of ifdef __cplusplus */

#include <rtgc/config.hh>
#include <rtgc/langtype.h>
#include <rtgc/gcserver.h>


/* ROOT SET FUNCTIONS */

/* The interface to the root set in the client code is implemented as an     */
/* iterator over the pointers into the garbage collected heap.  A call to    */
/* root_reset sets the iterator to the first pointer in the root set, and    */
/* a call to root_next returns the next pointer from the root set and        */
/* increments the iterator.  root_next returns NULL (0) when the entire root */
/* set has been scanned.                                                     */
/*                                                                           */
/* Since it is possible for some roots to have their values change less      */
/* often than other roots, the language interface provides for three root    */
/* scanning primitives.  One lets the collector scan the stable roots first, */
/* the second scans the intermediate roots, and the last scans the unstable  */
/* roots.                                                                    */
/*                                                                           */
/* NB: It is very important that the client do *no* allocation during a call */
/* by the collector to one of the root scanning routines.                    */

LINK_TYPE void stable_root_reset( void );
LINK_TYPE gc_obj_addr stable_root_next( void );

LINK_TYPE void quasistable_root_reset( void );
LINK_TYPE gc_obj_addr quasistable_root_next( void );

LINK_TYPE void unstable_root_reset( void );
LINK_TYPE gc_obj_addr unstable_root_next( void );

/* is_root returns true if pos_ptr_addr is the address of a pointer contained
   in a root variable. */

LINK_TYPE int is_root(pos_ptr_addr ptr);


/* on_stack returns true if the object pointed to is on the stack. */

LINK_TYPE int on_stack(gc_obj_addr ptr);


/* init_gcclient is called when the garbage collector is initialized,
   so that the client can initialize any of its root parameters or
   whatever.  If this functionality is not needed, define this as a
   null function. */

LINK_TYPE void init_gcclient(int argc, const char **argv);


/* POINTER FINDING FUNCTIONS

 The function find_pointers will be called with a pointer into the user's 
 object.  The user is then responsible for iteratively calling the function 
 gc_next_object (declared in gcserver.h) with a pointer to each of the
 pointers in the pointed-to object.

 For example:

 Suppose the user object has two pointers (call them p1 and p2).  The user
 would then make the following *two* calls to gc_next_object:

	gc_next_object(p1);
	gc_next_object(p2);

The first argument "max_bytex_to_trace" is the maximum bytes of the object
that should be looked at for pointers *reguardless of whether any pointers
exists in that many bytes*.  In otherwords, simply count the number of bytes
in the object and make sure this number doesn't exceed max_bytes_to_trace.
If it does, then find_pointers should return early, but save enough state to
resume tracing where if previously left off.

Incrementally tracing objects is required to meet hard real-time deadlines.
If you are only interested in soft real-time collection, then you can trace
more than max_bytes_to_trace.  Just be aware that this will cause slightly
longer GC pauses.

The argument Done should be set to true iff the entire object is traced.

The return value should be the actual number of bytes traced during this
call to find_pointers.

*/

LINK_TYPE UINT_32 find_pointers(UINT_32 max_bytes_to_trace,
				bool& Done,
				gc_obj_addr ptr_to_client_object);

/* MANAGING THE STORED INTO LIST

 Calls to the write barrier might cause us to save a pointer to one of the 
 pointers in the user's object.  Because the user's pointer might not be
 a valid C pointer, the following function is to be supplied.  It takes
 as its argument, a pointer to the user's pointer and returns a C pointer
 version of the user's pointer.

 The convention is that a gc_obj_addr of NULL means "No Object", 
 which this function may return if a word is rewritten (after the
 write barrier remembers it) with a value that is not a pointer
 type, or, in C, has value NULL. */

LINK_TYPE gc_obj_addr cast_and_deref_ptr(pos_ptr_addr ptr);

/* FINALIZATION

 If the language system wishes to implement finalization of objects, it needs
 to know when it is safe to proceed.  This point is just after all objects
 have been traced, and just before the garbage objects are reclaimed.  The
 garbage collector will call the function finalize_now at this point.  If
 the language system does not implement finalization, then this routine
 can just be defined as empty. */

LINK_TYPE bool safe_for_is_object_dead(void);

#endif
