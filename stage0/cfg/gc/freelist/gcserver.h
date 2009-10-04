/*-----------------------------------------------------------------*-C-*---
 * File:    handc/cfg/gc/freelist/gcserver.h
 *
 *          Copyright (C)1997 Donovan Kolbly <d.kolbly@rscheme.org>
 *          as part of the RScheme project, licensed for free use.
 *          See <http://www.rscheme.org/> for the latest information.
 *
 * File version:     1.4
 * File mod date:    1997-11-29 23:10:47
 * System build:     v0.7.3.4-b7u, 2007-05-30
 *
 *------------------------------------------------------------------------*/

#ifndef _H_GCSERVER
#define _H_GCSERVER

#include <stddef.h>

typedef void *gc_obj_addr;

#include <rscheme/langtype.h>
#ifdef INLINES
#include <rscheme/gcxverse.h>
#endif

/*
 This file defines prototypes for function that the garbage collector is
 compelled to supply.  The client code should call these routines:


 WRITE BARRIER FUNCTIONS

 The following functions implement the write barrier used by the garbage
 collector to track mutations of user objects.  Every time the client code
 modifies a pointer value in a client object, the client code must call
 one of the supplied write barrier function.  The choice of which function
 to call depends on what the client code knows about the object being
 mutated, and the state of the garbage collector.  Note, in all calls, you
 are passing a pointer to the location into which the pointer value is
 being stored as the lvalue, and the pointer value itself as the robject.
 Also note that the value actually stored in a word of memory may not be
 the bits that are passed as "robject".  The purpose of
 cast_and_deref_ptr() (declared in gcclient.h) is to extract the actual
 pointer.

 I introduce some type names here to help distinguish
 things that are pointers to objects and things that
 are pointers to small pieces of memory (words) which 
 may contain pointers (the pos_ptr_addr type, defined in "langtype.h")

 In C++, sometimes these are blurred, but in the absence
 of derived pointers (ie, pointers to the inside of an 
 object that is really (also) a pointer to an object),
 the two should be carefully distinguished.

 In particular, note that the "lvalue" is passed as a
 pointer to the word of memory that WILL CONTAIN a
 representation of the ACTUAL POINTER which is "robject"

*/



/* If there can be pointers *into* client objects, then use one of the following
   write barrier functions:

   If the rval is known to be a pointer to a white object (say, because the
   garbage collector is allocating white, and the rval is a pointer to a
   new object), call:  */

CI_DECL void write_barrier_rval_white(pos_ptr_addr lvalue);
    
/* If the lval is known to be a black object, then call: */

CI_DECL void write_barrier_lval_fresh(pos_ptr_addr lvalue, gc_obj_addr robject);
    
/* If nothing is known about the rval or the lval, then call: */

CI_DECL void write_barrier(pos_ptr_addr lvalue, gc_obj_addr robject);

/* If the system can't generate derived pointers (pointers into objects), then
   call the following versions of the above write barrier functions: */

CI_DECL void write_barrier_rval_white_NDP(gc_obj_addr lobject, pos_ptr_addr lvalue);
CI_DECL void write_barrier_lval_fresh_NDP(gc_obj_addr lobject,
					       pos_ptr_addr lvalue,
					       gc_obj_addr robject);
CI_DECL void write_barrier_NDP( gc_obj_addr lobject, pos_ptr_addr lvalue, gc_obj_addr robject );

CI_DECL void init_gc( int argc, const char **argv );

CI_DECL gc_obj_addr gc_alloc(size_t size);

CI_DECL gc_obj_addr gc_alloc_from_size_class( size_t size, void *sz_c );
CI_DECL void *gc_get_size_class( size_t size );

CI_DECL void gc_safe_point( size_t bytes );

void gc_full_collect( void );
void gc_weak_pointer( lang_weak_ptr_addr wpa );

#ifdef VALIDATE_BLOCKS
void validate_block( gc_obj_addr p );
#endif

#ifdef INLINES
#include "gcserver1.ci"
#include "gcserver2.ci"
#endif


#endif
