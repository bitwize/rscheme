/*-----------------------------------------------------------------*-C-*---
 * File:    handc/cfg/gc/freelist/gcclient.h
 *
 *          Copyright (C)1997 Donovan Kolbly <d.kolbly@rscheme.org>
 *          as part of the RScheme project, licensed for free use.
 *          See <http://www.rscheme.org/> for the latest information.
 *
 * File version:     1.4
 * File mod date:    1997-11-29 23:10:46
 * System build:     v0.7.3.4-b7u, 2007-05-30
 *
 *------------------------------------------------------------------------*/

#ifndef gcclient_h
#define gcclient_h

#include <rscheme/gcserver.h>

#ifdef INLINES
#include <rscheme/gcclient1.ci>
#include <rscheme/gcclient2.ci>
#endif

/* This file contains prototypes for functions that the client code is     */
/* compelled to supply.  The garbage collector will call these functions.  */

/* ROOT SET FUNCTIONS */

/* The interface to the root set in the client code is implemented as an         */
/* iterator over the pointers into the garbage collected heap.  A call to        */
/* root_reset sets the iterator to the first pointer in the root set, and        */
/* a call to root_next returns the next pointer from the root set and increments */
/* the iterator.  root_next returns NULL (0) when the entire root set has        */
/* been scanned.                                                                 */

void stable_root_reset( void );
gc_obj_addr stable_root_next( void );

void quasistable_root_reset( void );
gc_obj_addr quasistable_root_next( void );

void unstable_root_reset( void );
gc_obj_addr unstable_root_next( void );


/* POINTER FINDING FUNCTIONS

 The function find_pointers will be called with a pointer into the user's 
 object.  The user is then responsible for iteratively calling the function 
 gc_call_back (declared in gcserver.h) with  a pointer to each of the pointers
 in the pointed-to object.

 For example:

 Suppose the user object has two pointers (call them p1 and p2).  
 The user would then make the following *two* calls to next_object:

 next_object(&p1);
 next_object(&p2);

*/


void find_pointers( gc_obj_addr pointer_to_client_object );

/* MANAGING THE STORED INTO LIST

 Calls to the write barrier might cause us to save a pointer to one of the 
 pointers in the user's object.  Because the user's pointer might not be
 a valid C pointer, the following function is to be supplied.  It takes
 as its argument, a pointer to the user's pointer and returns a C pointer
 version of the pointed to object.

 The convention is that a gc_obj_addr of NULL means "No Object", 
 which this fn may return if a word is rewritten (after the
 write barrier remembers it) with a value that is not a pointer
 type, or, in C, has value NULL. */

gc_obj_addr cast_and_deref_ptr( pos_ptr_addr slot );


#endif
