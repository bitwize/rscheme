/*-----------------------------------------------------------------*-C-*---
 * File:    handc/cfg/gc/irc/gcserver.h
 *
 *          Copyright (C)1997 Donovan Kolbly <d.kolbly@rscheme.org>
 *          as part of the RScheme project, licensed for free use.
 *          See <http://www.rscheme.org/> for the latest information.
 *
 * File version:     1.7
 * File mod date:    1998-12-04 10:37:36
 * System build:     v0.7.3.4-b7u, 2007-05-30
 *
 *------------------------------------------------------------------------*/

#ifndef _H_GCSERVER
#define _H_GCSERVER

#define GC_IRC

#include <rscheme/obj.h>

typedef struct IRC_Header *gc_obj_addr;

typedef struct IRC_Heap IRC_Heap;

/*
   define the basic interface
*/

IRC_Heap *IRC_newHeap( void );

/*
   some of these functions get shadowed by macros
   with the same name when GC_MACROS is defined
*/

void *IRC_alloc( IRC_Heap *heap, UINT_32 bytes );
void *IRC_allocFromSizeClass( IRC_Heap *heap, UINT_32 bytes, int sc );

int IRC_logicalSizeClassOf( UINT_32 bytes );

int IRC_writeBarrier( IRC_Heap *heap, 
		      void *lvalue, UINT_32 offset, 
		      void *rvalue );
int IRC_tripWriteBarrier( IRC_Heap *heap, 
		          void *lvalue, UINT_32 offset, 
		          void *rvalue );
void IRC_clientWriteViolation( IRC_Heap *heap, void *item, UINT_32 offset );

/***************************************************/
/* a safe point to do garbage collection
   returns a bound on how much allocation we should
   do before calling safePoint again to maintain
   a good distribution of work
*/

UINT_32 IRC_safePoint( IRC_Heap *heap );

/* do a full collection in the youngest numGens generations 
   (0 = none, 1 = youngest only, etc)
   the semantics here are that anything that's actually 
   unreachable and in one of the collected generations will
   be marked dead.  this means that any current collection
   is finished before starting another, and hence we may do
   up to just short of TWO FULL traversals!!
*/

void IRC_fullCollect( IRC_Heap *heap, unsigned numGens );

/*
   provide a mapper over the heap objects
*/

int IRC_forEachObject( IRC_Heap *heap, 
		       int (*fn)( void *info, void *heap_obj ), 
		       void *info );

/*
   provide macros for important functions
*/
#ifdef GC_MACROS
#include <rscheme/irctypes.h>
#include <rscheme/sizeclas.h>
#include <rscheme/alloc.h>

#define IRC_alloc(heap,size) \
    IRC_getBlock(heap,\
	        (heap)->sizeClassesIndex[LOGICAL_SIZE_CLASS_OF(size)], \
		size)

#define IRC_writeBarrier(heap,lvalue,offset,rvalue) \
		(IRC_writeBarrierCode(heap,lvalue,rvalue) \
		 ? IRC_tripWriteBarrier( heap, lvalue, offset, rvalue ) \
		 : 0)
#endif

#include <rscheme/gcglue.h>
#include <rscheme/gcadapt.h>

int is_object_dead( void *ptr );

#endif /* _H_GCSERVER */
