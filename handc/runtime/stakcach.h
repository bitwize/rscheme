/*-----------------------------------------------------------------*-C-*---
 * File:    handc/runtime/stakcach.h
 *
 *          Copyright (C)1997 Donovan Kolbly <d.kolbly@rscheme.org>
 *          as part of the RScheme project, licensed for free use.
 *          See <http://www.rscheme.org/> for the latest information.
 *
 * File version:     1.7
 * File mod date:    2005-03-04 21:01:27
 * System build:     v0.7.3.4-b7u, 2007-05-30
 *
 * Purpose:          cached continuation stack implementation
 *------------------------------------------------------------------------*/

#ifndef _H_STAKCACH
#define _H_STAKCACH

#include <rscheme/runtime.h>
#include <rscheme/smemory.h>
#include <rscheme/gcserver.h>

#define CACHE_INTERIOR_SIZE (1000)
#define CACHE_SIZE (CACHE_INTERIOR_SIZE+IMPL_ARG_LIMIT)

extern obj the_stack_cache[CACHE_SIZE];

#define CACHE_POSN(i) (((UINT_32)(&the_stack_cache[i])) + POINTER_TAG)
#define MAX_FRAME_SIZE (IMPL_ARG_LIMIT+CONT_FIXED)

#define cache_upper_limit CACHE_POSN(CACHE_SIZE)
#define cache_lower_limit CACHE_POSN(0)
#define cache_lower_margin CACHE_POSN(MAX_FRAME_SIZE)

CIH_DECL UINT_32 alloc_cache_frame( unsigned num_slots );
CIH_DECL obj *push_cont( jump_addr label, unsigned reg_space );

CIH_DECL void cache_iterator_reset( void );
CIH_DECL void *cache_iterator_next( void );

CIH_DECL rs_bool in_stack_cache( obj value );
void flush_stack_cache( void );
void init_stack_cache( void );
unsigned restore_arb( void );		/* restore run-time dep't # regs */
unsigned get_partcont_size( obj cro );  /* figure out how big a frame is */

#ifdef INLINES
#include <rscheme/stakcach.ci>
#endif /* INLINES */

/* 
 *  Note that filling in the partial continuation,
 *  doesn't require a write barrier because we are only
 *  filling in the slots of a freshly created partial-continuation
 *  which is for-sure in the stack cache
 */

#define SET_PARTCONT_REG(reg,value) frame[reg] = value
#define PUSH_PARTCONT_ADDR(addr,space) obj *frame = push_cont(addr,space)
#define PARTCONT_REF(i) (((obj *)(VAL(continuation_reg)-POINTER_TAG))[i])

#endif /* _H_STAKCACH */
