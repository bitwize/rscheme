/*-----------------------------------------------------------------*-C-*---
 * File:    handc/cfg/gcadapt/irc/gcadapt.h
 *
 *          Copyright (C)1997 Donovan Kolbly <d.kolbly@rscheme.org>
 *          as part of the RScheme project, licensed for free use.
 *          See <http://www.rscheme.org/> for the latest information.
 *
 * File version:     1.7
 * File mod date:    1997-11-29 23:10:45
 * System build:     v0.7.3.4-b7u, 2007-05-30
 *
 * Purpose:          RScheme/GC Adapter interface declaration
 *------------------------------------------------------------------------*/

/* interface to the garbage collector */

#include <rscheme/gcserver.h>

extern INT_32 gc_alloc_time;

CIH_DECL void gc_safe_point( UINT_32 size );

#define gc_alloc(n) (gc_alloc_time -= (n), IRC_alloc( gc_arena, (n) ))

#define write_barrier(lobj,ptr,robj) IRC_writeBarrier(gc_arena, \
						       lobj,((char *)(ptr)\
							     -(char*)(lobj)),\
						      robj)\

#define write_barrier_lval_clobber(x,y) do { } while (0)
#define write_barrier_lval_fresh(x,y,z) do { } while (0)
#define write_barrier_lval_init(x,y,z) do { } while (0)

void init_gc( int argc, const char **argv );

int gc_for_each( int (*fn)( void *info, void *heap_obj ), void *info );

CIH_DECL gc_obj_addr cast_and_deref_ptr( pos_ptr_addr slot );
