/*-----------------------------------------------------------------*-C-*---
 * File:    handc/runtime/stack.c
 *
 *          Copyright (C)1997 Donovan Kolbly <d.kolbly@rscheme.org>
 *          as part of the RScheme project, licensed for free use.
 *          See <http://www.rscheme.org/> for the latest information.
 *
 * File version:     1.5
 * File mod date:    1997-11-29 23:10:49
 * System build:     v0.7.3.4-b7u, 2007-05-30
 *
 * Purpose:          VM stack interface
 *------------------------------------------------------------------------*/

#include <rscheme/vinsns.h>

#ifndef STACK_CACHE

#ifndef INLINES
#include <rscheme/stack.ci>
#endif


/************************ Continuations ************************/

void push_cont( jump_addr label, unsigned reg_space )
{
obj pc = alloc( SLOT(CONT_FIXED+reg_space), partcont_class );
    
    gvec_write_init( pc, SLOT(0), envt_reg );
    gvec_write_init( pc, SLOT(1), literals_reg );
    gvec_write_init( pc, SLOT(2), dynamic_state_reg );
    gvec_write_init( pc, SLOT(3), continuation_reg );
    gvec_write_init_non_ptr( pc, SLOT(4), JUMP_ADDR_TO_OBJ(label) );
    continuation_reg = pc;
}

/*
    Restores a dynamically-determined number of
    argument-passing registers (ie, for use in the
    extremely rare cases when it cannot be determined
    statically -- nb, this only happens when compiling
    to closure-threaded code and on the occasion of
    interrupts) and the continuation register itself
*/

unsigned restore_arb( void )
{
unsigned n = (SIZEOF_PTR(continuation_reg) / sizeof(obj)) - CONT_FIXED;

    restore_cont( n );
    return n;
}

#endif /* STACK_CACHE */
