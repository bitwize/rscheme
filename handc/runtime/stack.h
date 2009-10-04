/*-----------------------------------------------------------------*-C-*---
 * File:    handc/runtime/stack.h
 *
 *          Copyright (C)1997 Donovan Kolbly <d.kolbly@rscheme.org>
 *          as part of the RScheme project, licensed for free use.
 *          See <http://www.rscheme.org/> for the latest information.
 *
 * File version:     1.4
 * File mod date:    1997-11-29 23:10:49
 * System build:     v0.7.3.4-b7u, 2007-05-30
 *
 * Purpose:          continuation stack interface
 *------------------------------------------------------------------------*/

#ifndef _H_STACK
#define _H_STACK

/************************ NOT using a stack cache ************************/

CIH_DECL bool in_stack_cache( obj value );
CIH_DECL void flush_stack_cache( void );
CIH_DECL void init_stack_cache( void );

void push_cont( jump_addr label, unsigned reg_space );

#define SET_PARTCONT_REG(reg,value) gvec_write_fresh( \
			continuation_reg, SLOT((reg)+CONT_FIXED), value )
#define PUSH_PARTCONT_ADDR(addr,space) push_cont(addr,space)
#define PARTCONT_REF(i) gvec_read(continuation_reg,SLOT(i))

unsigned restore_arb( void );		/* restore run-time dep't # regs */

#ifdef INLINES
#include <rscheme/stack.ci>
#endif

#endif /* _H_STACK */
