/*-----------------------------------------------------------------*-C-*---
 * File:    handc/runtime/regs.h
 *
 *          Copyright (C)1997 Donovan Kolbly <d.kolbly@rscheme.org>
 *          as part of the RScheme project, licensed for free use.
 *          See <http://www.rscheme.org/> for the latest information.
 *
 * File version:     1.9
 * File mod date:    2000-11-21 23:25:08
 * System build:     v0.7.3.4-b7u, 2007-05-30
 *
 * Purpose:          virtual machine register declarations
 *------------------------------------------------------------------------*/

#ifndef _H_REGS
#define _H_REGS

#include <rscheme/obj.h>

void init_regs( void );

#define IMPL_ARG_LIMIT (1000)

#ifdef M68K_REGS
    register obj envt_reg asm( "a5" );		/* any_reg 0 */
    register obj literals_reg asm( "a3" );	/* any_reg 1 */
    register obj dynamic_state_reg asm( "d4" );	/* any_reg 2 */
    register obj continuation_reg asm( "a4" );	/* any_reg 3 */
    extern obj thread_state_reg;                /* any_reg 4 */
    register unsigned arg_count_reg asm( "d5" );

    register obj REG0 asm( "d7" );
    register obj REG1 asm( "d6" );
    extern obj REG2, REG3, REG4, REG5, REG6, REG7, REG8, REG9;
#else
#ifdef MIPS_REGS
    register obj envt_reg asm( "$16" );		/* any_reg 0 */
    register obj literals_reg asm( "$17" );	/* any_reg 1 */
    register obj dynamic_state_reg asm( "$18" );/* any_reg 2 */
    register obj continuation_reg asm( "$19" );	/* any_reg 3 */
    extern obj thread_state_reg;                /* any_reg 4 */
    register unsigned arg_count_reg asm( "$20" );

    register obj REG0 asm( "$21" );
    register obj REG1 asm( "$22" );
    extern obj REG2, REG3;
    extern obj REG4, REG5, REG6;
    extern obj REG7, REG8, REG9;
#else
#ifdef RS6000_REGS
    register obj envt_reg asm( "13" );          /* any_reg 0 */
    register obj literals_reg asm( "14" );      /* any_reg 1 */
    extern obj dynamic_state_reg;               /* any_reg 2 */
    register obj continuation_reg asm( "16" );  /* any_reg 3 */
    extern obj thread_state_reg;                /* any_reg 4 */
    register unsigned arg_count_reg asm( "17" );

    register obj REG0 asm( "18" );
    register obj REG1 asm( "19" );
    register obj REG2 asm( "20" );
    register obj REG3 asm( "21" );
    register obj REG4 asm( "22" );
    register obj REG5 asm( "23" );
    register obj REG6 asm( "24" );
    register obj REG7 asm( "25" );
    register obj REG8 asm( "26" );
    register obj REG9 asm( "15" );
#else
#ifdef SPARC_REGS
    extern obj envt_reg;		        /* any_reg 0 */
    extern obj literals_reg;	                /* any_reg 1 */
    extern obj dynamic_state_reg;	        /* any_reg 2 */
    register obj continuation_reg asm( "%g7" );	/* any_reg 3 */
    extern obj thread_state_reg;                /* any_reg 4 */
    register unsigned arg_count_reg asm( "%g5" );

    register obj REG0 asm( "%g6" );
    extern obj REG1, REG2, REG3;
    extern obj REG4, REG5, REG6;
    extern obj REG7, REG8, REG9;
#else
#ifdef RS_PROFILE
    extern obj envt_reg;		/* any_reg 0 */
    extern obj literals_reg;		/* any_reg 1 */
    extern obj dynamic_state_reg;	/* any_reg 2 */
    extern obj continuation_reg;	/* any_reg 3 */
    extern obj thread_state_reg;        /* any_reg 4 */
    extern unsigned arg_count_reg;
    
    extern obj _REG0;
    extern obj _REG1, _REG2, _REG3;
    extern obj _REG4, _REG5, _REG6;
    extern obj _REG7, _REG8, _REG9;

#define REG0 (*(rs_profile1("reg",0),&_REG0))
#define REG1 (*(rs_profile1("reg",1),&_REG1))
#define REG2 (*(rs_profile1("reg",2),&_REG2))
#define REG3 (*(rs_profile1("reg",3),&_REG3))
#define REG4 (*(rs_profile1("reg",4),&_REG4))
#define REG5 (*(rs_profile1("reg",5),&_REG5))
#define REG6 (*(rs_profile1("reg",6),&_REG6))
#define REG7 (*(rs_profile1("reg",7),&_REG7))
#define REG8 (*(rs_profile1("reg",8),&_REG8))
#define REG9 (*(rs_profile1("reg",9),&_REG9))
#else
    extern obj envt_reg;		/* any_reg 0 */
    extern obj literals_reg;		/* any_reg 1 */
    extern obj dynamic_state_reg;	/* any_reg 2 */
    extern obj continuation_reg;	/* any_reg 3 */
    extern obj thread_state_reg;        /* any_reg 4 */
    extern unsigned arg_count_reg;
    
    extern obj REG0;
    extern obj REG1, REG2, REG3;
    extern obj REG4, REG5, REG6;
    extern obj REG7, REG8, REG9;
#endif
#endif /* SPARC_REGS */
#endif /* RS6000_REGS */
#endif /* MIPS_REGS */
#endif /* M68K_REGS */

/* use these at the EXIT from Scheme into system code */
/* (currently, we only use nonvolatile regs, no nothing to be done) */

#define SAVE_HW_REGS		/* nothing to save */
#define RESTORE_HW_REGS		/* and nothing to restore */


/* use these at the ENTRY into Scheme */

#ifdef USE_HW_REGS
void switch_hw_regs_into_scheme( void );
void switch_hw_regs_back_to_os( void );
#else
#define switch_hw_regs_into_scheme()  do {} while (0)
#define switch_hw_regs_back_to_os()   do {} while (0)
#endif

/*
For example, if we were using 5 hardware registers...

    extern obj HW_REGS_save_area[5];
    
    Note that we only need to save registers in this way
    that the system's
    calling convention does not mark as "callee saves"
    
    #define SAVE_HW_REGS	save_area[0] = REG1;
    				save_area[1] = REG2;
				save_area[2] = envt_reg;
				save_area[3] = arg_count_reg;
				save_area[4] = continuation_reg;
*/

/* this macro is used to save/restore the registers
   on certain calls out of the system which may clobber
   said registers.  The GC is considered to be INSIDE
   the system in this sense, so it has to be compiled
   with these register decls in scope and making use of
   OUT_CALL when calling outside the system (ie, sbrk()
   or malloc())
*/


#define OUT_CALL(body)	do { SAVE_HW_REGS  \
			     body \
			     RESTORE_HW_REGS } while (0)

extern obj reg_array[IMPL_ARG_LIMIT];
extern obj temp_space[IMPL_ARG_LIMIT+10];	/* not part of root set,
						   so invalid across 
						   gc_safe_point's */

/*
 *  NOTE: the argument to REG() may be evaluated multiple times,
 *  so it should be a simple expression (constant or variable reference)
 */

#define REG(i)	(*(rs_profile1("reg",i),&reg_array[i]))
/* #define REG(i)	reg_array[i] */


/* These functions are pretty slow, so use only when it
   saves significantly on development time */

/* these access the arg-passing registers... */

obj reg_ref( unsigned arg_reg_num );
void reg_set( unsigned arg_reg_num, obj value );

/* these access any register (used for scanning the roots) */

#define NUM_FIXED_REGS (5)

/* the number of valid registers is, at any safe point:
		NUM_FIXED_REGS+arg_count_reg
   note that this may not return an actual value for
   the continuation reg -- returns ZERO if the current
   continuation is in the stack cache
*/

#define NUM_REGS (NUM_FIXED_REGS + arg_count_reg)

/*
 *  These functions assign register numbers 0-4 to core VM
 *  registers, i.e.,
 *
 *      0   = environment register
 *      1   = literals register
 *      2   = dynamic state register
 *      3   = continuation register (fixnum 0 if its in the stack cache)
 *      4   = thread state register
 *      >=5 = general-purpose value registers (REG0, ...)
 */

obj any_reg_ref( unsigned reg_num );
void any_reg_set( unsigned reg_num, obj val );

/*
 *   root variable processing
 */

typedef int process_root_fn( obj *root, void *info );

int process_register_roots( process_root_fn *fn, void *info );
int process_stack_roots( process_root_fn *fn, void *info );
int process_module_roots( process_root_fn *fn, void *info );
int process_all_roots( process_root_fn *fn, void *info );

#endif /* _H_REGS */
