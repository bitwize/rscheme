/*-----------------------------------------------------------------*-C-*---
 * File:    handc/runtime/regs.c
 *
 *          Copyright (C)1997 Donovan Kolbly <d.kolbly@rscheme.org>
 *          as part of the RScheme project, licensed for free use.
 *          See <http://www.rscheme.org/> for the latest information.
 *
 * File version:     1.10
 * File mod date:    2000-11-21 23:25:08
 * System build:     v0.7.3.4-b7u, 2007-05-30
 *
 * Purpose:          Virtual Machine registers & register accessors/setters
 *------------------------------------------------------------------------*/

#include <rscheme/vinsns.h>
#include <rscheme/runtime.h>

#ifdef M68K_REGS
     obj thread_state_reg;
     obj REG2, REG3;
     obj REG4, REG5, REG6;
     obj REG7, REG8, REG9;
#else
#ifdef MIPS_REGS
     obj thread_state_reg;
     obj REG2, REG3;
     obj REG4, REG5, REG6;
     obj REG7, REG8, REG9;
#else
#ifdef RS6000_REGS
  obj thread_state_reg;
  obj dynamic_state_reg;
#else
#ifdef SPARC_REGS
     obj envt_reg;		/* any_reg 0 */
     obj literals_reg;	/* any_reg 1 */
     obj dynamic_state_reg;	/* any_reg 2 */
     obj thread_state_reg;
     obj REG1, REG2, REG3;
     obj REG4, REG5, REG6;
     obj REG7, REG8, REG9;
#else
#ifdef RS_PROFILE
     obj envt_reg;		/* any_reg 0 */
     obj literals_reg;		/* any_reg 1 */
     obj dynamic_state_reg;	/* any_reg 2 */
     obj continuation_reg;	/* any_reg 3 */
     obj thread_state_reg;
     unsigned arg_count_reg;
    
     obj _REG0;
     obj _REG1, _REG2, _REG3;
     obj _REG4, _REG5, _REG6;
     obj _REG7, _REG8, _REG9;
#else
     obj envt_reg;		/* any_reg 0 */
     obj literals_reg;		/* any_reg 1 */
     obj dynamic_state_reg;	/* any_reg 2 */
     obj continuation_reg;	/* any_reg 3 */
     obj thread_state_reg;
     unsigned arg_count_reg;
    
     obj REG0;
     obj REG1, REG2, REG3;
     obj REG4, REG5, REG6;
     obj REG7, REG8, REG9;
#endif /* RS_PROFILE */
#endif /* SPARC_REGS */
#endif /* RS6000_REGS */
#endif /* MIPS_REGS */
#endif /* M68K_REGS */

obj reg_array[IMPL_ARG_LIMIT];
obj temp_space[IMPL_ARG_LIMIT+10];

obj reg_ref( unsigned num )
{
    assert( num < IMPL_ARG_LIMIT );
    switch (num)
    {
	case 0:		return REG0;
	case 1:		return REG1;
	case 2:		return REG2;
	case 3:		return REG3;
	case 4:		return REG4;
	case 5:		return REG5;
	case 6:		return REG6;
	case 7:		return REG7;
	case 8:		return REG8;
	case 9:		return REG9;
	default:	return REG(num);
    }
}

void reg_set( unsigned num, obj value )
{
    assert( num < IMPL_ARG_LIMIT );
    switch (num)
    {
	case 0:		REG0 = value; break;
	case 1:		REG1 = value; break;
	case 2:		REG2 = value; break;
	case 3:		REG3 = value; break;
	case 4:		REG4 = value; break;
	case 5:		REG5 = value; break;
	case 6:		REG6 = value; break;
	case 7:		REG7 = value; break;
	case 8:		REG8 = value; break;
	case 9:		REG9 = value; break;
	default:	REG(num) = value; break;
    }
}

obj any_reg_ref( unsigned reg_num )
{
    switch (reg_num)
    {
	case 0:		return envt_reg;
	case 1:		return literals_reg;
	case 2:		return dynamic_state_reg;
	case 3:		if (in_stack_cache(continuation_reg))
			    return ZERO;
			return continuation_reg;
        case 4:         return thread_state_reg;
	default:	
	  assert( (reg_num - NUM_FIXED_REGS) < arg_count_reg );
	  return reg_ref( reg_num - NUM_FIXED_REGS );
    }
}

void any_reg_set( unsigned reg_num, obj val )
{
  switch (reg_num)
    {
    case 0:     envt_reg = val; break;
    case 1:     literals_reg = val; break;
    case 2:     dynamic_state_reg = val; break;
    case 3:     continuation_reg = val; break;
    case 4:     thread_state_reg = val; break;
    default:
      assert( (reg_num - NUM_FIXED_REGS) < arg_count_reg );
      reg_set( reg_num - NUM_FIXED_REGS, val );
    }
}

#ifdef USE_HW_REGS
static obj sys_reg_save_area[16],      /* where to save the OS's regs */
           scheme_reg_save_area[16];	/* where to save our regs */

static void save_hw_regs( obj *save_buffer )
{
    save_buffer[0] = REG0;
    save_buffer[1] = REG1;
    save_buffer[2] = REG2;
    save_buffer[3] = REG3;
    save_buffer[4] = REG4;
    save_buffer[5] = REG5;
    save_buffer[6] = REG6;
    save_buffer[7] = REG7;
    save_buffer[8] = REG8;
    save_buffer[9] = REG9;
    save_buffer[10] = envt_reg;
    save_buffer[11] = literals_reg;
    save_buffer[12] = dynamic_state_reg;
    save_buffer[13] = continuation_reg;
    save_buffer[14] = OBJ(arg_count_reg);
    save_buffer[15] = thread_state_reg;
}

static void load_hw_regs( obj *save_buffer )
{
    REG0 = save_buffer[0];
    REG1 = save_buffer[1];
    REG2 = save_buffer[2];
    REG3 = save_buffer[3];
    REG4 = save_buffer[4];
    REG5 = save_buffer[5];
    REG6 = save_buffer[6];
    REG7 = save_buffer[7];
    REG8 = save_buffer[8];
    REG9 = save_buffer[9];
    envt_reg = save_buffer[10];
    literals_reg = save_buffer[11];
    dynamic_state_reg = save_buffer[12];
    continuation_reg = save_buffer[13];
    arg_count_reg = VAL(save_buffer[14]);
    thread_state_reg = save_buffer[15];
}

void switch_hw_regs_into_scheme( void )
{
    save_hw_regs( sys_reg_save_area );
    load_hw_regs( scheme_reg_save_area );
}

void switch_hw_regs_back_to_os( void )
{
    save_hw_regs( scheme_reg_save_area );
    load_hw_regs( sys_reg_save_area );
}

#endif /* USE_HW_REGS */

/* clear all registers */

void init_regs( void )
{
unsigned i;

    arg_count_reg = 0;
    for (i=0; i<IMPL_ARG_LIMIT; i++)
	reg_set( i, UNINITIALIZED_OBJ );
    envt_reg = NIL_OBJ;
    literals_reg = FALSE_OBJ;
    dynamic_state_reg = NIL_OBJ;
    continuation_reg = FALSE_OBJ;
    thread_state_reg = FALSE_OBJ;
}

#define PROCESS_REG(r) temp = r; rc = fn( &temp, info ); if (rc) return rc

int process_register_roots( process_root_fn *fn, void *info )
{
  int i, rc;
  obj temp;

  PROCESS_REG(REG0);
  PROCESS_REG(REG1);
  PROCESS_REG(REG2);
  PROCESS_REG(REG3);
  PROCESS_REG(REG4);
  PROCESS_REG(REG5);
  PROCESS_REG(REG6);
  PROCESS_REG(REG7);
  PROCESS_REG(REG8);
  PROCESS_REG(REG9);
  PROCESS_REG(envt_reg);
  PROCESS_REG(literals_reg);
  PROCESS_REG(continuation_reg);
  PROCESS_REG(thread_state_reg);
  PROCESS_REG(dynamic_state_reg);
  for (i=10; i<IMPL_ARG_LIMIT; i++)
    {
      rc = fn( &reg_array[i], info );
      if (rc)
	return rc;
    }
  return 0;
}

int process_all_roots( process_root_fn *fn, void *info )
{
  int rc;

  rc = process_module_roots( fn, info );
  if (rc)
    return rc;

  rc = process_stack_roots( fn, info );
  if (rc)
    return rc;

  return process_register_roots( fn, info );
}
