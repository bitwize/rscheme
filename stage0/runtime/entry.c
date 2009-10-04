/*-----------------------------------------------------------------*-C-*---
 * File:    handc/runtime/entry.c
 *
 *          Copyright (C)1997 Donovan Kolbly <d.kolbly@rscheme.org>
 *          as part of the RScheme project, licensed for free use.
 *          See <http://www.rscheme.org/> for the latest information.
 *
 * File version:     1.27
 * File mod date:    1999-01-23 15:30:51
 * System build:     v0.7.3.4-b7u, 2007-05-30
 *
 * Purpose:          Provide main entry points into RScheme runtime system
 *------------------------------------------------------------------------*/

#include <stdarg.h>
#include <stdlib.h>
#include <setjmp.h>
#include <rscheme/runtime.h>
#include <rscheme/vinsns.h>
#include <rscheme/intrs.h>
#include <rscheme/osglue.h>
#include <rscheme/rsmodule.h>
#include <rscheme/scheme.h>
#include <rscheme/allocns.h>

static jmp_buf *restart_run = NULL;
static unsigned run(obj cont);

#ifdef GNU_VINSNS
static void *restart_run_addr;
static int restart_run_value;
#endif

/* Manufacture a continuation which, when resumed, has the effect of
   calling the given closure with the given arguments.  We do this to
   start the interpretive loop of the system.  */
static void 
vpush_call_continuation(obj closure, unsigned num_args, va_list the_args)
{
  unsigned i;
  jump_addr addr;

  arg_count_reg = num_args;
  /* can't really signal a Scheme error here, because we might not
     have started the system yet. */
  assert(FUNCTION_P(closure));
  addr = apply(closure);
  {
    /* must have a block here since macro might declare a variable */
    PUSH_PARTCONT_ADDR(addr, arg_count_reg);
    for (i = 0; i < arg_count_reg; i++)
      SET_PARTCONT_REG(i, va_arg(the_args, obj));
  }
}

/* The only way to get out of the interpretive loop. */
_rs_volatile void
done_w_call_scheme(void)
{
  if (arg_count_reg == 0)
    temp_space[0] = NOVALUE_OBJ;
  else
    {
      unsigned i;
      for (i = 0; i < arg_count_reg; i++)
	temp_space[i] = reg_ref(i);
    }
  /* offset second arg so that values 1 and 2 convey other information */
  longjmp(*restart_run, arg_count_reg + 2);
}


/* The main entry point */
obj
call_scheme(obj closure, unsigned num_args, ...)
{
  va_list ap;

  assert(FUNCTION_P(closure));
  /* Caller might have clobbered our hardware regs.
     Restore from safe place where previously saved. */
  switch_hw_regs_into_scheme();
  /* manufacture a continuation which, when resumed, terminates the
     current run.  */
  literals_reg = return_from_call_scheme_template;
  /* When fasl images start up, entry points are faked by a stup which,
     when invoked patches up itself to point to the real C code.
     To avoid pointing to the stub, patch it manually. */
  template_unstub(literals_reg);
  envt_reg = NIL_OBJ;
  {
    struct function_descr *finish_run = (struct function_descr *)
      OBJ_TO_RAW_PTR(gvec_read(literals_reg, SLOT(1)));
    push_cont(finish_run -> monotones[1], 0);
  }
  va_start(ap, num_args);
  vpush_call_continuation(closure, num_args, ap);
  va_end(ap);
  run(continuation_reg);
  /* Restore hardware registers to keep caller happy */
  switch_hw_regs_back_to_os();
  return temp_space[0];
}

/* Return value is the number of valid values from entry procedure */
static unsigned
run(obj cont)
{
  int i;
  jump_addr f;
  jmp_buf *prev_jmp_buf, current_jmp_buf;
#ifdef GNU_VINSNS
  obj temp[1000];

  temp[0] = cont;
#endif
  temp_space[0] = cont;
  prev_jmp_buf = restart_run;
  restart_run = &current_jmp_buf;
  /* set up exception handler */
  while ((i = setjmp(current_jmp_buf)) == 1)
    { 
      /* nothing */
    }
  /* This is where we start. On an exception, we come back
     to here and reload the jump buffer. */
#ifdef GNU_VINSNS
  restart_run_value = 0;
restart:
  i = restart_run_value;
#endif
  if (i >= 2) 
    {
      /* finish_run was invoked.  Number of values is offset by 2 */
      restart_run = prev_jmp_buf;
      return i - 2;
    }
  /* resume the continuation indicated by cross-over buffer */
  continuation_reg = temp_space[0];
  f = half_restore();
  arg_count_reg = restore_arb();
  quasi_interp_spin(f);
  return 0; /* never reached */
}

_rs_volatile void
scheme_error(const char *msg, unsigned num_args, ...)
{
  va_list x;
  unsigned i;
  obj the_args;
  extern int bci_trace_flag;
  
  if (bci_trace_flag > 0)
    {
      printf("\n***\nscheme_error(\"%s\", %d, ...)\n", msg, num_args);
      printf("in: [%#lx] ", VAL(literals_reg));
      fprinto(stdout, gvec_read(literals_reg,SLOT(2)));
      printf("\n");
      va_start(x, num_args);
      for (i = 0; i < num_args; i++)
	{
	  printf("   arg[%u] ::= ", i);
	  fprinto(stdout, va_arg(x, obj));
	  printf("\n");
	}
      fflush(stdout);
      va_end(x);
    }
  
  /* collect the args into a list */
  va_start(x, num_args);
  for (i = 0; i < num_args; i++)
    temp_space[i] = va_arg(x, obj);
  va_end(x);
  
  the_args = NIL_OBJ;
  while (i > 0)
    the_args = cons(temp_space[--i], the_args);

  /* invoke the exception handler */
  raise_exception(0, 4, literals_reg, envt_reg, make_string(msg), the_args);
}

obj make_exception_stack( void )
{
  flush_stack_cache(); /* force continuation into heap */
  /*  we should generalize this to a <vm-state>, but that would
   *  involve more coordination with our uses.  In particular,
   *  we don't know that REG(0)...REG(n-1) have valid values...
   */
  return make5( condition_stack_class,
		literals_reg,
		continuation_reg,
		dynamic_state_reg,
		thread_state_reg,
		envt_reg );
}

_rs_volatile void
raise_error( obj err_object )
{
  if (instance_p( err_object, condition_class ))
    {
      if (truish( tlv_value( capture_stack_var ) ) )
	{
	  /* push a `stack' property onto the list */
	  gvec_set( err_object, 
		    SLOT(0), /* properties */
		    cons( cons( lookup_symbol( "stack" ), 
				make_exception_stack() ),
			  gvec_ref( err_object, SLOT(0) ) ) );
	}
      raise_exception( 7, 1, err_object );
    }
  else
    scheme_error( "raise_error(): ~s not a condition object",
		  1, err_object );
}

static obj
make_function_place(int fplace_code)
{
  return make2(function_place_class, 
	       literals_reg,
	       int2fx(fplace_code));
}

_rs_volatile void
type_check_error(obj thing, const char *class_name, int fplace_code)
{
  raise_error( make4( type_check_failed_class, 
		      NIL_OBJ,
		      intern( make_string( class_name ) ),
		      thing,
		      make_function_place( fplace_code ) ) );
}

_rs_volatile void
raise_exception(int type, int argc, ...)
{
  va_list args;
  unsigned i;
  jump_addr addr;
  
  if (rsprof_active)
    {
      /* this fragment is terminating abnormally... */
      rsprof_mt_fails();
    }
  if (EQ(exception_handler_proc,FALSE_OBJ))
    {
      fprintf(stderr, "** No exception handler in place **\n");
      abort();
    }
  arg_count_reg = argc + 1;
  REG0 = int2fx(type);

  va_start(args, argc);
  for (i = 0; i < (unsigned)argc; i++)
    reg_set(i + 1, va_arg(args, obj));
  va_end(args);

  /* note that signalling an error is pointless, because THIS
     is the error handling code */
  assert(FUNCTION_P(exception_handler_proc));
  addr = apply(exception_handler_proc);
  {
    PUSH_PARTCONT_ADDR(addr, arg_count_reg);
    for (i = 0; i < arg_count_reg; i++)
      SET_PARTCONT_REG(i, reg_ref(i));
  }
  temp_space[0] = continuation_reg;
  longjmp(*restart_run, 1);
}
