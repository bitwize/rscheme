/*-----------------------------------------------------------------*-C-*---
 * File:    handc/runtime/vinsns.c
 *
 *          Copyright (C)1997 Donovan Kolbly <d.kolbly@rscheme.org>
 *          as part of the RScheme project, licensed for free use.
 *          See <http://www.rscheme.org/> for the latest information.
 *
 * File version:     1.17
 * File mod date:    2007-02-01 13:34:50
 * System build:     v0.7.3.4-b7u, 2007-05-30
 *
 * Purpose:          Implementation of virtual instruction set
 *------------------------------------------------------------------------*
 * Notes:
 *      Much of the implementation of the vinsn set is to be found
 *      in vinsns.ci, from where it can be inlined
 *------------------------------------------------------------------------*/

#if HAVE_EXECINFO_H
#include <execinfo.h>
#endif

#include <stdlib.h>
#include <rscheme/vinsns.h>
#include <rscheme/runtime.h>
#include <rscheme/linktype.h>
#include <rscheme/scheme.h>
#include <rscheme/rsmodule.h>

#ifndef INLINES
#include "vinsns.ci"
#endif /* INLINES */

rs_bool do_record_call_chain = NO;
extern int bci_trace_flag;

PROLOGUE(full_call)

BEGIN_FWD(full_call)
    FWD_MONOTONE(full_call)
    FWD_MONOTONE(full_call_done)
END_FWD(full_call)

void register_apply( obj closure )
{
unsigned i;
obj call_ctx;

    PUSH_PARTCONT(full_call_done,1);
    SET_PARTCONT_REG(0,dynamic_state_reg);

    call_ctx = alloc( SLOT(arg_count_reg + 1), vector_class );
    gvec_write_init( call_ctx, SLOT(0), closure );
    for (i=0; i<arg_count_reg; i++)
    {
	gvec_write_init( call_ctx, SLOT(i+1), reg_ref(i) );
    }
    dynamic_state_reg = cons( call_ctx, dynamic_state_reg );

    if (bci_trace_flag > 0) 
      {
	fprintf( stdout, "calling: " );
	fprinto( stdout, gvec_read(literals_reg,SLOT(2)) );
	fprintf( stdout, "\n" );

	for (i=0; i<arg_count_reg; i++)
	  {
	    printf( "      reg[%u] = ", i );
	    fprinto( stdout, reg_ref(i) );
	    printf( "\n" );
	  }
	fflush(stdout);
      }
}

#define FPLACE_CODE (1000+0)
MONOTONE(full_call)
{
  register_apply( envt_reg );
  APPLY( arg_count_reg, envt_reg );
}
#undef FPLACE_CODE

/*  Note:  this function cannot assume that closure is really
    a closure, because it is invoked BEFORE that check is made --
    this is a FEATURE, used to track the arguments to a failed
    apply
*/

#define FPLACE_CODE (1000+1)
MONOTONE(full_call_done)
{
jump_addr f;

  dynamic_state_reg = PARTCONT_REG(0);
  RESTORE_CONT_REG();
  f = half_restore();

  if (bci_trace_flag > 0) 
    {
      fprintf( stdout, "returning to: " );
      fprinto( stdout, gvec_read(literals_reg,SLOT(2)) );
      fprintf( stdout, "\n" );
    }
  TAILCALL(f);
}
#undef FPLACE_CODE


_rs_volatile void apply_error( obj thing )
{
    scheme_error( "Apply to a non-closure: ~s with ~d args",
		    2, thing, int2fx(arg_count_reg) );
}

/*
 *  Dump out VM state (regs, etc.) in case of a fatal, horrific crash
 */

static void rs_vmd_prn( FILE *f, int pass, const char *key, obj val )
{
  fprintf( f, "  %20s: %08lx", key, VAL( val ) );
  if (pass) {
    fprintf( f, " : " );
    fflush( f );
    if (OBJ_ISA_PTR( val ) && in_stack_cache( val )) {
      fprintf( f, "<<in stack cache>>" );
    } else {
      fnprinto( f, val, 200 );
    }
  }
  fprintf( f, "\n" );
}


static void rs_vmd_envts( FILE *f, int pass, obj envt )
{
  unsigned i = 0;
  char tmp[20];

  fprintf( f, "\n" );

  while (OBJ_ISA_PTR_OF_CLASS( envt, binding_envt_class ) && (i < 20)) {
    unsigned k;

    fprintf( f, "  --: %08lx :-- (binding frame %u.)\n", VAL( envt ), i );

    sprintf( tmp, "%u.next", i );
    rs_vmd_prn( f, pass, tmp, gvec_ref( envt, SLOT(0) ) );
    
    for (k=SLOT(1); k<SIZEOF_PTR( envt ); k+=SLOT(1)) {
      sprintf( tmp, "%u.%u", i, (unsigned)(k / SLOT(1)) );
      rs_vmd_prn( f, pass, tmp, gvec_ref( envt, k ) );
    }
    envt = gvec_ref( envt, SLOT(0) );
    i++;
  }
}

static void rs_vmd_pass( FILE *f, int pass )
{
  obj saved_cr;
  unsigned i;
  obj envts[20];
  unsigned num_envts = 0;

  fprintf( f, "--------- VM State ----------\n" );
  fprintf( f, "Registers:\n" );
  rs_vmd_prn( f, pass, "envt_reg", envt_reg );
  if (OBJ_ISA_PTR_OF_CLASS( envt_reg, binding_envt_class )) {
    envts[num_envts++] = envt_reg;
  }

  rs_vmd_prn( f, pass, "literals_reg", literals_reg );
  rs_vmd_prn( f, pass, "dynamic_state_reg", dynamic_state_reg );
  rs_vmd_prn( f, pass, "thread_state_reg", thread_state_reg );
  fprintf( f, "         arg_count_reg: %8u\n", arg_count_reg );
  fprintf( f, "      continuation_reg: %08lx\n", VAL(continuation_reg) );

  for (i=0; i<arg_count_reg; i++) {
    char tmp[20];
    sprintf( tmp, "REG(%d)", i );
    
    rs_vmd_prn( f, pass, tmp, reg_ref(i) );
  }

  fflush( f );  /* get all of that out of the process boundary */

  /* now, try to dive into some more detail... */

  fprintf( f, "Continuation Chain:\n" );

  saved_cr = continuation_reg;

  for (i=0; i<10; i++) {
    unsigned n, j;
    obj x;

    fprintf( f, "   (%u) will continue in #[FRAME %08lx]", i, VAL(continuation_reg) );
    fflush( f );
    
    /* 'n' is the number of slots above and beyond CONT_FIXED */

    n = get_partcont_size( continuation_reg );
    fprintf( f, " (%u slots)\n", n );

    x = gvec_ref( continuation_reg, SLOT(0) );
    rs_vmd_prn( f, pass, ".envt", x );
    if (OBJ_ISA_PTR_OF_CLASS( x, binding_envt_class ) && (num_envts < 20)) {
      envts[num_envts++] = x;
    }
    rs_vmd_prn( f, pass, ".literals", gvec_ref( continuation_reg, SLOT(1) ) );
    rs_vmd_prn( f, pass, ".label", gvec_ref( continuation_reg, SLOT(2) ) );
    rs_vmd_prn( f, pass, ".contn", gvec_ref( continuation_reg, SLOT(3) ) );

    for (j=0; j<n; j++) {
      char tmp[20];
      sprintf( tmp, "slot[%u]", j );
      rs_vmd_prn( f, pass, tmp, gvec_ref( continuation_reg, SLOT(j + CONT_FIXED) ) );
    }
    RESTORE_CONT_REG();
    if (!OBJ_ISA_PTR( continuation_reg )) {
      break;
    }
  }

  fprintf( f, "Binding Environments:\n" );
  for (i=0; i<num_envts; i++) {
    rs_vmd_envts( f, pass, envts[i] );
  }

  continuation_reg = saved_cr;
}

#if HAVE_BACKTRACE

extern int backtrace( void **buffer, int size );
extern char **backtrace_symbols( void *const *buffer, int size );

void c_stacktrace( FILE *f )
{
  void *framelist[100];
  size_t nframes;
  char **symlist;
  unsigned i;

  nframes = backtrace( framelist, 100 );
  symlist = backtrace_symbols( framelist, nframes );

  fprintf( f, "C Stack Trace:\n" );
  for (i=0; i<nframes; i++) {
    fprintf( f, "    %s\n", symlist[i] );
  }
  free( symlist );
}

#endif

void rs_crash_dump_vmstate( FILE *f )
{
  rs_vmd_pass( f, 0 );
  fflush( f );

  /* now print the C stack trace */
#if HAVE_BACKTRACE
  c_stacktrace( f );
#endif

  /* go through again, this time trying to actually print out the objects */
  rs_vmd_pass( f, 1 );
}

void _rscrashout( void )
{
  rs_crash_dump_vmstate( stdout );
}

/************************ Binding Environments ************************/

obj nth_enclosing_envt( unsigned n )
{
obj x = envt_reg;

    while (n > 0)
    {
	x = enclosing_envt( x );
	n--;
    }
    return x;
}

/************************ Continuations ************************/

void save_cont( unsigned num, jump_addr addr )
{
    PUSH_PARTCONT_ADDR( addr, num );

    while (num > 10)
    {
	num--;
	SET_PARTCONT_REG(num,REG(num));
    }

    switch (num)
    {
	case 10:	SET_PARTCONT_REG(9,REG9);
	case 9:		SET_PARTCONT_REG(8,REG8);
	case 8:		SET_PARTCONT_REG(7,REG7);
	case 7:		SET_PARTCONT_REG(6,REG6);
	case 6:		SET_PARTCONT_REG(5,REG5);
	case 5:		SET_PARTCONT_REG(4,REG4);
	case 4:		SET_PARTCONT_REG(3,REG3);
	case 3:		SET_PARTCONT_REG(2,REG2);
	case 2:		SET_PARTCONT_REG(1,REG1);
	case 1:		SET_PARTCONT_REG(0,REG0);
	case 0:         /* nothing */;
    }
}

void restore_cont( unsigned num )
{
    while (num > 10)
    {
	num--;
	REG(num) = PARTCONT_REG(num);
    }

    switch (num)
    {
	case 10:	REG9 = PARTCONT_REG(9);
	case 9:		REG8 = PARTCONT_REG(8);
	case 8:		REG7 = PARTCONT_REG(7);
	case 7:		REG6 = PARTCONT_REG(6);
	case 6:		REG5 = PARTCONT_REG(5);
	case 5:		REG4 = PARTCONT_REG(4);
	case 4:		REG3 = PARTCONT_REG(3);
	case 3:		REG2 = PARTCONT_REG(2);
	case 2:		REG1 = PARTCONT_REG(1);
	case 1:		REG0 = PARTCONT_REG(0);
	case 0:         /* nothing */;
    }
    RESTORE_CONT_REG();
}

/************************ Argument Checking ************************/

_rs_volatile void wrong_num_args( const char *fn, unsigned num_required )
{
    scheme_error( "Function ~a called with ~d args, required exactly ~d",
    		  3,
		  make_string( fn ),
		  int2fx(arg_count_reg),
		  int2fx(num_required) );
}

_rs_volatile void wrong_num_args_range( const char *fn, 
				        unsigned mn, unsigned mx )
{
    scheme_error( "Function ~a called with ~d args, expected ~d to ~d",
    		  4,
		  make_string( fn ),
		  int2fx(arg_count_reg),
		  int2fx(mn),
		  int2fx(mx) );
}


_rs_volatile void too_few_args( const char *fn, unsigned min_required )
{
    scheme_error( "Function ~a called with ~d args, required at least ~d",
    		  3,
		  make_string( fn ),
		  int2fx(arg_count_reg),
		  int2fx(min_required) );
}

void collectn( unsigned first_reg )
{
  reg_set( first_reg, collect_top(first_reg) );
}

obj collect_top( unsigned first_reg )
{
obj list = NIL_OBJ;
unsigned i;

    if (first_reg < 10)
      {
	if (arg_count_reg > 10)
	  {
	    list = collect_top(10);
	    i = 10;
	  }
	else
	  i = arg_count_reg;

	while (i > first_reg)
	    list = cons( reg_ref(--i), list );
	return list;
      }
    
    i = arg_count_reg;
    while (i > first_reg)
      {
	unsigned r = --i;
	list = cons( REG(r), list );
      }
    return list;
}

void pad_with_false( unsigned limit_reg )
{
unsigned i = arg_count_reg;

    while (i < limit_reg)
	reg_set( i++, FALSE_OBJ );
}

/*  takes the last argument to be a proper list,
    and loads the items from the list in the registers,
    starting with the register that held the list.
    Returns the total number of resulting args.
*/


#define STAGE(n,m) case m: list = REG ## n; N = n; filled_ ## n : \
                   if (PAIR_P(list)) { REG ## n = pair_car(list); \
					 list = pair_cdr(list); \
					 N++; goto filled_ ## m; } break
					 

unsigned expand_last( void )
{
obj list = ZERO;
unsigned N = 0;

  switch (arg_count_reg)
    {
    case 0:
      scheme_error( "expand_list: no arguments", 0 );
      break;

      STAGE(0,1);
      STAGE(1,2);
      STAGE(2,3);
      STAGE(3,4);
      STAGE(4,5);
      STAGE(5,6);
      STAGE(6,7);
      STAGE(7,8);
      STAGE(8,9);
      STAGE(9,10);
    default:
      /* this is for cases 11, 12, ..., since STAGE(9,10) is case 10
       * hence, N = (arg_count_reg - 1) is at least 10
       */
      N = arg_count_reg - 1;
      list = REG(N);
    filled_10:
      while (PAIR_P(list))
	{
	  REG(N) = pair_car( list );
	  list = pair_cdr( list );
	  N++;
	  if (N >= IMPL_ARG_LIMIT)
	    scheme_error( "expand_last: list of args too long at: ~#*@40s",
			  1, list );
	}
      break;
    }
    if (!NULL_P(list))
    {
	scheme_error( "expand_last: last arg not a proper list at ~a",
		      1,
		      list );
    }
    return N;
}

_rs_volatile void failed_type_check( obj place, obj var, obj val, obj expect )
{
    if (!PAIR_P(expect))
	expect = cons( expect, NIL_OBJ );
    scheme_error( "failed type check: in ~a\n~a = ~s is not one of: ~a",
    	          4,
		  place,
		  var,
		  val,
		  expect );
}

_rs_volatile void type_check_failed( const char *fn )
{
  scheme_error( "type check failed in ~a", 1, make_string(fn) );
}

BEGIN_BACK(full_call)
  BACK_MONOTONE(full_call)
  BACK_MONOTONE(full_call_done)
END_BACK(full_call)

static struct function_descr full_call_descr = {
  &rscheme_runtime_part,
  JUMP_TABLE(full_call),
  "full-call"  };

static struct function_descr *(fn_tab[]) = {
    &full_call_descr,
    NULL
};

struct part_descr rscheme_runtime_part = { 
    9501, 
    &module_rscheme,
    fn_tab,
    "rscheme-hasht" };
