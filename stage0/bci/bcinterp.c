/*-----------------------------------------------------------------*-C-*---
 * File:    handc/bci/bcinterp.c
 *
 *          Copyright (C)1997 Donovan Kolbly <d.kolbly@rscheme.org>
 *          as part of the RScheme project, licensed for free use.
 *          See <http://www.rscheme.org/> for the latest information.
 *
 * File version:     1.37
 * File mod date:    2003-10-13 13:02:04
 * System build:     v0.7.3.4-b7u, 2007-05-30
 *
 * Purpose:          RScheme bytecode interpreter (8902)
 *------------------------------------------------------------------------*
 * Notes:
 *      This bytecode interpreter is hand-crafted.  In the future,
 *      BCIs can be constructed automatically using trace-driven
 *      instruction-sequence analysis for optimization.
 *      
 *      This BCI is identified as 8902 (it's part-tag) to distinguish
 *      it from any other possible BCIs
 *------------------------------------------------------------------------*/

#include <rscheme/scheme.h>
#include <rscheme/vinsns.h>
#include <rscheme/linktype.h>
#include <rscheme/regs.h>
#include <rscheme/bcextend.h>
#include <rscheme/hashfn.h>
#include <rscheme/hashmain.h>
#include <rscheme/osglue.h>
#include <rscheme/intrs.h>
#include <rscheme/allocns.h>
#include <string.h>
#include <math.h>

/* note:
	xlc (AIX) with -O2 finds this program to be too complicated
	to compile if DEBUG_BCI is turned on
*/

/* #define TYPECHECK_EVAL_STACK */

#if !defined(NDEBUG) || DEBUG_BCI
#define DEBUG_BYTECODES  (bci_trace_flag)
#define debug_bytecode_printf(args) if (bci_trace_flag) printf args;
#else
#define debug_bytecode_printf(args) /* do nothing */
#endif

/***************************************************************
 *
 *  The bytecode correlator is a profiling tool for measuring
 *  which bytecodes (and primops) come after which other ones.
 *
 *  It is only a 2-step correlator, for convenience and space.
 *
 *  Normal bytecode operators are assigned codes 0-255
 *  while primops are encoded as well, with codes 256-512 
 *  (ie, primop code + 256)
 *
 *  Since primops are encoded seperately, the primop dispatcher
 *  (255) never appears as a bytecode, so that is used as the
 *  "initial" or "start" state.
 *
 *  The primop `get-bytecode-correlation' is used to capture
 *  the data, as a vector of correlation entries, each of which
 *  is a 3-element vector consisting of:
 *   [0] - the number of occurrences
 *         (only non-zero entries are returned),
 *   [1] - the previous opcode (255=no previous)
 *   [2] - the opcode
 *  
 ****************************************************************/

#if ACCUM_BYTECODE_CORRELATION

/* turn off computed goto's */
#undef USE_COMPUTED_GOTO
#define USE_COMPUTED_GOTO (0)

static UINT_32 bci_corr[512][512];

static obj get_bytecode_correlation( void )
{
  obj q = make_dequeue();
  int prev, op;

  for (prev=0; prev<512; prev++)
    for (op=0; op<512; op++)
      {
	int n = bci_corr[prev][op];
	if (n != 0)
	  {
	    obj entry = make3( vector_class, 
			       int2fx(n), 
			       int2fx(prev), 
			       int2fx(op) );
	    dequeue_push_back( q, entry );
	  }
      }
  return dequeue_state(q);
}

/****************************************************************/
#else

static obj get_bytecode_correlation( void )
{
  return gvec_alloc( SLOT(0), vector_class );
}

#endif /* ACCUM_BYTECODE_CORRELATION */

#ifdef DEBUG_BYTECODES
#define INIT_VAL (0)
#else
#define INIT_VAL (-1)  /* signal that not available */
#endif

int bci_trace_flag = INIT_VAL;

#undef assert_type

#define BCI_PLACE make3( vector_class, \
			 literals_reg, int2fx(pc-base), FALSE_OBJ )
#define assert_type(expr) STMT( if (!(expr)) raise_exception(6,1,BCI_PLACE); )


extern struct part_descr bcinterp_part;
extern struct module_descr module_bci;

/*
 *  we redefine these functions here so that bytecoded programs are always
 *  ranged checked, even when the sytem is compiled w/-DNDEBUG (as is normal)
 */

static obj bc_bvec_hash( obj bvec, INT_32 offset, INT_32 len,
			 int fpc )
{
  if ((len < 0) || (offset < 0) || ((offset + len) > SIZEOF_PTR(bvec)))
    scheme_error( "bvec_hash: range error: (bvec-hash ~s ~d ~d)",
		  3, bvec, int2fx(offset), int2fx(len) );
  return bvec_hash( bvec, offset, len );
}

static obj bc_bvec_ci_hash( obj bvec, INT_32 offset, INT_32 len,
			     int fpc )
{
  if ((len < 0) || (offset < 0) || ((offset + len) > SIZEOF_PTR(bvec)))
    scheme_error( "bvec_ci_hash: range error: (bvec-ci-hash ~s ~d ~d)",
		  3, bvec, int2fx(offset), int2fx(len) );
  return bvec_ci_hash( bvec, offset, len );
}



static void bc_bvec_copy(obj dst, INT_32 dst_offset,
			 obj src, INT_32 src_offset, INT_32 len,
			 int fpc )
{
  char *dst_p;
  const char *src_p;

  if ((dst_offset < 0) 
      || ((dst_offset + len) > SIZEOF_PTR(dst))
      || (src_offset < 0)
      || ((src_offset + len) > SIZEOF_PTR(src))
      || (len < 0))
    scheme_error( "bvec_copy: range error: (bvec-copy ~s ~d ~s ~d ~d)",
		  5, dst, int2fx(dst_offset), src, int2fx(src_offset),
		  int2fx(len) );
  bvec_copy( dst, dst_offset, src, src_offset, len );
}

#define bvec_copy(d,do,s,so,l) bc_bvec_copy(d,do,s,so,l,FPLACE_CODE)
#define bvec_hash(s,so,l) bc_bvec_hash(s,so,l,FPLACE_CODE)
#define bvec_ci_hash(s,so,l) bc_bvec_ci_hash(s,so,l,FPLACE_CODE)

#undef rscheme_global_ref
#undef rscheme_global_set

obj rscheme_global_ref( UINT_32 offset )
{
  if (offset >= SLOT(NUM_RSCHEME_GLOBALS))
    scheme_error( "rscheme-global-ref: ~d out of range", 
		 1, RIBYTES_TO_FXWORDS(offset) );
  return *(obj *)(((char *)rscheme_global) + offset);
}

obj rscheme_global_set( UINT_32 offset, obj new_val )
{
  obj old_val;

  if (offset < SLOT(NUM_RSCHEME_GLOBALS))
    {
      old_val = *(obj *)(((char *)rscheme_global) + offset);
      *(obj *)(((char *)rscheme_global) + offset) = new_val;
    }
  else
   {
     scheme_error( "rscheme-global-ref: ~d out of range", 
		   1, RIBYTES_TO_FXWORDS(offset) );
     old_val = FALSE_OBJ; /* quiet compiler */
   }
  return old_val;
}


#define FUNCTION procedure_name(literals_reg)

BEGIN_FWD(bci)
   FWD_MONOTONE(bci_0)
   FWD_MONOTONE(bci_1)
   FWD_MONOTONE(bci_2)
END_FWD(bci)


static jump_addr bci_run( unsigned bci_pc );

MONOTONE(bci_0)
{
  /* this is the entry point for byte-coded procedures */
  return bci_run( 0 );
}

static obj bci_immob_table[6] = { UNDEFINED_OBJ,
				    NOVALUE_OBJ,
				    UNINITIALIZED_OBJ,
				    UNBOUND_OBJ,
				    MAKE_UNIQ_OBJ( KEY_TAG ),
				    MAKE_UNIQ_OBJ( REST_TAG ) };

#define NOFN (RS_bc_extension_fn *)0

static struct bcx_descr *(loaded_extensions[256]);
static RS_bc_extension_fn (*extension_fns[256]) = {
  NOFN, NOFN, NOFN, NOFN,   NOFN, NOFN, NOFN, NOFN, 
  NOFN, NOFN, NOFN, NOFN,   NOFN, NOFN, NOFN, NOFN, 
  NOFN, NOFN, NOFN, NOFN,   NOFN, NOFN, NOFN, NOFN, 
  NOFN, NOFN, NOFN, NOFN,   NOFN, NOFN, NOFN, NOFN, 

  NOFN, NOFN, NOFN, NOFN,   NOFN, NOFN, NOFN, NOFN, 
  NOFN, NOFN, NOFN, NOFN,   NOFN, NOFN, NOFN, NOFN, 
  NOFN, NOFN, NOFN, NOFN,   NOFN, NOFN, NOFN, NOFN, 
  NOFN, NOFN, NOFN, NOFN,   NOFN, NOFN, NOFN, NOFN };

void install_bc_extension( struct bcx_descr *extn )
{
  UINT_8 i = extn->extn_code;

  if (extension_fns[i] != NOFN)
    {
      scheme_error( "cannot load bytecode extension ~a.~a in ~d: "
		    "already loaded ~a.~a", 
		    5,
		    make_string( extn->owner->name ),
		    make_string( extn->name ),
		    int2fx(i),
		    make_string( loaded_extensions[i]->owner->name ),
		    make_string( loaded_extensions[i]->name ) );
    }
  loaded_extensions[i] = extn;
  extension_fns[i] = extn->handler;
}

/* define the kinds of things we may have on the eval stack */

#ifdef TYPECHECK_EVAL_STACK

enum eval_stack_type {
  est_none = 0,
  est_obj,
  est_raw_float,
  est_raw_str,
  est_raw_bool,
  est_raw_int,
  est_unknown    /* used for data generated by extensions,
		    since the RT type checking is internal only 
		 */
};

static int chkest( int is, RS_bc_datum *v, int need, UINT_8 *pc )
{
static char *(est_t[]) = { "<none>", "<obj>", "<raw-float>", "<raw-str>", "<raw-bool>", "<raw-int>", "-unknown-" };

  if (is != need && is != est_unknown)
    {
      UINT_8 *base = ((UINT_8 *)PTR_TO_DATAPTR(LITERAL(0)));

      fprintf( stderr, "bcieval stack error: PC=%d\n", pc - base );
      fprintf( stderr, "\ttype is %s (%x.%x), expected %s\n",
	      est_t[is], (v->raw_int_val) >> 2, v->raw_int_val & 3,
	      est_t[need] );
      scheme_error(
	    "bcieval stack error: type = ~d, expected ~d (~x ~x) pc=~d", 
		   5, int2fx(is), int2fx(need), 
		   (v->raw_int_val) & ~3,
		   int2fx((v->raw_int_val) & 3),
		   int2fx(pc - base) );
    }
}
#endif

static obj profile_key = FALSE_OBJ;

static void shift_for_reg_0( unsigned num_regs_before )
{
  unsigned i;

/*
  printf( "shifting %u...\n", num_regs_before );
  for (i=0; i<num_regs_before; i++)
    {
      printf( "  REG%u: ", i );
      fprinto( stdout, reg_ref(i) );
      printf( "\n" );
    }
*/       

  switch (num_regs_before)
    {
    default:
      for (i=num_regs_before; i>10; i--)
	REG(i) = REG(i-1);
    case 10:  REG(10) = REG9;
    case 9:  REG9 = REG8;
    case 8:  REG8 = REG7;
    case 7:  REG7 = REG6;
    case 6:  REG6 = REG5;
    case 5:  REG5 = REG4;
    case 4:  REG4 = REG3;
    case 3:  REG3 = REG2;
    case 2:  REG2 = REG1;
    case 1:  REG1 = REG0;
    case 0:  ;
    }
}

static unsigned unshift_for_reg_0( unsigned num_regs_after )
{
#define M(a,b) REG ## a = REG ## b
unsigned i, n;

  switch (num_regs_after)
    {
    case 1: /* do nothing.. REG0 was the only one */
      return 0;
    case 2: M(0,1); n = 1; break;
    case 3: M(0,1); M(1,2); n = 2; break;
    case 4: M(0,1); M(1,2); M(2,3); n = 3; break;
    case 5: M(0,1); M(1,2); M(2,3); M(3,4); n = 4; break;
    case 6: M(0,1); M(1,2); M(2,3); M(3,4); M(4,5); n = 5; break;
    case 7: M(0,1); M(1,2); M(2,3); M(3,4); M(4,5); M(5,6); n = 6; break;
    case 8: M(0,1); M(1,2); M(2,3); M(3,4); M(4,5); M(5,6); M(6,7); n = 7; 
      break;
    default:
      for (i=1; i<num_regs_after; i++)
	reg_set( i-1, reg_ref(i) );
      n = num_regs_after - 1;
      break;
    }
#undef M
/*
  for (i=0; i<n; i++)
    {
      printf( "  REG%u: ", i );
      fprinto( stdout, reg_ref(i) );
      printf( "\n" );
    }
*/
  return n;
}


#undef BJUMP
#define BJUMP(n,label) if (rssig_ready) \
                        { shift_for_reg_0(n); \
			  REG0 = int2fx(label); \
			  arg_count_reg = (n)+1; \
			  return (jump_addr)bci_2; \
			} else {  \
			  if (rsprof_active)  \
			    rsprof_mt_bjumps(); \
			  arg_count_reg = (n); \
                          gc_safe_point(SLOT(1000)); \
			  pc = base + label; \
			}

/* this is the main interpreter for byte-coded procedures */

jump_addr bci_run( unsigned bci_pc )
{
#if USE_COMPUTED_GOTO
/* opcode jump table */
static void *bco_jump_table [] =
{
#include "bcojump.ci"
};

/* primop jump table */
static void *bcp_jump_table [] =
{
#include "bcpjump.ci"
};
#endif

UINT_8 *base = ((UINT_8 *)PTR_TO_DATAPTR(LITERAL(0)));
UINT_8 *pc = ((UINT_8 *)PTR_TO_DATAPTR(LITERAL(0))) + bci_pc;
RS_bc_datum eval_stack[500], *sp;
int parallel_type_stack[500];
#if ACCUM_BYTECODE_CORRELATION
  UINT_16 this, prev = 255;
#endif /* ACCUM_BYTECODE_CORRELATION */

/*
 *  the FPLACE_CODE is used by certain error-signalling macros
 *  such as the CHECK_FOO family.  In the RScheme VM, fplace codes
 *  are used to identify distinct points in a function's execution.
 *  For bytecoded procedures done here, the fplace code is the offset
 *  in the bytecoded program of the NEXT bytecode instruction.  (For
 *  C-coded procedures, fplace codes are typically assigned sequentially
 *  as needed)
 */

#define FPLACE_CODE (pc - base)

#ifdef TYPECHECK_EVAL_STACK
#define TCHECK(t,cell) chkest(parallel_type_stack[(cell)-eval_stack], \
			    cell,est_ ## t,pc)
#define TDECL(t,cell)  parallel_type_stack[(cell)-eval_stack] = est_ ## t

#define PUSHT(t,x) (TDECL(t,sp),((sp++)->t ## _val = (x)))

#define POPT(t) (TCHECK(t,sp-1), ((--sp)->t ## _val))
#else
#define TCHECK(t,cell) 0
#define TDECL(t,cell) 0

#define PUSHT(t,x) (((sp++)->t ## _val = (x)))
#define POPT(t) (((--sp)->t ## _val))
#endif

#define PUSH(x) PUSHT(obj,x)
#define POP() POPT(obj)

#define NPOP(n) (sp -= (n))

/* the i'th top element (0 = top) */

#define TOPT(t,i) (TCHECK(t,sp-(i)-1),sp[-(i)-1].t ## _val)
#define TOP(i) TOPT(obj,i)

#define SETTOPT(t,i,rhs) (TCHECK(t,sp-(i)-1),(sp[-(i)-1].t ## _val = (rhs)))
#define SETTOP(i,rhs) SETTOPT(obj,i,rhs)

#ifdef DEBUG_BYTECODES
  if (DEBUG_BYTECODES)
    {
      printf( "---> continuing bytecoded template %#lx at +%d\n",
	      (unsigned long)VAL(literals_reg), pc - base );
      printf( "     [%s]\n", procedure_name( literals_reg ) );
    }
#endif

    sp = &eval_stack[0];

#if USE_COMPUTED_GOTO
#define BCI_SWITCH(x)        goto *bco_jump_table[x];
#define BCI_CASE(x)          bco__##x:
#define BCI_PRIMOP_SWITCH(x) goto *bcp_jump_table[x];
#define BCI_PRIMOP_CASE(x)   bcp__##x:

#ifdef DEBUG_BYTECODES
#  define BCI_BREAK            goto instruction_cycle;
#else
#  define BCI_BREAK            goto *bco_jump_table[*(pc++)];
#endif

#else
#define BCI_SWITCH(x)        switch(x)
#define BCI_CASE(x)          case x:
#define BCI_PRIMOP_SWITCH(x) switch(x)
#define BCI_PRIMOP_CASE(x)   case x:
#define BCI_BREAK            goto instruction_cycle;
#endif

 instruction_cycle:

#if ACCUM_BYTECODE_CORRELATION
  this = pc[0];
  if (this == 255)
    this = 256 + pc[1];
 
  bci_corr[prev][this]++;
  prev = this;
#endif /* ACCUM_BYTECODE_CORRELATION */

#ifdef DEBUG_BYTECODES
   if (DEBUG_BYTECODES)
     printf( "[%3d] byte code insn: %3d (stack: %d) ", 
	    pc-base, pc[0], sp - eval_stack ); 
#endif
   
   BCI_SWITCH(*(pc++))
     {
#      include "bcfrags.ci"
       BCI_BREAK;
       
#if USE_COMPUTED_GOTO
       bco__error:
       bcp__error:
#else
       default:
#endif       
       {
	 /* signal an ILLEGAL INSN error */
	 scheme_error( "illegal bytecode `~d' (PC ~d)",
		      2, int2fx( pc[-1] ), int2fx( pc - base ) );
	 BCI_BREAK;
       }

       /* EXTENSION PRIMOPS (dmk idea&impl 95.01.15)*/
       BCI_CASE(254)
       {
	 RS_bc_extension_fn *fn = extension_fns[*pc++];
#ifdef DEBUG_BYTECODES
	 UINT_8 *pc0;
#endif	      
	 if (fn != NOFN)
	   {
	     RS_bc_datum *p = sp;
	     /* avoid taking pointer to `sp' -- that may
		completely twang up the performance of
		the entire BCI because the optimizer will barf it
		*/
#ifdef DEBUG_BYTECODES
	     pc0 = pc;
	     if (DEBUG_BYTECODES)
	       printf( "extn[%d]:", pc[-1] );
#endif
	     pc = fn( pc, &p );
#ifdef DEBUG_BYTECODES
	     if (DEBUG_BYTECODES)
	       {
		 while (pc0 < pc)
		   printf( " %02x", *pc0++ );
		 printf( "\n" );
	       }
#endif
	     sp = p;
#ifdef TYPECHECK_EVAL_STACK
	     /* mark everything on the stack as of type UNKNOWN
		because multiple-value return of primops
		is not outside the general model of computation
		*/
	     while (p > eval_stack)
	       {
		 p--;
		 TDECL(unknown,p);
	       }
#endif
	   }
	 else
	   {
	     scheme_error( "extension primop `~d' not available (PC ~d)",
			  2,
			  int2fx( pc[-1] ),
			  int2fx( pc - base ) );
	   }
	 BCI_BREAK;
       }
     }

  goto instruction_cycle;  
#undef FPLACE_CODE
}

/* this is a jump point for byte-coded procedures,
   serves the role of a BJUMP target for interrupt
   purposes (the definition of BJUMP avaialble to bytecoded
   code returns bci_2 as the next monotone)
*/

MONOTONE(bci_2)
{
  unsigned pc = fx2int(REG0);

/*  printf( "unshifting %d, contn at pc %u...\n", arg_count_reg, pc );*/
  arg_count_reg = unshift_for_reg_0( arg_count_reg );
  return bci_run( pc );
}

/* this is the continuation point for byte-coded procedures */

MONOTONE(bci_1)
{
  unsigned bci_pc;

    bci_pc = fx2int(PARTCONT_REG(0));

#ifdef DEBUG_BYTECODES
    if (DEBUG_BYTECODES)
      {
	unsigned i;

	printf( "resuming BCI:  " );
	fprinto( stdout, gvec_read(literals_reg,SLOT(2)) );
	printf( " at [%3d]\n", bci_pc );

	for (i=0; i<arg_count_reg; i++)
	  {
	    printf( "      reg[%u] = ", i );
	    fprinto( stdout, reg_ref(i) );
	    printf( "\n" );
	  }
	fflush(stdout);
      }
#endif

  /* printf( "resuming BC program at: %d\n", bci_pc ); */
    return bci_run( bci_pc );
}

BEGIN_BACK(bci)
   BACK_MONOTONE(bci_0)
   BACK_MONOTONE(bci_1)
   BACK_MONOTONE(bci_2)
END_BACK(bci)

static struct function_descr bci_descr = {
	&bcinterp_part,
	JUMP_TABLE( bci ),
	"bci" };

static struct function_descr *(fn_tab[]) = {
    &bci_descr,
    NULL };

struct part_descr bcinterp_part = { 
    8902,
    &module_bci, 
    fn_tab,
    "bcinterp",
    0 };

static struct part_descr *(tab[]) = {
    &bcinterp_part, NULL };

struct module_descr module_bci = { 
               /* name */  "bci", 
              /* parts */  tab,
          /* num_roots */  0,
        /* root_vector */  NULL,
       /* roots (info) */  NULL,
      /* bc_extensions */  NULL,
  /* num_bc_extensions */  0 };
