/**********************************************
THIS FILE WAS AUTOMATICALLY GENERATED, AND MAY
BE AUTOMATICALLY RE-GENERATED WHEN THE COMPILER
OR SOURCE CHANGES.  DO NOT MODIFY THIS FILE BY HAND!
RScheme Build (v0.7.3.4-b7u, 2007-05-30)
**********************************************/

/******************************** Preamble ********************************/

#define _MODULE_MATHLIB
#define _SCM_RANDOM
#define _C_RANDOM
#include "mathlib_p.h"
#include <rscheme/vinsns.h>
extern struct module_descr module_mathlib;
extern struct part_descr mathlib_part_random;
static char sccsid[] = "@(#)mathlib modules/mathlib/random.scm [246604801] (RS v0.7.3.4-b7u, 2007-05-30)";

/************************** Function Definitions **************************/


/********************** Raw glue `make-random-state' **********************/
#define seed REG0

static char rsfn_make_random_state_name[] = "make-random-state";
#define FUNCTION rsfn_make_random_state_name

PROLOGUE(make_random_state)

BEGIN_FWD(make_random_state)
  FWD_MONOTONE(make_random_state_0)
END_FWD(make_random_state)

#define FPLACE_CODE (1000+0)
MONOTONE(make_random_state_0)
{
{
  UINT_32 s;
  unsigned i;
  obj result;

  COUNT_ARGS(1);
  if (!OBJ_ISA_FIXNUM(seed))
    {
      scheme_error( "(make-random-state ~s): bad arg", 1, seed );
    }
  s = VAL(rehash_fixnum(seed));

  REG0 = result = alloc( SLOT(12), TLREF(0) );
  gvec_write_init_non_ptr( result, SLOT(0), int2fx(1) );
  for (i=0; i<11; i++)
    {
      gvec_write_init_non_ptr( result, 
			        SLOT(i+1), 
			        OBJ((s & ~PRIMARY_TAG_MASK) + FIXNUM_TAG) );
      s = ((s >> 1) & ~3) + (((s+4) & 8) ? 0x80000000 : 0);
      s = ((s >> 1) & ~3) + (((s+4) & 8) ? 0x80000000 : 0);
      s = ((s >> 1) & ~3) + (((s+4) & 8) ? 0x80000000 : 0);
    }
  RETURN1();
}}
#undef FPLACE_CODE

EPILOGUE(make_random_state)

BEGIN_BACK(make_random_state)
  BACK_MONOTONE(make_random_state_0)
END_BACK(make_random_state)

static struct function_descr make_random_state_descr = {
	&mathlib_part_random,
	JUMP_TABLE( make_random_state ),
	rsfn_make_random_state_name };
#undef FUNCTION

#undef seed

/************************* Raw glue `next-random' *************************/
#define state REG0
#define range REG1

static char rsfn_next_random_name[] = "next-random";
#define FUNCTION rsfn_next_random_name

PROLOGUE(next_random)

BEGIN_FWD(next_random)
  FWD_MONOTONE(next_random_0)
END_FWD(next_random)

#define FPLACE_CODE (1000+0)
MONOTONE(next_random_0)
{
{
  obj index, next_index, z;

  if (arg_count_reg == 2)
    {
      if (!OBJ_ISA_FIXNUM(range))
	scheme_error( "next-random: range arg ~s invalid", 1, range );
      else if (fx2int(range) < 1)
	scheme_error( "next-random: range arg ~d out of range", 1, range );
    }
  else if (arg_count_reg != 1)
    scheme_error( "next-random: ~d args, expected 1 or 2", 
		  1, int2fx(arg_count_reg) );

  if (!OBJ_ISA_PTR_OF_CLASS(state,TLREF(0)))
    {
      scheme_error( "next-random: state arg ~s invalid", 1, state );
    }

  index = gvec_read( state, SLOT(0) );
  assert( OBJ_ISA_FIXNUM(index) );

  next_index = ADD1(index);
  if (FXWORDS_TO_RIBYTES(next_index) >= SIZEOF_PTR(state))
    next_index = int2fx(1);
  z = FX_ADD( gvec_read( state, FXWORDS_TO_RIBYTES(index) ),
	      gvec_read( state, FXWORDS_TO_RIBYTES(next_index) ) );
  z = OBJ(VAL(z) & 0x7FFFFFFF);
  assert( OBJ_ISA_FIXNUM(z) );

  assert( OBJ_ISA_FIXNUM(next_index) );
  gvec_write_non_ptr( state, FXWORDS_TO_RIBYTES(index), z );
  gvec_write_non_ptr( state, SLOT(0), next_index );

  if (arg_count_reg == 2)
    z = OBJ(VAL(z) % VAL(range));
  REG0 = z;
  RETURN1();
}}
#undef FPLACE_CODE

EPILOGUE(next_random)

BEGIN_BACK(next_random)
  BACK_MONOTONE(next_random_0)
END_BACK(next_random)

static struct function_descr next_random_descr = {
	&mathlib_part_random,
	JUMP_TABLE( next_random ),
	rsfn_next_random_name };
#undef FUNCTION

#undef state
#undef range

/******************* Raw glue `make-random-float-state' *******************/
#define seed REG0

static char rsfn_make_random_float_state_name[] = "make-random-float-state";
#define FUNCTION rsfn_make_random_float_state_name

PROLOGUE(make_random_float_state)

BEGIN_FWD(make_random_float_state)
  FWD_MONOTONE(make_random_float_state_0)
END_FWD(make_random_float_state)

#define FPLACE_CODE (1000+0)
MONOTONE(make_random_float_state_0)
{  COUNT_ARGS(1);

{
  obj s = alloc( sizeof(unsigned short) * 3, TLREF(0) );
  unsigned short *uss = PTR_TO_DATAPTR(s);

  uss[0] = crc_hash_int2( 0x12348765, VAL(seed) );
  uss[1] = crc_hash_int2( VAL(seed), 0xFACEF00D );
  uss[2] = crc_hash_int2( 0xb5c0fbcf, (uss[0] << 16) + uss[1] );
  REG0 = s;
  RETURN1();
}}
#undef FPLACE_CODE

EPILOGUE(make_random_float_state)

BEGIN_BACK(make_random_float_state)
  BACK_MONOTONE(make_random_float_state_0)
END_BACK(make_random_float_state)

static struct function_descr make_random_float_state_descr = {
	&mathlib_part_random,
	JUMP_TABLE( make_random_float_state ),
	rsfn_make_random_float_state_name };
#undef FUNCTION

#undef seed

/********************** Raw glue `next-random-float' **********************/
#define raw_state REG0

static char rsfn_next_random_float_name[] = "next-random-float";
#define FUNCTION rsfn_next_random_float_name

PROLOGUE(next_random_float)

BEGIN_FWD(next_random_float)
  FWD_MONOTONE(next_random_float_0)
END_FWD(next_random_float)

#define FPLACE_CODE (1000+0)
MONOTONE(next_random_float_0)
{  obj state;
  COUNT_ARGS(1);
  if (!instance_p(raw_state,TLREFB(0)))
    {
      obj c;
      c = make5( TLREFB(1),
                 NIL_OBJ,  /* properties */
                 lookup_symbol( FUNCTION ),
                 cons( raw_state, NIL_OBJ ),
                 lookup_symbol( "state" ),
                 TLREFB(0) );
      raise_error( c );
    }
  state = raw_state;


{
  double f;

#if HAVE_ERAND48
  f = erand48( (unsigned short *)PTR_TO_DATAPTR( state ) );
#else
#if HAVE_RANDOM
  f = (double)random() / (double)LONG_MAX;
#else
  f = (double)rand() / (double)RAND_MAX;
#endif
#endif
  REG0 = make_float( f );
  RETURN1();
}}
#undef FPLACE_CODE

EPILOGUE(next_random_float)

BEGIN_BACK(next_random_float)
  BACK_MONOTONE(next_random_float_0)
END_BACK(next_random_float)

static struct function_descr next_random_float_descr = {
	&mathlib_part_random,
	JUMP_TABLE( next_random_float ),
	rsfn_next_random_float_name };
#undef FUNCTION

#undef raw_state
/******************************* Postamble *******************************/
/**************************** Part Link Table ****************************/


static struct function_descr *(part_random_tab[]) = {
    &make_random_state_descr,
    &next_random_descr,
    &make_random_float_state_descr,
    &next_random_float_descr,
    NULL };
struct part_descr mathlib_part_random = {
    246604801,
    &module_mathlib,
    part_random_tab,
    "random",
    0, sccsid };
#undef _MODULE_MATHLIB
#undef _SCM_RANDOM
#undef _C_RANDOM
