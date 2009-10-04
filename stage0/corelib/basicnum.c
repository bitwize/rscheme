/**********************************************
THIS FILE WAS AUTOMATICALLY GENERATED, AND MAY
BE AUTOMATICALLY RE-GENERATED WHEN THE COMPILER
OR SOURCE CHANGES.  DO NOT MODIFY THIS FILE BY HAND!
RScheme Build (v0.7.3.4-b7u, 2007-05-30)
**********************************************/

/******************************** Preamble ********************************/

#define _MODULE_CORELIB
#define _SCM_BASICNUM
#define _C_BASICNUM
#include "corelib_p.h"
#include <rscheme/vinsns.h>
extern struct module_descr module_corelib;
extern struct part_descr corelib_part_basicnum;
static char sccsid[] = "@(#)corelib modules/corelib/basicnum.scm [34398221] (RS v0.7.3.4-b7u, 2007-05-30)";

/************************** Function Definitions **************************/


/****************** Raw glue `raw-longint->double-float' ******************/
#define a REG0

static char rsfn_raw_longint_double_float_name[] = "raw-longint->double-float";
#define FUNCTION rsfn_raw_longint_double_float_name

PROLOGUE(raw_longint_double_float)

BEGIN_FWD(raw_longint_double_float)
  FWD_MONOTONE(raw_longint_double_float_0)
END_FWD(raw_longint_double_float)

#define FPLACE_CODE (1000+0)
MONOTONE(raw_longint_double_float_0)
{
{
#if !FULL_NUMERIC_TOWER
    scheme_error( "bignum.scm:raw-longint->double-float:~s: function stubbed out", 1, int2fx(__LINE__) ); 
    REG0 = FALSE_OBJ;
    RETURN1();
#else
  IEEE_64 x = longint_to_raw_float(a);
  REG0 = make_float(x);
  RETURN1();
#endif
}}
#undef FPLACE_CODE

EPILOGUE(raw_longint_double_float)

BEGIN_BACK(raw_longint_double_float)
  BACK_MONOTONE(raw_longint_double_float_0)
END_BACK(raw_longint_double_float)

static struct function_descr raw_longint_double_float_descr = {
	&corelib_part_basicnum,
	JUMP_TABLE( raw_longint_double_float ),
	rsfn_raw_longint_double_float_name };
#undef FUNCTION

#undef a

/****************** Raw glue `raw-bignum->double-float' ******************/
#define a REG0

static char rsfn_raw_bignum_double_float_name[] = "raw-bignum->double-float";
#define FUNCTION rsfn_raw_bignum_double_float_name

PROLOGUE(raw_bignum_double_float)

BEGIN_FWD(raw_bignum_double_float)
  FWD_MONOTONE(raw_bignum_double_float_0)
END_FWD(raw_bignum_double_float)

#define FPLACE_CODE (1000+0)
MONOTONE(raw_bignum_double_float_0)
{
{
#if !FULL_NUMERIC_TOWER
    scheme_error( "basicnum.scm:raw-bignum->double-float:~s: function stubbed out", 1, int2fx(__LINE__) ); 
    REG0 = ZERO;
    RETURN1();
#else
  IEEE_64 x = bignum_to_raw_float(a);
  REG0 = make_float(x);
  RETURN1();
#endif
}}
#undef FPLACE_CODE

EPILOGUE(raw_bignum_double_float)

BEGIN_BACK(raw_bignum_double_float)
  BACK_MONOTONE(raw_bignum_double_float_0)
END_BACK(raw_bignum_double_float)

static struct function_descr raw_bignum_double_float_descr = {
	&corelib_part_basicnum,
	JUMP_TABLE( raw_bignum_double_float ),
	rsfn_raw_bignum_double_float_name };
#undef FUNCTION

#undef a
/******************************* Postamble *******************************/
/**************************** Part Link Table ****************************/


static struct function_descr *(part_basicnum_tab[]) = {
    &raw_longint_double_float_descr,
    &raw_bignum_double_float_descr,
    NULL };
struct part_descr corelib_part_basicnum = {
    34398221,
    &module_corelib,
    part_basicnum_tab,
    "basicnum",
    0, sccsid };
#undef _MODULE_CORELIB
#undef _SCM_BASICNUM
#undef _C_BASICNUM
