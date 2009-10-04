/**********************************************
THIS FILE WAS AUTOMATICALLY GENERATED, AND MAY
BE AUTOMATICALLY RE-GENERATED WHEN THE COMPILER
OR SOURCE CHANGES.  DO NOT MODIFY THIS FILE BY HAND!
RScheme Build (v0.7.3.4-b7u, 2007-05-30)
**********************************************/

/******************************** Preamble ********************************/

#define _MODULE_CORELIB
#define _SCM_RATIONAL
#define _C_RATIONAL
#include "corelib_p.h"
#include <rscheme/vinsns.h>
extern struct module_descr module_corelib;
extern struct part_descr corelib_part_rational;
static char sccsid[] = "@(#)corelib modules/corelib/rational.scm [110377996] (RS v0.7.3.4-b7u, 2007-05-30)";

/************************** Function Definitions **************************/


/***************** Raw glue `raw-rational->double-float' *****************/
#define a REG0

static char rsfn_raw_rational_double_float_name[] = "raw-rational->double-float";
#define FUNCTION rsfn_raw_rational_double_float_name

PROLOGUE(raw_rational_double_float)

BEGIN_FWD(raw_rational_double_float)
  FWD_MONOTONE(raw_rational_double_float_0)
END_FWD(raw_rational_double_float)

#define FPLACE_CODE (1000+0)
MONOTONE(raw_rational_double_float_0)
{
{
#if !FULL_NUMERIC_TOWER
    scheme_error( "rational.scm:raw-rational->double-float:~s: function stubbed out", 1, int2fx(__LINE__) ); 
    REG0 = ZERO;
    RETURN1();
#else
  IEEE_64 x = rational_to_raw_float(a);
  REG0 = make_float(x);
  RETURN1();
#endif
}}
#undef FPLACE_CODE

EPILOGUE(raw_rational_double_float)

BEGIN_BACK(raw_rational_double_float)
  BACK_MONOTONE(raw_rational_double_float_0)
END_BACK(raw_rational_double_float)

static struct function_descr raw_rational_double_float_descr = {
	&corelib_part_rational,
	JUMP_TABLE( raw_rational_double_float ),
	rsfn_raw_rational_double_float_name };
#undef FUNCTION

#undef a
/******************************* Postamble *******************************/
/**************************** Part Link Table ****************************/


static struct function_descr *(part_rational_tab[]) = {
    &raw_rational_double_float_descr,
    NULL };
struct part_descr corelib_part_rational = {
    110377996,
    &module_corelib,
    part_rational_tab,
    "rational",
    0, sccsid };
#undef _MODULE_CORELIB
#undef _SCM_RATIONAL
#undef _C_RATIONAL
