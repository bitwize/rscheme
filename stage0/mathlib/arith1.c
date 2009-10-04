/**********************************************
THIS FILE WAS AUTOMATICALLY GENERATED, AND MAY
BE AUTOMATICALLY RE-GENERATED WHEN THE COMPILER
OR SOURCE CHANGES.  DO NOT MODIFY THIS FILE BY HAND!
RScheme Build (v0.7.3.4-b7u, 2007-05-30)
**********************************************/

/******************************** Preamble ********************************/

#define _MODULE_MATHLIB
#define _SCM_ARITH
#define _C_ARITH
#include "mathlib_p.h"
#include <rscheme/vinsns.h>
extern struct module_descr module_mathlib;
extern struct part_descr mathlib_part_arith;
static char sccsid[] = "@(#)mathlib modules/mathlib/arith.scm [434406400] (RS v0.7.3.4-b7u, 2007-05-30)";

/************************** Function Definitions **************************/


/************************* Raw glue `float-round' *************************/
#define x REG0

static char rsfn_float_round_name[] = "float-round";
#define FUNCTION rsfn_float_round_name

PROLOGUE(float_round)

BEGIN_FWD(float_round)
  FWD_MONOTONE(float_round_0)
END_FWD(float_round)

#define FPLACE_CODE (1000+0)
MONOTONE(float_round_0)
{
{
#if !FULL_NUMERIC_TOWER
  IEEE_64 a = extract_float(x);
  INT_32 ai;
    
  if (a < 0)
    {
      ai = (int)(a - 0.5);
      if (ai >= -536870912)
	REG0 = int2fx( ai );
      else
	REG0 = x;
    }
  else
    {
      ai = (int)(a + 0.5);
      if (ai <= 536870911)
	REG0 = int2fx( ai );
      else
	REG0 = x;
    }
    RETURN(1);
#else
  extern obj float_truncate( IEEE_64 x );
  IEEE_64 a = extract_float( x );

  if (a < 0)
    a -= 0.5; 
  else 
    a += 0.5;
  REG0 = float_truncate( a );
  RETURN(1);
#endif
}}
#undef FPLACE_CODE

EPILOGUE(float_round)

BEGIN_BACK(float_round)
  BACK_MONOTONE(float_round_0)
END_BACK(float_round)

static struct function_descr float_round_descr = {
	&mathlib_part_arith,
	JUMP_TABLE( float_round ),
	rsfn_float_round_name };
#undef FUNCTION

#undef x
/******************************* Postamble *******************************/
/**************************** Part Link Table ****************************/


static struct function_descr *(part_arith_tab[]) = {
    &float_round_descr,
    NULL };
struct part_descr mathlib_part_arith = {
    434406400,
    &module_mathlib,
    part_arith_tab,
    "arith",
    0, sccsid };
#undef _MODULE_MATHLIB
#undef _SCM_ARITH
#undef _C_ARITH
