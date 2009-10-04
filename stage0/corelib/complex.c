/**********************************************
THIS FILE WAS AUTOMATICALLY GENERATED, AND MAY
BE AUTOMATICALLY RE-GENERATED WHEN THE COMPILER
OR SOURCE CHANGES.  DO NOT MODIFY THIS FILE BY HAND!
RScheme Build (v0.7.3.4-b7u, 2007-05-30)
**********************************************/

/******************************** Preamble ********************************/

#define _MODULE_CORELIB
#define _SCM_COMPLEX
#define _C_COMPLEX
#include "corelib_p.h"
#include <rscheme/vinsns.h>
extern struct module_descr module_corelib;
extern struct part_descr corelib_part_complex;
static char sccsid[] = "@(#)corelib modules/corelib/complex.scm [321654795] (RS v0.7.3.4-b7u, 2007-05-30)";

/************************** Function Definitions **************************/


/********************** Raw glue `make-rectangular' **********************/
#define raw_re REG0
#define raw_im REG1

static char rsfn_make_rectangular_name[] = "make-rectangular";
#define FUNCTION rsfn_make_rectangular_name

PROLOGUE(make_rectangular)

BEGIN_FWD(make_rectangular)
  FWD_MONOTONE(make_rectangular_0)
END_FWD(make_rectangular)

#define FPLACE_CODE (1000+0)
MONOTONE(make_rectangular_0)
{  obj re;
  obj im;
  COUNT_ARGS(2);
  if (!instance_p(raw_re,TLREFB(0)))
    {
      obj c;
      c = make5( TLREFB(1),
                 NIL_OBJ,  /* properties */
                 lookup_symbol( FUNCTION ),
                 cons( raw_re, NIL_OBJ ),
                 lookup_symbol( "re" ),
                 TLREFB(0) );
      raise_error( c );
    }
  re = raw_re;

  if (!instance_p(raw_im,TLREFB(0)))
    {
      obj c;
      c = make5( TLREFB(1),
                 NIL_OBJ,  /* properties */
                 lookup_symbol( FUNCTION ),
                 cons( raw_im, NIL_OBJ ),
                 lookup_symbol( "im" ),
                 TLREFB(0) );
      raise_error( c );
    }
  im = raw_im;


{
#if !FULL_NUMERIC_TOWER
    scheme_error( "complex.scm:make-rectanglar:~s: function stubbed out", 1, int2fx(__LINE__) ); 
    REG0 = ZERO;
    RETURN1();
#else
  REG0 = make_complex_obj(re, im);
  RETURN1();
#endif
}}
#undef FPLACE_CODE

EPILOGUE(make_rectangular)

BEGIN_BACK(make_rectangular)
  BACK_MONOTONE(make_rectangular_0)
END_BACK(make_rectangular)

static struct function_descr make_rectangular_descr = {
	&corelib_part_complex,
	JUMP_TABLE( make_rectangular ),
	rsfn_make_rectangular_name };
#undef FUNCTION

#undef raw_re
#undef raw_im
/******************************* Postamble *******************************/
/**************************** Part Link Table ****************************/


static struct function_descr *(part_complex_tab[]) = {
    &make_rectangular_descr,
    NULL };
struct part_descr corelib_part_complex = {
    321654795,
    &module_corelib,
    part_complex_tab,
    "complex",
    0, sccsid };
#undef _MODULE_CORELIB
#undef _SCM_COMPLEX
#undef _C_COMPLEX
