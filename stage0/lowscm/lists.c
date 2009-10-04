/**********************************************
THIS FILE WAS AUTOMATICALLY GENERATED, AND MAY
BE AUTOMATICALLY RE-GENERATED WHEN THE COMPILER
OR SOURCE CHANGES.  DO NOT MODIFY THIS FILE BY HAND!
RScheme Build (v0.7.3.4-b7u, 2007-05-30)
**********************************************/

/******************************** Preamble ********************************/

#define _MODULE_LOW_SCHEME
#define _SCM_LISTS
#define _C_LISTS
#include "low_scheme_p.h"
#include <rscheme/vinsns.h>
extern struct module_descr module_low_scheme;
extern struct part_descr low_scheme_part_lists;
static char sccsid[] = "@(#)low-scheme modules/lowscm/lists.scm [382228480] (RS v0.7.3.4-b7u, 2007-05-30)";

/************************** Function Definitions **************************/


/*************************** Raw glue `length' ***************************/
#define lst REG0

static char rsfn_length_name[] = "length";
#define FUNCTION rsfn_length_name

PROLOGUE(length)

BEGIN_FWD(length)
  FWD_MONOTONE(length_0)
  FWD_MONOTONE(list_length_cont)
END_FWD(length)

#define FPLACE_CODE (1000+0)
MONOTONE(length_0)
{
{
  obj n = ZERO;
  obj l = lst;

  while (PAIR_P( l ) && FX_LT( n, int2fx( 1000 )))
    {
      l = pair_cdr( l );
      n = ADD1( n );
    }
  if (NULL_P( l ))
    {
      REG0 = n;
      RETURN1();
    }
  else
    {
      /* need to keep looking, or it was an error */
      REG0 = lst;
      REG1 = n;
      REG2 = l;
      JUMP( 3, list_length_cont );
    }
}}
#undef FPLACE_CODE

#define FPLACE_CODE (1000+1)
MONOTONE(list_length_cont)
{
  obj n = REG1;
  obj l = REG2;
  int k = 0;

  if (!PAIR_P( l ))
    {
      raise_error( make3( TLREFB(0), NIL_OBJ, REG0, l ) );
    }
  for (k=0; PAIR_P( l ) && (k < 1000); k++)
    {
      l = pair_cdr( l );
      n = ADD1( n );
    }
  if (NULL_P( l ))
    {
      REG0 = n;
      RETURN1();
    }
  else
    {
      /* need to keep looking, or it was an error */
      REG0 = lst;
      REG1 = n;
      REG2 = l;
      BJUMP( 3, list_length_cont );
    }
}
#undef FPLACE_CODE

EPILOGUE(length)

BEGIN_BACK(length)
  BACK_MONOTONE(length_0)
  BACK_MONOTONE(list_length_cont)
END_BACK(length)

static struct function_descr length_descr = {
	&low_scheme_part_lists,
	JUMP_TABLE( length ),
	rsfn_length_name };
#undef FUNCTION

#undef lst
/******************************* Postamble *******************************/
/**************************** Part Link Table ****************************/


static struct function_descr *(part_lists_tab[]) = {
    &length_descr,
    NULL };
struct part_descr low_scheme_part_lists = {
    382228480,
    &module_low_scheme,
    part_lists_tab,
    "lists",
    0, sccsid };
#undef _MODULE_LOW_SCHEME
#undef _SCM_LISTS
#undef _C_LISTS
