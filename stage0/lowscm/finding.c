/**********************************************
THIS FILE WAS AUTOMATICALLY GENERATED, AND MAY
BE AUTOMATICALLY RE-GENERATED WHEN THE COMPILER
OR SOURCE CHANGES.  DO NOT MODIFY THIS FILE BY HAND!
RScheme Build (v0.7.3.4-b7u, 2007-05-30)
**********************************************/

/******************************** Preamble ********************************/

#define _MODULE_LOW_SCHEME
#define _SCM_FINDING
#define _C_FINDING
#include "low_scheme_p.h"
#include <rscheme/vinsns.h>
extern struct module_descr module_low_scheme;
extern struct part_descr low_scheme_part_finding;
static char sccsid[] = "@(#)low-scheme modules/lowscm/finding.scm [386500612] (RS v0.7.3.4-b7u, 2007-05-30)";

/************************** Function Definitions **************************/


/**************************** Raw glue `assq' ****************************/
#define item REG0
#define list REG1

static char rsfn_assq_name[] = "assq";
#define FUNCTION rsfn_assq_name

PROLOGUE(assq)

BEGIN_FWD(assq)
  FWD_MONOTONE(assq_0)
  FWD_MONOTONE(assq_2)
END_FWD(assq)

#define FPLACE_CODE (1000+0)
MONOTONE(assq_0)
{
{
  COUNT_ARGS(2);
  REG2 = REG1;  /* copy base list into REG2 */
  JUMP(3,assq_2);
}}
#undef FPLACE_CODE

#define FPLACE_CODE (1000+1)
MONOTONE(assq_2)
{
unsigned i;
obj entry;

  for (i=0; i<20; i++)
    {
      if (!PAIR_P(REG1))
	{
	  if (EQ(REG1,NIL_OBJ))
	    {
	      REG0 = FALSE_OBJ;
	      RETURN1();
	    }
	  else
	    {
	      REG0 = TLREF(0);
	      APPLY(2,TLREF(2));
	    }
	}
      entry = pair_car(REG1);
      if (!PAIR_P(entry))
	{
	  REG0 = TLREF(0);
	  REG1 = entry;
	  APPLY(2,TLREF(1));
	}
      else if (EQ(pair_car(entry),REG0))
	{
	  REG0 = entry;
	  RETURN1();
	}
      REG1 = pair_cdr(REG1);
    }
/* loop for some more */
    BJUMP(2,assq_2);
}
#undef FPLACE_CODE

EPILOGUE(assq)

BEGIN_BACK(assq)
  BACK_MONOTONE(assq_0)
  BACK_MONOTONE(assq_2)
END_BACK(assq)

static struct function_descr assq_descr = {
	&low_scheme_part_finding,
	JUMP_TABLE( assq ),
	rsfn_assq_name };
#undef FUNCTION

#undef item
#undef list
/******************************* Postamble *******************************/
/**************************** Part Link Table ****************************/


static struct function_descr *(part_finding_tab[]) = {
    &assq_descr,
    NULL };
struct part_descr low_scheme_part_finding = {
    386500612,
    &module_low_scheme,
    part_finding_tab,
    "finding",
    0, sccsid };
#undef _MODULE_LOW_SCHEME
#undef _SCM_FINDING
#undef _C_FINDING
