/**********************************************
THIS FILE WAS AUTOMATICALLY GENERATED, AND MAY
BE AUTOMATICALLY RE-GENERATED WHEN THE COMPILER
OR SOURCE CHANGES.  DO NOT MODIFY THIS FILE BY HAND!
RScheme Build (v0.7.3.4-b7u, 2007-05-30)
**********************************************/

/******************************** Preamble ********************************/

#define _MODULE_LOW_SCHEME
#define _SCM_APPEND
#define _C_APPEND
#include "low_scheme_p.h"
#include <rscheme/vinsns.h>
extern struct module_descr module_low_scheme;
extern struct part_descr low_scheme_part_append;
static char sccsid[] = "@(#)low-scheme modules/lowscm/append.scm [170951682] (RS v0.7.3.4-b7u, 2007-05-30)";

/************************** Function Definitions **************************/


/************************** Function `append-2' **************************/
static char rsfn_append_2_name[] = "append-2";
#define FUNCTION rsfn_append_2_name

PROLOGUE(append_2)

BEGIN_FWD(append_2)
  FWD_MONOTONE(append_2_0)
  FWD_MONOTONE(append_2_1)
END_FWD(append_2)

#define FPLACE_CODE (1000+0)
MONOTONE(append_2_0)
{
    COUNT_ARGS(2);
    /* NOP: REG1 = REG1; */
    /* NOP: REG0 = REG0; */
    if (PAIR_P(REG0 /* lst1 */))
    {
	    REG2 = cons(checked_car(REG0 /* lst1 */),NIL_OBJ);
	    /* NOP: REG2 = REG2; */
	    REG3 = REG2 /* first */;
	    REG4 = checked_cdr(REG0 /* lst1 */);
	    /* NOP: REG3 = REG3; */
	    /* NOP: REG4 = REG4; */
	    JUMP(5,append_2_1);
    }
    else
    {
	    REG0 = REG1 /* lst2 */;
	    RETURN1();
    }
}
#undef FPLACE_CODE

#define FPLACE_CODE (1000+1)
MONOTONE(append_2_1)
{
    /* NOP: REG4 = REG4; */
    /* NOP: REG3 = REG3; */
    if (PAIR_P(REG4 /* src */))
    {
	    REG5 = REG4 /* src */;
	    REG6 = cons(checked_car(REG5 /* s */),NIL_OBJ);
	    /* NOP: REG6 = REG6; */
	    pair_set_cdr(REG3 /* prev */,REG6 /* c */);
	    REG7 = REG6 /* c */;
	    REG8 = checked_cdr(REG5 /* s */);
	    /* NOP: REG7 = REG7; */
	    /* NOP: REG8 = REG8; */
	    REG3 = REG7;
	    REG4 = REG8;
	    BJUMP(5,append_2_1);
    }
    else
    {
	    pair_set_cdr(REG3 /* prev */,REG1 /* lst2 */);
	    REG0 = REG2 /* first */;
	    RETURN1();
    }
}
#undef FPLACE_CODE

EPILOGUE(append_2)

BEGIN_BACK(append_2)
  BACK_MONOTONE(append_2_0)
  BACK_MONOTONE(append_2_1)
END_BACK(append_2)

static struct function_descr append_2_descr = {
	&low_scheme_part_append,
	JUMP_TABLE( append_2 ),
	rsfn_append_2_name };
#undef FUNCTION


/************************** Function `append-n' **************************/
static char rsfn_append_n_name[] = "append-n";
#define FUNCTION rsfn_append_n_name

PROLOGUE(append_n)

BEGIN_FWD(append_n)
  FWD_MONOTONE(append_n_0)
  FWD_MONOTONE(append_n_1)
END_FWD(append_n)

#define FPLACE_CODE (1000+0)
MONOTONE(append_n_0)
{
    COUNT_ARGS(2);
    /* NOP: REG1 = REG1; */
    /* NOP: REG0 = REG0; */
    if (NULL_P(REG1 /* more-lists */))
    {
	    /* NOP: REG0 = REG0; */
	    RETURN1();
    }
    else
    {
	    REG2 = REG0 /* list-1 */;
	    SAVE_CONT3(append_n_1);
	    REG3 = checked_car(REG1 /* more-lists */);
	    REG4 = checked_cdr(REG1 /* more-lists */);
	    REG0 = REG3;
	    REG1 = REG4;
	    APPLY(2,TLREF(0) /* append-n */);
    }
}
#undef FPLACE_CODE

#define FPLACE_CODE (1000+1)
MONOTONE(append_n_1)
{
    REG3 = REG0;
    RESTORE_CONT3();
    /* NOP: REG3 = REG3; */
    REG0 = REG2;
    REG1 = REG3;
    APPLYF(2,TLREFB(1) /* append-2 */);
}
#undef FPLACE_CODE

EPILOGUE(append_n)

BEGIN_BACK(append_n)
  BACK_MONOTONE(append_n_0)
  BACK_MONOTONE(append_n_1)
END_BACK(append_n)

static struct function_descr append_n_descr = {
	&low_scheme_part_append,
	JUMP_TABLE( append_n ),
	rsfn_append_n_name };
#undef FUNCTION


/*************************** Function `append' ***************************/
static char rsfn_append_name[] = "append";
#define FUNCTION rsfn_append_name

PROLOGUE(append)

BEGIN_FWD(append)
  FWD_MONOTONE(append_0)
END_FWD(append)

#define FPLACE_CODE (1000+0)
MONOTONE(append_0)
{
    COLLECT0();
    /* NOP: REG0 = REG0; */
    if (PAIR_P(REG0 /* lists */))
    {
	    if (PAIR_P(checked_cdr(REG0 /* lists */)))
	    {
	    if (PAIR_P(checked_cdr(checked_cdr(REG0 /* lists */))))
	    {
		    REG1 = checked_car(REG0 /* lists */);
		    REG2 = checked_cdr(REG0 /* lists */);
		    REG0 = REG1;
		    REG1 = REG2;
		    APPLYF(2,TLREFB(0) /* append-n */);
	    }
	    else
	    {
		    REG1 = checked_car(REG0 /* lists */);
		    REG2 = checked_car(checked_cdr(REG0 /* lists */));
		    REG0 = REG1;
		    REG1 = REG2;
		    APPLYF(2,TLREFB(1) /* append-2 */);
	    }
	    }
	    else
	    {
	    REG0 = checked_car(REG0 /* lists */);
	    RETURN1();
	    }
    }
    else
    {
	    REG0 = NIL_OBJ;
	    RETURN1();
    }
}
#undef FPLACE_CODE

EPILOGUE(append)

BEGIN_BACK(append)
  BACK_MONOTONE(append_0)
END_BACK(append)

static struct function_descr append_descr = {
	&low_scheme_part_append,
	JUMP_TABLE( append ),
	rsfn_append_name };
#undef FUNCTION

/******************************* Postamble *******************************/
/**************************** Part Link Table ****************************/


static struct function_descr *(part_append_tab[]) = {
    &append_2_descr,
    &append_n_descr,
    &append_descr,
    NULL };
struct part_descr low_scheme_part_append = {
    170951682,
    &module_low_scheme,
    part_append_tab,
    "append",
    0, sccsid };
#undef _MODULE_LOW_SCHEME
#undef _SCM_APPEND
#undef _C_APPEND
