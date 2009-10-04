/**********************************************
THIS FILE WAS AUTOMATICALLY GENERATED, AND MAY
BE AUTOMATICALLY RE-GENERATED WHEN THE COMPILER
OR SOURCE CHANGES.  DO NOT MODIFY THIS FILE BY HAND!
RScheme Build (v0.7.3.4-b7u, 2007-05-30)
**********************************************/

/******************************** Preamble ********************************/

#define _MODULE_SORT
#define _SCM_SORT
#define _C_SORT
#include "sort_p.h"
#include <rscheme/vinsns.h>
extern struct module_descr module_sort;
extern struct part_descr sort_part_sort;
static char sccsid[] = "@(#)sort modules/sort/sort.scm [239655936] (RS v0.7.3.4-b7u, 2007-05-30)";

/************************** Function Definitions **************************/


/*********************** Function `sort:down-heap' ***********************/
static char rsfn_sort_down_heap_name[] = "sort:down-heap";
#define FUNCTION rsfn_sort_down_heap_name

PROLOGUE(sort_down_heap)

BEGIN_FWD(sort_down_heap)
  FWD_MONOTONE(sort_down_heap_0)
  FWD_MONOTONE(sort_down_heap_1)
  FWD_MONOTONE(sort_down_heap_2)
  FWD_MONOTONE(sort_down_heap_3)
  FWD_MONOTONE(sort_down_heap_4)
  FWD_MONOTONE(sort_down_heap_5)
END_FWD(sort_down_heap)

#define FPLACE_CODE (1000+0)
MONOTONE(sort_down_heap_0)
{
    COUNT_ARGS(4);
    /* NOP: REG3 = REG3; */
    /* NOP: REG2 = REG2; */
    /* NOP: REG1 = REG1; */
    /* NOP: REG0 = REG0; */
    CHECK_VECTOR(REG0 /* heap */);
    CHECK_FIXNUM(REG1 /* N */);
    CHECK_FIXNUM(REG3 /* k0 */);
    REG4 = gvec_ref(REG0 /* heap */,FXWORDS_TO_RIBYTES(REG3 /* k0 */));
    REG5 = REG0 /* heap */;
    REG6 = DIV2(REG1 /* N */);
    REG7 = REG3 /* k0 */;
    REG8 = ADD1(MUL2(REG3 /* k0 */));
    JUMP(9,sort_down_heap_1);
}
#undef FPLACE_CODE

#define FPLACE_CODE (1000+1)
MONOTONE(sort_down_heap_1)
{
    BEGIN_BIND2()
	    BIND_ARG(0,REG7);
	    BIND_ARG(1,REG8);
    END_BIND
    if (FX_LT(LEXREF0(0) /* k */,REG6 /* N/2 */))
    {
	    if (FX_LT(ADD1(LEXREF0(1) /* j */),REG1 /* N */))
	    {
	    SAVE_CONT7(sort_down_heap_2);
	    REG7 /* $fn */ = REG2 /* compare-fn */;
	    REG0 = gvec_ref(REG5 /* heap */,FXWORDS_TO_RIBYTES(LEXREF0(1) /* j */));
	    REG1 = gvec_ref(REG5 /* heap */,FXWORDS_TO_RIBYTES(ADD1(LEXREF0(1) /* j */)));
	    APPLY(2,REG7 /* $fn */);
	    }
	    else
	    {
	    JUMP(7,sort_down_heap_4);
	    }
    }
    else
    {
	    gvec_set(REG5 /* heap */,FXWORDS_TO_RIBYTES(LEXREF0(0) /* k */),REG4 /* v */);
	    RETURN0();
    }
}
#undef FPLACE_CODE

#define FPLACE_CODE (1000+2)
MONOTONE(sort_down_heap_2)
{
    REG7 = REG0;
    RESTORE_CONT7();
    if (truish(REG7))
    {
	    LEXSET0(1 /* j */,ADD1(LEXREF0(1) /* j */));
	    JUMP(7,sort_down_heap_3);
    }
    else
    {
	    JUMP(7,sort_down_heap_3);
    }
}
#undef FPLACE_CODE

#define FPLACE_CODE (1000+3)
MONOTONE(sort_down_heap_3)
{
    JUMP(7,sort_down_heap_4);
}
#undef FPLACE_CODE

#define FPLACE_CODE (1000+4)
MONOTONE(sort_down_heap_4)
{
    SAVE_CONT7(sort_down_heap_5);
    REG7 /* $fn */ = REG2 /* compare-fn */;
    REG0 = REG4 /* v */;
    REG1 = gvec_ref(REG5 /* heap */,FXWORDS_TO_RIBYTES(LEXREF0(1) /* j */));
    APPLY(2,REG7 /* $fn */);
}
#undef FPLACE_CODE

#define FPLACE_CODE (1000+5)
MONOTONE(sort_down_heap_5)
{
    REG7 = REG0;
    RESTORE_CONT7();
    if (truish(REG7))
    {
	    gvec_set(REG5 /* heap */,FXWORDS_TO_RIBYTES(LEXREF0(0) /* k */),gvec_ref(REG5 /* heap */,FXWORDS_TO_RIBYTES(LEXREF0(1) /* j */)));
	    REG7 = LEXREF0(1) /* j */;
	    REG8 = ADD1(MUL2(LEXREF0(1) /* j */));
	    POPENVT();
	    BJUMP(9,sort_down_heap_1);
    }
    else
    {
	    gvec_set(REG5 /* heap */,FXWORDS_TO_RIBYTES(LEXREF0(0) /* k */),REG4 /* v */);
	    RETURN0();
    }
}
#undef FPLACE_CODE

EPILOGUE(sort_down_heap)

BEGIN_BACK(sort_down_heap)
  BACK_MONOTONE(sort_down_heap_0)
  BACK_MONOTONE(sort_down_heap_1)
  BACK_MONOTONE(sort_down_heap_2)
  BACK_MONOTONE(sort_down_heap_3)
  BACK_MONOTONE(sort_down_heap_4)
  BACK_MONOTONE(sort_down_heap_5)
END_BACK(sort_down_heap)

static struct function_descr sort_down_heap_descr = {
	&sort_part_sort,
	JUMP_TABLE( sort_down_heap ),
	rsfn_sort_down_heap_name };
#undef FUNCTION

/******************************* Postamble *******************************/
/**************************** Part Link Table ****************************/


static struct function_descr *(part_sort_tab[]) = {
    &sort_down_heap_descr,
    NULL };
struct part_descr sort_part_sort = {
    239655936,
    &module_sort,
    part_sort_tab,
    "sort",
    0, sccsid };
#undef _MODULE_SORT
#undef _SCM_SORT
#undef _C_SORT
