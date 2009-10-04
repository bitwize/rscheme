/**********************************************
THIS FILE WAS AUTOMATICALLY GENERATED, AND MAY
BE AUTOMATICALLY RE-GENERATED WHEN THE COMPILER
OR SOURCE CHANGES.  DO NOT MODIFY THIS FILE BY HAND!
RScheme Build (v0.7.3.4-b7u, 2007-05-30)
**********************************************/

/******************************** Preamble ********************************/

#define _MODULE_LOW_SCHEME
#define _SCM_MAPPING
#define _C_MAPPING
#include "low_scheme_p.h"
#include <rscheme/vinsns.h>
extern struct module_descr module_low_scheme;
extern struct part_descr low_scheme_part_mapping;
static char sccsid[] = "@(#)low-scheme modules/lowscm/mapping.scm [118447107] (RS v0.7.3.4-b7u, 2007-05-30)";

/************************** Function Definitions **************************/


/**************************** Function `.map1' ****************************/
static char rsfn_map1_name[] = ".map1";
#define FUNCTION rsfn_map1_name

PROLOGUE(map1)

BEGIN_FWD(map1)
  FWD_MONOTONE(map1_0)
  FWD_MONOTONE(map1_1)
  FWD_MONOTONE(map1_2)
  FWD_MONOTONE(map1_3)
END_FWD(map1)

#define FPLACE_CODE (1000+0)
MONOTONE(map1_0)
{
    COUNT_ARGS(2);
    /* NOP: REG1 = REG1; */
    /* NOP: REG0 = REG0; */
    if (NULL_P(REG1 /* list */))
    {
	    REG0 = NIL_OBJ;
	    RETURN1();
    }
    else
    {
	    SAVE_CONT2(map1_1);
	    REG2 = checked_car(REG1 /* list */);
	    REG3 /* $fn */ = REG0 /* proc */;
	    REG0 = REG2;
	    APPLY(1,REG3 /* $fn */);
    }
}
#undef FPLACE_CODE

#define FPLACE_CODE (1000+1)
MONOTONE(map1_1)
{
    REG2 = REG0;
    RESTORE_CONT2();
    REG2 = cons(REG2,NIL_OBJ);
    /* NOP: REG2 = REG2; */
    REG3 = checked_cdr(REG1 /* list */);
    /* NOP: REG3 = REG3; */
    REG4 = REG2 /* first */;
    JUMP(5,map1_2);
}
#undef FPLACE_CODE

#define FPLACE_CODE (1000+2)
MONOTONE(map1_2)
{
    BEGIN_BIND2()
	    BIND_ARG(0,REG3);
	    BIND_ARG(1,REG4);
    END_BIND
    if (NULL_P(LEXREF0(0) /* l */))
    {
	    REG0 = REG2 /* first */;
	    RETURN1();
    }
    else
    {
	    SAVE_CONT3(map1_3);
	    REG3 = checked_car(LEXREF0(0) /* l */);
	    REG4 /* $fn */ = REG0 /* proc */;
	    REG0 = REG3;
	    APPLY(1,REG4 /* $fn */);
    }
}
#undef FPLACE_CODE

#define FPLACE_CODE (1000+3)
MONOTONE(map1_3)
{
    REG3 = REG0;
    RESTORE_CONT3();
    REG3 = cons(REG3,NIL_OBJ);
    /* NOP: REG3 = REG3; */
    pair_set_cdr(LEXREF0(1) /* dest */,REG3 /* next */);
    REG4 = checked_cdr(LEXREF0(0) /* l */);
    /* NOP: REG4 = REG4; */
    REG5 = REG3 /* next */;
    POPENVT();
    REG3 = REG4;
    REG4 = REG5;
    BJUMP(5,map1_2);
}
#undef FPLACE_CODE

EPILOGUE(map1)

BEGIN_BACK(map1)
  BACK_MONOTONE(map1_0)
  BACK_MONOTONE(map1_1)
  BACK_MONOTONE(map1_2)
  BACK_MONOTONE(map1_3)
END_BACK(map1)

static struct function_descr map1_descr = {
	&low_scheme_part_mapping,
	JUMP_TABLE( map1 ),
	rsfn_map1_name };
#undef FUNCTION


/**************************** Function `anon' ****************************/
static char rsfn_anon_name[] = "anon";
#define FUNCTION rsfn_anon_name

PROLOGUE(anon)

BEGIN_FWD(anon)
  FWD_MONOTONE(anon_0)
  FWD_MONOTONE(anon_1)
  FWD_MONOTONE(anon_2)
END_FWD(anon)

#define FPLACE_CODE (1000+0)
MONOTONE(anon_0)
{
    USE_FUNCTION_ENVT();
    COUNT_ARGS(2);
    /* NOP: REG1 = REG1; */
    /* NOP: REG0 = REG0; */
    if (NULL_P(REG0 /* l1 */))
    {
	    REG0 = NIL_OBJ;
	    RETURN1();
    }
    else
    {
	    SAVE_CONT2(anon_1);
	    REG2 = checked_car(REG0 /* l1 */);
	    REG3 = checked_car(REG1 /* l2 */);
	    REG0 = REG2;
	    REG1 = REG3;
	    APPLY(2,LEXREF1(0) /* proc */);
    }
}
#undef FPLACE_CODE

#define FPLACE_CODE (1000+1)
MONOTONE(anon_1)
{
    REG2 = REG0;
    RESTORE_CONT2();
    /* NOP: REG2 = REG2; */
    SAVE_CONT3(anon_2);
    REG3 = checked_cdr(REG0 /* l1 */);
    REG4 = checked_cdr(REG1 /* l2 */);
    REG0 = REG3;
    REG1 = REG4;
    APPLY(2,LEXREF0(0) /* loop */);
}
#undef FPLACE_CODE

#define FPLACE_CODE (1000+2)
MONOTONE(anon_2)
{
    REG3 = REG0;
    RESTORE_CONT3();
    REG0 = cons(REG2,REG3);
    RETURN1();
}
#undef FPLACE_CODE

EPILOGUE(anon)

BEGIN_BACK(anon)
  BACK_MONOTONE(anon_0)
  BACK_MONOTONE(anon_1)
  BACK_MONOTONE(anon_2)
END_BACK(anon)

static struct function_descr anon_descr = {
	&low_scheme_part_mapping,
	JUMP_TABLE( anon ),
	rsfn_anon_name };
#undef FUNCTION


/**************************** Function `.map2' ****************************/
static char rsfn_map2_name[] = ".map2";
#define FUNCTION rsfn_map2_name

PROLOGUE(map2)

BEGIN_FWD(map2)
  FWD_MONOTONE(map2_0)
END_FWD(map2)

#define FPLACE_CODE (1000+0)
MONOTONE(map2_0)
{
    COUNT_ARGS(3);
    BEGIN_BIND3()
	    BIND_ARG(0,REG0);
	    BIND_ARG(1,REG1);
	    BIND_ARG(2,REG2);
    END_BIND
    BEGIN_BIND1()
	    BIND_ARG(0,UNINITIALIZED_OBJ);
    END_BIND
    LEXSET0(0 /* loop */,CLOSURE(0));
    REG0 = LEXREF1(1) /* l1 */;
    REG1 = LEXREF1(2) /* l2 */;
    APPLY(2,LEXREF0(0) /* loop */);
}
#undef FPLACE_CODE

EPILOGUE(map2)

BEGIN_BACK(map2)
  BACK_MONOTONE(map2_0)
END_BACK(map2)

static struct function_descr map2_descr = {
	&low_scheme_part_mapping,
	JUMP_TABLE( map2 ),
	rsfn_map2_name };
#undef FUNCTION


/************************** Function `for-each1' **************************/
static char rsfn_for_each1_name[] = "for-each1";
#define FUNCTION rsfn_for_each1_name

PROLOGUE(for_each1)

BEGIN_FWD(for_each1)
  FWD_MONOTONE(for_each1_0)
  FWD_MONOTONE(for_each1_1)
  FWD_MONOTONE(for_each1_2)
END_FWD(for_each1)

#define FPLACE_CODE (1000+0)
MONOTONE(for_each1_0)
{
    COUNT_ARGS(2);
    /* NOP: REG1 = REG1; */
    /* NOP: REG0 = REG0; */
    CHECK_FUNCTION(REG0 /* proc */);
    if (PAIR_P(REG1 /* list */))
    {
	    REG2 = REG1 /* list */;
	    JUMP(3,for_each1_1);
    }
    else
    {
	    RETURN0();
    }
}
#undef FPLACE_CODE

#define FPLACE_CODE (1000+1)
MONOTONE(for_each1_1)
{
    BEGIN_BIND1()
	    BIND_ARG(0,REG2);
    END_BIND
    if (PAIR_P(checked_cdr(LEXREF0(0) /* list */)))
    {
	    SAVE_CONT2(for_each1_2);
	    REG2 = checked_car(LEXREF0(0) /* list */);
	    REG3 /* $fn */ = REG0 /* proc */;
	    REG0 = REG2;
	    APPLYF(1,REG3 /* $fn */);
    }
    else
    {
	    REG2 = checked_car(LEXREF0(0) /* list */);
	    REG3 /* $fn */ = REG0 /* proc */;
	    REG0 = REG2;
	    APPLYF(1,REG3 /* $fn */);
    }
}
#undef FPLACE_CODE

#define FPLACE_CODE (1000+2)
MONOTONE(for_each1_2)
{
    RESTORE_CONT2();
    REG2 = checked_cdr(LEXREF0(0) /* list */);
    /* NOP: REG2 = REG2; */
    POPENVT();
    BJUMP(3,for_each1_1);
}
#undef FPLACE_CODE

EPILOGUE(for_each1)

BEGIN_BACK(for_each1)
  BACK_MONOTONE(for_each1_0)
  BACK_MONOTONE(for_each1_1)
  BACK_MONOTONE(for_each1_2)
END_BACK(for_each1)

static struct function_descr for_each1_descr = {
	&low_scheme_part_mapping,
	JUMP_TABLE( for_each1 ),
	rsfn_for_each1_name };
#undef FUNCTION


/************************** Function `for-each2' **************************/
static char rsfn_for_each2_name[] = "for-each2";
#define FUNCTION rsfn_for_each2_name

PROLOGUE(for_each2)

BEGIN_FWD(for_each2)
  FWD_MONOTONE(for_each2_0)
  FWD_MONOTONE(for_each2_1)
  FWD_MONOTONE(for_each2_2)
  FWD_MONOTONE(for_each2_3)
  FWD_MONOTONE(for_each2_4)
END_FWD(for_each2)

#define FPLACE_CODE (1000+0)
MONOTONE(for_each2_0)
{
    COUNT_ARGS(3);
    /* NOP: REG2 = REG2; */
    /* NOP: REG1 = REG1; */
    /* NOP: REG0 = REG0; */
    CHECK_FUNCTION(REG0 /* proc */);
    if (PAIR_P(REG1 /* l1 */))
    {
	    REG3 = rb_to_bo(PAIR_P(REG2 /* l2 */));
	    JUMP(4,for_each2_1);
    }
    else
    {
	    REG3 = FALSE_OBJ;
	    JUMP(4,for_each2_1);
    }
}
#undef FPLACE_CODE

#define FPLACE_CODE (1000+1)
MONOTONE(for_each2_1)
{
    if (truish(REG3))
    {
	    REG3 = REG1 /* l1 */;
	    REG4 = REG2 /* l2 */;
	    JUMP(5,for_each2_2);
    }
    else
    {
	    RETURN0();
    }
}
#undef FPLACE_CODE

#define FPLACE_CODE (1000+2)
MONOTONE(for_each2_2)
{
    BEGIN_BIND2()
	    BIND_ARG(0,REG3);
	    BIND_ARG(1,REG4);
    END_BIND
    if (PAIR_P(checked_cdr(LEXREF0(0) /* l1 */)))
    {
	    REG3 = rb_to_bo(PAIR_P(checked_cdr(LEXREF0(1) /* l2 */)));
	    JUMP(4,for_each2_3);
    }
    else
    {
	    REG3 = FALSE_OBJ;
	    JUMP(4,for_each2_3);
    }
}
#undef FPLACE_CODE

#define FPLACE_CODE (1000+3)
MONOTONE(for_each2_3)
{
    if (truish(REG3))
    {
	    SAVE_CONT3(for_each2_4);
	    REG3 = checked_car(LEXREF0(0) /* l1 */);
	    REG4 = checked_car(LEXREF0(1) /* l2 */);
	    REG5 /* $fn */ = REG0 /* proc */;
	    REG0 = REG3;
	    REG1 = REG4;
	    APPLYF(2,REG5 /* $fn */);
    }
    else
    {
	    REG3 = checked_car(LEXREF0(0) /* l1 */);
	    REG4 = checked_car(LEXREF0(1) /* l2 */);
	    REG5 /* $fn */ = REG0 /* proc */;
	    REG0 = REG3;
	    REG1 = REG4;
	    APPLYF(2,REG5 /* $fn */);
    }
}
#undef FPLACE_CODE

#define FPLACE_CODE (1000+4)
MONOTONE(for_each2_4)
{
    RESTORE_CONT3();
    REG3 = checked_cdr(LEXREF0(0) /* l1 */);
    REG4 = checked_cdr(LEXREF0(1) /* l2 */);
    /* NOP: REG3 = REG3; */
    /* NOP: REG4 = REG4; */
    POPENVT();
    BJUMP(5,for_each2_2);
}
#undef FPLACE_CODE

EPILOGUE(for_each2)

BEGIN_BACK(for_each2)
  BACK_MONOTONE(for_each2_0)
  BACK_MONOTONE(for_each2_1)
  BACK_MONOTONE(for_each2_2)
  BACK_MONOTONE(for_each2_3)
  BACK_MONOTONE(for_each2_4)
END_BACK(for_each2)

static struct function_descr for_each2_descr = {
	&low_scheme_part_mapping,
	JUMP_TABLE( for_each2 ),
	rsfn_for_each2_name };
#undef FUNCTION


/**************************** Function `any?' ****************************/
static char rsfn_any_name[] = "any?";
#define FUNCTION rsfn_any_name

PROLOGUE(any)

BEGIN_FWD(any)
  FWD_MONOTONE(any_0)
  FWD_MONOTONE(any_1)
  FWD_MONOTONE(any_2)
END_FWD(any)

#define FPLACE_CODE (1000+0)
MONOTONE(any_0)
{
    COUNT_ARGS(2);
    /* NOP: REG1 = REG1; */
    /* NOP: REG0 = REG0; */
    CHECK_FUNCTION(REG0 /* predicate */);
    REG2 = REG1 /* list */;
    JUMP(3,any_1);
}
#undef FPLACE_CODE

#define FPLACE_CODE (1000+1)
MONOTONE(any_1)
{
    BEGIN_BIND1()
	    BIND_ARG(0,REG2);
    END_BIND
    if (PAIR_P(LEXREF0(0) /* x */))
    {
	    SAVE_CONT2(any_2);
	    REG2 = checked_car(LEXREF0(0) /* x */);
	    REG3 /* $fn */ = REG0 /* predicate */;
	    REG0 = REG2;
	    APPLYF(1,REG3 /* $fn */);
    }
    else
    {
	    REG0 = FALSE_OBJ;
	    RETURN1();
    }
}
#undef FPLACE_CODE

#define FPLACE_CODE (1000+2)
MONOTONE(any_2)
{
    REG2 = REG0;
    RESTORE_CONT2();
    if (truish(REG2))
    {
	    REG0 = TRUE_OBJ;
	    RETURN1();
    }
    else
    {
	    REG2 = checked_cdr(LEXREF0(0) /* x */);
	    /* NOP: REG2 = REG2; */
	    POPENVT();
	    BJUMP(3,any_1);
    }
}
#undef FPLACE_CODE

EPILOGUE(any)

BEGIN_BACK(any)
  BACK_MONOTONE(any_0)
  BACK_MONOTONE(any_1)
  BACK_MONOTONE(any_2)
END_BACK(any)

static struct function_descr any_descr = {
	&low_scheme_part_mapping,
	JUMP_TABLE( any ),
	rsfn_any_name };
#undef FUNCTION


/*************************** Function `every?' ***************************/
static char rsfn_every_name[] = "every?";
#define FUNCTION rsfn_every_name

PROLOGUE(every)

BEGIN_FWD(every)
  FWD_MONOTONE(every_0)
  FWD_MONOTONE(every_1)
  FWD_MONOTONE(every_2)
END_FWD(every)

#define FPLACE_CODE (1000+0)
MONOTONE(every_0)
{
    COUNT_ARGS(2);
    /* NOP: REG1 = REG1; */
    /* NOP: REG0 = REG0; */
    CHECK_FUNCTION(REG0 /* predicate */);
    REG2 = REG1 /* list */;
    JUMP(3,every_1);
}
#undef FPLACE_CODE

#define FPLACE_CODE (1000+1)
MONOTONE(every_1)
{
    BEGIN_BIND1()
	    BIND_ARG(0,REG2);
    END_BIND
    if (PAIR_P(LEXREF0(0) /* x */))
    {
	    SAVE_CONT2(every_2);
	    REG2 = checked_car(LEXREF0(0) /* x */);
	    REG3 /* $fn */ = REG0 /* predicate */;
	    REG0 = REG2;
	    APPLYF(1,REG3 /* $fn */);
    }
    else
    {
	    REG0 = TRUE_OBJ;
	    RETURN1();
    }
}
#undef FPLACE_CODE

#define FPLACE_CODE (1000+2)
MONOTONE(every_2)
{
    REG2 = REG0;
    RESTORE_CONT2();
    if (truish(REG2))
    {
	    REG2 = checked_cdr(LEXREF0(0) /* x */);
	    /* NOP: REG2 = REG2; */
	    POPENVT();
	    BJUMP(3,every_1);
    }
    else
    {
	    REG0 = FALSE_OBJ;
	    RETURN1();
    }
}
#undef FPLACE_CODE

EPILOGUE(every)

BEGIN_BACK(every)
  BACK_MONOTONE(every_0)
  BACK_MONOTONE(every_1)
  BACK_MONOTONE(every_2)
END_BACK(every)

static struct function_descr every_descr = {
	&low_scheme_part_mapping,
	JUMP_TABLE( every ),
	rsfn_every_name };
#undef FUNCTION


/********************** Function `dequeue-for-each' **********************/
static char rsfn_dequeue_for_each_name[] = "dequeue-for-each";
#define FUNCTION rsfn_dequeue_for_each_name

PROLOGUE(dequeue_for_each)

BEGIN_FWD(dequeue_for_each)
  FWD_MONOTONE(dequeue_for_each_0)
  FWD_MONOTONE(dequeue_for_each_1)
  FWD_MONOTONE(dequeue_for_each_2)
END_FWD(dequeue_for_each)

#define FPLACE_CODE (1000+0)
MONOTONE(dequeue_for_each_0)
{
    COUNT_ARGS(2);
    /* NOP: REG1 = REG1; */
    /* NOP: REG0 = REG0; */
    CHECK_FUNCTION(REG0 /* proc */);
    REG2 = dequeue_count(REG1 /* q */);
    REG3 = int2fx(0);
    JUMP(4,dequeue_for_each_1);
}
#undef FPLACE_CODE

#define FPLACE_CODE (1000+1)
MONOTONE(dequeue_for_each_1)
{
    BEGIN_BIND1()
	    BIND_ARG(0,REG3);
    END_BIND
    if (EQ(LEXREF0(0) /* i */,REG2 /* n */))
    {
	    RETURN0();
    }
    else
    {
	    REG3 = dequeue_ref(REG1 /* q */,LEXREF0(0) /* i */);
	    SAVE_CONT4(dequeue_for_each_2);
	    REG4 /* $fn */ = REG0 /* proc */;
	    REG0 = REG3 /* item */;
	    APPLYF(1,REG4 /* $fn */);
    }
}
#undef FPLACE_CODE

#define FPLACE_CODE (1000+2)
MONOTONE(dequeue_for_each_2)
{
    RESTORE_CONT4();
    REG4 = ADD1(LEXREF0(0) /* i */);
    POPENVT();
    REG3 = REG4;
    BJUMP(4,dequeue_for_each_1);
}
#undef FPLACE_CODE

EPILOGUE(dequeue_for_each)

BEGIN_BACK(dequeue_for_each)
  BACK_MONOTONE(dequeue_for_each_0)
  BACK_MONOTONE(dequeue_for_each_1)
  BACK_MONOTONE(dequeue_for_each_2)
END_BACK(dequeue_for_each)

static struct function_descr dequeue_for_each_descr = {
	&low_scheme_part_mapping,
	JUMP_TABLE( dequeue_for_each ),
	rsfn_dequeue_for_each_name };
#undef FUNCTION


/******************** Function `dequeue-state-as-list' ********************/
static char rsfn_dequeue_state_as_list_name[] = "dequeue-state-as-list";
#define FUNCTION rsfn_dequeue_state_as_list_name

PROLOGUE(dequeue_state_as_list)

BEGIN_FWD(dequeue_state_as_list)
  FWD_MONOTONE(dequeue_state_as_list_0)
  FWD_MONOTONE(dequeue_state_as_list_1)
END_FWD(dequeue_state_as_list)

#define FPLACE_CODE (1000+0)
MONOTONE(dequeue_state_as_list_0)
{
    COUNT_ARGS(1);
    /* NOP: REG0 = REG0; */
    REG1 = dequeue_count(REG0 /* q */);
    REG2 = REG1 /* n */;
    REG3 = NIL_OBJ;
    JUMP(4,dequeue_state_as_list_1);
}
#undef FPLACE_CODE

#define FPLACE_CODE (1000+1)
MONOTONE(dequeue_state_as_list_1)
{
    /* NOP: REG3 = REG3; */
    /* NOP: REG2 = REG2; */
    if (EQ(REG2 /* i */,int2fx(0)))
    {
	    REG0 = REG3 /* r */;
	    RETURN1();
    }
    else
    {
	    REG4 = SUB1(REG2 /* i */);
	    REG5 = cons(dequeue_ref(REG0 /* q */,SUB1(REG2 /* i */)),REG3 /* r */);
	    /* NOP: REG4 = REG4; */
	    /* NOP: REG5 = REG5; */
	    REG2 = REG4;
	    REG3 = REG5;
	    BJUMP(4,dequeue_state_as_list_1);
    }
}
#undef FPLACE_CODE

EPILOGUE(dequeue_state_as_list)

BEGIN_BACK(dequeue_state_as_list)
  BACK_MONOTONE(dequeue_state_as_list_0)
  BACK_MONOTONE(dequeue_state_as_list_1)
END_BACK(dequeue_state_as_list)

static struct function_descr dequeue_state_as_list_descr = {
	&low_scheme_part_mapping,
	JUMP_TABLE( dequeue_state_as_list ),
	rsfn_dequeue_state_as_list_name };
#undef FUNCTION

/******************************* Postamble *******************************/
/**************************** Part Link Table ****************************/


static struct function_descr *(part_mapping_tab[]) = {
    &map1_descr,
    &anon_descr,
    &map2_descr,
    &for_each1_descr,
    &for_each2_descr,
    &any_descr,
    &every_descr,
    &dequeue_for_each_descr,
    &dequeue_state_as_list_descr,
    NULL };
struct part_descr low_scheme_part_mapping = {
    118447107,
    &module_low_scheme,
    part_mapping_tab,
    "mapping",
    0, sccsid };
#undef _MODULE_LOW_SCHEME
#undef _SCM_MAPPING
#undef _C_MAPPING
