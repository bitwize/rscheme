/**********************************************
THIS FILE WAS AUTOMATICALLY GENERATED, AND MAY
BE AUTOMATICALLY RE-GENERATED WHEN THE COMPILER
OR SOURCE CHANGES.  DO NOT MODIFY THIS FILE BY HAND!
RScheme Build (v0.7.3.4-b7u, 2007-05-30)
**********************************************/

/******************************** Preamble ********************************/

#define _MODULE_OBJSYS
#define _SCM_MAKEINST
#define _C_MAKEINST
#include "objsys_p.h"
#include <rscheme/vinsns.h>
extern struct module_descr module_objsys;
extern struct part_descr objsys_part_makeinst;
static char sccsid[] = "@(#)objsys modules/objsys/makeinst.scm [396967937] (RS v0.7.3.4-b7u, 2007-05-30)";

/************************** Function Definitions **************************/


/********************** Function `<slot-descriptor>' **********************/
static char rsfn_slot_descriptor__default_slot_value_name[] = "<slot-descriptor>";
#define FUNCTION rsfn_slot_descriptor__default_slot_value_name

PROLOGUE(slot_descriptor__default_slot_value)

BEGIN_FWD(slot_descriptor__default_slot_value)
  FWD_MONOTONE(slot_descriptor__default_slot_value_0)
  FWD_MONOTONE(slot_descriptor__default_slot_value_1)
  FWD_MONOTONE(slot_descriptor__default_slot_value_2)
END_FWD(slot_descriptor__default_slot_value)

#define FPLACE_CODE (1000+0)
MONOTONE(slot_descriptor__default_slot_value_0)
{
    COUNT_ARGS(3);
    /* NOP: REG2 = REG2; */
    /* NOP: REG1 = REG1; */
    /* NOP: REG0 = REG0; */
    CHECK_VECTOR(REG2 /* inits */);
    REG3 = gvec_ref(REG0 /* self */,SLOT(1));
    /* NOP: REG3 = REG3; */
    if (EQ(REG3 /* temp */,LITERAL(0) /* optional */))
    {
	    REG4 = TRUE_OBJ;
	    JUMP(5,slot_descriptor__default_slot_value_1);
    }
    else
    {
	    REG4 = rb_to_bo(EQ(REG3 /* temp */,LITERAL(1) /* prohibited */));
	    JUMP(5,slot_descriptor__default_slot_value_1);
    }
}
#undef FPLACE_CODE

#define FPLACE_CODE (1000+1)
MONOTONE(slot_descriptor__default_slot_value_1)
{
    if (truish(REG4))
    {
	    REG0 = gvec_ref(REG0 /* self */,SLOT(2));
	    RETURN1();
    }
    else
    {
	    if (EQ(REG3 /* temp */,LITERAL(2) /* function */))
	    {
	    REG4 = gvec_ref(REG0 /* self */,SLOT(2));
	    REG0 = REG4;
	    APPLY(0,REG0 /* fn */);
	    }
	    else
	    {
	    if (EQ(REG3 /* temp */,LITERAL(3) /* required */))
	    {
		    REG4 = make3(TLREFB(5) /* <initializer-missing> */,NIL_OBJ,object_class(REG1 /* target */),REG0 /* self */);
		    /* NOP: REG4 = REG4; */
		    SAVE_CONT5(slot_descriptor__default_slot_value_2);
		    REG0 = REG4 /* new-instance */;
		    APPLYG(1,TLREFB(4) /* initialize */);
	    }
	    else
	    {
		    RETURN0();
	    }
	    }
    }
}
#undef FPLACE_CODE

#define FPLACE_CODE (1000+2)
MONOTONE(slot_descriptor__default_slot_value_2)
{
    RESTORE_CONT5();
    /* NOP: REG4 = REG4; */
    /* NOP: REG4 = REG4; */
    REG0 = REG4;
    APPLYF(1,TLREFB(6) /* signal */);
}
#undef FPLACE_CODE

EPILOGUE(slot_descriptor__default_slot_value)

BEGIN_BACK(slot_descriptor__default_slot_value)
  BACK_MONOTONE(slot_descriptor__default_slot_value_0)
  BACK_MONOTONE(slot_descriptor__default_slot_value_1)
  BACK_MONOTONE(slot_descriptor__default_slot_value_2)
END_BACK(slot_descriptor__default_slot_value)

static struct function_descr slot_descriptor__default_slot_value_descr = {
	&objsys_part_makeinst,
	JUMP_TABLE( slot_descriptor__default_slot_value ),
	rsfn_slot_descriptor__default_slot_value_name };
#undef FUNCTION


/******************** Function `(0 initialize-slot!)' ********************/
static char rsfn_initialize_slot_name[] = "(0 initialize-slot!)";
#define FUNCTION rsfn_initialize_slot_name

PROLOGUE(initialize_slot)

BEGIN_FWD(initialize_slot)
  FWD_MONOTONE(initialize_slot_0)
  FWD_MONOTONE(initialize_slot_1)
END_FWD(initialize_slot)

#define FPLACE_CODE (1000+0)
MONOTONE(initialize_slot_0)
{
    USE_FUNCTION_ENVT();
    COUNT_ARGS(1);
    /* NOP: REG0 = REG0; */
    REG1 = REG0 /* val */;
    if (instance_p(REG1,CHECK_CLASS(gvec_ref(LEXREF0(0) /* self */,SLOT(3)))))
    {
	    REG1 = CHECK_GVEC(LEXREF0(1) /* target */);
	    gvec_set(REG1,FXWORDS_TO_RIBYTES(gvec_ref(LEXREF0(0) /* self */,SLOT(4))),REG0 /* val */);
	    RETURN0();
    }
    else
    {
	    REG1 = make4(TLREFB(1) /* <initializer-type-error> */,NIL_OBJ,object_class(LEXREF0(1) /* target */),LEXREF0(0) /* self */,REG0 /* val */);
	    /* NOP: REG1 = REG1; */
	    SAVE_CONT2(initialize_slot_1);
	    REG0 = REG1 /* new-instance */;
	    APPLYG(1,TLREFB(0) /* initialize */);
    }
}
#undef FPLACE_CODE

#define FPLACE_CODE (1000+1)
MONOTONE(initialize_slot_1)
{
    RESTORE_CONT2();
    /* NOP: REG1 = REG1; */
    /* NOP: REG1 = REG1; */
    REG0 = REG1;
    APPLYF(1,TLREFB(2) /* signal */);
}
#undef FPLACE_CODE

EPILOGUE(initialize_slot)

BEGIN_BACK(initialize_slot)
  BACK_MONOTONE(initialize_slot_0)
  BACK_MONOTONE(initialize_slot_1)
END_BACK(initialize_slot)

static struct function_descr initialize_slot_descr = {
	&objsys_part_makeinst,
	JUMP_TABLE( initialize_slot ),
	rsfn_initialize_slot_name };
#undef FUNCTION


/******************** Function `(1 initialize-slot!)' ********************/
static char rsfn_initialize_slot1_name[] = "(1 initialize-slot!)";
#define FUNCTION rsfn_initialize_slot1_name

PROLOGUE(initialize_slot1)

BEGIN_FWD(initialize_slot1)
  FWD_MONOTONE(initialize_slot1_0)
  FWD_MONOTONE(initialize_slot1_1)
END_FWD(initialize_slot1)

#define FPLACE_CODE (1000+0)
MONOTONE(initialize_slot1_0)
{
    USE_FUNCTION_ENVT();
    COUNT_ARGS(0);
    REG0 = CHECK_GVEC(LEXREF0(1) /* target */);
    REG1 = gvec_ref(LEXREF0(0) /* self */,SLOT(4));
    SAVE_CONT2(initialize_slot1_1);
    REG0 = LEXREF0(0) /* self */;
    REG1 = LEXREF0(1) /* target */;
    REG2 = LEXREF0(2) /* inits */;
    APPLYG(3,TLREFB(0) /* default-slot-value */);
}
#undef FPLACE_CODE

#define FPLACE_CODE (1000+1)
MONOTONE(initialize_slot1_1)
{
    REG2 = REG0;
    RESTORE_CONT2();
    gvec_set(REG0,FXWORDS_TO_RIBYTES(REG1),REG2);
    RETURN0();
}
#undef FPLACE_CODE

EPILOGUE(initialize_slot1)

BEGIN_BACK(initialize_slot1)
  BACK_MONOTONE(initialize_slot1_0)
  BACK_MONOTONE(initialize_slot1_1)
END_BACK(initialize_slot1)

static struct function_descr initialize_slot1_descr = {
	&objsys_part_makeinst,
	JUMP_TABLE( initialize_slot1 ),
	rsfn_initialize_slot1_name };
#undef FUNCTION


/********************** Function `<slot-descriptor>' **********************/
static char rsfn_slot_descriptor__initialize_slot_name[] = "<slot-descriptor>";
#define FUNCTION rsfn_slot_descriptor__initialize_slot_name

PROLOGUE(slot_descriptor__initialize_slot)

BEGIN_FWD(slot_descriptor__initialize_slot)
  FWD_MONOTONE(slot_descriptor__initialize_slot_0)
  FWD_MONOTONE(slot_descriptor__initialize_slot_1)
END_FWD(slot_descriptor__initialize_slot)

#define FPLACE_CODE (1000+0)
MONOTONE(slot_descriptor__initialize_slot_0)
{
    COUNT_ARGS(3);
    BEGIN_BIND3()
	    BIND_ARG(0,REG0);
	    BIND_ARG(1,REG1);
	    BIND_ARG(2,REG2);
    END_BIND
    CHECK_VECTOR(LEXREF0(2) /* inits */);
    if (truish(gvec_ref(LEXREF0(0) /* self */,SLOT(5))))
    {
	    REG0 = gvec_ref(LEXREF0(0) /* self */,SLOT(5));
	    REG1 = LEXREF0(2) /* inits */;
	    REG2 = CLOSURE(0);
	    REG3 = CLOSURE(1);
	    /* NOP: REG0 = REG0; */
	    /* NOP: REG1 = REG1; */
	    /* NOP: REG2 = REG2; */
	    /* NOP: REG3 = REG3; */
	    APPLYF(4,TLREFB(2) /* using-keyword-value */);
    }
    else
    {
	    REG0 = CHECK_GVEC(LEXREF0(1) /* target */);
	    REG1 = gvec_ref(LEXREF0(0) /* self */,SLOT(4));
	    SAVE_CONT2(slot_descriptor__initialize_slot_1);
	    REG0 = LEXREF0(0) /* self */;
	    REG1 = LEXREF0(1) /* target */;
	    REG2 = LEXREF0(2) /* inits */;
	    APPLYG(3,TLREFB(3) /* default-slot-value */);
    }
}
#undef FPLACE_CODE

#define FPLACE_CODE (1000+1)
MONOTONE(slot_descriptor__initialize_slot_1)
{
    REG2 = REG0;
    RESTORE_CONT2();
    gvec_set(REG0,FXWORDS_TO_RIBYTES(REG1),REG2);
    RETURN0();
}
#undef FPLACE_CODE

EPILOGUE(slot_descriptor__initialize_slot)

BEGIN_BACK(slot_descriptor__initialize_slot)
  BACK_MONOTONE(slot_descriptor__initialize_slot_0)
  BACK_MONOTONE(slot_descriptor__initialize_slot_1)
END_BACK(slot_descriptor__initialize_slot)

static struct function_descr slot_descriptor__initialize_slot_descr = {
	&objsys_part_makeinst,
	JUMP_TABLE( slot_descriptor__initialize_slot ),
	rsfn_slot_descriptor__initialize_slot_name };
#undef FUNCTION


/************************ Function `make-instance' ************************/
static char rsfn_make_instance_name[] = "make-instance";
#define FUNCTION rsfn_make_instance_name

PROLOGUE(make_instance)

BEGIN_FWD(make_instance)
  FWD_MONOTONE(make_instance_0)
  FWD_MONOTONE(make_instance_1)
  FWD_MONOTONE(make_instance_2)
  FWD_MONOTONE(make_instance_3)
  FWD_MONOTONE(make_instance_4)
  FWD_MONOTONE(make_instance_5)
  FWD_MONOTONE(make_instance_6)
  FWD_MONOTONE(make_instance_7)
  FWD_MONOTONE(make_instance_8)
  FWD_MONOTONE(make_instance_9)
  FWD_MONOTONE(make_instance_10)
  FWD_MONOTONE(make_instance_11)
  FWD_MONOTONE(make_instance_12)
END_FWD(make_instance)

#define FPLACE_CODE (1000+0)
MONOTONE(make_instance_0)
{
    COUNT_ARGS_AT_LEAST(1);
    COLLECT1();
    /* NOP: REG1 = REG1; */
    /* NOP: REG0 = REG0; */
    CHECK_CLASS(REG0 /* class */);
    REG2 = rb_to_bo(EQ(gvec_ref(REG0 /* class */,SLOT(1)),int2fx(0)));
    /* NOP: REG2 = REG2; */
    if (truish(REG2 /* temp */))
    {
	    REG3 = REG2 /* temp */;
	    JUMP(4,make_instance_1);
    }
    else
    {
	    REG3 = rb_to_bo(EQ(gvec_ref(REG0 /* class */,SLOT(1)),int2fx(4)));
	    JUMP(4,make_instance_1);
    }
}
#undef FPLACE_CODE

#define FPLACE_CODE (1000+1)
MONOTONE(make_instance_1)
{
    REG2 = REG3;
    if (NOT(REG2))
    {
	    if (EQ(gvec_ref(REG0 /* class */,SLOT(1)),int2fx(3)))
	    {
	    SAVE_CONT2(make_instance_2);
	    REG1 = REG0 /* class */;
	    REG0 = LITERAL(0) /* "class ~s is abstract; instantiation is not permitted" */;
	    APPLYF(2,TLREFB(1) /* error */);
	    }
	    else
	    {
	    SAVE_CONT2(make_instance_3);
	    REG2 = REG0 /* class */;
	    REG3 = gvec_ref(REG0 /* class */,SLOT(1));
	    REG0 = LITERAL(2) /* "cannot instantiate class ~s; heap type = ~d" */;
	    REG1 = REG2;
	    REG2 = REG3;
	    APPLYF(3,TLREFB(1) /* error */);
	    }
    }
    else
    {
	    JUMP(2,make_instance_5);
    }
}
#undef FPLACE_CODE

#define FPLACE_CODE (1000+2)
MONOTONE(make_instance_2)
{
    RESTORE_CONT2();
    JUMP(2,make_instance_4);
}
#undef FPLACE_CODE

#define FPLACE_CODE (1000+3)
MONOTONE(make_instance_3)
{
    RESTORE_CONT2();
    JUMP(2,make_instance_4);
}
#undef FPLACE_CODE

#define FPLACE_CODE (1000+4)
MONOTONE(make_instance_4)
{
    JUMP(2,make_instance_5);
}
#undef FPLACE_CODE

#define FPLACE_CODE (1000+5)
MONOTONE(make_instance_5)
{
    SAVE_CONT2(make_instance_6);
    /* NOP: REG0 = REG0; */
    APPLYF(1,TLREFB(9) /* class-compute-slots */);
}
#undef FPLACE_CODE

#define FPLACE_CODE (1000+6)
MONOTONE(make_instance_6)
{
    REG2 = REG0;
    RESTORE_CONT2();
    /* NOP: REG2 = REG2; */
    /* NOP: REG2 = REG2; */
    SAVE_CONT3(make_instance_7);
    REG0 = REG1 /* inits */;
    APPLYF(1,TLREFB(8) /* keyword-value-list->vector */);
}
#undef FPLACE_CODE

#define FPLACE_CODE (1000+7)
MONOTONE(make_instance_7)
{
    REG3 = REG0;
    RESTORE_CONT3();
    /* NOP: REG3 = REG3; */
    /* NOP: REG3 = REG3; */
    CHECK_VECTOR(REG3 /* v */);
    /* NOP: REG3 = REG3; */
    SAVE_CONT4(make_instance_8);
    /* NOP: REG0 = REG0; */
    REG1 = REG3 /* v */;
    APPLYF(2,TLREFB(6) /* get-allocation-area */);
}
#undef FPLACE_CODE

#define FPLACE_CODE (1000+8)
MONOTONE(make_instance_8)
{
    REG4 = REG0;
    RESTORE_CONT4();
    /* NOP: REG4 = REG4; */
    REG5 = REG0 /* class */;
    SAVE_CONT6(make_instance_9);
    /* NOP: REG0 = REG0; */
    APPLYF(1,TLREFB(7) /* class-instance-size */);
}
#undef FPLACE_CODE

#define FPLACE_CODE (1000+9)
MONOTONE(make_instance_9)
{
    REG6 = REG0;
    RESTORE_CONT6();
    REG4 = make_gvec_in_area(REG4,REG5,FXWORDS_TO_RIBYTES(CHECK_FIXNUM(REG6)),FALSE_OBJ);
    /* NOP: REG4 = REG4; */
    REG5 = REG2 /* slots */;
    JUMP(6,make_instance_10);
}
#undef FPLACE_CODE

#define FPLACE_CODE (1000+10)
MONOTONE(make_instance_10)
{
    BEGIN_BIND1()
	    BIND_ARG(0,REG5);
    END_BIND
    if (NULL_P(LEXREF0(0) /* slots */))
    {
	    REG5 = REG4 /* instance */;
	    SAVE_CONT6(make_instance_11);
	    REG0 = REG3 /* v */;
	    APPLYF(1,TLREFB(3) /* remainder->list */);
    }
    else
    {
	    SAVE_CONT5(make_instance_12);
	    REG5 = checked_car(LEXREF0(0) /* slots */);
	    REG0 = REG5;
	    REG1 = REG4 /* instance */;
	    REG2 = REG3 /* v */;
	    APPLYG(3,TLREFB(5) /* initialize-slot! */);
    }
}
#undef FPLACE_CODE

#define FPLACE_CODE (1000+11)
MONOTONE(make_instance_11)
{
    REG6 = REG0;
    RESTORE_CONT6();
    /* NOP: REG6 = REG6; */
    REG0 = REG5;
    REG1 = REG6;
    APPLY(2,TLREFB(4) /* finish-initialization */);
}
#undef FPLACE_CODE

#define FPLACE_CODE (1000+12)
MONOTONE(make_instance_12)
{
    RESTORE_CONT5();
    REG5 = checked_cdr(LEXREF0(0) /* slots */);
    /* NOP: REG5 = REG5; */
    POPENVT();
    BJUMP(6,make_instance_10);
}
#undef FPLACE_CODE

EPILOGUE(make_instance)

BEGIN_BACK(make_instance)
  BACK_MONOTONE(make_instance_0)
  BACK_MONOTONE(make_instance_1)
  BACK_MONOTONE(make_instance_2)
  BACK_MONOTONE(make_instance_3)
  BACK_MONOTONE(make_instance_4)
  BACK_MONOTONE(make_instance_5)
  BACK_MONOTONE(make_instance_6)
  BACK_MONOTONE(make_instance_7)
  BACK_MONOTONE(make_instance_8)
  BACK_MONOTONE(make_instance_9)
  BACK_MONOTONE(make_instance_10)
  BACK_MONOTONE(make_instance_11)
  BACK_MONOTONE(make_instance_12)
END_BACK(make_instance)

static struct function_descr make_instance_descr = {
	&objsys_part_makeinst,
	JUMP_TABLE( make_instance ),
	rsfn_make_instance_name };
#undef FUNCTION

/******************************* Postamble *******************************/
/**************************** Part Link Table ****************************/


static struct function_descr *(part_makeinst_tab[]) = {
    &slot_descriptor__default_slot_value_descr,
    &initialize_slot_descr,
    &initialize_slot1_descr,
    &slot_descriptor__initialize_slot_descr,
    &make_instance_descr,
    NULL };
struct part_descr objsys_part_makeinst = {
    396967937,
    &module_objsys,
    part_makeinst_tab,
    "makeinst",
    0, sccsid };
#undef _MODULE_OBJSYS
#undef _SCM_MAKEINST
#undef _C_MAKEINST
