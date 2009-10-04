/**********************************************
THIS FILE WAS AUTOMATICALLY GENERATED, AND MAY
BE AUTOMATICALLY RE-GENERATED WHEN THE COMPILER
OR SOURCE CHANGES.  DO NOT MODIFY THIS FILE BY HAND!
RScheme Build (v0.7.3.4-b7u, 2007-05-30)
**********************************************/

/******************************** Preamble ********************************/

#define _MODULE_OBJSYS
#define _SCM_GENERICF
#define _C_GENERICF
#include "objsys_p.h"
#include <rscheme/vinsns.h>
extern struct module_descr module_objsys;
extern struct part_descr objsys_part_genericf;
static char sccsid[] = "@(#)objsys modules/objsys/genericf.scm [72769536] (RS v0.7.3.4-b7u, 2007-05-30)";

/************************** Function Definitions **************************/


/****************** Raw glue `generic-function-dispatch' ******************/

static char rsfn_generic_function_dispatch_name[] = "generic-function-dispatch";
#define FUNCTION rsfn_generic_function_dispatch_name

PROLOGUE(generic_function_dispatch)

BEGIN_FWD(generic_function_dispatch)
  FWD_MONOTONE(generic_function_dispatch_0)
END_FWD(generic_function_dispatch)

#define FPLACE_CODE (1000+0)
MONOTONE(generic_function_dispatch_0)
{
{
  return rs_gf_dispatch( envt_reg );
}}
#undef FPLACE_CODE

EPILOGUE(generic_function_dispatch)

BEGIN_BACK(generic_function_dispatch)
  BACK_MONOTONE(generic_function_dispatch_0)
END_BACK(generic_function_dispatch)

static struct function_descr generic_function_dispatch_descr = {
	&objsys_part_genericf,
	JUMP_TABLE( generic_function_dispatch ),
	rsfn_generic_function_dispatch_name };
#undef FUNCTION


/******************** Raw glue `get-gf-lru-histogram' ********************/
#define resetq REG0

static char rsfn_get_gf_lru_histogram_name[] = "get-gf-lru-histogram";
#define FUNCTION rsfn_get_gf_lru_histogram_name

PROLOGUE(get_gf_lru_histogram)

BEGIN_FWD(get_gf_lru_histogram)
  FWD_MONOTONE(get_gf_lru_histogram_0)
END_FWD(get_gf_lru_histogram)

#define FPLACE_CODE (1000+0)
MONOTONE(get_gf_lru_histogram_0)
{  COUNT_ARGS(1);

{
  REG0 = collect_gf_cache_histogram( truish(resetq) );
  RETURN1();
}}
#undef FPLACE_CODE

EPILOGUE(get_gf_lru_histogram)

BEGIN_BACK(get_gf_lru_histogram)
  BACK_MONOTONE(get_gf_lru_histogram_0)
END_BACK(get_gf_lru_histogram)

static struct function_descr get_gf_lru_histogram_descr = {
	&objsys_part_genericf,
	JUMP_TABLE( get_gf_lru_histogram ),
	rsfn_get_gf_lru_histogram_name };
#undef FUNCTION

#undef resetq

/******************** Raw glue `pre-compute-dispatch' ********************/
#define raw_gf REG0
#define rcvr REG1

static char rsfn_pre_compute_dispatch_name[] = "pre-compute-dispatch";
#define FUNCTION rsfn_pre_compute_dispatch_name

PROLOGUE(pre_compute_dispatch)

BEGIN_FWD(pre_compute_dispatch)
  FWD_MONOTONE(pre_compute_dispatch_0)
END_FWD(pre_compute_dispatch)

#define FPLACE_CODE (1000+0)
MONOTONE(pre_compute_dispatch_0)
{  obj gf;
  COUNT_ARGS_AT_LEAST(2);
  if (!instance_p(raw_gf,TLREFB(1)))
    {
      obj c;
      c = make5( TLREFB(2),
                 NIL_OBJ,  /* properties */
                 lookup_symbol( FUNCTION ),
                 cons( raw_gf, NIL_OBJ ),
                 lookup_symbol( "gf" ),
                 TLREFB(1) );
      raise_error( c );
    }
  gf = raw_gf;


{
  obj m;

  m = rs_gf_find_method( gf, rcvr );
  if (EQ(m,FALSE_OBJ))
    {
      COLLECT1();
      APPLYF( 2, TLREF(0) );
    }
  else
    {
      REG0 = m;
      RETURN1();
    }
}}
#undef FPLACE_CODE

EPILOGUE(pre_compute_dispatch)

BEGIN_BACK(pre_compute_dispatch)
  BACK_MONOTONE(pre_compute_dispatch_0)
END_BACK(pre_compute_dispatch)

static struct function_descr pre_compute_dispatch_descr = {
	&objsys_part_genericf,
	JUMP_TABLE( pre_compute_dispatch ),
	rsfn_pre_compute_dispatch_name };
#undef FUNCTION

#undef raw_gf
#undef rcvr

/******************** Function `find-method-by-class' ********************/
static char rsfn_find_method_by_class_name[] = "find-method-by-class";
#define FUNCTION rsfn_find_method_by_class_name

PROLOGUE(find_method_by_class)

BEGIN_FWD(find_method_by_class)
  FWD_MONOTONE(find_method_by_class_0)
  FWD_MONOTONE(find_method_by_class_1)
END_FWD(find_method_by_class)

#define FPLACE_CODE (1000+0)
MONOTONE(find_method_by_class_0)
{
    COUNT_ARGS(2);
    /* NOP: REG1 = REG1; */
    /* NOP: REG0 = REG0; */
    CHECK_INSTANCE(REG0 /* gf */,LITERAL(0) /* #[<patch> @0867_30a3] */);
    REG2 = gvec_ref(REG0 /* gf */,SLOT(1));
    /* NOP: REG2 = REG2; */
    JUMP(3,find_method_by_class_1);
}
#undef FPLACE_CODE

#define FPLACE_CODE (1000+1)
MONOTONE(find_method_by_class_1)
{
    /* NOP: REG2 = REG2; */
    if (PAIR_P(REG2 /* i */))
    {
	    REG3 = CHECK_CLASS(REG1 /* class */);
	    if (subclass_p(REG3,CHECK_CLASS(checked_car(gvec_ref(CHECK_GVEC(checked_car(REG2 /* i */)),SLOT(2))))))
	    {
	    REG0 = checked_car(REG2 /* i */);
	    RETURN1();
	    }
	    else
	    {
	    REG3 = checked_cdr(REG2 /* i */);
	    /* NOP: REG3 = REG3; */
	    REG2 = REG3;
	    BJUMP(3,find_method_by_class_1);
	    }
    }
    else
    {
	    REG0 = FALSE_OBJ;
	    RETURN1();
    }
}
#undef FPLACE_CODE

EPILOGUE(find_method_by_class)

BEGIN_BACK(find_method_by_class)
  BACK_MONOTONE(find_method_by_class_0)
  BACK_MONOTONE(find_method_by_class_1)
END_BACK(find_method_by_class)

static struct function_descr find_method_by_class_descr = {
	&objsys_part_genericf,
	JUMP_TABLE( find_method_by_class ),
	rsfn_find_method_by_class_name };
#undef FUNCTION


/************************* Function `find-method' *************************/
static char rsfn_find_method_name[] = "find-method";
#define FUNCTION rsfn_find_method_name

PROLOGUE(find_method)

BEGIN_FWD(find_method)
  FWD_MONOTONE(find_method_0)
END_FWD(find_method)

#define FPLACE_CODE (1000+0)
MONOTONE(find_method_0)
{
    COUNT_ARGS(2);
    /* NOP: REG1 = REG1; */
    /* NOP: REG0 = REG0; */
    CHECK_INSTANCE(REG0 /* gf */,LITERAL(0) /* #[<patch> @0867_30a3] */);
    CHECK_PAIR(REG1 /* args */);
    REG2 = REG0 /* gf */;
    REG3 = object_class(checked_car(REG1 /* args */));
    REG0 = REG2;
    REG1 = REG3;
    APPLYF(2,TLREFB(1) /* find-method-by-class */);
}
#undef FPLACE_CODE

EPILOGUE(find_method)

BEGIN_BACK(find_method)
  BACK_MONOTONE(find_method_0)
END_BACK(find_method)

static struct function_descr find_method_descr = {
	&objsys_part_genericf,
	JUMP_TABLE( find_method ),
	rsfn_find_method_name };
#undef FUNCTION


/************************* Function `load-cache' *************************/
static char rsfn_load_cache_name[] = "load-cache";
#define FUNCTION rsfn_load_cache_name

PROLOGUE(load_cache)

BEGIN_FWD(load_cache)
  FWD_MONOTONE(load_cache_0)
  FWD_MONOTONE(load_cache_1)
END_FWD(load_cache)

#define FPLACE_CODE (1000+0)
MONOTONE(load_cache_0)
{
    COUNT_ARGS(2);
    /* NOP: REG1 = REG1; */
    /* NOP: REG0 = REG0; */
    CHECK_INSTANCE(REG0 /* gf */,LITERAL(0) /* #[<patch> @0867_30a3] */);
    CHECK_PAIR(REG1 /* args */);
    SAVE_CONT2(load_cache_1);
    REG2 = REG0 /* gf */;
    REG3 = object_class(checked_car(REG1 /* args */));
    REG0 = REG2;
    REG1 = REG3;
    APPLYF(2,TLREFB(3) /* find-method-by-class */);
}
#undef FPLACE_CODE

#define FPLACE_CODE (1000+1)
MONOTONE(load_cache_1)
{
    REG2 = REG0;
    RESTORE_CONT2();
    /* NOP: REG2 = REG2; */
    /* NOP: REG2 = REG2; */
    if (truish(REG2 /* m */))
    {
	    REG3 = object_class(checked_car(REG1 /* args */));
	    /* NOP: REG3 = REG3; */
	    REG4 = FX_ADD(int2fx(4),CHECK_FIXNUM(basic_bitwise_and(gvec_ref(REG3 /* c */,SLOT(5)),int2fx(6))));
	    /* NOP: REG4 = REG4; */
	    if (truish(gvec_ref(REG0 /* gf */,FXWORDS_TO_RIBYTES(REG4 /* ix */))))
	    {
	    if (truish(gvec_ref(REG0 /* gf */,SLOT(12))))
	    {
		    REG5 = REG0 /* gf */;
		    REG6 = TLREFB(1) /* <vector> */;
		    REG7 = gvec_ref(REG0 /* gf */,SLOT(14));
		    REG8 = gvec_ref(REG0 /* gf */,SLOT(12));
		    gvec_set(REG5,SLOT(14),make3(REG6,REG7,REG8,gvec_ref(REG0 /* gf */,SLOT(13))));
		    gvec_set(REG0 /* gf */,SLOT(13),gvec_ref(REG0 /* gf */,FXWORDS_TO_RIBYTES(ADD1(REG4 /* ix */))));
		    gvec_set(REG0 /* gf */,SLOT(12),gvec_ref(REG0 /* gf */,FXWORDS_TO_RIBYTES(REG4 /* ix */)));
		    gvec_set(REG0 /* gf */,FXWORDS_TO_RIBYTES(REG4 /* ix */),REG3 /* c */);
		    gvec_set(REG0 /* gf */,FXWORDS_TO_RIBYTES(ADD1(REG4 /* ix */)),REG2 /* m */);
		    REG0 = REG2 /* m */;
		    RETURN1();
	    }
	    else
	    {
		    gvec_set(REG0 /* gf */,SLOT(13),gvec_ref(REG0 /* gf */,FXWORDS_TO_RIBYTES(ADD1(REG4 /* ix */))));
		    gvec_set(REG0 /* gf */,SLOT(12),gvec_ref(REG0 /* gf */,FXWORDS_TO_RIBYTES(REG4 /* ix */)));
		    gvec_set(REG0 /* gf */,FXWORDS_TO_RIBYTES(REG4 /* ix */),REG3 /* c */);
		    gvec_set(REG0 /* gf */,FXWORDS_TO_RIBYTES(ADD1(REG4 /* ix */)),REG2 /* m */);
		    REG0 = REG2 /* m */;
		    RETURN1();
	    }
	    }
	    else
	    {
	    gvec_set(REG0 /* gf */,FXWORDS_TO_RIBYTES(REG4 /* ix */),REG3 /* c */);
	    gvec_set(REG0 /* gf */,FXWORDS_TO_RIBYTES(ADD1(REG4 /* ix */)),REG2 /* m */);
	    REG0 = REG2 /* m */;
	    RETURN1();
	    }
    }
    else
    {
	    /* NOP: REG0 = REG0; */
	    /* NOP: REG1 = REG1; */
	    APPLYG(2,TLREFB(2) /* does-not-understand */);
    }
}
#undef FPLACE_CODE

EPILOGUE(load_cache)

BEGIN_BACK(load_cache)
  BACK_MONOTONE(load_cache_0)
  BACK_MONOTONE(load_cache_1)
END_BACK(load_cache)

static struct function_descr load_cache_descr = {
	&objsys_part_genericf,
	JUMP_TABLE( load_cache ),
	rsfn_load_cache_name };
#undef FUNCTION


/********************* Function `load-cache-and-call' *********************/
static char rsfn_load_cache_and_call_name[] = "load-cache-and-call";
#define FUNCTION rsfn_load_cache_and_call_name

PROLOGUE(load_cache_and_call)

BEGIN_FWD(load_cache_and_call)
  FWD_MONOTONE(load_cache_and_call_0)
  FWD_MONOTONE(load_cache_and_call_1)
END_FWD(load_cache_and_call)

#define FPLACE_CODE (1000+0)
MONOTONE(load_cache_and_call_0)
{
    COUNT_ARGS(2);
    /* NOP: REG1 = REG1; */
    /* NOP: REG0 = REG0; */
    CHECK_INSTANCE(REG0 /* gf */,LITERAL(0) /* #[<patch> @0867_30a3] */);
    CHECK_PAIR(REG1 /* args */);
    REG2 = REG0 /* gf */;
    gvec_set(REG2,SLOT(15),ADD1(gvec_ref(REG0 /* gf */,SLOT(15))));
    SAVE_CONT2(load_cache_and_call_1);
    REG2 = REG0 /* gf */;
    REG3 = object_class(checked_car(REG1 /* args */));
    REG0 = REG2;
    REG1 = REG3;
    APPLYF(2,TLREFB(4) /* find-method-by-class */);
}
#undef FPLACE_CODE

#define FPLACE_CODE (1000+1)
MONOTONE(load_cache_and_call_1)
{
    REG2 = REG0;
    RESTORE_CONT2();
    /* NOP: REG2 = REG2; */
    /* NOP: REG2 = REG2; */
    if (truish(REG2 /* m */))
    {
	    REG3 = object_class(checked_car(REG1 /* args */));
	    /* NOP: REG3 = REG3; */
	    REG4 = FX_ADD(int2fx(4),CHECK_FIXNUM(basic_bitwise_and(gvec_ref(REG3 /* c */,SLOT(5)),int2fx(6))));
	    /* NOP: REG4 = REG4; */
	    if (truish(gvec_ref(REG0 /* gf */,FXWORDS_TO_RIBYTES(REG4 /* ix */))))
	    {
	    if (truish(gvec_ref(REG0 /* gf */,SLOT(12))))
	    {
		    REG5 = REG0 /* gf */;
		    REG6 = TLREFB(1) /* <vector> */;
		    REG7 = gvec_ref(REG0 /* gf */,SLOT(14));
		    REG8 = gvec_ref(REG0 /* gf */,SLOT(12));
		    gvec_set(REG5,SLOT(14),make3(REG6,REG7,REG8,gvec_ref(REG0 /* gf */,SLOT(13))));
		    gvec_set(REG0 /* gf */,SLOT(13),gvec_ref(REG0 /* gf */,FXWORDS_TO_RIBYTES(ADD1(REG4 /* ix */))));
		    gvec_set(REG0 /* gf */,SLOT(12),gvec_ref(REG0 /* gf */,FXWORDS_TO_RIBYTES(REG4 /* ix */)));
		    gvec_set(REG0 /* gf */,FXWORDS_TO_RIBYTES(REG4 /* ix */),REG3 /* c */);
		    gvec_set(REG0 /* gf */,FXWORDS_TO_RIBYTES(ADD1(REG4 /* ix */)),REG2 /* m */);
		    REG0 = REG1 /* args */;
		    REG1 = REG2 /* m */;
		    APPLYF(2,TLREFB(2) /* apply* */);
	    }
	    else
	    {
		    gvec_set(REG0 /* gf */,SLOT(13),gvec_ref(REG0 /* gf */,FXWORDS_TO_RIBYTES(ADD1(REG4 /* ix */))));
		    gvec_set(REG0 /* gf */,SLOT(12),gvec_ref(REG0 /* gf */,FXWORDS_TO_RIBYTES(REG4 /* ix */)));
		    gvec_set(REG0 /* gf */,FXWORDS_TO_RIBYTES(REG4 /* ix */),REG3 /* c */);
		    gvec_set(REG0 /* gf */,FXWORDS_TO_RIBYTES(ADD1(REG4 /* ix */)),REG2 /* m */);
		    REG0 = REG1 /* args */;
		    REG1 = REG2 /* m */;
		    APPLYF(2,TLREFB(2) /* apply* */);
	    }
	    }
	    else
	    {
	    gvec_set(REG0 /* gf */,FXWORDS_TO_RIBYTES(REG4 /* ix */),REG3 /* c */);
	    gvec_set(REG0 /* gf */,FXWORDS_TO_RIBYTES(ADD1(REG4 /* ix */)),REG2 /* m */);
	    REG0 = REG1 /* args */;
	    REG1 = REG2 /* m */;
	    APPLYF(2,TLREFB(2) /* apply* */);
	    }
    }
    else
    {
	    /* NOP: REG0 = REG0; */
	    /* NOP: REG1 = REG1; */
	    APPLYG(2,TLREFB(3) /* does-not-understand */);
    }
}
#undef FPLACE_CODE

EPILOGUE(load_cache_and_call)

BEGIN_BACK(load_cache_and_call)
  BACK_MONOTONE(load_cache_and_call_0)
  BACK_MONOTONE(load_cache_and_call_1)
END_BACK(load_cache_and_call)

static struct function_descr load_cache_and_call_descr = {
	&objsys_part_genericf,
	JUMP_TABLE( load_cache_and_call ),
	rsfn_load_cache_and_call_name };
#undef FUNCTION

/******************************* Postamble *******************************/
/**************************** Part Link Table ****************************/


static struct function_descr *(part_genericf_tab[]) = {
    &generic_function_dispatch_descr,
    &get_gf_lru_histogram_descr,
    &pre_compute_dispatch_descr,
    &find_method_by_class_descr,
    &find_method_descr,
    &load_cache_descr,
    &load_cache_and_call_descr,
    NULL };
struct part_descr objsys_part_genericf = {
    72769536,
    &module_objsys,
    part_genericf_tab,
    "genericf",
    0, sccsid };
#undef _MODULE_OBJSYS
#undef _SCM_GENERICF
#undef _C_GENERICF
