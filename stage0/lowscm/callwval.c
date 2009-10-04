/**********************************************
THIS FILE WAS AUTOMATICALLY GENERATED, AND MAY
BE AUTOMATICALLY RE-GENERATED WHEN THE COMPILER
OR SOURCE CHANGES.  DO NOT MODIFY THIS FILE BY HAND!
RScheme Build (v0.7.3.4-b7u, 2007-05-30)
**********************************************/

/******************************** Preamble ********************************/

#define _MODULE_LOW_SCHEME
#define _SCM_CALLWVAL
#define _C_CALLWVAL
#include "low_scheme_p.h"
#include <rscheme/vinsns.h>
extern struct module_descr module_low_scheme;
extern struct part_descr low_scheme_part_callwval;
static char sccsid[] = "@(#)low-scheme modules/lowscm/callwval.scm [366223367] (RS v0.7.3.4-b7u, 2007-05-30)";

/************************** Function Definitions **************************/


/********************** Raw glue `call-with-values' **********************/
#define raw_producer REG0
#define raw_consumer REG1

static char rsfn_call_with_values_name[] = "call-with-values";
#define FUNCTION rsfn_call_with_values_name

PROLOGUE(call_with_values)

BEGIN_FWD(call_with_values)
  FWD_MONOTONE(call_with_values_0)
  FWD_MONOTONE(done_with_producer)
END_FWD(call_with_values)

#define FPLACE_CODE (1000+0)
MONOTONE(call_with_values_0)
{  obj producer;
  obj consumer;
  COUNT_ARGS(2);
  if (!instance_p(raw_producer,TLREFB(0)))
    {
      obj c;
      c = make5( TLREFB(1),
                 NIL_OBJ,  /* properties */
                 lookup_symbol( FUNCTION ),
                 cons( raw_producer, NIL_OBJ ),
                 lookup_symbol( "producer" ),
                 TLREFB(0) );
      raise_error( c );
    }
  producer = raw_producer;

  if (!instance_p(raw_consumer,TLREFB(0)))
    {
      obj c;
      c = make5( TLREFB(1),
                 NIL_OBJ,  /* properties */
                 lookup_symbol( FUNCTION ),
                 cons( raw_consumer, NIL_OBJ ),
                 lookup_symbol( "consumer" ),
                 TLREFB(0) );
      raise_error( c );
    }
  consumer = raw_consumer;


{
  obj next = consumer;
  REG0 = consumer;
  SAVE_CONT1(done_with_producer);
  APPLY(0, producer);
}}
#undef FPLACE_CODE

#define FPLACE_CODE (1000+1)
MONOTONE(done_with_producer)
{
  obj consumer = PARTCONT_REG(0);
  RESTORE_CONT_REG();
  APPLY(arg_count_reg, consumer);
}
#undef FPLACE_CODE

EPILOGUE(call_with_values)

BEGIN_BACK(call_with_values)
  BACK_MONOTONE(call_with_values_0)
  BACK_MONOTONE(done_with_producer)
END_BACK(call_with_values)

static struct function_descr call_with_values_descr = {
	&low_scheme_part_callwval,
	JUMP_TABLE( call_with_values ),
	rsfn_call_with_values_name };
#undef FUNCTION

#undef raw_producer
#undef raw_consumer

/************************* Raw glue `%composite' *************************/

static char rsfn_composite_name[] = "%composite";
#define FUNCTION rsfn_composite_name

PROLOGUE(composite)

BEGIN_FWD(composite)
  FWD_MONOTONE(composite_0)
  FWD_MONOTONE(composite_1)
END_FWD(composite)

#define FPLACE_CODE (1000+0)
MONOTONE(composite_0)
{
{
  USE_FUNCTION_ENVT();
  {
    PUSH_PARTCONT( composite_1, 1 );
    SET_PARTCONT_REG( 0, LEXREF0(0) );
  }
  if (arg_count_reg == 0) {
    RETURN0();
  } else {
    RETURN(arg_count_reg);
  }
}}
#undef FPLACE_CODE

#define FPLACE_CODE (1000+1)
MONOTONE(composite_1)
{
  obj lst = PARTCONT_REG(0);
  RESTORE_CONT_REG();

  if (NULL_P( lst )) {
    /* if arg_count_reg == 0, then REG0 was already set to #f 
     * by whoever returned into the composite_1 continuation
     */
    RETURN( arg_count_reg );
  } else {
    PUSH_PARTCONT( composite_1, 1 );
    SET_PARTCONT_REG( 0, pair_cdr(lst) );
    APPLY( arg_count_reg, pair_car(lst) );
  }
}
#undef FPLACE_CODE

EPILOGUE(composite)

BEGIN_BACK(composite)
  BACK_MONOTONE(composite_0)
  BACK_MONOTONE(composite_1)
END_BACK(composite)

static struct function_descr composite_descr = {
	&low_scheme_part_callwval,
	JUMP_TABLE( composite ),
	rsfn_composite_name };
#undef FUNCTION

/******************************* Postamble *******************************/
/**************************** Part Link Table ****************************/


static struct function_descr *(part_callwval_tab[]) = {
    &call_with_values_descr,
    &composite_descr,
    NULL };
struct part_descr low_scheme_part_callwval = {
    366223367,
    &module_low_scheme,
    part_callwval_tab,
    "callwval",
    0, sccsid };
#undef _MODULE_LOW_SCHEME
#undef _SCM_CALLWVAL
#undef _C_CALLWVAL
