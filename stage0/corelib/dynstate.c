/**********************************************
THIS FILE WAS AUTOMATICALLY GENERATED, AND MAY
BE AUTOMATICALLY RE-GENERATED WHEN THE COMPILER
OR SOURCE CHANGES.  DO NOT MODIFY THIS FILE BY HAND!
RScheme Build (v0.7.3.4-b7u, 2007-05-30)
**********************************************/

/******************************** Preamble ********************************/

#define _MODULE_CORELIB
#define _SCM_DYNSTATE
#define _C_DYNSTATE
#include "corelib_p.h"
#include <rscheme/vinsns.h>
extern struct module_descr module_corelib;
extern struct part_descr corelib_part_dynstate;
static char sccsid[] = "@(#)corelib modules/corelib/dynstate.scm [451018755] (RS v0.7.3.4-b7u, 2007-05-30)";

/************************** Function Definitions **************************/


/************************** Raw glue `continuer' **************************/

static char rsfn_continuer_name[] = "continuer";
#define FUNCTION rsfn_continuer_name

PROLOGUE(continuer)

BEGIN_FWD(continuer)
  FWD_MONOTONE(continuer_0)
END_FWD(continuer)

#define FPLACE_CODE (1000+0)
MONOTONE(continuer_0)
{
{
    USE_FUNCTION_ENVT();
    continuation_reg = LEXREF0(0);
    if (rsprof_active)
       rsprof_contn_restored(continuation_reg);
    dynamic_state_reg = LEXREF0(1);
    thread_state_reg = LEXREF0(2);
    if (arg_count_reg == 0)
	RETURN0();
    else
	RETURN(arg_count_reg);
}}
#undef FPLACE_CODE

EPILOGUE(continuer)

BEGIN_BACK(continuer)
  BACK_MONOTONE(continuer_0)
END_BACK(continuer)

static struct function_descr continuer_descr = {
	&corelib_part_dynstate,
	JUMP_TABLE( continuer ),
	rsfn_continuer_name };
#undef FUNCTION


/********************* Raw glue `one-shot-continuer' *********************/

static char rsfn_one_shot_continuer_name[] = "one-shot-continuer";
#define FUNCTION rsfn_one_shot_continuer_name

PROLOGUE(one_shot_continuer)

BEGIN_FWD(one_shot_continuer)
  FWD_MONOTONE(one_shot_continuer_0)
END_FWD(one_shot_continuer)

#define FPLACE_CODE (1000+0)
MONOTONE(one_shot_continuer_0)
{
{
  obj cr;
  obj me = envt_reg;

  USE_FUNCTION_ENVT();

  cr = LEXREF0(0);
  if (EQ( cr, FALSE_OBJ )) {
    REG0 = me;
    APPLY( 1, TLREF(0) );
  } else {
    continuation_reg = cr;
    LEXSET0( 0, FALSE_OBJ );

    if (rsprof_active) {
       rsprof_contn_restored( continuation_reg );
    }
    dynamic_state_reg = LEXREF0( 1 );
    thread_state_reg = LEXREF0( 2 );

    if (arg_count_reg == 0) {
      RETURN0();
    } else {
      RETURN( arg_count_reg );
    }
  }
}}
#undef FPLACE_CODE

EPILOGUE(one_shot_continuer)

BEGIN_BACK(one_shot_continuer)
  BACK_MONOTONE(one_shot_continuer_0)
END_BACK(one_shot_continuer)

static struct function_descr one_shot_continuer_descr = {
	&corelib_part_dynstate,
	JUMP_TABLE( one_shot_continuer ),
	rsfn_one_shot_continuer_name };
#undef FUNCTION


/********************** Raw glue `restore-then-call' **********************/

static char rsfn_restore_then_call_name[] = "restore-then-call";
#define FUNCTION rsfn_restore_then_call_name

PROLOGUE(restore_then_call)

BEGIN_FWD(restore_then_call)
  FWD_MONOTONE(restore_then_call_0)
END_FWD(restore_then_call)

#define FPLACE_CODE (1000+0)
MONOTONE(restore_then_call_0)
{
{
unsigned i, n;
obj proc;

    USE_FUNCTION_ENVT();
    continuation_reg = LEXREF0(0);
    dynamic_state_reg = LEXREF0(1);
    thread_state_reg = LEXREF0(2);

    COUNT_ARGS_AT_LEAST(1);
    proc = REG0;
    n = arg_count_reg - 1;
    
    /*  shift the registers, because we are removing the
     *  'proc' arg from REG0, just like (apply) does
     */
    
    for (i=0; i<n; i++)
	reg_set( i, reg_ref(i+1) );

    APPLY( n, proc );
}}
#undef FPLACE_CODE

EPILOGUE(restore_then_call)

BEGIN_BACK(restore_then_call)
  BACK_MONOTONE(restore_then_call_0)
END_BACK(restore_then_call)

static struct function_descr restore_then_call_descr = {
	&corelib_part_dynstate,
	JUMP_TABLE( restore_then_call ),
	rsfn_restore_then_call_name };
#undef FUNCTION


/******************** Raw glue `proc->low-level-contn' ********************/
#define proc REG0

static char rsfn_proc_low_level_contn_name[] = "proc->low-level-contn";
#define FUNCTION rsfn_proc_low_level_contn_name

PROLOGUE(proc_low_level_contn)

BEGIN_FWD(proc_low_level_contn)
  FWD_MONOTONE(proc_low_level_contn_0)
  FWD_MONOTONE(proc2contn)
END_FWD(proc_low_level_contn)

#define FPLACE_CODE (1000+0)
MONOTONE(proc_low_level_contn_0)
{
{
  obj contn = alloc( SLOT(CONT_FIXED+1), partcont_class );
#if CONT_FIXED != 4
#error oops: CONTN_FIXED != 4
#endif
  gvec_write_init( contn, SLOT(0), FALSE_OBJ );    /* envt_reg */
  gvec_write_init( contn, SLOT(1), literals_reg ); /* literals_reg */
  gvec_write_init( contn, SLOT(2), JUMP_ADDR_TO_OBJ(proc2contn) );
  gvec_write_init( contn, SLOT(3), FALSE_OBJ );    /* continuation_reg */
  gvec_write_init( contn, SLOT(CONT_FIXED), proc );

  if (rsprof_active)
     rsprof_contn_captured( continuation_reg );

  BIND3( contn, dynamic_state_reg, thread_state_reg );
  REG0 = make_closure( envt_reg, TLREF(0) /* continuer */ );
  RETURN1();
}}
#undef FPLACE_CODE

#define FPLACE_CODE (1000+1)
MONOTONE(proc2contn)
{
  obj proc;
  proc = PARTCONT_REG(0);
  RESTORE_CONT_REG();
  APPLY(arg_count_reg,proc);
}
#undef FPLACE_CODE

EPILOGUE(proc_low_level_contn)

BEGIN_BACK(proc_low_level_contn)
  BACK_MONOTONE(proc_low_level_contn_0)
  BACK_MONOTONE(proc2contn)
END_BACK(proc_low_level_contn)

static struct function_descr proc_low_level_contn_descr = {
	&corelib_part_dynstate,
	JUMP_TABLE( proc_low_level_contn ),
	rsfn_proc_low_level_contn_name };
#undef FUNCTION

#undef proc

/********************** Raw glue `low-level-call/cc' **********************/
#define proc REG0
#define onesh REG1

static char rsfn_low_level_call_cc_name[] = "low-level-call/cc";
#define FUNCTION rsfn_low_level_call_cc_name

PROLOGUE(low_level_call_cc)

BEGIN_FWD(low_level_call_cc)
  FWD_MONOTONE(low_level_call_cc_0)
END_FWD(low_level_call_cc)

#define FPLACE_CODE (1000+0)
MONOTONE(low_level_call_cc_0)
{
{
  obj the_proc = proc;
  obj use_continuer;

  if (arg_count_reg == 2) {
    if (truish( onesh )) {
      use_continuer = TLREF( 1 );   /* one-shot-continuer */
    } else {
      use_continuer = TLREF( 0 );   /* continuer */
    }
  } else {
    COUNT_ARGS(1);
    use_continuer = TLREF( 0 );   /* continuer */
  }

  USE_EMPTY_ENVT();

  flush_stack_cache();	/* flush the stack cache, if any */
  if (rsprof_active) {
    rsprof_contn_captured( continuation_reg );
  }

  BIND3( continuation_reg, dynamic_state_reg, thread_state_reg );
  REG0 = make_closure( envt_reg, use_continuer );
  APPLY( 1, the_proc );
}}
#undef FPLACE_CODE

EPILOGUE(low_level_call_cc)

BEGIN_BACK(low_level_call_cc)
  BACK_MONOTONE(low_level_call_cc_0)
END_BACK(low_level_call_cc)

static struct function_descr low_level_call_cc_descr = {
	&corelib_part_dynstate,
	JUMP_TABLE( low_level_call_cc ),
	rsfn_low_level_call_cc_name };
#undef FUNCTION

#undef proc
#undef onesh

/********************* Raw glue `dynamic-call-thunk' *********************/
#define pre_thunk REG0
#define post_thunk REG1
#define body_thunk REG2
#define new_ds REG3
#define new_ts REG4

static char rsfn_dynamic_call_thunk_name[] = "dynamic-call-thunk";
#define FUNCTION rsfn_dynamic_call_thunk_name

PROLOGUE(dynamic_call_thunk)

BEGIN_FWD(dynamic_call_thunk)
  FWD_MONOTONE(dynamic_call_thunk_0)
  FWD_MONOTONE(dct_2)
  FWD_MONOTONE(dct_3)
  FWD_MONOTONE(dct_5)
  FWD_MONOTONE(dct_6)
  FWD_MONOTONE(dct_4)
END_FWD(dynamic_call_thunk)

#define FPLACE_CODE (1000+0)
MONOTONE(dynamic_call_thunk_0)
{
{
  USE_EMPTY_ENVT();
  BEGIN_BIND6();
    BIND_ARG(0,body_thunk);
    BIND_ARG(1,post_thunk);
    BIND_ARG(2,new_ds);
    BIND_ARG(3,new_ts);
    BIND_ARG(4,dynamic_state_reg);
    BIND_ARG(5,thread_state_reg);
  END_BIND

  if (truish(pre_thunk))
   {
     SAVE_CONT0(dct_2);
     APPLY(0,pre_thunk);
   }
  else
   {
     SAVE_CONT0(dct_3);
     dynamic_state_reg = new_ds /* new_ds */;
     thread_state_reg = new_ts /* new_ts */;
     APPLY(0,LEXREF0(0) /*body_thunk*/);
   }
}}
#undef FPLACE_CODE

#define FPLACE_CODE (1000+1)
MONOTONE(dct_2)
{
  RESTORE_CONT0();
  SAVE_CONT0(dct_3);
  dynamic_state_reg = LEXREF0(2) /* new_ds */;
  thread_state_reg = LEXREF0(3) /* new_ts */;
  APPLY(0,LEXREF0(0) /*body_thunk*/);
}
#undef FPLACE_CODE

#define FPLACE_CODE (1000+2)
MONOTONE(dct_3)
{
  obj post;

  RESTORE_CONT0();
  dynamic_state_reg = LEXREF0(4) /* saved_ds */;
  thread_state_reg = LEXREF0(5) /* saved_ts */;
  post = LEXREF0(1) /*post_thunk*/;
  if (EQ(post, FALSE_OBJ))
   {
     /* if arg_count_reg=0, then REG0 was already set to #f
        by whoever returned to the dct_3 continuation */
     RETURN(arg_count_reg);
   }
  else if (arg_count_reg == 1)
   {
    /* special-case returning a single arg from body */
    SAVE_CONT1(dct_5);
    APPLY(0,post);
   }
  else if (arg_count_reg == 0)
   {
    /* special-case returning a *no* values from body */
    SAVE_CONT0(dct_6);
    APPLY(0,post);
   }
  else
   {
     COLLECT0();  /* stores result into REG0 */
     SAVE_CONT1(dct_4);
     APPLY(0,post);
   }
}
#undef FPLACE_CODE

#define FPLACE_CODE (1000+3)
MONOTONE(dct_5)
{
  RESTORE_CONT1();
  RETURN1();
}
#undef FPLACE_CODE

#define FPLACE_CODE (1000+4)
MONOTONE(dct_6)
{
  RESTORE_CONT0();
  RETURN0();
}
#undef FPLACE_CODE

#define FPLACE_CODE (1000+5)
MONOTONE(dct_4)
{
  unsigned n;

  RESTORE_CONT1();
  arg_count_reg = 1;
  n = expand_last();

  RETURN(n);    /* above logic and use of dct_5, dct_5 ensures that n>=2 */
}
#undef FPLACE_CODE

EPILOGUE(dynamic_call_thunk)

BEGIN_BACK(dynamic_call_thunk)
  BACK_MONOTONE(dynamic_call_thunk_0)
  BACK_MONOTONE(dct_2)
  BACK_MONOTONE(dct_3)
  BACK_MONOTONE(dct_5)
  BACK_MONOTONE(dct_6)
  BACK_MONOTONE(dct_4)
END_BACK(dynamic_call_thunk)

static struct function_descr dynamic_call_thunk_descr = {
	&corelib_part_dynstate,
	JUMP_TABLE( dynamic_call_thunk ),
	rsfn_dynamic_call_thunk_name };
#undef FUNCTION

#undef pre_thunk
#undef post_thunk
#undef body_thunk
#undef new_ds
#undef new_ts

/******************** Raw glue `find-common-ancestor' ********************/
#define from REG0
#define to REG1

static char rsfn_find_common_ancestor_name[] = "find-common-ancestor";
#define FUNCTION rsfn_find_common_ancestor_name

PROLOGUE(find_common_ancestor)

BEGIN_FWD(find_common_ancestor)
  FWD_MONOTONE(find_common_ancestor_0)
END_FWD(find_common_ancestor)

#define FPLACE_CODE (1000+0)
MONOTONE(find_common_ancestor_0)
{
{
  extern obj find_common_ancestor( obj f, obj t );
  REG0 = find_common_ancestor( from, to );
  RETURN1();
}}
#undef FPLACE_CODE

EPILOGUE(find_common_ancestor)

BEGIN_BACK(find_common_ancestor)
  BACK_MONOTONE(find_common_ancestor_0)
END_BACK(find_common_ancestor)

static struct function_descr find_common_ancestor_descr = {
	&corelib_part_dynstate,
	JUMP_TABLE( find_common_ancestor ),
	rsfn_find_common_ancestor_name };
#undef FUNCTION

#undef from
#undef to

/******************* Raw glue `wind-fluid-tlv-contour' *******************/
#define ftlc REG0

static char rsfn_wind_fluid_tlv_contour_name[] = "wind-fluid-tlv-contour";
#define FUNCTION rsfn_wind_fluid_tlv_contour_name

PROLOGUE(wind_fluid_tlv_contour)

BEGIN_FWD(wind_fluid_tlv_contour)
  FWD_MONOTONE(wind_fluid_tlv_contour_0)
END_FWD(wind_fluid_tlv_contour)

#define FPLACE_CODE (1000+0)
MONOTONE(wind_fluid_tlv_contour_0)
{
{
  extern void wind_fluid_tl_contour( obj item );
  wind_fluid_tl_contour( ftlc );
  RETURN0();
}}
#undef FPLACE_CODE

EPILOGUE(wind_fluid_tlv_contour)

BEGIN_BACK(wind_fluid_tlv_contour)
  BACK_MONOTONE(wind_fluid_tlv_contour_0)
END_BACK(wind_fluid_tlv_contour)

static struct function_descr wind_fluid_tlv_contour_descr = {
	&corelib_part_dynstate,
	JUMP_TABLE( wind_fluid_tlv_contour ),
	rsfn_wind_fluid_tlv_contour_name };
#undef FUNCTION

#undef ftlc

/****************** Raw glue `unwind-fluid-tlv-contour' ******************/
#define ftlc REG0

static char rsfn_unwind_fluid_tlv_contour_name[] = "unwind-fluid-tlv-contour";
#define FUNCTION rsfn_unwind_fluid_tlv_contour_name

PROLOGUE(unwind_fluid_tlv_contour)

BEGIN_FWD(unwind_fluid_tlv_contour)
  FWD_MONOTONE(unwind_fluid_tlv_contour_0)
END_FWD(unwind_fluid_tlv_contour)

#define FPLACE_CODE (1000+0)
MONOTONE(unwind_fluid_tlv_contour_0)
{
{
  extern void unwind_fluid_tl_contour( obj item );
  unwind_fluid_tl_contour( ftlc );
  RETURN0();
}}
#undef FPLACE_CODE

EPILOGUE(unwind_fluid_tlv_contour)

BEGIN_BACK(unwind_fluid_tlv_contour)
  BACK_MONOTONE(unwind_fluid_tlv_contour_0)
END_BACK(unwind_fluid_tlv_contour)

static struct function_descr unwind_fluid_tlv_contour_descr = {
	&corelib_part_dynstate,
	JUMP_TABLE( unwind_fluid_tlv_contour ),
	rsfn_unwind_fluid_tlv_contour_name };
#undef FUNCTION

#undef ftlc

/************************** Raw glue `do-unwind' **************************/
#define oldstate REG0
#define common REG1

static char rsfn_do_unwind_name[] = "do-unwind";
#define FUNCTION rsfn_do_unwind_name

PROLOGUE(do_unwind)

BEGIN_FWD(do_unwind)
  FWD_MONOTONE(do_unwind_0)
  FWD_MONOTONE(do_unwind_1)
  FWD_MONOTONE(do_unwind_cont)
END_FWD(do_unwind)

#define FPLACE_CODE (1000+0)
MONOTONE(do_unwind_0)
{
{
  /* extract the common cell from the ancestor descriptor */
  common = gvec_ref(common,SLOT(0));
  JUMP(2,do_unwind_1);
}}
#undef FPLACE_CODE

#define FPLACE_CODE (1000+1)
MONOTONE(do_unwind_1)
{
  extern void unwind_fluid_tl_contour( obj ftlc );
  obj s = oldstate;
again:
  if (EQ(s,common))
    {
      RETURN0();
    }
  else
    {
      obj item = pair_car(s);
      s = pair_cdr(s);

      if (OBJ_ISA_PTR_OF_CLASS(item,TLREF(0)))
	{
	  unwind_fluid_tl_contour(item);
	  goto again;
	}
      else if (VECTOR_P(item))
	{
          /* ignore <vector> nodes -- they are debugging info */
          goto again;
        }
      else
	{
	  oldstate = s;
	  SAVE_CONT2(do_unwind_cont);
	  REG0 = item;
	  APPLY(1,TLREF(1));
	}
    }
}
#undef FPLACE_CODE

#define FPLACE_CODE (1000+2)
MONOTONE(do_unwind_cont)
{
  RESTORE_CONT2();
  BJUMP(2,do_unwind_1);
}
#undef FPLACE_CODE

EPILOGUE(do_unwind)

BEGIN_BACK(do_unwind)
  BACK_MONOTONE(do_unwind_0)
  BACK_MONOTONE(do_unwind_1)
  BACK_MONOTONE(do_unwind_cont)
END_BACK(do_unwind)

static struct function_descr do_unwind_descr = {
	&corelib_part_dynstate,
	JUMP_TABLE( do_unwind ),
	rsfn_do_unwind_name };
#undef FUNCTION

#undef oldstate
#undef common

/************************** Raw glue `do-rewind' **************************/
#define ancestor_descr REG0

static char rsfn_do_rewind_name[] = "do-rewind";
#define FUNCTION rsfn_do_rewind_name

PROLOGUE(do_rewind)

BEGIN_FWD(do_rewind)
  FWD_MONOTONE(do_rewind_0)
  FWD_MONOTONE(do_rewind_1)
  FWD_MONOTONE(do_rewind_cont)
END_FWD(do_rewind)

#define FPLACE_CODE (1000+0)
MONOTONE(do_rewind_0)
{
{
  /* figure out how many points along the path we have to traverse */
  REG1 = SUB1(RIBYTES_TO_FXWORDS(SIZEOF_PTR(ancestor_descr)));
  assert(OBJ_ISA_FIXNUM(REG1));
  JUMP(2,do_rewind_1);
}}
#undef FPLACE_CODE

#define FPLACE_CODE (1000+1)
MONOTONE(do_rewind_1)
{
  extern void wind_fluid_tl_contour( obj ftlc );
  obj ix = REG1;
  obj ftlcc = TLREF(0);

again:
  assert(OBJ_ISA_FIXNUM(ix) && FX_GE(ix,ZERO));
  if (EQ(ix,ZERO))
    {
      RETURN0();
    }
  else
    {
      obj item = gvec_ref( ancestor_descr, FXWORDS_TO_RIBYTES(ix) );
      ix = SUB1(ix);

      if (OBJ_ISA_PTR_OF_CLASS(item,ftlcc))
	{
	  wind_fluid_tl_contour(item);
	  goto again;
	}
      else
	{
	  REG1 = ix;
	  SAVE_CONT2(do_rewind_cont);
	  REG0 = item;
	  APPLY(1,TLREF(1));
	}
    }
}
#undef FPLACE_CODE

#define FPLACE_CODE (1000+2)
MONOTONE(do_rewind_cont)
{
  RESTORE_CONT2();
  BJUMP(2,do_rewind_1);
}
#undef FPLACE_CODE

EPILOGUE(do_rewind)

BEGIN_BACK(do_rewind)
  BACK_MONOTONE(do_rewind_0)
  BACK_MONOTONE(do_rewind_1)
  BACK_MONOTONE(do_rewind_cont)
END_BACK(do_rewind)

static struct function_descr do_rewind_descr = {
	&corelib_part_dynstate,
	JUMP_TABLE( do_rewind ),
	rsfn_do_rewind_name };
#undef FUNCTION

#undef ancestor_descr

/********************* Function `saved-continuation' *********************/
static char rsfn_saved_continuation_name[] = "saved-continuation";
#define FUNCTION rsfn_saved_continuation_name

PROLOGUE(saved_continuation)

BEGIN_FWD(saved_continuation)
  FWD_MONOTONE(saved_continuation_0)
  FWD_MONOTONE(saved_continuation_1)
  FWD_MONOTONE(saved_continuation_2)
  FWD_MONOTONE(saved_continuation_3)
END_FWD(saved_continuation)

#define FPLACE_CODE (1000+0)
MONOTONE(saved_continuation_0)
{
    USE_FUNCTION_ENVT();
    COLLECT0();
    /* NOP: REG0 = REG0; */
    REG1 = GET_DYNAMIC_STATE_REG();
    if (EQ(REG1 /* ds */,LEXREF0(1) /* saved-dynamic-envt */))
    {
	    /* NOP: REG0 = REG0; */
	    REG1 = LEXREF0(0) /* saved-contn */;
	    APPLYF(2,TLREFB(0) /* apply* */);
    }
    else
    {
	    SAVE_CONT2(saved_continuation_1);
	    REG0 = REG1 /* ds */;
	    REG1 = LEXREF0(1) /* saved-dynamic-envt */;
	    APPLYF(2,TLREFB(3) /* find-common-ancestor */);
    }
}
#undef FPLACE_CODE

#define FPLACE_CODE (1000+1)
MONOTONE(saved_continuation_1)
{
    REG2 = REG0;
    RESTORE_CONT2();
    /* NOP: REG2 = REG2; */
    /* NOP: REG2 = REG2; */
    SAVE_CONT3(saved_continuation_2);
    REG0 = REG1 /* ds */;
    REG1 = REG2 /* ancestor-descr */;
    APPLYF(2,TLREFB(1) /* do-unwind */);
}
#undef FPLACE_CODE

#define FPLACE_CODE (1000+2)
MONOTONE(saved_continuation_2)
{
    RESTORE_CONT3();
    SAVE_CONT3(saved_continuation_3);
    REG0 = REG2 /* ancestor-descr */;
    APPLYF(1,TLREFB(2) /* do-rewind */);
}
#undef FPLACE_CODE

#define FPLACE_CODE (1000+3)
MONOTONE(saved_continuation_3)
{
    RESTORE_CONT3();
    /* NOP: REG0 = REG0; */
    REG1 = LEXREF0(0) /* saved-contn */;
    APPLYF(2,TLREFB(0) /* apply* */);
}
#undef FPLACE_CODE

EPILOGUE(saved_continuation)

BEGIN_BACK(saved_continuation)
  BACK_MONOTONE(saved_continuation_0)
  BACK_MONOTONE(saved_continuation_1)
  BACK_MONOTONE(saved_continuation_2)
  BACK_MONOTONE(saved_continuation_3)
END_BACK(saved_continuation)

static struct function_descr saved_continuation_descr = {
	&corelib_part_dynstate,
	JUMP_TABLE( saved_continuation ),
	rsfn_saved_continuation_name };
#undef FUNCTION


/************************ Function `make-resumer' ************************/
static char rsfn_make_resumer_name[] = "make-resumer";
#define FUNCTION rsfn_make_resumer_name

PROLOGUE(make_resumer)

BEGIN_FWD(make_resumer)
  FWD_MONOTONE(make_resumer_0)
END_FWD(make_resumer)

#define FPLACE_CODE (1000+0)
MONOTONE(make_resumer_0)
{
    COUNT_ARGS(2);
    BEGIN_BIND2()
	    BIND_ARG(0,REG0);
	    BIND_ARG(1,REG1);
    END_BIND
    REG0 = CLOSURE(0);
    RETURN1();
}
#undef FPLACE_CODE

EPILOGUE(make_resumer)

BEGIN_BACK(make_resumer)
  BACK_MONOTONE(make_resumer_0)
END_BACK(make_resumer)

static struct function_descr make_resumer_descr = {
	&corelib_part_dynstate,
	JUMP_TABLE( make_resumer ),
	rsfn_make_resumer_name };
#undef FUNCTION


/************* Function `(0 call-with-current-continuation)' *************/
static char rsfn_call_with_current_continuation_name[] = "(0 call-with-current-continuation)";
#define FUNCTION rsfn_call_with_current_continuation_name

PROLOGUE(call_with_current_continuation)

BEGIN_FWD(call_with_current_continuation)
  FWD_MONOTONE(call_with_current_continuation_0)
  FWD_MONOTONE(call_with_current_continuation_1)
END_FWD(call_with_current_continuation)

#define FPLACE_CODE (1000+0)
MONOTONE(call_with_current_continuation_0)
{
    USE_FUNCTION_ENVT();
    COUNT_ARGS(1);
    /* NOP: REG0 = REG0; */
    SAVE_CONT1(call_with_current_continuation_1);
    /* NOP: REG0 = REG0; */
    REG1 = GET_DYNAMIC_STATE_REG();
    APPLYF(2,TLREFB(0) /* make-resumer */);
}
#undef FPLACE_CODE

#define FPLACE_CODE (1000+1)
MONOTONE(call_with_current_continuation_1)
{
    REG1 = REG0;
    RESTORE_CONT1();
    /* NOP: REG1 = REG1; */
    REG0 = REG1;
    APPLY(1,LEXREF0(0) /* proc */);
}
#undef FPLACE_CODE

EPILOGUE(call_with_current_continuation)

BEGIN_BACK(call_with_current_continuation)
  BACK_MONOTONE(call_with_current_continuation_0)
  BACK_MONOTONE(call_with_current_continuation_1)
END_BACK(call_with_current_continuation)

static struct function_descr call_with_current_continuation_descr = {
	&corelib_part_dynstate,
	JUMP_TABLE( call_with_current_continuation ),
	rsfn_call_with_current_continuation_name };
#undef FUNCTION


/*************** Function `call-with-current-continuation' ***************/
static char rsfn_call_with_current_continuation1_name[] = "call-with-current-continuation";
#define FUNCTION rsfn_call_with_current_continuation1_name

PROLOGUE(call_with_current_continuation1)

BEGIN_FWD(call_with_current_continuation1)
  FWD_MONOTONE(call_with_current_continuation1_0)
END_FWD(call_with_current_continuation1)

#define FPLACE_CODE (1000+0)
MONOTONE(call_with_current_continuation1_0)
{
    COUNT_ARGS(1);
    BEGIN_BIND1()
	    BIND_ARG(0,REG0);
    END_BIND
    REG0 = CLOSURE(0);
    /* NOP: REG0 = REG0; */
    APPLYF(1,TLREFB(1) /* low-level-call/cc */);
}
#undef FPLACE_CODE

EPILOGUE(call_with_current_continuation1)

BEGIN_BACK(call_with_current_continuation1)
  BACK_MONOTONE(call_with_current_continuation1_0)
END_BACK(call_with_current_continuation1)

static struct function_descr call_with_current_continuation1_descr = {
	&corelib_part_dynstate,
	JUMP_TABLE( call_with_current_continuation1 ),
	rsfn_call_with_current_continuation1_name };
#undef FUNCTION


/********* Function `(0 call-with-current-continuation/one-shot)' *********/
static char rsfn_call_with_current_continuation_one_shot_name[] = "(0 call-with-current-continuation/one-shot)";
#define FUNCTION rsfn_call_with_current_continuation_one_shot_name

PROLOGUE(call_with_current_continuation_one_shot)

BEGIN_FWD(call_with_current_continuation_one_shot)
  FWD_MONOTONE(call_with_current_continuation_one_shot_0)
  FWD_MONOTONE(call_with_current_continuation_one_shot_1)
END_FWD(call_with_current_continuation_one_shot)

#define FPLACE_CODE (1000+0)
MONOTONE(call_with_current_continuation_one_shot_0)
{
    USE_FUNCTION_ENVT();
    COUNT_ARGS(1);
    /* NOP: REG0 = REG0; */
    SAVE_CONT1(call_with_current_continuation_one_shot_1);
    /* NOP: REG0 = REG0; */
    REG1 = GET_DYNAMIC_STATE_REG();
    APPLYF(2,TLREFB(0) /* make-resumer */);
}
#undef FPLACE_CODE

#define FPLACE_CODE (1000+1)
MONOTONE(call_with_current_continuation_one_shot_1)
{
    REG1 = REG0;
    RESTORE_CONT1();
    /* NOP: REG1 = REG1; */
    REG0 = REG1;
    APPLY(1,LEXREF0(0) /* proc */);
}
#undef FPLACE_CODE

EPILOGUE(call_with_current_continuation_one_shot)

BEGIN_BACK(call_with_current_continuation_one_shot)
  BACK_MONOTONE(call_with_current_continuation_one_shot_0)
  BACK_MONOTONE(call_with_current_continuation_one_shot_1)
END_BACK(call_with_current_continuation_one_shot)

static struct function_descr call_with_current_continuation_one_shot_descr = {
	&corelib_part_dynstate,
	JUMP_TABLE( call_with_current_continuation_one_shot ),
	rsfn_call_with_current_continuation_one_shot_name };
#undef FUNCTION


/*********** Function `call-with-current-continuation/one-shot' ***********/
static char rsfn_call_with_current_continuation_one_shot1_name[] = "call-with-current-continuation/one-shot";
#define FUNCTION rsfn_call_with_current_continuation_one_shot1_name

PROLOGUE(call_with_current_continuation_one_shot1)

BEGIN_FWD(call_with_current_continuation_one_shot1)
  FWD_MONOTONE(call_with_current_continuation_one_shot1_0)
END_FWD(call_with_current_continuation_one_shot1)

#define FPLACE_CODE (1000+0)
MONOTONE(call_with_current_continuation_one_shot1_0)
{
    COUNT_ARGS(1);
    BEGIN_BIND1()
	    BIND_ARG(0,REG0);
    END_BIND
    REG0 = CLOSURE(0);
    /* NOP: REG0 = REG0; */
    REG1 = TRUE_OBJ;
    APPLYF(2,TLREFB(1) /* low-level-call/cc */);
}
#undef FPLACE_CODE

EPILOGUE(call_with_current_continuation_one_shot1)

BEGIN_BACK(call_with_current_continuation_one_shot1)
  BACK_MONOTONE(call_with_current_continuation_one_shot1_0)
END_BACK(call_with_current_continuation_one_shot1)

static struct function_descr call_with_current_continuation_one_shot1_descr = {
	&corelib_part_dynstate,
	JUMP_TABLE( call_with_current_continuation_one_shot1 ),
	rsfn_call_with_current_continuation_one_shot1_name };
#undef FUNCTION


/**************** Function `(0 close-over-dynamic-state)' ****************/
static char rsfn_close_over_dynamic_state_name[] = "(0 close-over-dynamic-state)";
#define FUNCTION rsfn_close_over_dynamic_state_name

PROLOGUE(close_over_dynamic_state)

BEGIN_FWD(close_over_dynamic_state)
  FWD_MONOTONE(close_over_dynamic_state_0)
  FWD_MONOTONE(close_over_dynamic_state_1)
  FWD_MONOTONE(close_over_dynamic_state_2)
  FWD_MONOTONE(close_over_dynamic_state_3)
  FWD_MONOTONE(close_over_dynamic_state_4)
END_FWD(close_over_dynamic_state)

#define FPLACE_CODE (1000+0)
MONOTONE(close_over_dynamic_state_0)
{
    USE_FUNCTION_ENVT();
    COLLECT0();
    /* NOP: REG0 = REG0; */
    REG1 = GET_DYNAMIC_STATE_REG();
    if (NOT(rb_to_bo(EQ(REG1 /* ds */,LEXREF0(0) /* saved-ds */))))
    {
	    SAVE_CONT2(close_over_dynamic_state_1);
	    REG0 = REG1 /* ds */;
	    REG1 = LEXREF0(0) /* saved-ds */;
	    APPLYF(2,TLREFB(2) /* find-common-ancestor */);
    }
    else
    {
	    JUMP(2,close_over_dynamic_state_4);
    }
}
#undef FPLACE_CODE

#define FPLACE_CODE (1000+1)
MONOTONE(close_over_dynamic_state_1)
{
    REG2 = REG0;
    RESTORE_CONT2();
    /* NOP: REG2 = REG2; */
    /* NOP: REG2 = REG2; */
    SAVE_CONT3(close_over_dynamic_state_2);
    REG0 = REG1 /* ds */;
    REG1 = REG2 /* ancestor-descr */;
    APPLYF(2,TLREFB(0) /* do-unwind */);
}
#undef FPLACE_CODE

#define FPLACE_CODE (1000+2)
MONOTONE(close_over_dynamic_state_2)
{
    RESTORE_CONT3();
    SAVE_CONT3(close_over_dynamic_state_3);
    REG0 = REG2 /* ancestor-descr */;
    APPLYF(1,TLREFB(1) /* do-rewind */);
}
#undef FPLACE_CODE

#define FPLACE_CODE (1000+3)
MONOTONE(close_over_dynamic_state_3)
{
    RESTORE_CONT3();
    JUMP(2,close_over_dynamic_state_4);
}
#undef FPLACE_CODE

#define FPLACE_CODE (1000+4)
MONOTONE(close_over_dynamic_state_4)
{
    /* NOP: REG0 = REG0; */
    REG1 = LEXREF0(1) /* ll-proc */;
    APPLYF(2,TLREFB(3) /* apply* */);
}
#undef FPLACE_CODE

EPILOGUE(close_over_dynamic_state)

BEGIN_BACK(close_over_dynamic_state)
  BACK_MONOTONE(close_over_dynamic_state_0)
  BACK_MONOTONE(close_over_dynamic_state_1)
  BACK_MONOTONE(close_over_dynamic_state_2)
  BACK_MONOTONE(close_over_dynamic_state_3)
  BACK_MONOTONE(close_over_dynamic_state_4)
END_BACK(close_over_dynamic_state)

static struct function_descr close_over_dynamic_state_descr = {
	&corelib_part_dynstate,
	JUMP_TABLE( close_over_dynamic_state ),
	rsfn_close_over_dynamic_state_name };
#undef FUNCTION


/****************** Function `close-over-dynamic-state' ******************/
static char rsfn_close_over_dynamic_state1_name[] = "close-over-dynamic-state";
#define FUNCTION rsfn_close_over_dynamic_state1_name

PROLOGUE(close_over_dynamic_state1)

BEGIN_FWD(close_over_dynamic_state1)
  FWD_MONOTONE(close_over_dynamic_state1_0)
  FWD_MONOTONE(close_over_dynamic_state1_1)
END_FWD(close_over_dynamic_state1)

#define FPLACE_CODE (1000+0)
MONOTONE(close_over_dynamic_state1_0)
{
    COUNT_ARGS(1);
    /* NOP: REG0 = REG0; */
    REG1 = GET_DYNAMIC_STATE_REG();
    SAVE_CONT2(close_over_dynamic_state1_1);
    /* NOP: REG0 = REG0; */
    APPLYF(1,TLREFB(1) /* proc->low-level-contn */);
}
#undef FPLACE_CODE

#define FPLACE_CODE (1000+1)
MONOTONE(close_over_dynamic_state1_1)
{
    REG2 = REG0;
    RESTORE_CONT2();
    /* NOP: REG2 = REG2; */
    BEGIN_BIND2()
	    BIND_ARG(0,REG1);
	    BIND_ARG(1,REG2);
    END_BIND
    REG0 = CLOSURE(0);
    RETURN1();
}
#undef FPLACE_CODE

EPILOGUE(close_over_dynamic_state1)

BEGIN_BACK(close_over_dynamic_state1)
  BACK_MONOTONE(close_over_dynamic_state1_0)
  BACK_MONOTONE(close_over_dynamic_state1_1)
END_BACK(close_over_dynamic_state1)

static struct function_descr close_over_dynamic_state1_descr = {
	&corelib_part_dynstate,
	JUMP_TABLE( close_over_dynamic_state1 ),
	rsfn_close_over_dynamic_state1_name };
#undef FUNCTION

/******************************* Postamble *******************************/
/**************************** Part Link Table ****************************/


static struct function_descr *(part_dynstate_tab[]) = {
    &continuer_descr,
    &one_shot_continuer_descr,
    &restore_then_call_descr,
    &proc_low_level_contn_descr,
    &low_level_call_cc_descr,
    &dynamic_call_thunk_descr,
    &find_common_ancestor_descr,
    &wind_fluid_tlv_contour_descr,
    &unwind_fluid_tlv_contour_descr,
    &do_unwind_descr,
    &do_rewind_descr,
    &saved_continuation_descr,
    &make_resumer_descr,
    &call_with_current_continuation_descr,
    &call_with_current_continuation1_descr,
    &call_with_current_continuation_one_shot_descr,
    &call_with_current_continuation_one_shot1_descr,
    &close_over_dynamic_state_descr,
    &close_over_dynamic_state1_descr,
    NULL };
struct part_descr corelib_part_dynstate = {
    451018755,
    &module_corelib,
    part_dynstate_tab,
    "dynstate",
    0, sccsid };
#undef _MODULE_CORELIB
#undef _SCM_DYNSTATE
#undef _C_DYNSTATE
