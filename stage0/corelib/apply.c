/**********************************************
THIS FILE WAS AUTOMATICALLY GENERATED, AND MAY
BE AUTOMATICALLY RE-GENERATED WHEN THE COMPILER
OR SOURCE CHANGES.  DO NOT MODIFY THIS FILE BY HAND!
RScheme Build (v0.7.3.4-b7u, 2007-05-30)
**********************************************/

/******************************** Preamble ********************************/

#define _MODULE_CORELIB
#define _SCM_APPLY
#define _C_APPLY
#include "corelib_p.h"
#include <rscheme/vinsns.h>
extern struct module_descr module_corelib;
extern struct part_descr corelib_part_apply;
static char sccsid[] = "@(#)corelib modules/corelib/apply.scm [202800128] (RS v0.7.3.4-b7u, 2007-05-30)";

/************************** Function Definitions **************************/


/**************************** Raw glue `apply' ****************************/

static char rsfn_apply_name[] = "apply";
#define FUNCTION rsfn_apply_name

PROLOGUE(apply)

BEGIN_FWD(apply)
  FWD_MONOTONE(apply_0)
END_FWD(apply)

#define FPLACE_CODE (1000+0)
MONOTONE(apply_0)
{
{
unsigned i, n;
obj proc;

    COUNT_ARGS_AT_LEAST(2);
    proc = REG0;
    
    n = expand_last() - 1;
    
    /* shift the registers, because we are removing the
       'proc' arg from REG0
       (might want to optimize this for smaller n's...) */
    
    for (i=0; i<n; i++)
	reg_set( i, reg_ref(i+1) );

    APPLY( n, proc );
}}
#undef FPLACE_CODE

EPILOGUE(apply)

BEGIN_BACK(apply)
  BACK_MONOTONE(apply_0)
END_BACK(apply)

static struct function_descr apply_descr = {
	&corelib_part_apply,
	JUMP_TABLE( apply ),
	rsfn_apply_name };
#undef FUNCTION


/*************************** Raw glue `funcall' ***************************/

static char rsfn_funcall_name[] = "funcall";
#define FUNCTION rsfn_funcall_name

PROLOGUE(funcall)

BEGIN_FWD(funcall)
  FWD_MONOTONE(funcall_0)
END_FWD(funcall)

#define FPLACE_CODE (1000+0)
MONOTONE(funcall_0)
{
{
unsigned i, n;
obj proc;

    COUNT_ARGS_AT_LEAST(1);
    proc = REG0;
    
    n = arg_count_reg - 1;
    
    /* shift the registers, because we are removing the
       'proc' arg from REG0
       (might want to optimize this for smaller n's...) */
    
    for (i=0; i<n; i++)
	reg_set( i, reg_ref(i+1) );

    APPLY( n, proc );
}}
#undef FPLACE_CODE

EPILOGUE(funcall)

BEGIN_BACK(funcall)
  BACK_MONOTONE(funcall_0)
END_BACK(funcall)

static struct function_descr funcall_descr = {
	&corelib_part_apply,
	JUMP_TABLE( funcall ),
	rsfn_funcall_name };
#undef FUNCTION


/*********************** Raw glue `apply-template' ***********************/

static char rsfn_apply_template_name[] = "apply-template";
#define FUNCTION rsfn_apply_template_name

PROLOGUE(apply_template)

BEGIN_FWD(apply_template)
  FWD_MONOTONE(apply_template_0)
END_FWD(apply_template)

#define FPLACE_CODE (1000+0)
MONOTONE(apply_template_0)
{
{
unsigned i, n;
obj proc, tmpl;

    COUNT_ARGS_AT_LEAST(3);
    tmpl = REG0;
    proc = REG1;
    
    n = expand_last() - 2;
    
    /* shift the registers, because we are removing the
       'proc' arg from REG0
       (might want to optimize this for smaller n's...) */
    
    for (i=0; i<n; i++)
	reg_set( i, reg_ref(i+2) );

    APPLY_TMPL( n, proc, tmpl );
}}
#undef FPLACE_CODE

EPILOGUE(apply_template)

BEGIN_BACK(apply_template)
  BACK_MONOTONE(apply_template_0)
END_BACK(apply_template)

static struct function_descr apply_template_descr = {
	&corelib_part_apply,
	JUMP_TABLE( apply_template ),
	rsfn_apply_template_name };
#undef FUNCTION


/********************* Raw glue `standalone-template' *********************/

static char rsfn_standalone_template_name[] = "standalone-template";
#define FUNCTION rsfn_standalone_template_name

PROLOGUE(standalone_template)

BEGIN_FWD(standalone_template)
  FWD_MONOTONE(standalone_template_0)
END_FWD(standalone_template)

#define FPLACE_CODE (1000+0)
MONOTONE(standalone_template_0)
{
{
obj caller = envt_reg;

   COLLECT0();
   REG1 = REG0;
   REG0 = caller;
   APPLY( 2, LITERAL(0) );
}}
#undef FPLACE_CODE

EPILOGUE(standalone_template)

BEGIN_BACK(standalone_template)
  BACK_MONOTONE(standalone_template_0)
END_BACK(standalone_template)

static struct function_descr standalone_template_descr = {
	&corelib_part_apply,
	JUMP_TABLE( standalone_template ),
	rsfn_standalone_template_name };
#undef FUNCTION


/******************** Raw glue `lazy-flush-trampoline' ********************/

static char rsfn_lazy_flush_trampoline_name[] = "lazy-flush-trampoline";
#define FUNCTION rsfn_lazy_flush_trampoline_name

PROLOGUE(lazy_flush_trampoline)

BEGIN_FWD(lazy_flush_trampoline)
  FWD_MONOTONE(lazy_flush_trampoline_0)
END_FWD(lazy_flush_trampoline)

#define FPLACE_CODE (1000+0)
MONOTONE(lazy_flush_trampoline_0)
{
{
  obj moi = LITERAL(1);

  /*  check if the shadowed template has
   *  already been loaded (it may be visible
   *  through multiple closures, although
   *  on deeper thought I'm not sure exactly
   *  how that could happen; anyway,
   *  someone else may have triggered the flush)
   */

  if (!OBJ_ISA_PTR( gvec_ref( moi, SLOT(1) ) ) )
    {
      /*  update our procedure to point to the
       *  real template
       */
      gvec_set( envt_reg, SLOT(0), moi );
      APPLYF( arg_count_reg, envt_reg ); /* call our patched self */
    }
  else
    {
      /* flush all the code */
      /*   call the flusher with enough info to
       *   resume _this_ call, ie, our procedure
       *   and arguments
       */
      COLLECT0();
      REG1 = envt_reg;
      APPLYF( 2, TLREFB(0) );
    }
}}
#undef FPLACE_CODE

EPILOGUE(lazy_flush_trampoline)

BEGIN_BACK(lazy_flush_trampoline)
  BACK_MONOTONE(lazy_flush_trampoline_0)
END_BACK(lazy_flush_trampoline)

static struct function_descr lazy_flush_trampoline_descr = {
	&corelib_part_apply,
	JUMP_TABLE( lazy_flush_trampoline ),
	rsfn_lazy_flush_trampoline_name };
#undef FUNCTION


/*************************** Raw glue `apply*' ***************************/

static char rsfn_apply1_name[] = "apply*";
#define FUNCTION rsfn_apply1_name

PROLOGUE(apply1)

BEGIN_FWD(apply1)
  FWD_MONOTONE(apply1_0)
END_FWD(apply1)

#define FPLACE_CODE (1000+0)
MONOTONE(apply1_0)
{
{
unsigned n;
obj proc;

    COUNT_ARGS_AT_LEAST(2);

    proc = reg_ref( --arg_count_reg );
    n = expand_last();
    
    APPLY( n, proc );
}}
#undef FPLACE_CODE

EPILOGUE(apply1)

BEGIN_BACK(apply1)
  BACK_MONOTONE(apply1_0)
END_BACK(apply1)

static struct function_descr apply1_descr = {
	&corelib_part_apply,
	JUMP_TABLE( apply1 ),
	rsfn_apply1_name };
#undef FUNCTION


/************************ Raw glue `list->values' ************************/

static char rsfn_list_values_name[] = "list->values";
#define FUNCTION rsfn_list_values_name

PROLOGUE(list_values)

BEGIN_FWD(list_values)
  FWD_MONOTONE(list_values_0)
END_FWD(list_values)

#define FPLACE_CODE (1000+0)
MONOTONE(list_values_0)
{
{
unsigned n;

    n = expand_last();
    if (n == 0)
      RETURN0();
    else
      RETURN(n);
}}
#undef FPLACE_CODE

EPILOGUE(list_values)

BEGIN_BACK(list_values)
  BACK_MONOTONE(list_values_0)
END_BACK(list_values)

static struct function_descr list_values_descr = {
	&corelib_part_apply,
	JUMP_TABLE( list_values ),
	rsfn_list_values_name };
#undef FUNCTION


/*********************** Raw glue `vector->values' ***********************/
#define raw_v REG0

static char rsfn_vector_values_name[] = "vector->values";
#define FUNCTION rsfn_vector_values_name

PROLOGUE(vector_values)

BEGIN_FWD(vector_values)
  FWD_MONOTONE(vector_values_0)
END_FWD(vector_values)

#define FPLACE_CODE (1000+0)
MONOTONE(vector_values_0)
{  obj v;
  COUNT_ARGS(1);
  if (!instance_p(raw_v,TLREFB(0)))
    {
      obj c;
      c = make5( TLREFB(1),
                 NIL_OBJ,  /* properties */
                 lookup_symbol( FUNCTION ),
                 cons( raw_v, NIL_OBJ ),
                 lookup_symbol( "v" ),
                 TLREFB(0) );
      raise_error( c );
    }
  v = raw_v;


{
  unsigned n;

  n = SIZEOF_PTR(v) / SLOT(1);
  switch (n)
   {
     case 0: RETURN0();
             break;
     case 1: REG0 = gvec_ref( v, SLOT(0) );
             RETURN1();
             break;
     case 2: REG0 = gvec_ref( v, SLOT(0) );
             REG1 = gvec_ref( v, SLOT(1) );
             RETURN(2);
             break;
     case 3: REG0 = gvec_ref( v, SLOT(0) );
             REG1 = gvec_ref( v, SLOT(1) );
             REG2 = gvec_ref( v, SLOT(2) );
             RETURN(3);
             break;
    default: REG0 = gvec_ref( v, SLOT(0) );
             REG1 = gvec_ref( v, SLOT(1) );
             REG2 = gvec_ref( v, SLOT(2) );
	     {
	       unsigned i;

	       for (i=3; i<n; i++)
	        {
	          reg_set( i, gvec_ref( v, SLOT( i ) ) );
	        }
               RETURN(n);
             }
             break;
   }
}}
#undef FPLACE_CODE

EPILOGUE(vector_values)

BEGIN_BACK(vector_values)
  BACK_MONOTONE(vector_values_0)
END_BACK(vector_values)

static struct function_descr vector_values_descr = {
	&corelib_part_apply,
	JUMP_TABLE( vector_values ),
	rsfn_vector_values_name };
#undef FUNCTION

#undef raw_v

/************************** Raw glue `backstop' **************************/
#define mark REG0
#define thunk REG1

static char rsfn_backstop_name[] = "backstop";
#define FUNCTION rsfn_backstop_name

PROLOGUE(backstop)

BEGIN_FWD(backstop)
  FWD_MONOTONE(backstop_0)
  FWD_MONOTONE(backstop_cont)
END_FWD(backstop)

#define FPLACE_CODE (1000+0)
MONOTONE(backstop_0)
{
{
  SAVE_CONT1( backstop_cont );
  APPLY( 0, thunk );
}}
#undef FPLACE_CODE

#define FPLACE_CODE (1000+1)
MONOTONE(backstop_cont)
{
  /* This is a little evil; we saved 1 register
   * for marking purposes, but we're not restoring
   * any because we don't want to rearrange registers
   * to return whatever the thunk returned.
   */
  RESTORE_CONT0();
  if (arg_count_reg == 0) {
    RETURN0();
  } else {
    RETURN(arg_count_reg);
  }
}
#undef FPLACE_CODE

EPILOGUE(backstop)

BEGIN_BACK(backstop)
  BACK_MONOTONE(backstop_0)
  BACK_MONOTONE(backstop_cont)
END_BACK(backstop)

static struct function_descr backstop_descr = {
	&corelib_part_apply,
	JUMP_TABLE( backstop ),
	rsfn_backstop_name };
#undef FUNCTION

#undef mark
#undef thunk
/******************************* Postamble *******************************/
/**************************** Part Link Table ****************************/


static struct function_descr *(part_apply_tab[]) = {
    &apply_descr,
    &funcall_descr,
    &apply_template_descr,
    &standalone_template_descr,
    &lazy_flush_trampoline_descr,
    &apply1_descr,
    &list_values_descr,
    &vector_values_descr,
    &backstop_descr,
    NULL };
struct part_descr corelib_part_apply = {
    202800128,
    &module_corelib,
    part_apply_tab,
    "apply",
    0, sccsid };
#undef _MODULE_CORELIB
#undef _SCM_APPLY
#undef _C_APPLY
