/**********************************************
THIS FILE WAS AUTOMATICALLY GENERATED, AND MAY
BE AUTOMATICALLY RE-GENERATED WHEN THE COMPILER
OR SOURCE CHANGES.  DO NOT MODIFY THIS FILE BY HAND!
RScheme Build (v0.7.3.4-b7u, 2007-05-30)
**********************************************/

/******************************** Preamble ********************************/

#define _MODULE_CORELIB
#define _SCM_INTRGLUE
#define _C_INTRGLUE
#include "corelib_p.h"
#include <rscheme/vinsns.h>
extern struct module_descr module_corelib;
extern struct part_descr corelib_part_intrglue;
static char sccsid[] = "@(#)corelib modules/corelib/intrglue.scm [477520904] (RS v0.7.3.4-b7u, 2007-05-30)";

/************************** Function Definitions **************************/


/******************* Raw glue `setup-c-signal-handler!' *******************/
#define raw_sig_name REG0

static char rsfn_setup_c_signal_handler_name[] = "setup-c-signal-handler!";
#define FUNCTION rsfn_setup_c_signal_handler_name

PROLOGUE(setup_c_signal_handler)

BEGIN_FWD(setup_c_signal_handler)
  FWD_MONOTONE(setup_c_signal_handler_0)
END_FWD(setup_c_signal_handler)

#define FPLACE_CODE (1000+0)
MONOTONE(setup_c_signal_handler_0)
{  obj sig_name;
  COUNT_ARGS(1);
  if (!instance_p(raw_sig_name,TLREFB(0)))
    {
      obj c;
      c = make5( TLREFB(1),
                 NIL_OBJ,  /* properties */
                 lookup_symbol( FUNCTION ),
                 cons( raw_sig_name, NIL_OBJ ),
                 lookup_symbol( "sig_name" ),
                 TLREFB(0) );
      raise_error( c );
    }
  sig_name = raw_sig_name;


{
  int n = rs_c_signal_num(sig_name);
  if ((n < 0)
      ||
      (os_register_signal_handler( n, c_signal_catcher ) < 0))
    {
      scheme_error( "C signal '~s' is invalid", 1, sig_name );
    }
  RETURN0();
}}
#undef FPLACE_CODE

EPILOGUE(setup_c_signal_handler)

BEGIN_BACK(setup_c_signal_handler)
  BACK_MONOTONE(setup_c_signal_handler_0)
END_BACK(setup_c_signal_handler)

static struct function_descr setup_c_signal_handler_descr = {
	&corelib_part_intrglue,
	JUMP_TABLE( setup_c_signal_handler ),
	rsfn_setup_c_signal_handler_name };
#undef FUNCTION

#undef raw_sig_name

/********************** Raw glue `ignore-c-signal!' **********************/
#define raw_sig_name REG0

static char rsfn_ignore_c_signal_name[] = "ignore-c-signal!";
#define FUNCTION rsfn_ignore_c_signal_name

PROLOGUE(ignore_c_signal)

BEGIN_FWD(ignore_c_signal)
  FWD_MONOTONE(ignore_c_signal_0)
END_FWD(ignore_c_signal)

#define FPLACE_CODE (1000+0)
MONOTONE(ignore_c_signal_0)
{  obj sig_name;
  COUNT_ARGS(1);
  if (!instance_p(raw_sig_name,TLREFB(0)))
    {
      obj c;
      c = make5( TLREFB(1),
                 NIL_OBJ,  /* properties */
                 lookup_symbol( FUNCTION ),
                 cons( raw_sig_name, NIL_OBJ ),
                 lookup_symbol( "sig_name" ),
                 TLREFB(0) );
      raise_error( c );
    }
  sig_name = raw_sig_name;


{
  int n = rs_c_signal_num(sig_name);
  if ((n < 0)
      ||
      (os_register_signal_handler( n, NULL ) < 0))
    {
      scheme_error( "C signal '~s' is invalid", 1, sig_name );
    }
  RETURN0();
}}
#undef FPLACE_CODE

EPILOGUE(ignore_c_signal)

BEGIN_BACK(ignore_c_signal)
  BACK_MONOTONE(ignore_c_signal_0)
END_BACK(ignore_c_signal)

static struct function_descr ignore_c_signal_descr = {
	&corelib_part_intrglue,
	JUMP_TABLE( ignore_c_signal ),
	rsfn_ignore_c_signal_name };
#undef FUNCTION

#undef raw_sig_name

/******************* Raw glue `continue-intr-template' *******************/

static char rsfn_continue_intr_template_name[] = "continue-intr-template";
#define FUNCTION rsfn_continue_intr_template_name

PROLOGUE(continue_intr_template)

BEGIN_FWD(continue_intr_template)
  FWD_MONOTONE(continue_intr_template_0)
END_FWD(continue_intr_template)

#define FPLACE_CODE (1000+0)
MONOTONE(continue_intr_template_0)
{
{
jump_addr f;

    RESTORE_CONT0();	/* personally, we don't have anything
    			   saved in our continuation */
    f = half_restore();			/* but get the interrupted
    					   context's fixed registers */
    arg_count_reg = restore_arb();	/* and all the other regs */
    os_set_sigenable( YES );

#ifdef GNU_VINSNS
    goto *f;
#else
    return f;
#endif
}}
#undef FPLACE_CODE

EPILOGUE(continue_intr_template)

BEGIN_BACK(continue_intr_template)
  BACK_MONOTONE(continue_intr_template_0)
END_BACK(continue_intr_template)

static struct function_descr continue_intr_template_descr = {
	&corelib_part_intrglue,
	JUMP_TABLE( continue_intr_template ),
	rsfn_continue_intr_template_name };
#undef FUNCTION


/******************* Raw glue `return-from-call-scheme' *******************/

static char rsfn_return_from_call_scheme_name[] = "return-from-call-scheme";
#define FUNCTION rsfn_return_from_call_scheme_name

PROLOGUE(return_from_call_scheme)

BEGIN_FWD(return_from_call_scheme)
  FWD_MONOTONE(return_from_call_scheme_0)
  FWD_MONOTONE(finish_run_1)
END_FWD(return_from_call_scheme)

#define FPLACE_CODE (1000+0)
MONOTONE(return_from_call_scheme_0)
{
{
  JUMP(arg_count_reg,finish_run_1);  /* never used */
}}
#undef FPLACE_CODE

#define FPLACE_CODE (1000+1)
MONOTONE(finish_run_1)
{
   done_w_call_scheme();
   RETURN1(); /* never reached */
}
#undef FPLACE_CODE

EPILOGUE(return_from_call_scheme)

BEGIN_BACK(return_from_call_scheme)
  BACK_MONOTONE(return_from_call_scheme_0)
  BACK_MONOTONE(finish_run_1)
END_BACK(return_from_call_scheme)

static struct function_descr return_from_call_scheme_descr = {
	&corelib_part_intrglue,
	JUMP_TABLE( return_from_call_scheme ),
	rsfn_return_from_call_scheme_name };
#undef FUNCTION


/****************** Raw glue `enable-subprocess-capture' ******************/

static char rsfn_enable_subprocess_capture_name[] = "enable-subprocess-capture";
#define FUNCTION rsfn_enable_subprocess_capture_name

PROLOGUE(enable_subprocess_capture)

BEGIN_FWD(enable_subprocess_capture)
  FWD_MONOTONE(enable_subprocess_capture_0)
END_FWD(enable_subprocess_capture)

#define FPLACE_CODE (1000+0)
MONOTONE(enable_subprocess_capture_0)
{
{
  enable_subprocess_capture();
  RETURN0();
}}
#undef FPLACE_CODE

EPILOGUE(enable_subprocess_capture)

BEGIN_BACK(enable_subprocess_capture)
  BACK_MONOTONE(enable_subprocess_capture_0)
END_BACK(enable_subprocess_capture)

static struct function_descr enable_subprocess_capture_descr = {
	&corelib_part_intrglue,
	JUMP_TABLE( enable_subprocess_capture ),
	rsfn_enable_subprocess_capture_name };
#undef FUNCTION

/******************************* Postamble *******************************/
/**************************** Part Link Table ****************************/


static struct function_descr *(part_intrglue_tab[]) = {
    &setup_c_signal_handler_descr,
    &ignore_c_signal_descr,
    &continue_intr_template_descr,
    &return_from_call_scheme_descr,
    &enable_subprocess_capture_descr,
    NULL };
struct part_descr corelib_part_intrglue = {
    477520904,
    &module_corelib,
    part_intrglue_tab,
    "intrglue",
    0, sccsid };
#undef _MODULE_CORELIB
#undef _SCM_INTRGLUE
#undef _C_INTRGLUE
