/**********************************************
THIS FILE WAS AUTOMATICALLY GENERATED, AND MAY
BE AUTOMATICALLY RE-GENERATED WHEN THE COMPILER
OR SOURCE CHANGES.  DO NOT MODIFY THIS FILE BY HAND!
RScheme Build (v0.7.3.4-b7u, 2007-05-30)
**********************************************/

/******************************** Preamble ********************************/

#define _MODULE_CORELIB
#define _SCM_PROCESS
#define _C_PROCESS
#include "corelib_p.h"
#include <rscheme/vinsns.h>
extern struct module_descr module_corelib;
extern struct part_descr corelib_part_process;
static char sccsid[] = "@(#)corelib modules/corelib/process.scm [503523330] (RS v0.7.3.4-b7u, 2007-05-30)";

/************************** Function Definitions **************************/


/************************ Raw glue `process-abort' ************************/

static char rsfn_process_abort_name[] = "process-abort";
#define FUNCTION rsfn_process_abort_name

PROLOGUE(process_abort)

BEGIN_FWD(process_abort)
  FWD_MONOTONE(process_abort_0)
END_FWD(process_abort)

#define FPLACE_CODE (1000+0)
MONOTONE(process_abort_0)
{
{
unsigned i;

    for (i=0; i<arg_count_reg; i++)
      {
	fprintf( stderr, " process-abort[%u] := ", i );
	fprinto( stderr, reg_ref(i) );
	fprintf( stderr, "\n" );
      }
    abort();
}}
#undef FPLACE_CODE

EPILOGUE(process_abort)

BEGIN_BACK(process_abort)
  BACK_MONOTONE(process_abort_0)
END_BACK(process_abort)

static struct function_descr process_abort_descr = {
	&corelib_part_process,
	JUMP_TABLE( process_abort ),
	rsfn_process_abort_name };
#undef FUNCTION


/************************ Raw glue `process-exit*' ************************/
#define code REG0

static char rsfn_process_exit_name[] = "process-exit*";
#define FUNCTION rsfn_process_exit_name

PROLOGUE(process_exit)

BEGIN_FWD(process_exit)
  FWD_MONOTONE(process_exit_0)
END_FWD(process_exit)

#define FPLACE_CODE (1000+0)
MONOTONE(process_exit_0)
{
{
   exit( fx2int( code ) );
   RETURN0();
}}
#undef FPLACE_CODE

EPILOGUE(process_exit)

BEGIN_BACK(process_exit)
  BACK_MONOTONE(process_exit_0)
END_BACK(process_exit)

static struct function_descr process_exit_descr = {
	&corelib_part_process,
	JUMP_TABLE( process_exit ),
	rsfn_process_exit_name };
#undef FUNCTION

#undef code

/*************************** Raw glue `os-type' ***************************/

static char rsfn_os_type_name[] = "os-type";
#define FUNCTION rsfn_os_type_name

PROLOGUE(os_type)

BEGIN_FWD(os_type)
  FWD_MONOTONE(os_type_0)
END_FWD(os_type)

#define FPLACE_CODE (1000+0)
MONOTONE(os_type_0)
{
{
   REG0 = os_type();
   RETURN1();
}}
#undef FPLACE_CODE

EPILOGUE(os_type)

BEGIN_BACK(os_type)
  BACK_MONOTONE(os_type_0)
END_BACK(os_type)

static struct function_descr os_type_descr = {
	&corelib_part_process,
	JUMP_TABLE( os_type ),
	rsfn_os_type_name };
#undef FUNCTION


/*********************** Raw glue `word-size-bits' ***********************/

static char rsfn_word_size_bits_name[] = "word-size-bits";
#define FUNCTION rsfn_word_size_bits_name

PROLOGUE(word_size_bits)

BEGIN_FWD(word_size_bits)
  FWD_MONOTONE(word_size_bits_0)
END_FWD(word_size_bits)

#define FPLACE_CODE (1000+0)
MONOTONE(word_size_bits_0)
{
{
   REG0 = int2fx( WORD_SIZE_BITS );
   RETURN1();
}}
#undef FPLACE_CODE

EPILOGUE(word_size_bits)

BEGIN_BACK(word_size_bits)
  BACK_MONOTONE(word_size_bits_0)
END_BACK(word_size_bits)

static struct function_descr word_size_bits_descr = {
	&corelib_part_process,
	JUMP_TABLE( word_size_bits ),
	rsfn_word_size_bits_name };
#undef FUNCTION


/*************************** Raw glue `getenv' ***************************/
#define raw_str REG0

static char rsfn_getenv_name[] = "getenv";
#define FUNCTION rsfn_getenv_name

PROLOGUE(getenv)

BEGIN_FWD(getenv)
  FWD_MONOTONE(getenv_0)
END_FWD(getenv)

#define FPLACE_CODE (1000+0)
MONOTONE(getenv_0)
{  char *str;
  COUNT_ARGS(1);
  if (!STRING_P(raw_str))
    {
      obj c;
      c = make5( TLREFB(0),
                 NIL_OBJ,  /* properties */
                 lookup_symbol( FUNCTION ),
                 cons( raw_str, NIL_OBJ ),
                 lookup_symbol( "str" ),
                 TLREFB(1) );
      raise_error( c );
    }
  str = (char *)string_text(raw_str);


{
  const char *r = os_getenv( str );
  REG0 = r ? make_string( r ) : FALSE_OBJ;
  RETURN1();
}}
#undef FPLACE_CODE

EPILOGUE(getenv)

BEGIN_BACK(getenv)
  BACK_MONOTONE(getenv_0)
END_BACK(getenv)

static struct function_descr getenv_descr = {
	&corelib_part_process,
	JUMP_TABLE( getenv ),
	rsfn_getenv_name };
#undef FUNCTION

#undef raw_str

/************************** Raw glue `os-getwd' **************************/

static char rsfn_os_getwd_name[] = "os-getwd";
#define FUNCTION rsfn_os_getwd_name

PROLOGUE(os_getwd)

BEGIN_FWD(os_getwd)
  FWD_MONOTONE(os_getwd_0)
END_FWD(os_getwd)

#define FPLACE_CODE (1000+0)
MONOTONE(os_getwd_0)
{
{
   REG0 = os_getwd();
   RETURN1();
}}
#undef FPLACE_CODE

EPILOGUE(os_getwd)

BEGIN_BACK(os_getwd)
  BACK_MONOTONE(os_getwd_0)
END_BACK(os_getwd)

static struct function_descr os_getwd_descr = {
	&corelib_part_process,
	JUMP_TABLE( os_getwd ),
	rsfn_os_getwd_name };
#undef FUNCTION


/************************** Raw glue `os-setwd!' **************************/
#define raw_path REG0

static char rsfn_os_setwd_name[] = "os-setwd!";
#define FUNCTION rsfn_os_setwd_name

PROLOGUE(os_setwd)

BEGIN_FWD(os_setwd)
  FWD_MONOTONE(os_setwd_0)
END_FWD(os_setwd)

#define FPLACE_CODE (1000+0)
MONOTONE(os_setwd_0)
{  char *path;
  COUNT_ARGS(1);
  if (!STRING_P(raw_path))
    {
      obj c;
      c = make5( TLREFB(0),
                 NIL_OBJ,  /* properties */
                 lookup_symbol( FUNCTION ),
                 cons( raw_path, NIL_OBJ ),
                 lookup_symbol( "path" ),
                 TLREFB(1) );
      raise_error( c );
    }
  path = (char *)string_text(raw_path);


{
  os_setwd(path);
  RETURN0();
}}
#undef FPLACE_CODE

EPILOGUE(os_setwd)

BEGIN_BACK(os_setwd)
  BACK_MONOTONE(os_setwd_0)
END_BACK(os_setwd)

static struct function_descr os_setwd_descr = {
	&corelib_part_process,
	JUMP_TABLE( os_setwd ),
	rsfn_os_setwd_name };
#undef FUNCTION

#undef raw_path

/********************* Raw glue `get-compile-options' *********************/

static char rsfn_get_compile_options_name[] = "get-compile-options";
#define FUNCTION rsfn_get_compile_options_name

PROLOGUE(get_compile_options)

BEGIN_FWD(get_compile_options)
  FWD_MONOTONE(get_compile_options_0)
END_FWD(get_compile_options)

#define FPLACE_CODE (1000+0)
MONOTONE(get_compile_options_0)
{
{
extern int bci_trace_flag;
obj opts = NIL_OBJ;

  if (bci_trace_flag >= 0)
    opts = cons( LITERAL(0), opts );

#ifdef RECORD_CALL_CHAIN
  opts = cons( LITERAL(1), opts );
#endif
  REG0 = opts;
  RETURN1();
}}
#undef FPLACE_CODE

EPILOGUE(get_compile_options)

BEGIN_BACK(get_compile_options)
  BACK_MONOTONE(get_compile_options_0)
END_BACK(get_compile_options)

static struct function_descr get_compile_options_descr = {
	&corelib_part_process,
	JUMP_TABLE( get_compile_options ),
	rsfn_get_compile_options_name };
#undef FUNCTION


/********************* Raw glue `set-bci-trace-flag!' *********************/
#define to REG0

static char rsfn_set_bci_trace_flag_name[] = "set-bci-trace-flag!";
#define FUNCTION rsfn_set_bci_trace_flag_name

PROLOGUE(set_bci_trace_flag)

BEGIN_FWD(set_bci_trace_flag)
  FWD_MONOTONE(set_bci_trace_flag_0)
END_FWD(set_bci_trace_flag)

#define FPLACE_CODE (1000+0)
MONOTONE(set_bci_trace_flag_0)
{
{
extern int bci_trace_flag;
int old;

 if (bci_trace_flag >= 0)
   {
     old = bci_trace_flag;

     if (EQ(to,TRUE_OBJ))
       bci_trace_flag = 1;
     else if (EQ(to,FALSE_OBJ))
       bci_trace_flag = 0;

     REG0 = rb_to_bo(old);
   }
  else
    {
      /* this should never happen, as nobody should call 
	 this function if get-compile-options indicates
	 that this functionality is not available 
	 */
      REG0 = FALSE_OBJ;
    }
   RETURN1();
}}
#undef FPLACE_CODE

EPILOGUE(set_bci_trace_flag)

BEGIN_BACK(set_bci_trace_flag)
  BACK_MONOTONE(set_bci_trace_flag_0)
END_BACK(set_bci_trace_flag)

static struct function_descr set_bci_trace_flag_descr = {
	&corelib_part_process,
	JUMP_TABLE( set_bci_trace_flag ),
	rsfn_set_bci_trace_flag_name };
#undef FUNCTION

#undef to

/******************** Raw glue `set-apply-trace-flag!' ********************/
#define to REG0

static char rsfn_set_apply_trace_flag_name[] = "set-apply-trace-flag!";
#define FUNCTION rsfn_set_apply_trace_flag_name

PROLOGUE(set_apply_trace_flag)

BEGIN_FWD(set_apply_trace_flag)
  FWD_MONOTONE(set_apply_trace_flag_0)
END_FWD(set_apply_trace_flag)

#define FPLACE_CODE (1000+0)
MONOTONE(set_apply_trace_flag_0)
{
{
#ifndef RECORD_CALL_CHAIN
  REG0 = FALSE_OBJ;
  /* this should never happen, as nobody should call 
     this function if get-compile-options indicates
     that this functionality is not available 
     */
#else
extern rs_bool do_record_call_chain;
rs_bool old;

  old = do_record_call_chain;
  if (EQ(to,TRUE_OBJ))
    do_record_call_chain = YES;
  else if (EQ(to,FALSE_OBJ))
    do_record_call_chain = NO;
  REG0 = rb_to_bo(old);
#endif
  RETURN1();
}}
#undef FPLACE_CODE

EPILOGUE(set_apply_trace_flag)

BEGIN_BACK(set_apply_trace_flag)
  BACK_MONOTONE(set_apply_trace_flag_0)
END_BACK(set_apply_trace_flag)

static struct function_descr set_apply_trace_flag_descr = {
	&corelib_part_process,
	JUMP_TABLE( set_apply_trace_flag ),
	rsfn_set_apply_trace_flag_name };
#undef FUNCTION

#undef to
/******************************* Postamble *******************************/
/**************************** Part Link Table ****************************/


static struct function_descr *(part_process_tab[]) = {
    &process_abort_descr,
    &process_exit_descr,
    &os_type_descr,
    &word_size_bits_descr,
    &getenv_descr,
    &os_getwd_descr,
    &os_setwd_descr,
    &get_compile_options_descr,
    &set_bci_trace_flag_descr,
    &set_apply_trace_flag_descr,
    NULL };
struct part_descr corelib_part_process = {
    503523330,
    &module_corelib,
    part_process_tab,
    "process",
    0, sccsid };
#undef _MODULE_CORELIB
#undef _SCM_PROCESS
#undef _C_PROCESS
