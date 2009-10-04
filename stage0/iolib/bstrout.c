/**********************************************
THIS FILE WAS AUTOMATICALLY GENERATED, AND MAY
BE AUTOMATICALLY RE-GENERATED WHEN THE COMPILER
OR SOURCE CHANGES.  DO NOT MODIFY THIS FILE BY HAND!
RScheme Build (v0.7.3.4-b7u, 2007-05-30)
**********************************************/

/******************************** Preamble ********************************/

#define _MODULE_IOLIB
#define _SCM_BSTROUT
#define _C_BSTROUT
#include "iolib_p.h"
#include <rscheme/vinsns.h>
extern struct module_descr module_iolib;
extern struct part_descr iolib_part_bstrout;
static char sccsid[] = "@(#)iolib modules/iolib/bstrout.scm [129288193] (RS v0.7.3.4-b7u, 2007-05-30)";

/************************** Function Definitions **************************/


/************** Raw glue `bounded-string-output-port-flush' **************/
#define port REG0
#define closeq REG1

static char rsfn_bounded_string_output_port_flush_name[] = "bounded-string-output-port-flush";
#define FUNCTION rsfn_bounded_string_output_port_flush_name

PROLOGUE(bounded_string_output_port_flush)

BEGIN_FWD(bounded_string_output_port_flush)
  FWD_MONOTONE(bounded_string_output_port_flush_0)
END_FWD(bounded_string_output_port_flush)

#define FPLACE_CODE (1000+0)
MONOTONE(bounded_string_output_port_flush_0)
{
{
  if (NOT( gvec_ref( port, BSOP_BUFFER ))) {
    REG0 = port;
    REG1 = LITERAL(1);
    APPLYF( 2, TLREFB(0) );
  } else {
    REG0 = BSOP_flush( port, truish(closeq) );
    RETURN1();
  }
}}
#undef FPLACE_CODE

EPILOGUE(bounded_string_output_port_flush)

BEGIN_BACK(bounded_string_output_port_flush)
  BACK_MONOTONE(bounded_string_output_port_flush_0)
END_BACK(bounded_string_output_port_flush)

static struct function_descr bounded_string_output_port_flush_descr = {
	&iolib_part_bstrout,
	JUMP_TABLE( bounded_string_output_port_flush ),
	rsfn_bounded_string_output_port_flush_name };
#undef FUNCTION

#undef port
#undef closeq

/************ Raw glue `bounded-string-output-port-write-char' ************/
#define port REG0
#define the_char REG1

static char rsfn_bounded_string_output_port_write_char_name[] = "bounded-string-output-port-write-char";
#define FUNCTION rsfn_bounded_string_output_port_write_char_name

PROLOGUE(bounded_string_output_port_write_char)

BEGIN_FWD(bounded_string_output_port_write_char)
  FWD_MONOTONE(bounded_string_output_port_write_char_0)
END_FWD(bounded_string_output_port_write_char)

#define FPLACE_CODE (1000+0)
MONOTONE(bounded_string_output_port_write_char_0)
{
{
char ch;

  if (NOT( gvec_ref( port, BSOP_BUFFER ))) {
    REG0 = port;
    REG1 = LITERAL(2);
    APPLYF( 2, TLREFB(1) );
  } else {
    ch = GET_IMMEDIATE_VALUE(the_char);
    if (!BSOP_write( port, &ch, 1 ))
      APPLYF(1,TLREFB(0));
    else
      RETURN0();
  }
}}
#undef FPLACE_CODE

EPILOGUE(bounded_string_output_port_write_char)

BEGIN_BACK(bounded_string_output_port_write_char)
  BACK_MONOTONE(bounded_string_output_port_write_char_0)
END_BACK(bounded_string_output_port_write_char)

static struct function_descr bounded_string_output_port_write_char_descr = {
	&iolib_part_bstrout,
	JUMP_TABLE( bounded_string_output_port_write_char ),
	rsfn_bounded_string_output_port_write_char_name };
#undef FUNCTION

#undef port
#undef the_char

/*********** Raw glue `bounded-string-output-port-write-string' ***********/
#define port REG0
#define the_str REG1

static char rsfn_bounded_string_output_port_write_string_name[] = "bounded-string-output-port-write-string";
#define FUNCTION rsfn_bounded_string_output_port_write_string_name

PROLOGUE(bounded_string_output_port_write_string)

BEGIN_FWD(bounded_string_output_port_write_string)
  FWD_MONOTONE(bounded_string_output_port_write_string_0)
END_FWD(bounded_string_output_port_write_string)

#define FPLACE_CODE (1000+0)
MONOTONE(bounded_string_output_port_write_string_0)
{
{
  if (NOT( gvec_ref( port, BSOP_BUFFER ))) {
    REG0 = port;
    REG1 = LITERAL(2);
    APPLYF( 2, TLREFB(1) );
  } else {
    if (!BSOP_write( port, string_text(the_str), string_length(the_str) ))
      APPLYF(1,TLREFB(0));
    else
      RETURN0();
  }
}}
#undef FPLACE_CODE

EPILOGUE(bounded_string_output_port_write_string)

BEGIN_BACK(bounded_string_output_port_write_string)
  BACK_MONOTONE(bounded_string_output_port_write_string_0)
END_BACK(bounded_string_output_port_write_string)

static struct function_descr bounded_string_output_port_write_string_descr = {
	&iolib_part_bstrout,
	JUMP_TABLE( bounded_string_output_port_write_string ),
	rsfn_bounded_string_output_port_write_string_name };
#undef FUNCTION

#undef port
#undef the_str

/************ Raw glue `bounded-string-output-port-write-int' ************/
#define port REG0
#define the_int REG1

static char rsfn_bounded_string_output_port_write_int_name[] = "bounded-string-output-port-write-int";
#define FUNCTION rsfn_bounded_string_output_port_write_int_name

PROLOGUE(bounded_string_output_port_write_int)

BEGIN_FWD(bounded_string_output_port_write_int)
  FWD_MONOTONE(bounded_string_output_port_write_int_0)
END_FWD(bounded_string_output_port_write_int)

#define FPLACE_CODE (1000+0)
MONOTONE(bounded_string_output_port_write_int_0)
{
{
char temp[40];

  if (NOT( gvec_ref( port, BSOP_BUFFER ))) {
    REG0 = port;
    REG1 = LITERAL(2);
    APPLYF( 2, TLREFB(1) );
  } else {
    sprintf( temp, "%ld", (long)fx2int(the_int) );
    if (!BSOP_write( port, temp, strlen(temp) ))
      APPLY(1,TLREF(0));
    else
      RETURN0();
  }
}}
#undef FPLACE_CODE

EPILOGUE(bounded_string_output_port_write_int)

BEGIN_BACK(bounded_string_output_port_write_int)
  BACK_MONOTONE(bounded_string_output_port_write_int_0)
END_BACK(bounded_string_output_port_write_int)

static struct function_descr bounded_string_output_port_write_int_descr = {
	&iolib_part_bstrout,
	JUMP_TABLE( bounded_string_output_port_write_int ),
	rsfn_bounded_string_output_port_write_int_name };
#undef FUNCTION

#undef port
#undef the_int
/******************************* Postamble *******************************/
/**************************** Part Link Table ****************************/


static struct function_descr *(part_bstrout_tab[]) = {
    &bounded_string_output_port_flush_descr,
    &bounded_string_output_port_write_char_descr,
    &bounded_string_output_port_write_string_descr,
    &bounded_string_output_port_write_int_descr,
    NULL };
struct part_descr iolib_part_bstrout = {
    129288193,
    &module_iolib,
    part_bstrout_tab,
    "bstrout",
    0, sccsid };
#undef _MODULE_IOLIB
#undef _SCM_BSTROUT
#undef _C_BSTROUT
