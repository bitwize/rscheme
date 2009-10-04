/**********************************************
THIS FILE WAS AUTOMATICALLY GENERATED, AND MAY
BE AUTOMATICALLY RE-GENERATED WHEN THE COMPILER
OR SOURCE CHANGES.  DO NOT MODIFY THIS FILE BY HAND!
RScheme Build (v0.7.3.4-b7u, 2007-05-30)
**********************************************/

/******************************** Preamble ********************************/

#define _MODULE_IOLIB
#define _SCM_STROUT
#define _C_STROUT
#include "iolib_p.h"
#include <rscheme/vinsns.h>
extern struct module_descr module_iolib;
extern struct part_descr iolib_part_strout;
static char sccsid[] = "@(#)iolib modules/iolib/strout.scm [317089792] (RS v0.7.3.4-b7u, 2007-05-30)";

/************************** Function Definitions **************************/


/****************** Raw glue `string-output-port-flush' ******************/
#define port REG0
#define closeq REG1

static char rsfn_string_output_port_flush_name[] = "string-output-port-flush";
#define FUNCTION rsfn_string_output_port_flush_name

PROLOGUE(string_output_port_flush)

BEGIN_FWD(string_output_port_flush)
  FWD_MONOTONE(string_output_port_flush_0)
END_FWD(string_output_port_flush)

#define FPLACE_CODE (1000+0)
MONOTONE(string_output_port_flush_0)
{
{
  if (NOT( gvec_ref( port, SOP_BUFFER ))) {
    REG0 = port;
    REG1 = LITERAL(1);
    APPLYF( 2, TLREFB(0) );
  } else {
    REG0 = SOP_flush( port, truish(closeq) );
    RETURN1();
  }
}}
#undef FPLACE_CODE

EPILOGUE(string_output_port_flush)

BEGIN_BACK(string_output_port_flush)
  BACK_MONOTONE(string_output_port_flush_0)
END_BACK(string_output_port_flush)

static struct function_descr string_output_port_flush_descr = {
	&iolib_part_strout,
	JUMP_TABLE( string_output_port_flush ),
	rsfn_string_output_port_flush_name };
#undef FUNCTION

#undef port
#undef closeq

/**************** Raw glue `string-output-port-write-char' ****************/
#define port REG0
#define the_char REG1

static char rsfn_string_output_port_write_char_name[] = "string-output-port-write-char";
#define FUNCTION rsfn_string_output_port_write_char_name

PROLOGUE(string_output_port_write_char)

BEGIN_FWD(string_output_port_write_char)
  FWD_MONOTONE(string_output_port_write_char_0)
END_FWD(string_output_port_write_char)

#define FPLACE_CODE (1000+0)
MONOTONE(string_output_port_write_char_0)
{
{
char ch;

  if (NOT( gvec_ref( port, SOP_BUFFER ))) {
    REG0 = port;
    REG1 = LITERAL(1);
    APPLYF( 2, TLREFB(0) );
  } else {
    ch = GET_IMMEDIATE_VALUE(the_char);
    SOP_write( port, &ch, 1 );
    RETURN0();
  }
}}
#undef FPLACE_CODE

EPILOGUE(string_output_port_write_char)

BEGIN_BACK(string_output_port_write_char)
  BACK_MONOTONE(string_output_port_write_char_0)
END_BACK(string_output_port_write_char)

static struct function_descr string_output_port_write_char_descr = {
	&iolib_part_strout,
	JUMP_TABLE( string_output_port_write_char ),
	rsfn_string_output_port_write_char_name };
#undef FUNCTION

#undef port
#undef the_char

/*************** Raw glue `string-output-port-write-string' ***************/
#define port REG0
#define the_str REG1

static char rsfn_string_output_port_write_string_name[] = "string-output-port-write-string";
#define FUNCTION rsfn_string_output_port_write_string_name

PROLOGUE(string_output_port_write_string)

BEGIN_FWD(string_output_port_write_string)
  FWD_MONOTONE(string_output_port_write_string_0)
END_FWD(string_output_port_write_string)

#define FPLACE_CODE (1000+0)
MONOTONE(string_output_port_write_string_0)
{
{
  if (NOT( gvec_ref( port, SOP_BUFFER ))) {
    REG0 = port;
    REG1 = LITERAL(1);
    APPLYF( 2, TLREFB(0) );
  } else {
    SOP_write( port, string_text(the_str), string_length(the_str) );
    RETURN0();
  }
}}
#undef FPLACE_CODE

EPILOGUE(string_output_port_write_string)

BEGIN_BACK(string_output_port_write_string)
  BACK_MONOTONE(string_output_port_write_string_0)
END_BACK(string_output_port_write_string)

static struct function_descr string_output_port_write_string_descr = {
	&iolib_part_strout,
	JUMP_TABLE( string_output_port_write_string ),
	rsfn_string_output_port_write_string_name };
#undef FUNCTION

#undef port
#undef the_str

/**************** Raw glue `string-output-port-write-int' ****************/
#define port REG0
#define the_int REG1

static char rsfn_string_output_port_write_int_name[] = "string-output-port-write-int";
#define FUNCTION rsfn_string_output_port_write_int_name

PROLOGUE(string_output_port_write_int)

BEGIN_FWD(string_output_port_write_int)
  FWD_MONOTONE(string_output_port_write_int_0)
END_FWD(string_output_port_write_int)

#define FPLACE_CODE (1000+0)
MONOTONE(string_output_port_write_int_0)
{
{
char temp[40];
int n;

  if (NOT( gvec_ref( port, SOP_BUFFER ))) {
    REG0 = port;
    REG1 = LITERAL(1);
    APPLYF( 2, TLREFB(0) );
  } else {
    n = sprintf( temp, "%ld", (long)fx2int(the_int) );
    SOP_write( port, temp, n );
    RETURN0();
  }
}}
#undef FPLACE_CODE

EPILOGUE(string_output_port_write_int)

BEGIN_BACK(string_output_port_write_int)
  BACK_MONOTONE(string_output_port_write_int_0)
END_BACK(string_output_port_write_int)

static struct function_descr string_output_port_write_int_descr = {
	&iolib_part_strout,
	JUMP_TABLE( string_output_port_write_int ),
	rsfn_string_output_port_write_int_name };
#undef FUNCTION

#undef port
#undef the_int
/******************************* Postamble *******************************/
/**************************** Part Link Table ****************************/


static struct function_descr *(part_strout_tab[]) = {
    &string_output_port_flush_descr,
    &string_output_port_write_char_descr,
    &string_output_port_write_string_descr,
    &string_output_port_write_int_descr,
    NULL };
struct part_descr iolib_part_strout = {
    317089792,
    &module_iolib,
    part_strout_tab,
    "strout",
    0, sccsid };
#undef _MODULE_IOLIB
#undef _SCM_STROUT
#undef _C_STROUT
