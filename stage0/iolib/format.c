/**********************************************
THIS FILE WAS AUTOMATICALLY GENERATED, AND MAY
BE AUTOMATICALLY RE-GENERATED WHEN THE COMPILER
OR SOURCE CHANGES.  DO NOT MODIFY THIS FILE BY HAND!
RScheme Build (v0.7.3.4-b7u, 2007-05-30)
**********************************************/

/******************************** Preamble ********************************/

#define _MODULE_IOLIB
#define _SCM_FORMAT
#define _C_FORMAT
#include "iolib_p.h"
#include <rscheme/vinsns.h>
extern struct module_descr module_iolib;
extern struct part_descr iolib_part_format;
static char sccsid[] = "@(#)iolib modules/iolib/format.scm [105812994] (RS v0.7.3.4-b7u, 2007-05-30)";

/************************** Function Definitions **************************/


/********************* Raw glue `parse-format-string' *********************/
#define str REG0

static char rsfn_parse_format_string_name[] = "parse-format-string";
#define FUNCTION rsfn_parse_format_string_name

PROLOGUE(parse_format_string)

BEGIN_FWD(parse_format_string)
  FWD_MONOTONE(parse_format_string_0)
END_FWD(parse_format_string)

#define FPLACE_CODE (1000+0)
MONOTONE(parse_format_string_0)
{
{
extern obj parse_format_string( obj string );

   REG0 = parse_format_string(str);
   RETURN1();
}}
#undef FPLACE_CODE

EPILOGUE(parse_format_string)

BEGIN_BACK(parse_format_string)
  BACK_MONOTONE(parse_format_string_0)
END_BACK(parse_format_string)

static struct function_descr parse_format_string_descr = {
	&iolib_part_format,
	JUMP_TABLE( parse_format_string ),
	rsfn_parse_format_string_name };
#undef FUNCTION

#undef str
/******************************* Postamble *******************************/
/**************************** Part Link Table ****************************/


static struct function_descr *(part_format_tab[]) = {
    &parse_format_string_descr,
    NULL };
struct part_descr iolib_part_format = {
    105812994,
    &module_iolib,
    part_format_tab,
    "format",
    0, sccsid };
#undef _MODULE_IOLIB
#undef _SCM_FORMAT
#undef _C_FORMAT
