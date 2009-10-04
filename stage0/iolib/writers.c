/**********************************************
THIS FILE WAS AUTOMATICALLY GENERATED, AND MAY
BE AUTOMATICALLY RE-GENERATED WHEN THE COMPILER
OR SOURCE CHANGES.  DO NOT MODIFY THIS FILE BY HAND!
RScheme Build (v0.7.3.4-b7u, 2007-05-30)
**********************************************/

/******************************** Preamble ********************************/

#define _MODULE_IOLIB
#define _SCM_WRITERS
#define _C_WRITERS
#include "iolib_p.h"
#include <rscheme/vinsns.h>
extern struct module_descr module_iolib;
extern struct part_descr iolib_part_writers;
static char sccsid[] = "@(#)iolib modules/iolib/writers.scm [53308419] (RS v0.7.3.4-b7u, 2007-05-30)";

/************************** Function Definitions **************************/


/********************** Raw glue `string->printable' **********************/
#define str REG0
#define str_index REG1

static char rsfn_string_printable_name[] = "string->printable";
#define FUNCTION rsfn_string_printable_name

PROLOGUE(string_printable)

BEGIN_FWD(string_printable)
  FWD_MONOTONE(string_printable_0)
END_FWD(string_printable)

#define FPLACE_CODE (1000+0)
MONOTONE(string_printable_0)
{
{
    /* stores it's results directly into REG0 and REG1 */
    printablize_string( str, str_index );
    RETURN(2);
}}
#undef FPLACE_CODE

EPILOGUE(string_printable)

BEGIN_BACK(string_printable)
  BACK_MONOTONE(string_printable_0)
END_BACK(string_printable)

static struct function_descr string_printable_descr = {
	&iolib_part_writers,
	JUMP_TABLE( string_printable ),
	rsfn_string_printable_name };
#undef FUNCTION

#undef str
#undef str_index
/******************************* Postamble *******************************/
/**************************** Part Link Table ****************************/


static struct function_descr *(part_writers_tab[]) = {
    &string_printable_descr,
    NULL };
struct part_descr iolib_part_writers = {
    53308419,
    &module_iolib,
    part_writers_tab,
    "writers",
    0, sccsid };
#undef _MODULE_IOLIB
#undef _SCM_WRITERS
#undef _C_WRITERS
