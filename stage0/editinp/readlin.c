/**********************************************
THIS FILE WAS AUTOMATICALLY GENERATED, AND MAY
BE AUTOMATICALLY RE-GENERATED WHEN THE COMPILER
OR SOURCE CHANGES.  DO NOT MODIFY THIS FILE BY HAND!
RScheme Build (v0.7.3.4-b7u, 2007-05-30)
**********************************************/

/******************************** Preamble ********************************/

#define _MODULE_EDITINP
#define _SCM_READLIN
#define _C_READLIN
#include "editinp_p.h"
#include <rscheme/vinsns.h>
extern struct module_descr module_editinp;
extern struct part_descr editinp_part_readlin;
static char sccsid[] = "@(#)editinp modules/editinp/readlin.scm [244776960] (RS v0.7.3.4-b7u, 2007-05-30)";

/************************** Function Definitions **************************/


/*********************** Raw glue `input-isa-tty?' ***********************/

static char rsfn_input_isa_tty_name[] = "input-isa-tty?";
#define FUNCTION rsfn_input_isa_tty_name

PROLOGUE(input_isa_tty)

BEGIN_FWD(input_isa_tty)
  FWD_MONOTONE(input_isa_tty_0)
END_FWD(input_isa_tty)

#define FPLACE_CODE (1000+0)
MONOTONE(input_isa_tty_0)
{
{
    REG0 = rb_to_bo(rdln_isa_tty());
    RETURN1();
}}
#undef FPLACE_CODE

EPILOGUE(input_isa_tty)

BEGIN_BACK(input_isa_tty)
  BACK_MONOTONE(input_isa_tty_0)
END_BACK(input_isa_tty)

static struct function_descr input_isa_tty_descr = {
	&editinp_part_readlin,
	JUMP_TABLE( input_isa_tty ),
	rsfn_input_isa_tty_name };
#undef FUNCTION


/********************** Raw glue `readline-enabled?' **********************/

static char rsfn_readline_enabled_name[] = "readline-enabled?";
#define FUNCTION rsfn_readline_enabled_name

PROLOGUE(readline_enabled)

BEGIN_FWD(readline_enabled)
  FWD_MONOTONE(readline_enabled_0)
END_FWD(readline_enabled)

#define FPLACE_CODE (1000+0)
MONOTONE(readline_enabled_0)
{
{
    REG0 = rb_to_bo(rdln_enabled());
    RETURN1();
}}
#undef FPLACE_CODE

EPILOGUE(readline_enabled)

BEGIN_BACK(readline_enabled)
  BACK_MONOTONE(readline_enabled_0)
END_BACK(readline_enabled)

static struct function_descr readline_enabled_descr = {
	&editinp_part_readlin,
	JUMP_TABLE( readline_enabled ),
	rsfn_readline_enabled_name };
#undef FUNCTION


/******************* Raw glue `readline-add-to-history' *******************/
#define str REG0

static char rsfn_readline_add_to_history_name[] = "readline-add-to-history";
#define FUNCTION rsfn_readline_add_to_history_name

PROLOGUE(readline_add_to_history)

BEGIN_FWD(readline_add_to_history)
  FWD_MONOTONE(readline_add_to_history_0)
END_FWD(readline_add_to_history)

#define FPLACE_CODE (1000+0)
MONOTONE(readline_add_to_history_0)
{
{
    rdln_add_history( str );
    RETURN0();
}}
#undef FPLACE_CODE

EPILOGUE(readline_add_to_history)

BEGIN_BACK(readline_add_to_history)
  BACK_MONOTONE(readline_add_to_history_0)
END_BACK(readline_add_to_history)

static struct function_descr readline_add_to_history_descr = {
	&editinp_part_readlin,
	JUMP_TABLE( readline_add_to_history ),
	rsfn_readline_add_to_history_name };
#undef FUNCTION

#undef str

/********************* Raw glue `readline-read-line' *********************/
#define completions REG0
#define prompt REG1

static char rsfn_readline_read_line_name[] = "readline-read-line";
#define FUNCTION rsfn_readline_read_line_name

PROLOGUE(readline_read_line)

BEGIN_FWD(readline_read_line)
  FWD_MONOTONE(readline_read_line_0)
END_FWD(readline_read_line)

#define FPLACE_CODE (1000+0)
MONOTONE(readline_read_line_0)
{
{
    REG0 = read_console_line( completions, string_text(prompt) );
    RETURN1();
}}
#undef FPLACE_CODE

EPILOGUE(readline_read_line)

BEGIN_BACK(readline_read_line)
  BACK_MONOTONE(readline_read_line_0)
END_BACK(readline_read_line)

static struct function_descr readline_read_line_descr = {
	&editinp_part_readlin,
	JUMP_TABLE( readline_read_line ),
	rsfn_readline_read_line_name };
#undef FUNCTION

#undef completions
#undef prompt
/******************************* Postamble *******************************/
/**************************** Part Link Table ****************************/


static struct function_descr *(part_readlin_tab[]) = {
    &input_isa_tty_descr,
    &readline_enabled_descr,
    &readline_add_to_history_descr,
    &readline_read_line_descr,
    NULL };
struct part_descr editinp_part_readlin = {
    244776960,
    &module_editinp,
    part_readlin_tab,
    "readlin",
    0, sccsid };
#undef _MODULE_EDITINP
#undef _SCM_READLIN
#undef _C_READLIN
