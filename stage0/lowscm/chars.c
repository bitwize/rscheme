/**********************************************
THIS FILE WAS AUTOMATICALLY GENERATED, AND MAY
BE AUTOMATICALLY RE-GENERATED WHEN THE COMPILER
OR SOURCE CHANGES.  DO NOT MODIFY THIS FILE BY HAND!
RScheme Build (v0.7.3.4-b7u, 2007-05-30)
**********************************************/

/******************************** Preamble ********************************/

#define _MODULE_LOW_SCHEME
#define _SCM_CHARS
#define _C_CHARS
#include "low_scheme_p.h"
#include <rscheme/vinsns.h>
extern struct module_descr module_low_scheme;
extern struct part_descr low_scheme_part_chars;
static char sccsid[] = "@(#)low-scheme modules/lowscm/chars.scm [81390597] (RS v0.7.3.4-b7u, 2007-05-30)";

/************************** Function Definitions **************************/


/********************** Raw glue `char-alphabetic?' **********************/
#define ch REG0

static char rsfn_char_alphabetic_name[] = "char-alphabetic?";
#define FUNCTION rsfn_char_alphabetic_name

PROLOGUE(char_alphabetic)

BEGIN_FWD(char_alphabetic)
  FWD_MONOTONE(char_alphabetic_0)
END_FWD(char_alphabetic)

#define FPLACE_CODE (1000+0)
MONOTONE(char_alphabetic_0)
{
{
   REG0 = rb_to_bo(OBJ_ISA_ASCII_CHAR(ch) && isalpha(GET_IMMEDIATE_VALUE(ch)));
   RETURN1();
}}
#undef FPLACE_CODE

EPILOGUE(char_alphabetic)

BEGIN_BACK(char_alphabetic)
  BACK_MONOTONE(char_alphabetic_0)
END_BACK(char_alphabetic)

static struct function_descr char_alphabetic_descr = {
	&low_scheme_part_chars,
	JUMP_TABLE( char_alphabetic ),
	rsfn_char_alphabetic_name };
#undef FUNCTION

#undef ch

/************************ Raw glue `char-numeric?' ************************/
#define ch REG0

static char rsfn_char_numeric_name[] = "char-numeric?";
#define FUNCTION rsfn_char_numeric_name

PROLOGUE(char_numeric)

BEGIN_FWD(char_numeric)
  FWD_MONOTONE(char_numeric_0)
END_FWD(char_numeric)

#define FPLACE_CODE (1000+0)
MONOTONE(char_numeric_0)
{
{
   REG0 = rb_to_bo(OBJ_ISA_ASCII_CHAR(ch) && isdigit(GET_IMMEDIATE_VALUE(ch)));
   RETURN1();
}}
#undef FPLACE_CODE

EPILOGUE(char_numeric)

BEGIN_BACK(char_numeric)
  BACK_MONOTONE(char_numeric_0)
END_BACK(char_numeric)

static struct function_descr char_numeric_descr = {
	&low_scheme_part_chars,
	JUMP_TABLE( char_numeric ),
	rsfn_char_numeric_name };
#undef FUNCTION

#undef ch

/********************** Raw glue `char-whitespace?' **********************/
#define ch REG0

static char rsfn_char_whitespace_name[] = "char-whitespace?";
#define FUNCTION rsfn_char_whitespace_name

PROLOGUE(char_whitespace)

BEGIN_FWD(char_whitespace)
  FWD_MONOTONE(char_whitespace_0)
END_FWD(char_whitespace)

#define FPLACE_CODE (1000+0)
MONOTONE(char_whitespace_0)
{
{
   REG0 = rb_to_bo(OBJ_ISA_ASCII_CHAR(ch) && isspace(GET_IMMEDIATE_VALUE(ch)));
   RETURN1();
}}
#undef FPLACE_CODE

EPILOGUE(char_whitespace)

BEGIN_BACK(char_whitespace)
  BACK_MONOTONE(char_whitespace_0)
END_BACK(char_whitespace)

static struct function_descr char_whitespace_descr = {
	&low_scheme_part_chars,
	JUMP_TABLE( char_whitespace ),
	rsfn_char_whitespace_name };
#undef FUNCTION

#undef ch

/********************** Raw glue `char-upper-case?' **********************/
#define ch REG0

static char rsfn_char_upper_case_name[] = "char-upper-case?";
#define FUNCTION rsfn_char_upper_case_name

PROLOGUE(char_upper_case)

BEGIN_FWD(char_upper_case)
  FWD_MONOTONE(char_upper_case_0)
END_FWD(char_upper_case)

#define FPLACE_CODE (1000+0)
MONOTONE(char_upper_case_0)
{
{
   REG0 = rb_to_bo(OBJ_ISA_ASCII_CHAR(ch) && isupper(GET_IMMEDIATE_VALUE(ch)));
   RETURN1();
}}
#undef FPLACE_CODE

EPILOGUE(char_upper_case)

BEGIN_BACK(char_upper_case)
  BACK_MONOTONE(char_upper_case_0)
END_BACK(char_upper_case)

static struct function_descr char_upper_case_descr = {
	&low_scheme_part_chars,
	JUMP_TABLE( char_upper_case ),
	rsfn_char_upper_case_name };
#undef FUNCTION

#undef ch

/********************** Raw glue `char-lower-case?' **********************/
#define ch REG0

static char rsfn_char_lower_case_name[] = "char-lower-case?";
#define FUNCTION rsfn_char_lower_case_name

PROLOGUE(char_lower_case)

BEGIN_FWD(char_lower_case)
  FWD_MONOTONE(char_lower_case_0)
END_FWD(char_lower_case)

#define FPLACE_CODE (1000+0)
MONOTONE(char_lower_case_0)
{
{
   REG0 = rb_to_bo(OBJ_ISA_ASCII_CHAR(ch) && islower(GET_IMMEDIATE_VALUE(ch)));
   RETURN1();
}}
#undef FPLACE_CODE

EPILOGUE(char_lower_case)

BEGIN_BACK(char_lower_case)
  BACK_MONOTONE(char_lower_case_0)
END_BACK(char_lower_case)

static struct function_descr char_lower_case_descr = {
	&low_scheme_part_chars,
	JUMP_TABLE( char_lower_case ),
	rsfn_char_lower_case_name };
#undef FUNCTION

#undef ch

/*************************** Raw glue `bit-ref' ***************************/
#define value REG0
#define bit REG1

static char rsfn_bit_ref_name[] = "bit-ref";
#define FUNCTION rsfn_bit_ref_name

PROLOGUE(bit_ref)

BEGIN_FWD(bit_ref)
  FWD_MONOTONE(bit_ref_0)
END_FWD(bit_ref)

#define FPLACE_CODE (1000+0)
MONOTONE(bit_ref_0)
{
{
int i;

    COUNT_ARGS(2);
    if (!OBJ_ISA_FIXNUM(value) || !OBJ_ISA_FIXNUM(bit))
        scheme_error( "(bit-ref ~s ~s): invalid arg", 2, value, bit );

    i = fx2int(bit);
    if (i < 0 || i >= 30)
	scheme_error( "bit-ref: bit ~d is invalid", 1, bit );

    REG0 = (VAL(value) & (1 << (i+2))) ? TRUE_OBJ : FALSE_OBJ;
    RETURN(1);
}}
#undef FPLACE_CODE

EPILOGUE(bit_ref)

BEGIN_BACK(bit_ref)
  BACK_MONOTONE(bit_ref_0)
END_BACK(bit_ref)

static struct function_descr bit_ref_descr = {
	&low_scheme_part_chars,
	JUMP_TABLE( bit_ref ),
	rsfn_bit_ref_name };
#undef FUNCTION

#undef value
#undef bit

/************************** Raw glue `bit-set!' **************************/
#define value REG0
#define bit REG1
#define flag REG2

static char rsfn_bit_set_name[] = "bit-set!";
#define FUNCTION rsfn_bit_set_name

PROLOGUE(bit_set)

BEGIN_FWD(bit_set)
  FWD_MONOTONE(bit_set_0)
END_FWD(bit_set)

#define FPLACE_CODE (1000+0)
MONOTONE(bit_set_0)
{
{
int i;

    if (arg_count_reg == 2)
      flag = TRUE_OBJ;
    else
      COUNT_ARGS(3);
    if (!OBJ_ISA_FIXNUM(value) || !OBJ_ISA_FIXNUM(bit))
        scheme_error( "(bit-set! ~s ~s): invalid arg", 2, value, bit );

    i = fx2int(bit);

    if (i < 0 || i >= 30)
	scheme_error( "bit-set: bit ~d is invalid", 1, bit );

    if (truish(flag))
      REG0 = OBJ(VAL(value) | (1 << (i+2)));
    else
      REG0 = OBJ(VAL(value) & ~(1 << (i+2)));
    RETURN(1);
}}
#undef FPLACE_CODE

EPILOGUE(bit_set)

BEGIN_BACK(bit_set)
  BACK_MONOTONE(bit_set_0)
END_BACK(bit_set)

static struct function_descr bit_set_descr = {
	&low_scheme_part_chars,
	JUMP_TABLE( bit_set ),
	rsfn_bit_set_name };
#undef FUNCTION

#undef value
#undef bit
#undef flag
/******************************* Postamble *******************************/
/**************************** Part Link Table ****************************/


static struct function_descr *(part_chars_tab[]) = {
    &char_alphabetic_descr,
    &char_numeric_descr,
    &char_whitespace_descr,
    &char_upper_case_descr,
    &char_lower_case_descr,
    &bit_ref_descr,
    &bit_set_descr,
    NULL };
struct part_descr low_scheme_part_chars = {
    81390597,
    &module_low_scheme,
    part_chars_tab,
    "chars",
    0, sccsid };
#undef _MODULE_LOW_SCHEME
#undef _SCM_CHARS
#undef _C_CHARS
