/**********************************************
THIS FILE WAS AUTOMATICALLY GENERATED, AND MAY
BE AUTOMATICALLY RE-GENERATED WHEN THE COMPILER
OR SOURCE CHANGES.  DO NOT MODIFY THIS FILE BY HAND!
RScheme Build (v0.7.3.4-b7u, 2007-05-30)
**********************************************/

/******************************** Preamble ********************************/

#define _MODULE_CORELIB
#define _SCM_NUM2STR
#define _C_NUM2STR
#include "corelib_p.h"
#include <rscheme/vinsns.h>
extern struct module_descr module_corelib;
extern struct part_descr corelib_part_num2str;
static char sccsid[] = "@(#)corelib modules/corelib/num2str.scm [57916422] (RS v0.7.3.4-b7u, 2007-05-30)";

/************************** Function Definitions **************************/


/******************** Raw glue `double-float->string' ********************/
#define num REG0

static char rsfn_double_float_string_name[] = "double-float->string";
#define FUNCTION rsfn_double_float_string_name

PROLOGUE(double_float_string)

BEGIN_FWD(double_float_string)
  FWD_MONOTONE(double_float_string_0)
END_FWD(double_float_string)

#define FPLACE_CODE (1000+0)
MONOTONE(double_float_string_0)
{
{
char temp[200];

    REG0 = make_string( double_float_to_string( temp, num ) );
    RETURN(1);
}}
#undef FPLACE_CODE

EPILOGUE(double_float_string)

BEGIN_BACK(double_float_string)
  BACK_MONOTONE(double_float_string_0)
END_BACK(double_float_string)

static struct function_descr double_float_string_descr = {
	&corelib_part_num2str,
	JUMP_TABLE( double_float_string ),
	rsfn_double_float_string_name };
#undef FUNCTION

#undef num

/*********************** Raw glue `fixnum->string' ***********************/
#define num REG0
#define radix REG1

static char rsfn_fixnum_string_name[] = "fixnum->string";
#define FUNCTION rsfn_fixnum_string_name

PROLOGUE(fixnum_string)

BEGIN_FWD(fixnum_string)
  FWD_MONOTONE(fixnum_string_0)
END_FWD(fixnum_string)

#define FPLACE_CODE (1000+0)
MONOTONE(fixnum_string_0)
{
{
  char temp[WORD_SIZE_BITS+10];

  REG0 = make_string( fixnum_to_string( &temp[WORD_SIZE_BITS+10], 
					num, 
					fx2int(radix) ) );
  RETURN(1);
}}
#undef FPLACE_CODE

EPILOGUE(fixnum_string)

BEGIN_BACK(fixnum_string)
  BACK_MONOTONE(fixnum_string_0)
END_BACK(fixnum_string)

static struct function_descr fixnum_string_descr = {
	&corelib_part_num2str,
	JUMP_TABLE( fixnum_string ),
	rsfn_fixnum_string_name };
#undef FUNCTION

#undef num
#undef radix

/********************** Raw glue `long-int->string' **********************/
#define num REG0
#define radix REG1

static char rsfn_long_int_string_name[] = "long-int->string";
#define FUNCTION rsfn_long_int_string_name

PROLOGUE(long_int_string)

BEGIN_FWD(long_int_string)
  FWD_MONOTONE(long_int_string_0)
END_FWD(long_int_string)

#define FPLACE_CODE (1000+0)
MONOTONE(long_int_string_0)
{
{
char temp[70];

    REG0 = make_string( int_64_to_string( &temp[70], 
					  *(INT_64*)PTR_TO_DATAPTR(num), 
					  fx2int(radix) ) );
    RETURN(1);
}}
#undef FPLACE_CODE

EPILOGUE(long_int_string)

BEGIN_BACK(long_int_string)
  BACK_MONOTONE(long_int_string_0)
END_BACK(long_int_string)

static struct function_descr long_int_string_descr = {
	&corelib_part_num2str,
	JUMP_TABLE( long_int_string ),
	rsfn_long_int_string_name };
#undef FUNCTION

#undef num
#undef radix

/*********************** Raw glue `bignum->string' ***********************/
#define num REG0
#define radix REG1

static char rsfn_bignum_string_name[] = "bignum->string";
#define FUNCTION rsfn_bignum_string_name

PROLOGUE(bignum_string)

BEGIN_FWD(bignum_string)
  FWD_MONOTONE(bignum_string_0)
END_FWD(bignum_string)

#define FPLACE_CODE (1000+0)
MONOTONE(bignum_string_0)
{
{
    REG0 = bignum_to_string_obj(num, fx2int(radix));
    RETURN(1);
}}
#undef FPLACE_CODE

EPILOGUE(bignum_string)

BEGIN_BACK(bignum_string)
  BACK_MONOTONE(bignum_string_0)
END_BACK(bignum_string)

static struct function_descr bignum_string_descr = {
	&corelib_part_num2str,
	JUMP_TABLE( bignum_string ),
	rsfn_bignum_string_name };
#undef FUNCTION

#undef num
#undef radix
/******************************* Postamble *******************************/
/**************************** Part Link Table ****************************/


static struct function_descr *(part_num2str_tab[]) = {
    &double_float_string_descr,
    &fixnum_string_descr,
    &long_int_string_descr,
    &bignum_string_descr,
    NULL };
struct part_descr corelib_part_num2str = {
    57916422,
    &module_corelib,
    part_num2str_tab,
    "num2str",
    0, sccsid };
#undef _MODULE_CORELIB
#undef _SCM_NUM2STR
#undef _C_NUM2STR
