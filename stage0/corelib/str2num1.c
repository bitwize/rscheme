/**********************************************
THIS FILE WAS AUTOMATICALLY GENERATED, AND MAY
BE AUTOMATICALLY RE-GENERATED WHEN THE COMPILER
OR SOURCE CHANGES.  DO NOT MODIFY THIS FILE BY HAND!
RScheme Build (v0.7.3.4-b7u, 2007-05-30)
**********************************************/

/******************************** Preamble ********************************/

#define _MODULE_CORELIB
#define _SCM_STR2NUM
#define _C_STR2NUM
#include "corelib_p.h"
#include <rscheme/vinsns.h>
extern struct module_descr module_corelib;
extern struct part_descr corelib_part_str2num;
static char sccsid[] = "@(#)corelib modules/corelib/str2num.scm [413962245] (RS v0.7.3.4-b7u, 2007-05-30)";

/************************** Function Definitions **************************/


/*********************** Raw glue `string->fixnum' ***********************/
#define str REG0
#define radix REG1

static char rsfn_string_fixnum_name[] = "string->fixnum";
#define FUNCTION rsfn_string_fixnum_name

PROLOGUE(string_fixnum)

BEGIN_FWD(string_fixnum)
  FWD_MONOTONE(string_fixnum_0)
END_FWD(string_fixnum)

#define FPLACE_CODE (1000+0)
MONOTONE(string_fixnum_0)
{
{
  REG0 = string_to_fixnum( string_text(str), 
			   string_length(str),
			   fx2int(radix) );
  RETURN1();
}}
#undef FPLACE_CODE

EPILOGUE(string_fixnum)

BEGIN_BACK(string_fixnum)
  BACK_MONOTONE(string_fixnum_0)
END_BACK(string_fixnum)

static struct function_descr string_fixnum_descr = {
	&corelib_part_str2num,
	JUMP_TABLE( string_fixnum ),
	rsfn_string_fixnum_name };
#undef FUNCTION

#undef str
#undef radix

/************************ Raw glue `string->float' ************************/
#define str REG0
#define radix REG1

static char rsfn_string_float_name[] = "string->float";
#define FUNCTION rsfn_string_float_name

PROLOGUE(string_float)

BEGIN_FWD(string_float)
  FWD_MONOTONE(string_float_0)
END_FWD(string_float)

#define FPLACE_CODE (1000+0)
MONOTONE(string_float_0)
{
{
  REG0 = string_to_float( string_text(str), 
			  string_length(str),
			  fx2int(radix) );
  RETURN1();
}}
#undef FPLACE_CODE

EPILOGUE(string_float)

BEGIN_BACK(string_float)
  BACK_MONOTONE(string_float_0)
END_BACK(string_float)

static struct function_descr string_float_descr = {
	&corelib_part_str2num,
	JUMP_TABLE( string_float ),
	rsfn_string_float_name };
#undef FUNCTION

#undef str
#undef radix

/********************** Raw glue `string->long-int' **********************/
#define str REG0
#define radix REG1

static char rsfn_string_long_int_name[] = "string->long-int";
#define FUNCTION rsfn_string_long_int_name

PROLOGUE(string_long_int)

BEGIN_FWD(string_long_int)
  FWD_MONOTONE(string_long_int_0)
END_FWD(string_long_int)

#define FPLACE_CODE (1000+0)
MONOTONE(string_long_int_0)
{
{
  INT_64 v;
  rs_bool ok;

  ok = string_to_int_64( string_text(str), 
			 string_length(str),
			 fx2int(radix),
			 &v );
  REG0 = ok ? make_long_int(v) : FALSE_OBJ;
  RETURN1();
}}
#undef FPLACE_CODE

EPILOGUE(string_long_int)

BEGIN_BACK(string_long_int)
  BACK_MONOTONE(string_long_int_0)
END_BACK(string_long_int)

static struct function_descr string_long_int_descr = {
	&corelib_part_str2num,
	JUMP_TABLE( string_long_int ),
	rsfn_string_long_int_name };
#undef FUNCTION

#undef str
#undef radix

/*********************** Raw glue `string->bignum' ***********************/
#define str REG0
#define radix REG1

static char rsfn_string_bignum_name[] = "string->bignum";
#define FUNCTION rsfn_string_bignum_name

PROLOGUE(string_bignum)

BEGIN_FWD(string_bignum)
  FWD_MONOTONE(string_bignum_0)
END_FWD(string_bignum)

#define FPLACE_CODE (1000+0)
MONOTONE(string_bignum_0)
{
{
#if !FULL_NUMERIC_TOWER
    REG0 = FALSE_OBJ;
    RETURN1();
#else
  REG0 = string_to_bignum_obj( string_text(str), 
			       fx2int(radix));
  RETURN1();
#endif
}}
#undef FPLACE_CODE

EPILOGUE(string_bignum)

BEGIN_BACK(string_bignum)
  BACK_MONOTONE(string_bignum_0)
END_BACK(string_bignum)

static struct function_descr string_bignum_descr = {
	&corelib_part_str2num,
	JUMP_TABLE( string_bignum ),
	rsfn_string_bignum_name };
#undef FUNCTION

#undef str
#undef radix

/********************** Raw glue `string->rational' **********************/
#define str REG0
#define radix REG1

static char rsfn_string_rational_name[] = "string->rational";
#define FUNCTION rsfn_string_rational_name

PROLOGUE(string_rational)

BEGIN_FWD(string_rational)
  FWD_MONOTONE(string_rational_0)
END_FWD(string_rational)

#define FPLACE_CODE (1000+0)
MONOTONE(string_rational_0)
{
{
#if !FULL_NUMERIC_TOWER
    REG0 = FALSE_OBJ;
    RETURN1();
#else
  REG0 = string_to_rational_obj( string_text(str), 
				 fx2int(radix));
  RETURN1();
#endif
}}
#undef FPLACE_CODE

EPILOGUE(string_rational)

BEGIN_BACK(string_rational)
  BACK_MONOTONE(string_rational_0)
END_BACK(string_rational)

static struct function_descr string_rational_descr = {
	&corelib_part_str2num,
	JUMP_TABLE( string_rational ),
	rsfn_string_rational_name };
#undef FUNCTION

#undef str
#undef radix

/************************ Raw glue `sprintf-float' ************************/
#define raw_fmt REG0
#define raw_len REG1
#define raw_num REG2

static char rsfn_sprintf_float_name[] = "sprintf-float";
#define FUNCTION rsfn_sprintf_float_name

PROLOGUE(sprintf_float)

BEGIN_FWD(sprintf_float)
  FWD_MONOTONE(sprintf_float_0)
END_FWD(sprintf_float)

#define FPLACE_CODE (1000+0)
MONOTONE(sprintf_float_0)
{  char *fmt;
  int len;
  obj num;
  COUNT_ARGS(3);
  if (!STRING_P(raw_fmt))
    {
      obj c;
      c = make5( TLREFB(0),
                 NIL_OBJ,  /* properties */
                 lookup_symbol( FUNCTION ),
                 cons( raw_fmt, NIL_OBJ ),
                 lookup_symbol( "fmt" ),
                 TLREFB(1) );
      raise_error( c );
    }
  fmt = (char *)string_text(raw_fmt);

  len = basic_raw_int(raw_len);

  if (!instance_p(raw_num,TLREFB(2)))
    {
      obj c;
      c = make5( TLREFB(0),
                 NIL_OBJ,  /* properties */
                 lookup_symbol( FUNCTION ),
                 cons( raw_num, NIL_OBJ ),
                 lookup_symbol( "num" ),
                 TLREFB(2) );
      raise_error( c );
    }
  num = raw_num;


{
  double x = extract_float(num);
  REG0 = c_vprintf( fmt, len, x );
  RETURN1();
}}
#undef FPLACE_CODE

EPILOGUE(sprintf_float)

BEGIN_BACK(sprintf_float)
  BACK_MONOTONE(sprintf_float_0)
END_BACK(sprintf_float)

static struct function_descr sprintf_float_descr = {
	&corelib_part_str2num,
	JUMP_TABLE( sprintf_float ),
	rsfn_sprintf_float_name };
#undef FUNCTION

#undef raw_fmt
#undef raw_len
#undef raw_num

/*********************** Raw glue `sprintf-fixnum' ***********************/
#define raw_fmt REG0
#define raw_len REG1
#define raw_num REG2

static char rsfn_sprintf_fixnum_name[] = "sprintf-fixnum";
#define FUNCTION rsfn_sprintf_fixnum_name

PROLOGUE(sprintf_fixnum)

BEGIN_FWD(sprintf_fixnum)
  FWD_MONOTONE(sprintf_fixnum_0)
END_FWD(sprintf_fixnum)

#define FPLACE_CODE (1000+0)
MONOTONE(sprintf_fixnum_0)
{  char *fmt;
  int len;
  int num;
  COUNT_ARGS(3);
  if (!STRING_P(raw_fmt))
    {
      obj c;
      c = make5( TLREFB(0),
                 NIL_OBJ,  /* properties */
                 lookup_symbol( FUNCTION ),
                 cons( raw_fmt, NIL_OBJ ),
                 lookup_symbol( "fmt" ),
                 TLREFB(1) );
      raise_error( c );
    }
  fmt = (char *)string_text(raw_fmt);

  len = basic_raw_int(raw_len);

  num = basic_raw_int(raw_num);


{
  REG0 = c_vprintf( fmt, len, num );
  RETURN1();
}}
#undef FPLACE_CODE

EPILOGUE(sprintf_fixnum)

BEGIN_BACK(sprintf_fixnum)
  BACK_MONOTONE(sprintf_fixnum_0)
END_BACK(sprintf_fixnum)

static struct function_descr sprintf_fixnum_descr = {
	&corelib_part_str2num,
	JUMP_TABLE( sprintf_fixnum ),
	rsfn_sprintf_fixnum_name };
#undef FUNCTION

#undef raw_fmt
#undef raw_len
#undef raw_num
/******************************* Postamble *******************************/
/**************************** Part Link Table ****************************/


static struct function_descr *(part_str2num_tab[]) = {
    &string_fixnum_descr,
    &string_float_descr,
    &string_long_int_descr,
    &string_bignum_descr,
    &string_rational_descr,
    &sprintf_float_descr,
    &sprintf_fixnum_descr,
    NULL };
struct part_descr corelib_part_str2num = {
    413962245,
    &module_corelib,
    part_str2num_tab,
    "str2num",
    0, sccsid };
#undef _MODULE_CORELIB
#undef _SCM_STR2NUM
#undef _C_STR2NUM
