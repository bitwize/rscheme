/**********************************************
THIS FILE WAS AUTOMATICALLY GENERATED, AND MAY
BE AUTOMATICALLY RE-GENERATED WHEN THE COMPILER
OR SOURCE CHANGES.  DO NOT MODIFY THIS FILE BY HAND!
RScheme Build (v0.7.3.4-b7u, 2007-05-30)
**********************************************/

/******************************** Preamble ********************************/

#define _MODULE_CORELIB
#define _SCM_KEYWORDS
#define _C_KEYWORDS
#include "corelib_p.h"
#include <rscheme/vinsns.h>
extern struct module_descr module_corelib;
extern struct part_descr corelib_part_keywords;
static char sccsid[] = "@(#)corelib modules/corelib/keywords.scm [161096714] (RS v0.7.3.4-b7u, 2007-05-30)";

/************************** Function Definitions **************************/


/************************** Function `keyword?' **************************/
static char rsfn_keyword_name[] = "keyword?";
#define FUNCTION rsfn_keyword_name

PROLOGUE(keyword)

BEGIN_FWD(keyword)
  FWD_MONOTONE(keyword_0)
  FWD_MONOTONE(keyword_1)
END_FWD(keyword)

#define FPLACE_CODE (1000+0)
MONOTONE(keyword_0)
{
    COUNT_ARGS(1);
    /* NOP: REG0 = REG0; */
    if (SYMBOL_P(REG0 /* x */))
    {
	    REG1 = symbol_str(CHECK_SYMBOL(REG0 /* x */));
	    SAVE_CONT2(keyword_1);
	    REG0 = REG1 /* s */;
	    APPLYG(1,TLREFB(0) /* string-length */);
    }
    else
    {
	    REG0 = FALSE_OBJ;
	    RETURN1();
    }
}
#undef FPLACE_CODE

#define FPLACE_CODE (1000+1)
MONOTONE(keyword_1)
{
    REG2 = REG0;
    RESTORE_CONT2();
    /* NOP: REG2 = REG2; */
    /* NOP: REG2 = REG2; */
    CHECK_FIXNUM(REG2 /* n */);
    /* NOP: REG2 = REG2; */
    if (FX_GT(REG2 /* n */,int2fx(1)))
    {
	    REG0 = rb_to_bo(EQ(int2fx(bvec_read_uint8(REG1 /* s */,fx2int(SUB1(REG2 /* n */)))),int2fx(58)));
	    RETURN1();
    }
    else
    {
	    REG0 = FALSE_OBJ;
	    RETURN1();
    }
}
#undef FPLACE_CODE

EPILOGUE(keyword)

BEGIN_BACK(keyword)
  BACK_MONOTONE(keyword_0)
  BACK_MONOTONE(keyword_1)
END_BACK(keyword)

static struct function_descr keyword_descr = {
	&corelib_part_keywords,
	JUMP_TABLE( keyword ),
	rsfn_keyword_name };
#undef FUNCTION


/**************************** Function `flag?' ****************************/
static char rsfn_flag_name[] = "flag?";
#define FUNCTION rsfn_flag_name

PROLOGUE(flag)

BEGIN_FWD(flag)
  FWD_MONOTONE(flag_0)
END_FWD(flag)

#define FPLACE_CODE (1000+0)
MONOTONE(flag_0)
{
    COUNT_ARGS(1);
    /* NOP: REG0 = REG0; */
    if (SYMBOL_P(REG0 /* x */))
    {
	    REG0 = rb_to_bo(EQ(int2fx(bvec_read_uint8(symbol_str(CHECK_SYMBOL(REG0 /* x */)),0)),int2fx(58)));
	    RETURN1();
    }
    else
    {
	    REG0 = FALSE_OBJ;
	    RETURN1();
    }
}
#undef FPLACE_CODE

EPILOGUE(flag)

BEGIN_BACK(flag)
  BACK_MONOTONE(flag_0)
END_BACK(flag)

static struct function_descr flag_descr = {
	&corelib_part_keywords,
	JUMP_TABLE( flag ),
	rsfn_flag_name };
#undef FUNCTION


/*********************** Function `keyword->symbol' ***********************/
static char rsfn_keyword_symbol_name[] = "keyword->symbol";
#define FUNCTION rsfn_keyword_symbol_name

PROLOGUE(keyword_symbol)

BEGIN_FWD(keyword_symbol)
  FWD_MONOTONE(keyword_symbol_0)
  FWD_MONOTONE(keyword_symbol_1)
  FWD_MONOTONE(keyword_symbol_2)
  FWD_MONOTONE(keyword_symbol_3)
  FWD_MONOTONE(keyword_symbol_4)
  FWD_MONOTONE(keyword_symbol_5)
END_FWD(keyword_symbol)

#define FPLACE_CODE (1000+0)
MONOTONE(keyword_symbol_0)
{
    COUNT_ARGS(1);
    /* NOP: REG0 = REG0; */
    SAVE_CONT1(keyword_symbol_1);
    /* NOP: REG0 = REG0; */
    APPLYF(1,TLREFB(0) /* keyword? */);
}
#undef FPLACE_CODE

#define FPLACE_CODE (1000+1)
MONOTONE(keyword_symbol_1)
{
    REG1 = REG0;
    RESTORE_CONT1();
    if (NOT(REG1))
    {
	    SAVE_CONT1(keyword_symbol_2);
	    REG0 = LITERAL(1) /* "assertion failed in ~s: ~s" */;
	    REG1 = LITERAL(2) /* (assert keyword->symbol) */;
	    REG2 = LITERAL(3) /* (keyword? x) */;
	    APPLYF(3,TLREFB(4) /* error */);
    }
    else
    {
	    JUMP(1,keyword_symbol_3);
    }
}
#undef FPLACE_CODE

#define FPLACE_CODE (1000+2)
MONOTONE(keyword_symbol_2)
{
    RESTORE_CONT1();
    JUMP(1,keyword_symbol_3);
}
#undef FPLACE_CODE

#define FPLACE_CODE (1000+3)
MONOTONE(keyword_symbol_3)
{
    REG1 = symbol_str(CHECK_SYMBOL(REG0 /* x */));
    SAVE_CONT2(keyword_symbol_5);
    REG2 = REG1 /* s */;
    SAVE_CONT3(keyword_symbol_4);
    REG0 = REG1 /* s */;
    APPLYG(1,TLREFB(5) /* string-length */);
}
#undef FPLACE_CODE

#define FPLACE_CODE (1000+4)
MONOTONE(keyword_symbol_4)
{
    REG3 = REG0;
    RESTORE_CONT3();
    REG3 = SUB1(CHECK_FIXNUM(REG3));
    REG0 = REG2;
    REG1 = int2fx(0);
    REG2 = REG3;
    APPLYF(3,TLREFB(6) /* substring */);
}
#undef FPLACE_CODE

#define FPLACE_CODE (1000+5)
MONOTONE(keyword_symbol_5)
{
    REG2 = REG0;
    RESTORE_CONT2();
    REG0 = intern(CHECK_STRING(REG2));
    RETURN1();
}
#undef FPLACE_CODE

EPILOGUE(keyword_symbol)

BEGIN_BACK(keyword_symbol)
  BACK_MONOTONE(keyword_symbol_0)
  BACK_MONOTONE(keyword_symbol_1)
  BACK_MONOTONE(keyword_symbol_2)
  BACK_MONOTONE(keyword_symbol_3)
  BACK_MONOTONE(keyword_symbol_4)
  BACK_MONOTONE(keyword_symbol_5)
END_BACK(keyword_symbol)

static struct function_descr keyword_symbol_descr = {
	&corelib_part_keywords,
	JUMP_TABLE( keyword_symbol ),
	rsfn_keyword_symbol_name };
#undef FUNCTION


/*********************** Function `symbol->keyword' ***********************/
static char rsfn_symbol_keyword_name[] = "symbol->keyword";
#define FUNCTION rsfn_symbol_keyword_name

PROLOGUE(symbol_keyword)

BEGIN_FWD(symbol_keyword)
  FWD_MONOTONE(symbol_keyword_0)
  FWD_MONOTONE(symbol_keyword_1)
END_FWD(symbol_keyword)

#define FPLACE_CODE (1000+0)
MONOTONE(symbol_keyword_0)
{
    COUNT_ARGS(1);
    /* NOP: REG0 = REG0; */
    CHECK_SYMBOL(REG0 /* s */);
    SAVE_CONT1(symbol_keyword_1);
    REG0 = symbol_str(REG0 /* s */);
    REG1 = LITERAL(0) /* ":" */;
    APPLYF(2,TLREFB(1) /* string-append */);
}
#undef FPLACE_CODE

#define FPLACE_CODE (1000+1)
MONOTONE(symbol_keyword_1)
{
    REG1 = REG0;
    RESTORE_CONT1();
    REG0 = intern(CHECK_STRING(REG1));
    RETURN1();
}
#undef FPLACE_CODE

EPILOGUE(symbol_keyword)

BEGIN_BACK(symbol_keyword)
  BACK_MONOTONE(symbol_keyword_0)
  BACK_MONOTONE(symbol_keyword_1)
END_BACK(symbol_keyword)

static struct function_descr symbol_keyword_descr = {
	&corelib_part_keywords,
	JUMP_TABLE( symbol_keyword ),
	rsfn_symbol_keyword_name };
#undef FUNCTION


/************************ Function `flag->symbol' ************************/
static char rsfn_flag_symbol_name[] = "flag->symbol";
#define FUNCTION rsfn_flag_symbol_name

PROLOGUE(flag_symbol)

BEGIN_FWD(flag_symbol)
  FWD_MONOTONE(flag_symbol_0)
  FWD_MONOTONE(flag_symbol_1)
  FWD_MONOTONE(flag_symbol_2)
  FWD_MONOTONE(flag_symbol_3)
  FWD_MONOTONE(flag_symbol_4)
END_FWD(flag_symbol)

#define FPLACE_CODE (1000+0)
MONOTONE(flag_symbol_0)
{
    COUNT_ARGS(1);
    /* NOP: REG0 = REG0; */
    SAVE_CONT1(flag_symbol_1);
    /* NOP: REG0 = REG0; */
    APPLYF(1,TLREFB(0) /* flag? */);
}
#undef FPLACE_CODE

#define FPLACE_CODE (1000+1)
MONOTONE(flag_symbol_1)
{
    REG1 = REG0;
    RESTORE_CONT1();
    if (NOT(REG1))
    {
	    SAVE_CONT1(flag_symbol_2);
	    REG0 = LITERAL(1) /* "assertion failed in ~s: ~s" */;
	    REG1 = LITERAL(2) /* (assert flag->symbol) */;
	    REG2 = LITERAL(3) /* (flag? x) */;
	    APPLYF(3,TLREFB(4) /* error */);
    }
    else
    {
	    JUMP(1,flag_symbol_3);
    }
}
#undef FPLACE_CODE

#define FPLACE_CODE (1000+2)
MONOTONE(flag_symbol_2)
{
    RESTORE_CONT1();
    JUMP(1,flag_symbol_3);
}
#undef FPLACE_CODE

#define FPLACE_CODE (1000+3)
MONOTONE(flag_symbol_3)
{
    REG1 = symbol_str(CHECK_SYMBOL(REG0 /* x */));
    SAVE_CONT2(flag_symbol_4);
    REG0 = REG1 /* s */;
    REG1 = int2fx(1);
    APPLYF(2,TLREFB(5) /* substring */);
}
#undef FPLACE_CODE

#define FPLACE_CODE (1000+4)
MONOTONE(flag_symbol_4)
{
    REG2 = REG0;
    RESTORE_CONT2();
    REG0 = intern(CHECK_STRING(REG2));
    RETURN1();
}
#undef FPLACE_CODE

EPILOGUE(flag_symbol)

BEGIN_BACK(flag_symbol)
  BACK_MONOTONE(flag_symbol_0)
  BACK_MONOTONE(flag_symbol_1)
  BACK_MONOTONE(flag_symbol_2)
  BACK_MONOTONE(flag_symbol_3)
  BACK_MONOTONE(flag_symbol_4)
END_BACK(flag_symbol)

static struct function_descr flag_symbol_descr = {
	&corelib_part_keywords,
	JUMP_TABLE( flag_symbol ),
	rsfn_flag_symbol_name };
#undef FUNCTION


/************************ Function `symbol->flag' ************************/
static char rsfn_symbol_flag_name[] = "symbol->flag";
#define FUNCTION rsfn_symbol_flag_name

PROLOGUE(symbol_flag)

BEGIN_FWD(symbol_flag)
  FWD_MONOTONE(symbol_flag_0)
  FWD_MONOTONE(symbol_flag_1)
END_FWD(symbol_flag)

#define FPLACE_CODE (1000+0)
MONOTONE(symbol_flag_0)
{
    COUNT_ARGS(1);
    /* NOP: REG0 = REG0; */
    CHECK_SYMBOL(REG0 /* s */);
    SAVE_CONT1(symbol_flag_1);
    REG1 = symbol_str(REG0 /* s */);
    REG0 = LITERAL(0) /* ":" */;
    APPLYF(2,TLREFB(1) /* string-append */);
}
#undef FPLACE_CODE

#define FPLACE_CODE (1000+1)
MONOTONE(symbol_flag_1)
{
    REG1 = REG0;
    RESTORE_CONT1();
    REG0 = intern(CHECK_STRING(REG1));
    RETURN1();
}
#undef FPLACE_CODE

EPILOGUE(symbol_flag)

BEGIN_BACK(symbol_flag)
  BACK_MONOTONE(symbol_flag_0)
  BACK_MONOTONE(symbol_flag_1)
END_BACK(symbol_flag)

static struct function_descr symbol_flag_descr = {
	&corelib_part_keywords,
	JUMP_TABLE( symbol_flag ),
	rsfn_symbol_flag_name };
#undef FUNCTION

/******************************* Postamble *******************************/
/**************************** Part Link Table ****************************/


static struct function_descr *(part_keywords_tab[]) = {
    &keyword_descr,
    &flag_descr,
    &keyword_symbol_descr,
    &symbol_keyword_descr,
    &flag_symbol_descr,
    &symbol_flag_descr,
    NULL };
struct part_descr corelib_part_keywords = {
    161096714,
    &module_corelib,
    part_keywords_tab,
    "keywords",
    0, sccsid };
#undef _MODULE_CORELIB
#undef _SCM_KEYWORDS
#undef _C_KEYWORDS
