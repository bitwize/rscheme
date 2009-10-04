/**********************************************
THIS FILE WAS AUTOMATICALLY GENERATED, AND MAY
BE AUTOMATICALLY RE-GENERATED WHEN THE COMPILER
OR SOURCE CHANGES.  DO NOT MODIFY THIS FILE BY HAND!
RScheme Build (v0.7.3.4-b7u, 2007-05-30)
**********************************************/

/******************************** Preamble ********************************/

#define _MODULE_OBJSYS
#define _SCM_GETNSET
#define _C_GETNSET
#include "objsys_p.h"
#include <rscheme/vinsns.h>
extern struct module_descr module_objsys;
extern struct part_descr objsys_part_getnset;
static char sccsid[] = "@(#)objsys modules/objsys/getnset.scm [373492738] (RS v0.7.3.4-b7u, 2007-05-30)";

/************************** Function Definitions **************************/


/*********************** Raw glue `getter-template' ***********************/
#define self REG0

static char rsfn_getter_template_name[] = "getter-template";
#define FUNCTION rsfn_getter_template_name

PROLOGUE(getter_template)

BEGIN_FWD(getter_template)
  FWD_MONOTONE(getter_template_0)
END_FWD(getter_template)

#define FPLACE_CODE (1000+0)
MONOTONE(getter_template_0)
{
{
obj slotnum;

    USE_FUNCTION_ENVT();
    COUNT_ARGS(1);
    slotnum = LEXREF0(0);
    assert( OBJ_ISA_PTR(self) 
	    && FXWORDS_TO_RIBYTES(slotnum) < SIZEOF_PTR(self) );
    REG0 = gvec_read( self, FXWORDS_TO_RIBYTES(slotnum) );
    RETURN1();
}}
#undef FPLACE_CODE

EPILOGUE(getter_template)

BEGIN_BACK(getter_template)
  BACK_MONOTONE(getter_template_0)
END_BACK(getter_template)

static struct function_descr getter_template_descr = {
	&objsys_part_getnset,
	JUMP_TABLE( getter_template ),
	rsfn_getter_template_name };
#undef FUNCTION

#undef self

/*********************** Raw glue `setter-template' ***********************/
#define self REG0

static char rsfn_setter_template_name[] = "setter-template";
#define FUNCTION rsfn_setter_template_name

PROLOGUE(setter_template)

BEGIN_FWD(setter_template)
  FWD_MONOTONE(setter_template_0)
END_FWD(setter_template)

#define FPLACE_CODE (1000+0)
MONOTONE(setter_template_0)
{
{
obj slotnum;

    USE_FUNCTION_ENVT();
    COUNT_ARGS(2);
    slotnum = LEXREF0(0);
    assert( OBJ_ISA_PTR(self) 
	    && FXWORDS_TO_RIBYTES(slotnum) < SIZEOF_PTR(self) );
    gvec_write( self, FXWORDS_TO_RIBYTES(slotnum), REG1 );
    REG0 = REG1;
    RETURN1();
}}
#undef FPLACE_CODE

EPILOGUE(setter_template)

BEGIN_BACK(setter_template)
  BACK_MONOTONE(setter_template_0)
END_BACK(setter_template)

static struct function_descr setter_template_descr = {
	&objsys_part_getnset,
	JUMP_TABLE( setter_template ),
	rsfn_setter_template_name };
#undef FUNCTION

#undef self

/***************** Raw glue `restricted-setter-template' *****************/
#define self REG0

static char rsfn_restricted_setter_template_name[] = "restricted-setter-template";
#define FUNCTION rsfn_restricted_setter_template_name

PROLOGUE(restricted_setter_template)

BEGIN_FWD(restricted_setter_template)
  FWD_MONOTONE(restricted_setter_template_0)
END_FWD(restricted_setter_template)

#define FPLACE_CODE (1000+0)
MONOTONE(restricted_setter_template_0)
{
{
obj slotnum;
obj setter;

    setter = envt_reg;
    USE_FUNCTION_ENVT();
    COUNT_ARGS(2);
    slotnum = LEXREF0(0);
    assert( OBJ_ISA_PTR(self) 
	    && FXWORDS_TO_RIBYTES(slotnum) < SIZEOF_PTR(self) );
    if (instance_p( REG1, LEXREF0(1) ))
     {
       gvec_write( self, FXWORDS_TO_RIBYTES(slotnum), REG1 );
       REG0 = REG1;
       RETURN1();
     }
    else
     {
       REG2 = REG0;
       REG0 = setter;
       APPLYF(3,TLREF(0));
     }
}}
#undef FPLACE_CODE

EPILOGUE(restricted_setter_template)

BEGIN_BACK(restricted_setter_template)
  BACK_MONOTONE(restricted_setter_template_0)
END_BACK(restricted_setter_template)

static struct function_descr restricted_setter_template_descr = {
	&objsys_part_getnset,
	JUMP_TABLE( restricted_setter_template ),
	rsfn_restricted_setter_template_name };
#undef FUNCTION

#undef self
/******************************* Postamble *******************************/
/**************************** Part Link Table ****************************/


static struct function_descr *(part_getnset_tab[]) = {
    &getter_template_descr,
    &setter_template_descr,
    &restricted_setter_template_descr,
    NULL };
struct part_descr objsys_part_getnset = {
    373492738,
    &module_objsys,
    part_getnset_tab,
    "getnset",
    0, sccsid };
#undef _MODULE_OBJSYS
#undef _SCM_GETNSET
#undef _C_GETNSET
