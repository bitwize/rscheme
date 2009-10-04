/**********************************************
THIS FILE WAS AUTOMATICALLY GENERATED, AND MAY
BE AUTOMATICALLY RE-GENERATED WHEN THE COMPILER
OR SOURCE CHANGES.  DO NOT MODIFY THIS FILE BY HAND!
RScheme Build (v0.7.3.4-b7u, 2007-05-30)
**********************************************/

/******************************** Preamble ********************************/

#define _MODULE_LOW_SCHEME
#define _SCM_VECTOPS
#define _C_VECTOPS
#include "low_scheme_p.h"
#include <rscheme/vinsns.h>
extern struct module_descr module_low_scheme;
extern struct part_descr low_scheme_part_vectops;
static char sccsid[] = "@(#)low-scheme modules/lowscm/vectops.scm [237344774] (RS v0.7.3.4-b7u, 2007-05-30)";

/************************** Function Definitions **************************/


/*********************** Raw glue `vector-bool-op' ***********************/
#define vec1 REG0
#define vec2 REG1
#define op REG2
#define newv REG3

static char rsfn_vector_bool_op_name[] = "vector-bool-op";
#define FUNCTION rsfn_vector_bool_op_name

PROLOGUE(vector_bool_op)

BEGIN_FWD(vector_bool_op)
  FWD_MONOTONE(vector_bool_op_0)
END_FWD(vector_bool_op)

#define FPLACE_CODE (1000+0)
MONOTONE(vector_bool_op_0)
{
{
char c;
int b, opcode = fx2int(op);
UINT_32 i, len;
obj dest;

    COUNT_ARGS(4);
    if(!(VECTOR_P(vec1) && VECTOR_P(vec2) 
    	 && OBJ_ISA_FIXNUM(op) && OBJ_ISA_BOOLEAN(newv)))
    {
	scheme_error( "vector-bool-op: bad arg", 0 );
    }
    len = SIZEOF_PTR(vec1);
    if (SIZEOF_PTR(vec2) < len)
	len = SIZEOF_PTR(vec2);
	
    if (truish(newv))
	dest = alloc( len, vector_class );
    else
	dest = vec1;
    for (i=0; i<len; i+=SLOT(1))
    {
	if (EQ(gvec_read(vec1,i),FALSE_OBJ))
	{
	    if (EQ(gvec_read(vec2,i),FALSE_OBJ))
		b = opcode & 1;
	    else
		b = opcode & 2;
	}
	else
	{
	    if (EQ(gvec_read(vec2,i),FALSE_OBJ))
		b = opcode & 4;
	    else
		b = opcode & 8;
	}
	if (truish(newv))
  	    gvec_write_init_non_ptr( dest, i, b ? TRUE_OBJ : FALSE_OBJ );
        else
	    gvec_write_non_ptr( dest, i, b ? TRUE_OBJ : FALSE_OBJ );
    }
    REG0 = dest;
    RETURN(1);
}}
#undef FPLACE_CODE

EPILOGUE(vector_bool_op)

BEGIN_BACK(vector_bool_op)
  BACK_MONOTONE(vector_bool_op_0)
END_BACK(vector_bool_op)

static struct function_descr vector_bool_op_descr = {
	&low_scheme_part_vectops,
	JUMP_TABLE( vector_bool_op ),
	rsfn_vector_bool_op_name };
#undef FUNCTION

#undef vec1
#undef vec2
#undef op
#undef newv

/**************************** Function `vassq' ****************************/
static char rsfn_vassq_name[] = "vassq";
#define FUNCTION rsfn_vassq_name

PROLOGUE(vassq)

BEGIN_FWD(vassq)
  FWD_MONOTONE(vassq_0)
  FWD_MONOTONE(vassq_1)
END_FWD(vassq)

#define FPLACE_CODE (1000+0)
MONOTONE(vassq_0)
{
    COUNT_ARGS(2);
    /* NOP: REG1 = REG1; */
    /* NOP: REG0 = REG0; */
    CHECK_VECTOR(REG1 /* v */);
    REG2 = SUB1(RIBYTES_TO_FXWORDS(SIZEOF_PTR(REG1 /* v */)));
    REG3 = int2fx(0);
    JUMP(4,vassq_1);
}
#undef FPLACE_CODE

#define FPLACE_CODE (1000+1)
MONOTONE(vassq_1)
{
    /* NOP: REG3 = REG3; */
    if (FX_LT(REG3 /* i */,REG2 /* n */))
    {
	    if (EQ(gvec_ref(REG1 /* v */,FXWORDS_TO_RIBYTES(REG3 /* i */)),REG0 /* key */))
	    {
	    REG0 = ADD1(REG3 /* i */);
	    RETURN1();
	    }
	    else
	    {
	    REG4 = FX_ADD(REG3 /* i */,int2fx(2));
	    REG3 = REG4;
	    BJUMP(4,vassq_1);
	    }
    }
    else
    {
	    REG0 = FALSE_OBJ;
	    RETURN1();
    }
}
#undef FPLACE_CODE

EPILOGUE(vassq)

BEGIN_BACK(vassq)
  BACK_MONOTONE(vassq_0)
  BACK_MONOTONE(vassq_1)
END_BACK(vassq)

static struct function_descr vassq_descr = {
	&low_scheme_part_vectops,
	JUMP_TABLE( vassq ),
	rsfn_vassq_name };
#undef FUNCTION


/**************************** Function `vmemq' ****************************/
static char rsfn_vmemq_name[] = "vmemq";
#define FUNCTION rsfn_vmemq_name

PROLOGUE(vmemq)

BEGIN_FWD(vmemq)
  FWD_MONOTONE(vmemq_0)
  FWD_MONOTONE(vmemq_1)
END_FWD(vmemq)

#define FPLACE_CODE (1000+0)
MONOTONE(vmemq_0)
{
    COUNT_ARGS(2);
    /* NOP: REG1 = REG1; */
    /* NOP: REG0 = REG0; */
    CHECK_VECTOR(REG1 /* v */);
    REG2 = RIBYTES_TO_FXWORDS(SIZEOF_PTR(REG1 /* v */));
    REG3 = int2fx(0);
    JUMP(4,vmemq_1);
}
#undef FPLACE_CODE

#define FPLACE_CODE (1000+1)
MONOTONE(vmemq_1)
{
    /* NOP: REG3 = REG3; */
    if (EQ(REG3 /* i */,REG2 /* n */))
    {
	    REG0 = FALSE_OBJ;
	    RETURN1();
    }
    else
    {
	    if (EQ(vector_ref(REG1 /* v */,REG3 /* i */),REG0 /* key */))
	    {
	    REG0 = REG3 /* i */;
	    RETURN1();
	    }
	    else
	    {
	    REG4 = ADD1(REG3 /* i */);
	    REG3 = REG4;
	    BJUMP(4,vmemq_1);
	    }
    }
}
#undef FPLACE_CODE

EPILOGUE(vmemq)

BEGIN_BACK(vmemq)
  BACK_MONOTONE(vmemq_0)
  BACK_MONOTONE(vmemq_1)
END_BACK(vmemq)

static struct function_descr vmemq_descr = {
	&low_scheme_part_vectops,
	JUMP_TABLE( vmemq ),
	rsfn_vmemq_name };
#undef FUNCTION

/******************************* Postamble *******************************/
/**************************** Part Link Table ****************************/


static struct function_descr *(part_vectops_tab[]) = {
    &vector_bool_op_descr,
    &vassq_descr,
    &vmemq_descr,
    NULL };
struct part_descr low_scheme_part_vectops = {
    237344774,
    &module_low_scheme,
    part_vectops_tab,
    "vectops",
    0, sccsid };
#undef _MODULE_LOW_SCHEME
#undef _SCM_VECTOPS
#undef _C_VECTOPS
