/**********************************************
THIS FILE WAS AUTOMATICALLY GENERATED, AND MAY
BE AUTOMATICALLY RE-GENERATED WHEN THE COMPILER
OR SOURCE CHANGES.  DO NOT MODIFY THIS FILE BY HAND!
RScheme Build (v0.7.3.4-b7u, 2007-05-30)
**********************************************/

/******************************** Preamble ********************************/

#define _MODULE_LOW_SCHEME
#define _SCM_VECTORS
#define _C_VECTORS
#include "low_scheme_p.h"
#include <rscheme/vinsns.h>
extern struct module_descr module_low_scheme;
extern struct part_descr low_scheme_part_vectors;
static char sccsid[] = "@(#)low-scheme modules/lowscm/vectors.scm [194426881] (RS v0.7.3.4-b7u, 2007-05-30)";

/************************** Function Definitions **************************/


/************************* Raw glue `make-vector' *************************/
#define vec_length REG0
#define vec_fill REG1

static char rsfn_make_vector_name[] = "make-vector";
#define FUNCTION rsfn_make_vector_name

PROLOGUE(make_vector)

BEGIN_FWD(make_vector)
  FWD_MONOTONE(make_vector_0)
END_FWD(make_vector)

#define FPLACE_CODE (1000+0)
MONOTONE(make_vector_0)
{
{
UINT_32 i, len;
obj fill = FALSE_OBJ;

    if (arg_count_reg != 1)
    {
	if (arg_count_reg == 2)
	    fill = vec_fill;
	else
	    wrong_num_args_range( FUNCTION, 1, 2 );
    }

    if (!OBJ_ISA_FIXNUM(vec_length))
	scheme_error( string_text(LITERAL(0)), 1, vec_length );

    len = FXWORDS_TO_RIBYTES(vec_length);
    REG0 = make_gvec( vector_class, len, fill );
    RETURN(1);
}}
#undef FPLACE_CODE

EPILOGUE(make_vector)

BEGIN_BACK(make_vector)
  BACK_MONOTONE(make_vector_0)
END_BACK(make_vector)

static struct function_descr make_vector_descr = {
	&low_scheme_part_vectors,
	JUMP_TABLE( make_vector ),
	rsfn_make_vector_name };
#undef FUNCTION

#undef vec_length
#undef vec_fill

/************************ Raw glue `list->vector' ************************/
#define list REG0

static char rsfn_list_vector_name[] = "list->vector";
#define FUNCTION rsfn_list_vector_name

PROLOGUE(list_vector)

BEGIN_FWD(list_vector)
  FWD_MONOTONE(list_vector_0)
END_FWD(list_vector)

#define FPLACE_CODE (1000+0)
MONOTONE(list_vector_0)
{
{
UINT_32 len;
obj lp;

    len = 0;
    for (lp=list; (len < SLOT(1000)) && PAIR_P( lp ); lp=pair_cdr( lp ))
      {
	len += sizeof(obj);
      }
    if (PAIR_P( lp ))
      {
        APPLY( 1, TLREFB( 0 ) ); /* (big-list->vector list) */
      }
    else if (NULL_P(lp))
      {
        obj vec = alloc( len, vector_class );
        UINT_32 i = 0;

        for (lp=list; !NULL_P(lp); lp=pair_cdr( lp ), i += SLOT(1))
	  {
            gvec_write_init( vec, i, pair_car( lp ) );
          }
        REG0 = vec;
        RETURN1();
      }
    else
      {
        REG0 = list;
	REG1 = lp;
        APPLY( 2, TLREFB( 1 ) );  /* (signal-improper-list list at) */
      }
}}
#undef FPLACE_CODE

EPILOGUE(list_vector)

BEGIN_BACK(list_vector)
  BACK_MONOTONE(list_vector_0)
END_BACK(list_vector)

static struct function_descr list_vector_descr = {
	&low_scheme_part_vectors,
	JUMP_TABLE( list_vector ),
	rsfn_list_vector_name };
#undef FUNCTION

#undef list

/********************** Raw glue `big-list->vector*' **********************/
#define list REG0
#define n REG1

static char rsfn_big_list_vector_name[] = "big-list->vector*";
#define FUNCTION rsfn_big_list_vector_name

PROLOGUE(big_list_vector)

BEGIN_FWD(big_list_vector)
  FWD_MONOTONE(big_list_vector_0)
END_FWD(big_list_vector)

#define FPLACE_CODE (1000+0)
MONOTONE(big_list_vector_0)
{
{
  obj lp, vec = alloc( FXWORDS_TO_RIBYTES( n ), vector_class );
  UINT_32 i = 0;

  for (lp=list; !NULL_P(lp); lp=pair_cdr( lp ), i += SLOT(1))
    {
      gvec_write_init( vec, i, pair_car( lp ) );
    }
  REG0 = vec;
  RETURN1();
}}
#undef FPLACE_CODE

EPILOGUE(big_list_vector)

BEGIN_BACK(big_list_vector)
  BACK_MONOTONE(big_list_vector_0)
END_BACK(big_list_vector)

static struct function_descr big_list_vector_descr = {
	&low_scheme_part_vectors,
	JUMP_TABLE( big_list_vector ),
	rsfn_big_list_vector_name };
#undef FUNCTION

#undef list
#undef n

/************************ Raw glue `vector-fill!' ************************/
#define vector REG0
#define fill REG1

static char rsfn_vector_fill_name[] = "vector-fill!";
#define FUNCTION rsfn_vector_fill_name

PROLOGUE(vector_fill)

BEGIN_FWD(vector_fill)
  FWD_MONOTONE(vector_fill_0)
END_FWD(vector_fill)

#define FPLACE_CODE (1000+0)
MONOTONE(vector_fill_0)
{
{
UINT_32 i, len;

    COUNT_ARGS(2);
    len = SIZEOF_PTR(vector);
    
    if (OBJ_ISA_PTR(fill))
    {
	for (i=0; i<len; i+=SLOT(1))
	    gvec_write_ptr( vector, i, fill );
    }
    else
    {
	for (i=0; i<len; i+=SLOT(1))
	    gvec_write_non_ptr( vector, i, fill );
    }
    RETURN1();
}}
#undef FPLACE_CODE

EPILOGUE(vector_fill)

BEGIN_BACK(vector_fill)
  BACK_MONOTONE(vector_fill_0)
END_BACK(vector_fill)

static struct function_descr vector_fill_descr = {
	&low_scheme_part_vectors,
	JUMP_TABLE( vector_fill ),
	rsfn_vector_fill_name };
#undef FUNCTION

#undef vector
#undef fill

/*************************** Raw glue `vector' ***************************/

static char rsfn_vector_name[] = "vector";
#define FUNCTION rsfn_vector_name

PROLOGUE(vector)

BEGIN_FWD(vector)
  FWD_MONOTONE(vector_0)
END_FWD(vector)

#define FPLACE_CODE (1000+0)
MONOTONE(vector_0)
{
{
unsigned i;
obj old0;

    old0 = REG0;
    REG0 = alloc( SLOT(arg_count_reg), vector_class );
    switch (arg_count_reg)
    {
	default:
	         for (i=10; i<arg_count_reg; i++)
		  gvec_write_init( REG0, SLOT(i), REG(i) );
	case 10: gvec_write_init( REG0, SLOT(9), REG9 );
	case  9: gvec_write_init( REG0, SLOT(8), REG8 );
	case  8: gvec_write_init( REG0, SLOT(7), REG7 );
	case  7: gvec_write_init( REG0, SLOT(6), REG6 );
	case  6: gvec_write_init( REG0, SLOT(5), REG5 );
	case  5: gvec_write_init( REG0, SLOT(4), REG4 );
	case  4: gvec_write_init( REG0, SLOT(3), REG3 );
	case  3: gvec_write_init( REG0, SLOT(2), REG2 );
	case  2: gvec_write_init( REG0, SLOT(1), REG1 );
	case  1: gvec_write_init( REG0, SLOT(0), old0 );
	case  0: /* the empty vector */;
    }
    RETURN1();
}}
#undef FPLACE_CODE

EPILOGUE(vector)

BEGIN_BACK(vector)
  BACK_MONOTONE(vector_0)
END_BACK(vector)

static struct function_descr vector_descr = {
	&low_scheme_part_vectors,
	JUMP_TABLE( vector ),
	rsfn_vector_name };
#undef FUNCTION


/************************ Raw glue `vector-append' ************************/

static char rsfn_vector_append_name[] = "vector-append";
#define FUNCTION rsfn_vector_append_name

PROLOGUE(vector_append)

BEGIN_FWD(vector_append)
  FWD_MONOTONE(vector_append_0)
END_FWD(vector_append)

#define FPLACE_CODE (1000+0)
MONOTONE(vector_append_0)
{
{
  obj r, v, totlen = ZERO;
  UINT_32 i, j, k;

  for (i=0; i<arg_count_reg; i++)
    {
      v = reg_ref(i);
      if (VECTOR_P(v))
	totlen = FX_ADD( totlen, RIBYTES_TO_FXWORDS(SIZEOF_PTR(v)) );
      else
	scheme_error( "vector-append: arg ~d is not a vector: ~s",
		      2, int2fx(i), v );
    }
  r = alloc( FXWORDS_TO_RIBYTES(totlen), vector_class );
  k = 0;
  for (i=0; i<arg_count_reg; i++)
    {
      v = reg_ref(i);
      for (j=0; j<SIZEOF_PTR(v); j+=SLOT(1))
	{
	  gvec_write_init( r, k, gvec_ref( v, j ) );
	  k += SLOT(1);
	}
    }
  REG0 = r;
  RETURN1();
}}
#undef FPLACE_CODE

EPILOGUE(vector_append)

BEGIN_BACK(vector_append)
  BACK_MONOTONE(vector_append_0)
END_BACK(vector_append)

static struct function_descr vector_append_descr = {
	&low_scheme_part_vectors,
	JUMP_TABLE( vector_append ),
	rsfn_vector_append_name };
#undef FUNCTION

/******************************* Postamble *******************************/
/**************************** Part Link Table ****************************/


static struct function_descr *(part_vectors_tab[]) = {
    &make_vector_descr,
    &list_vector_descr,
    &big_list_vector_descr,
    &vector_fill_descr,
    &vector_descr,
    &vector_append_descr,
    NULL };
struct part_descr low_scheme_part_vectors = {
    194426881,
    &module_low_scheme,
    part_vectors_tab,
    "vectors",
    0, sccsid };
#undef _MODULE_LOW_SCHEME
#undef _SCM_VECTORS
#undef _C_VECTORS
