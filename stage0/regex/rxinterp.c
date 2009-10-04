/**********************************************
THIS FILE WAS AUTOMATICALLY GENERATED, AND MAY
BE AUTOMATICALLY RE-GENERATED WHEN THE COMPILER
OR SOURCE CHANGES.  DO NOT MODIFY THIS FILE BY HAND!
RScheme Build (v0.7.3.4-b7u, 2007-05-30)
**********************************************/

/******************************** Preamble ********************************/

#define _MODULE_REGEX
#define _SCM_RXINTERP
#define _C_RXINTERP
#include "regex_p.h"
#include <rscheme/vinsns.h>
extern struct module_descr module_regex;
extern struct part_descr regex_part_rxinterp;
static char sccsid[] = "@(#)regex modules/regex/rxinterp.scm [67853312] (RS v0.7.3.4-b7u, 2007-05-30)";

/************************** Function Definitions **************************/


/************************ Raw glue `regex-interp' ************************/
#define str REG0
#define offset REG1
#define len REG2

static char rsfn_regex_interp_name[] = "regex-interp";
#define FUNCTION rsfn_regex_interp_name

PROLOGUE(regex_interp)

BEGIN_FWD(regex_interp)
  FWD_MONOTONE(regex_interp_0)
END_FWD(regex_interp)

#define FPLACE_CODE (1000+0)
MONOTONE(regex_interp_0)
{
{
  UINT_32 str_start, str_limit;
  UINT_8 *str_data = NULL;
  unsigned i, num_regs;
  UINT_8 *p, *result;
  UINT_8 *(save_array[50]);
  struct RXCounter counter_array[50];

  str_start = 0;
  USE_FUNCTION_ENVT();
  COUNT_ARGS_AT_LEAST(1);

  if (STRING_P(str))
    str_data = (UINT_8 *)string_text( str );
  else 
    scheme_error( "regex-interp ~s: arg `~s' not a string",
		 2, LEXREF0(0), str );

  str_limit = string_length(str);
  str_start = 0;
  if (arg_count_reg > 1)
    {
      if (OBJ_ISA_FIXNUM(offset))
	str_start = fx2int( offset );
      else
	scheme_error( "regex-interp ~s: offset `~s' not a fixnum",
		      2, LEXREF0(0), offset );

      if (str_start > str_limit)
	scheme_error( "regex-interp ~s: offset ~d > string length ~d",
		     3, LEXREF0(0), int2fx(str_start), int2fx(str_limit) );

      if (arg_count_reg == 3)
	{
	  if (OBJ_ISA_FIXNUM(len))
	    str_limit = str_start + fx2int(len);
	  else
	    scheme_error( "regex-interp ~s: length `~s' not a fixnum",
		      2, LEXREF0(0), len );
	  if (str_limit > string_length(str))
	    scheme_error( "regex-interp ~s: end ~d > string length ~d",
			 3, 
			 LEXREF0(0),
			 int2fx(str_limit), 
			 int2fx(string_length(str)) );
	}
      else if (arg_count_reg != 2)
	scheme_error( "regex-interp ~s: wrong number args (~d, expected 1-3)",
		     2, LEXREF0(0), int2fx(arg_count_reg) );
    }
  
  num_regs = fx2int( LEXREF0(2) );

  for (i=0; i<num_regs; i++)
       save_array[i] = NULL;

  rxmach_save_array = save_array;
  rxmach_bound = counter_array;

  rxmach_machine = (UINT_8 *)PTR_TO_DATAPTR( LEXREF0(1) );

  rxmach_start = str_data + str_start;
  rxmach_limit = str_data + str_limit;

  p = rxmach_start;
  rxmach_on_eos = NULL; /* EOS w/o match is an error */
  result = run_match( p, 0 );

  if (result)
    goto found_it;

  if (truish(LEXREF0(3)))
    {
      /* it's anchored... then we're lost */
      RETURN0();
    }
  else
    {
      /* not anchored... try other offsets */
      
      while (p < rxmach_limit)
	{
	  p++;
	  result = run_match( p, 0 );
	  if (result)
	    goto found_it;
	}
      RETURN0();
    }
  
 found_it:

  if (truish(LEXREF0(4)))
    {
      UINT_8 **z;
      obj b;

      /* map substring over remainder */
      REG0 = int2fx( p - str_data );
      REG1 = int2fx( result - str_data );

      z = save_array + 2;
      num_regs = num_regs/2 + 1;
      for (i=2; i<num_regs; i++, z+=2)
	{
	  if (z[0])
	    {
	      UINT_32 m_len;

	      if (!z[1] || (z[1] < z[0]))
		rxmach_internal_error( RXERR_INT_INCOMPLETE_LET );

	      m_len = z[1] - z[0];

	      /*printf( "filling in reg[%d]: %u bytes at '%s'\n",
		      i, len, z[0] );*/

	      /* bvec_alloc() ensures a NUL in the last byte */

	      b = bvec_alloc( z[1] - z[0] + 1, string_class );
	      memcpy( PTR_TO_DATAPTR(b), z[0], m_len );
	    }
	  else
	    b = FALSE_OBJ;
	  reg_set( i, b );
	}
      RETURN(i);        /* i >= 2; REG0 and REG1 have already been set up */
    }
  else
    {
      save_array[0] = p;
      save_array[1] = result;
      if (num_regs == 0) {
        RETURN0();
      } else {
        for (i=0; i<num_regs; i++) {
	  if (save_array[i]) {
	    reg_set( i, int2fx( save_array[i] - str_data ) );
	  } else {
	    reg_set( i, FALSE_OBJ );
          }
	}
        RETURN(num_regs);
      }
    }
}}
#undef FPLACE_CODE

EPILOGUE(regex_interp)

BEGIN_BACK(regex_interp)
  BACK_MONOTONE(regex_interp_0)
END_BACK(regex_interp)

static struct function_descr regex_interp_descr = {
	&regex_part_rxinterp,
	JUMP_TABLE( regex_interp ),
	rsfn_regex_interp_name };
#undef FUNCTION

#undef str
#undef offset
#undef len

/********************** Raw glue `regex-can-start?' **********************/
#define rxenvt REG0
#define str REG1

static char rsfn_regex_can_start_name[] = "regex-can-start?";
#define FUNCTION rsfn_regex_can_start_name

PROLOGUE(regex_can_start)

BEGIN_FWD(regex_can_start)
  FWD_MONOTONE(regex_can_start_0)
END_FWD(regex_can_start)

#define FPLACE_CODE (1000+0)
MONOTONE(regex_can_start_0)
{
{
  UINT_32 str_start, str_limit;
  UINT_8 *str_data;
  unsigned i, num_regs;
  UINT_8 *p, *result;
  UINT_8 *(save_array[50]);
  struct RXCounter bound_array[50];

  str_start = 0;
  envt_reg = rxenvt;
  COUNT_ARGS(2);

  str_data = (UINT_8 *)string_text( str );
  str_limit = string_length(str);
  str_start = 0;
  
  num_regs = fx2int( LEXREF0(2) );

  rxmach_save_array = save_array;
  rxmach_bound = bound_array;

  rxmach_machine = (UINT_8 *)PTR_TO_DATAPTR( LEXREF0(1) );

  rxmach_start = str_data + str_start;
  rxmach_limit = str_data + str_limit;

  p = rxmach_start;
  rxmach_on_eos = (UINT_8 *)1; /* EOS w/o match is success */
  result = run_match( p, 0 );

  if (result)
    REG0 = TRUE_OBJ;
  else
    REG0 = FALSE_OBJ;
  RETURN1();
}}
#undef FPLACE_CODE

EPILOGUE(regex_can_start)

BEGIN_BACK(regex_can_start)
  BACK_MONOTONE(regex_can_start_0)
END_BACK(regex_can_start)

static struct function_descr regex_can_start_descr = {
	&regex_part_rxinterp,
	JUMP_TABLE( regex_can_start ),
	rsfn_regex_can_start_name };
#undef FUNCTION

#undef rxenvt
#undef str
/******************************* Postamble *******************************/
/**************************** Part Link Table ****************************/


static struct function_descr *(part_rxinterp_tab[]) = {
    &regex_interp_descr,
    &regex_can_start_descr,
    NULL };
struct part_descr regex_part_rxinterp = {
    67853312,
    &module_regex,
    part_rxinterp_tab,
    "rxinterp",
    0, sccsid };
#undef _MODULE_REGEX
#undef _SCM_RXINTERP
#undef _C_RXINTERP
