#|------------------------------------------------------------*-Scheme-*--|
 | File:    modules/regex/rxinterp.scm
 |
 |          Copyright (C)1997 Donovan Kolbly <d.kolbly@rscheme.org>
 |          as part of the RScheme project, licensed for free use.
 |          See <http://www.rscheme.org/> for the latest information.
 |
 | File version:     1.14
 | File mod date:    2005-02-25 13:22:55
 | System build:     v0.7.3.4-b7u, 2007-05-30
 | Owned by module:  regex
 |
 | Purpose:          Provide the scheme/C glue for the regexpr interpreter
 |------------------------------------------------------------------------|
 | Notes:
 |      regex-interp is a template which gets bound with an environment
 |      containing the parameters, such as the machine.
 |      The make-regex-proc is used to build such a closure
 |      
 |      The regex procedure returns a variable number of arguments
 |       - if the match fails, it returns no arguments
 |       - if the match succeeds, it returns at least 2 arguments,
 |         * the offset (in the ORIGINAL string, not the SUBSTRING)
 |           of the beginning of the matched portion
 |         * the offset of the end of the matched portion
 |           and, for each "let" construct, either two offsets which
 |           are the start and end of the region, or a freshly created
 |           string which is the matched content (which is done depending
 |           on the stringify-lets? option)
 |      
 |      So, on success, there are either 2+2*N or 2+N values returned,
 |      depending on stringify-lets?
 |      
 |    Bugs:
 |      The whole "stringify-lets?" thing and the way offsets are
 |      returned is kind of an annoying artifact. Should be generalized
 |      to support:
 |       - ignoring entire string start/end matches
 |       - signalling an error (calling a procedure) in case of mismatch,
 |         or in case of match
 |
 |      Needs to handle searching in and with <unicode-string>s
 |      
 |      Quickly scanning for a possible start position is not implemented.
 `------------------------------------------------------------------------|#

#|
   The state machine is stored in the (first) binding envt.

   offset & len are optional, and are used to specify a substring
   of str to scan

   var 0: regex name
   var 1: machine
   var 2: total number registers to return (REG0 = start posn,
					    REG1 = end posn,
					    REG2,... = other posns)
   var 3: anchored?
   var 4: stringify-lets?
|#

(define-glue (regex-interp str offset len) :template
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
})

(define (reg-expr-can-start? (regex <closure>) (str <string>))
  (if (eq? (template regex) regex-interp)
      (regex-can-start? (environment regex) str)
      (error "~s: not a compiled regex" regex)))

(define-glue (regex-can-start? rxenvt str)
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
})
