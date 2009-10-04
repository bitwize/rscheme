#|------------------------------------------------------------*-Scheme-*--|
 | File:    modules/corelib/num2str.scm
 |
 |          Copyright (C)1997 Donovan Kolbly <d.kolbly@rscheme.org>
 |          as part of the RScheme project, licensed for free use.
 |          See <http://www.rscheme.org/> for the latest information.
 |
 | File version:     1.10
 | File mod date:    2003-06-12 21:53:55
 | System build:     v0.7.3.4-b7u, 2007-05-30
 | Owned by module:  corelib
 |
 | Purpose:          convert numbers to strings (optional radix)
 `------------------------------------------------------------------------|#

(define-glue (double-float->string num)
{
char temp[200];

    REG0 = make_string( double_float_to_string( temp, num ) );
    RETURN(1);
})

(define-glue (fixnum->string num radix)
{
  char temp[WORD_SIZE_BITS+10];

  REG0 = make_string( fixnum_to_string( &temp[WORD_SIZE_BITS+10], 
					num, 
					fx2int(radix) ) );
  RETURN(1);
})

(define-glue (long-int->string num radix)
{
char temp[70];

    REG0 = make_string( int_64_to_string( &temp[70], 
					  *(INT_64*)PTR_TO_DATAPTR(num), 
					  fx2int(radix) ) );
    RETURN(1);
})

(define-glue (bignum->string num radix)
{
    REG0 = bignum_to_string_obj(num, fx2int(radix));
    RETURN(1);
})


