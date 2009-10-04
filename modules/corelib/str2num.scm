#|------------------------------------------------------------*-Scheme-*--|
 | File:    modules/corelib/str2num.scm
 |
 |          Copyright (C)1997 Donovan Kolbly <d.kolbly@rscheme.org>
 |          as part of the RScheme project, licensed for free use.
 |          See <http://www.rscheme.org/> for the latest information.
 |
 | File version:     1.10
 | File mod date:    1998-12-28 10:26:30
 | System build:     v0.7.3.4-b7u, 2007-05-30
 | Owned by module:  corelib
 |
 | Purpose:          convert strings to numbers (option radix override)
 `------------------------------------------------------------------------|#

(define-glue (string->fixnum str radix)
{
  REG0 = string_to_fixnum( string_text(str), 
			   string_length(str),
			   fx2int(radix) );
  RETURN1();
})

(define-glue (string->float str radix)
{
  REG0 = string_to_float( string_text(str), 
			  string_length(str),
			  fx2int(radix) );
  RETURN1();
})

(define-glue (string->long-int str radix)
{
  INT_64 v;
  rs_bool ok;

  ok = string_to_int_64( string_text(str), 
			 string_length(str),
			 fx2int(radix),
			 &v );
  REG0 = ok ? make_long_int(v) : FALSE_OBJ;
  RETURN1();
})

(define-glue (string->bignum str radix)
{
#if !FULL_NUMERIC_TOWER
    REG0 = FALSE_OBJ;
    RETURN1();
#else
  REG0 = string_to_bignum_obj( string_text(str), 
			       fx2int(radix));
  RETURN1();
#endif
})

(define-glue (string->rational str radix)
{
#if !FULL_NUMERIC_TOWER
    REG0 = FALSE_OBJ;
    RETURN1();
#else
  REG0 = string_to_rational_obj( string_text(str), 
				 fx2int(radix));
  RETURN1();
#endif
})

(define-safe-glue (sprintf-float (fmt <raw-string>)
                                 (len <raw-int>)
                                 (num <double-float>))
{
  double x = extract_float(num);
  REG0 = c_vprintf( fmt, len, x );
  RETURN1();
})

(define-safe-glue (sprintf-fixnum (fmt <raw-string>)
                                  (len <raw-int>)
                                  (num <raw-int>))
{
  REG0 = c_vprintf( fmt, len, num );
  RETURN1();
})


(define (machine-bits->string arg)
  (let ((half-fmt (case (word-size-bits)
		    ((64) "%08lx")
		    (else "%04lx"))))
    (string-append
     (sprintf-fixnum half-fmt 40 (obj-high-bits arg))
     "_"
     (sprintf-fixnum half-fmt 40 (obj-low-bits arg)))))

