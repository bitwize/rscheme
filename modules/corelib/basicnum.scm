(define-glue (raw-longint->double-float a)
{
#if !FULL_NUMERIC_TOWER
    scheme_error( "bignum.scm:raw-longint->double-float:~s: function stubbed out", 1, int2fx(__LINE__) ); 
    REG0 = FALSE_OBJ;
    RETURN1();
#else
  IEEE_64 x = longint_to_raw_float(a);
  REG0 = make_float(x);
  RETURN1();
#endif
})

(define-glue (raw-bignum->double-float a)
{
#if !FULL_NUMERIC_TOWER
    scheme_error( "basicnum.scm:raw-bignum->double-float:~s: function stubbed out", 1, int2fx(__LINE__) ); 
    REG0 = ZERO;
    RETURN1();
#else
  IEEE_64 x = bignum_to_raw_float(a);
  REG0 = make_float(x);
  RETURN1();
#endif
})
