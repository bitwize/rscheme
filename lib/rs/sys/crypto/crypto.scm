
(define-safe-glue (bignum-probably-prime? (n <bignum>))
  properties: ((other-h-files "morebignum.h")
               (other-c-files "morebignum.c"))
{
  REG0 = rb_to_bo( rs_bignum_prob_primeq(n) );
  RETURN1(); 
})

(define-safe-glue (exp-modulo (base <bignum>) 
                              (exp <bignum>) 
                              (mod <bignum>))
  properties: ((other-h-files "morebignum.h")
               (other-c-files "morebignum.c"))
{
  REG0 = rs_exp_modulo( base, exp, mod );
  RETURN1();
})


(define-safe-glue (mod2exp (n <bignum>) 
                           (exp <fixnum>))
  properties: ((other-h-files "morebignum.h")
               (other-c-files "morebignum.c"))
{
  REG0 = rs_mod2exp( n, fx2int(exp) );
  RETURN1();
})

(define-safe-glue (invert-modulo (n <bignum>) (mod <bignum>))
  properties: ((other-h-files "morebignum.h")
               (other-c-files "morebignum.c"))
{
  REG0 = rs_invertmod( n, mod );
  RETURN1();
})

(define-safe-glue (bignum->octet-string (n <bignum>))
{
  REG0 = rs_export_bignum( n );
  RETURN1();
})

(define-safe-glue (octet-string->bignum (s <string>))
{
  REG0 = rs_import_bignum( s );
  RETURN1();
})
