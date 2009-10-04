(define-module rs.sys.crypto ()
  (&module
   (import usual-inlines)
   (load "crypto.scm")
   ;;
   (export bignum-probably-prime?
           exp-modulo
           mod2exp
           invert-modulo

           bignum->octet-string
           octet-string->bignum)))
