
(define-module rs.sys.generic-math ()
  (&module
   (import usual-inlines)
   (import rs.sys.multimethod)
   (import rs.sys.undefine)
   ;
   (load "generics.scm")
   (load "syntax.scm")
   ;
   (load "builtin.scm")
   ;
   (export + - * /)
   ;
   (export binary+ binary- binary* binary/)
   (export quotient remainder modulo)
   (export numerator denominator floor ceiling truncate round)
   (export exp log sin cos tan asin acos atan)
   (export sqrt expt)
   (export magnitude angle)))
   ;
