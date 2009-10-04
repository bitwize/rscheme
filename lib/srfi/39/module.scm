(define-module srfi.39 ()
  (&module
   (import usual-inlines)
   (implements SRFI-39 srfi-39)
   ;;
   ;;
   (load "parameters.scm")
   ;
   (export make-parameter
           parameterize)))
