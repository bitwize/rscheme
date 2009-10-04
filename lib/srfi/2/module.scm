(define-module srfi.2 ()
  (&module
   (import rs.lang)
   (implements SRFI-2 srfi-2)
   ;;
   ;;
   (load "ref.scm")
   ;
   (export and-let*)))
