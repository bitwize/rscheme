(define-module srfi.34 ()
  (&module
   (import rs.lang)
   ;;
   ;;
   (load "srfi-34.scm")
   ;
   (export
    guard with-exception-handler raise
    handler-case ;; BEWARE handler-case should actually not be exported
    )))
