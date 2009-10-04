(define-module srfi.8 ()
  (&module
   (implements SRFI-8 srfi-8)
   (import usual-inlines))
  ;;
  (define-macro (receive args expr . body)
    `(call-with-values (lambda () ,expr)
       (lambda ,args ,@body)))
  ;;
  (&module
   (export receive)))


