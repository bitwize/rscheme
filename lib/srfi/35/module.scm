(define-module srfi.35 ()
  (&module
   (import usual-inlines)
   ;;
   (load "srfi-35.scm")
   ;;
   (export make-condition-type
           condition-type?
           make-condition
           condition?
           condition-has-type?
           condition-ref
           make-compound-condition
           extract-condition
           
           define-condition-type
           condition
           
           &condition
           &message
           &serious
           &error
           )))
