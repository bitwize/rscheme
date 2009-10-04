(define-module srfi.9 ()
  (&module
   (implements SRFI-9 srfi-9)
   (import usual-inlines)
   (load "implementation.scm")
   (export define-record-type
           <<record-type>>
           <record>)))
