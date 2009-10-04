(define-module srfi.0 ()
  (&module
   (implements SRFI-0 srfi-0)
   ;;
   (import usual-inlines)
   ;;
   ;;  this is just reexports standard stuff, so that 
   ;;  you can start with r4rs (or r5rs, if I had that)
   ;;  and add on just what you need
   ;;
   (export cond-expand)))
           
