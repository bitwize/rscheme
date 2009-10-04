(define-module srfi.6 ()
  (&module
   (implements SRFI-6 srfi-6)
   ;;
   (import usual-inlines)
   ;;
   ;;  this is just reexports standard stuff, so that 
   ;;  you can start with r4rs (or r5rs, if I had that)
   ;;  and add on just what you need
   ;;
   (export open-output-string
           open-input-string
           get-output-string)))
           
