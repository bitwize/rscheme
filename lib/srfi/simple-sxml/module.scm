(define-module srfi.simple-sxml ()
  (&module
   ;;
   ;;  Because this module is actually the reference 
   ;;  implementation, we want to keep the namespace
   ;;  clean:  Start with R4RS and add only SRFIs that
   ;;  have full support
   ;;
   (import r4rs srfi.0 srfi.6 srfi.23)
   
   ;; our debug procedures need more stuff...
   ;(import usual-inlines)
   ;;
   (load "write.scm")
   (export write-sxml sxml->string)
   ;;
   (load "read.scm")
   (export read-sxml string->sxml)))
