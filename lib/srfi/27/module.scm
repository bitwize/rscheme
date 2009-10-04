(define-module srfi.27 ()
  (&module
   (import usual-inlines
           syscalls)
   ;;
   (load "api.scm")
   ;;
   (export make-random-source
           random-source?
           ;;
           random-source-pseudo-randomize!
           random-source-randomize!
           random-source-state-ref
           random-source-state-set!
           ;;
           random-source-make-integers
           random-source-make-reals
           ;;
           random-integer
           random-real
           default-random-source)))

           
           

   
