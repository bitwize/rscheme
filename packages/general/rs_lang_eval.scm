(define-module rs.lang.eval ()
  (&module
   (import primops *scheme* iolib low-scheme objsys mathlib tables high-scheme)
   (import compiler codegen editinp paths mlink start sort)
   (import corelib repl)
   ;;
   ;; silly, rsc doesn't support define-module inline defns
   (load "evalprocs.scm")
   ;; only two-arg form available if included offline
   (export eval)))
