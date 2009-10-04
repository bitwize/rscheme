(define-module rs.util.types ()
  (&module
   (import usual-inlines
           tables
           compiler
           repl)
   ;util.patterns
   (load "typecheck.scm")
   (export is-type?
           type-assert
           type-check
           define-type-checker)))
