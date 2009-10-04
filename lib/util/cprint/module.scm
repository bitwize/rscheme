(define-module util.cprint ()
  (&module
   (import usual-inlines)
   (import util.patterns)
   (load "cpr.scm")
   (load "pats.scm")
   (load "foldconst.scm")
   (export *fold-const-patterns*) ; would be nicer as a property off fold-const
   (export print-c fold-const)))
