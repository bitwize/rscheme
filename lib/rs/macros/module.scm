(define-module rs.macros ()
  (&module
   (import usual-inlines)
   (load "expand.sch")
   (load "misc.sch")
   (load "prefs.sch")
   (load "syntaxenv.sch")
   (load "syntaxrules.sch")
   (load "usual.sch")
   (load "insert.scm")
   (export macro-expand
	   define-syntax-scope
           define-syntax)))



