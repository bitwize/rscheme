(define-module rs.sys.multimethod ()
  (&module
   (import usual-inlines)
   ;
   (load "typesys.scm")
   (export singleton type=? type<=?)
   ;
   (load "dispatch.scm")
   (export define-mm-generic-function
	   make-mm-generic-function)
   ;
   (import objsys)
   (import repl)
   (load "addmethod.scm")))

