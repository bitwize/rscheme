(define-module rs.sys.undefine ()
  (&module
   (import usual-inlines)
   (import compiler)
   (import tables)
   ;
   (load "undefine.scm")
   (export undefine redefine)))
