(define-module rs.util.text ()
  (&module
   (import usual-inlines)
   ;;
   (load "matrix.scm")
   (load "render.scm")
   (export display-matrix)
   ;
   (load "table.scm")
   (export display-table)
   ;
   (import regex)
   (load "glob.scm")
   (export glob?  ;; does the string have a "*" in it?
	   glob->predicate       ;; make a "*.c" recognizer
	   globs->converter)))   ;; make a "*.c ==> *.o" converter
	   
