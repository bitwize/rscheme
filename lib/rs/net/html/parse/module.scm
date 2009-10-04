(define-module rs.net.html.parse ()
  (&module (import rs.lang
		   rs.sys.tables
		   rs.util.msgs
		   rs.lang.eval))
  ;
  (define-message-table rs.net.html 441)
  ;
  (&module
   (load "scan.scm")
   (load "earley.scm")
   (load "grammar.scm")
   (load "parse.scm")
   ;
   (export scan-html parse-html transform-html)))
