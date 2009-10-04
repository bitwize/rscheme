(define-module util.xpath ()
  (&module
   (use usual-inlines)
   (use util.xml)
   (use rs.util.charset
        tables
        earley)
   (use util.patterns)
   (use repl)
   ;;
   (load "delist.scm")
   (load "iterator.scm")
   (load "xpath.scm")
   ;;
   (export sxml:xpath-exec
           xpath
           xpath-unique    ; like xpath, but returns the one match else error
           xpath-str
           xpath-str-unique)))


