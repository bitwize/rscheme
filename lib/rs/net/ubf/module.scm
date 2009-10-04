(define-module rs.net.ubf ()
  (&module
   (import usual-inlines
           earley
           tables
           rs.util.properties
           regex)
   (load "ubf.scm")
   (export object->compact-ubf)
   ;;
   (load "ubf-b.scm")
   ;;
   (export load-ubf-b-spec
           compile-protocol-file
           generate-ubf-checker/scheme)
   
   ;;
   (load "read.scm")
   (export read-ubf             ; take a string and return the UBF object
           for-each-ubf)      ; take a port and call proc w/each UBF object
   ;;
   (load "c14n.scm")
   ;; Note that `write-ubf' is also the fallback for object->compact-ubf
   (export write-ubf
           object->ubf)
   
   ;; for extenders...
   (export ubf-grammar
           ;; YUCK:
           base-ubf-grammar match-ident match-string
           checkwrap
           gen-scheme-checker
           *scheme-compound-check-generator*
           *scheme-leaf-check-generator*
           *ubf-scan-hook*)
))
