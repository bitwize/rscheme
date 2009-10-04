(define-module rs.net.sha1 ()
  (&module (import usual-inlines)
           (import rs.util.pack))
  (&module
   (load "digest.scm")
   ;;
   (export sha1-binary-digest 
           sha1-digest)))
