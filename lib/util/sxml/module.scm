(define-module util.sxml ()
  (&module
   (import usual-inlines
           tables
           paths
           rs.io.pushback
           rs.util.properties)
   ;;
   (load "util.scm")
   (load "scan.scm")
   (load "namespaces.scm")
   (load "parse2.scm")
   (load "user.scm")
   ;;
   (export string->sxml
           port->sxml
           open-input-pushback-port
           
           set-xml-application-entity-ref!
           with-xml-application-entity-ref)))
