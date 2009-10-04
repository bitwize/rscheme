(define-module-extend rs.util.msgs () (&module (export <error-message>)))
 
(define-module app.ibis.backend ()
  (&module
   (import usual-inlines 
           sort
           tables
           rs.util.pstore.migrate
           rs.util.msgs))               ; define-message-table 704
  ;
  ;
  (&module
   (load "errors.scm")
   (load "relations.scm")
   (load "objmodel.scm")
   (load "authmodel.scm")
   ;
   (load "ibis.scm")
   (load "migrate.scm")
   ;
   (load "ibis-render.scm")
   (load "text-layout.scm")
   (load "interactive.scm")
   (load "pstore.scm")
   (load "server.scm")
   (load "server-commands.scm")
   ;
   (load "notify.scm")
   ;
   (export server-daemon
           ibisd
           migrate-ibis-store
           with-system-state
           set-system-state!
           create-ibis-store
           open-ibis-store
           
           set-notify-from!
           set-web-app-root!)))
