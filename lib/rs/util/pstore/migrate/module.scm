(define-module rs.util.pstore.migrate ()
  (&module
   (import usual-inlines
           rs.db.rstore
           tables
           sort
           objsys)
   ;;
   (load "migrate.scm")
   ;;
   (export make-simple-migrator
           migrate-pstore)))

    