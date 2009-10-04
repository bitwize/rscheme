(define-module-extend rs.lang.internal ()
  (&module
   (export *default-allocation-area*)))

(define-module-extend rs.lang ()
  (&module
   (export mquote
	   logical-shift-right)))

(define-module rs.db.oodb ()
  (&module
   (import rs.lang
	   rs.lang.internal
	   rs.sys.tables
	   rs.db.rstore
	   syscalls)
   ;;
   (load "world.scm")
   (export with-world-view
	   new-world-view
	   make-world-view
	   current-world-view
	   define-world-schema
	   make-world-schema
	   commit-world)
   ;;
   (load "txn.scm")
   (export do-transaction current-transaction)
   ;;
   (load "object.scm")
   (load "table.scm")
   (load "index.scm")
   (load "assoc.scm")
   (load "entity.scm")
   ;;
   (export <entity> pmake entity-allocation-area)
   ;;
   (load "pstore.scm")
   ))

  