#|
(define-module-extend rs.backend.c ()
  (set! *CFLAGS* (append *CFLAGS* '("-L/usr/local/pgsql/lib")))
  (set! *include-dirs*
	(append *include-dirs* '("/usr/local/pgsql/include"
				 "/u/donovan/rscheme/misc/pkgs/pg95"))))
|#
(define-module rs.db.pg ()
  (&module
   ;;
   (import rs.lang)
   (import rs.lang.internal)
   (import rs.sys.tables)
   (import rs.sys.reflect)
   (import rs.glue)
   (import regex objsys)

;  (import primops *scheme* low-scheme objsys iolib mathlib tables high-scheme)
;  (import paths mlink start corelib)
   (import syscalls)
   ;;
   (load "classes.scm")
   (load "transaction.scm")
   (load "safeglue.scm")

   (load "cnxn.scm")
   (load "query.scm")
   (load "result.scm")
   (load "gettuple.scm")
   (load "util.scm")
   (load "astable.scm")
   ;;
   (export 
    ;;
    ;;  setup
    ;;
    postgres-connect
    ;;
    ;;  low-level
    ;;
    pg-type-symbol
    pg-exec-command
    pg-with-tuples
    pg-type-symbol
    pg-field-names      ;; list of names
    pg-field-number     ;; field # given a name
    pg-field-type       ;; field type (id) given a #
    pg-field-size       ;; field size given a #
    pg-get-value
    ;;
    ;;  high-level
    ;;
    oid->instance
    query->list
    table->list
    make-pg-table
    clear-cache
    pg-marshall
    with-transaction
    )))

