(define-module rs.db.sqlite3 ()
  (&module
   (import usual-inlines
           tables)
   ;;
   (load "glue.scm")
   ;;
   (load "api.scm")
   ;;
   (export <sqlite3-error>
           <sqlite3-database>
           <sqlite3-statement>
           
           sqlite3-open
           sqlite3-close
           sqlite3-changes

           ;; statements
           sqlite3-prepare
           sqlite3-reset
           sqlite3-finalize
           sqlite3-step
           sqlite3-bind
           sqlite3-row          ; extract a row of data
           sqlite3-for-each     ; iterate over all rows
           ;;
           ;;  simple... take textual sql and return a list of row-lists
           sqlite3-exec
           )))
