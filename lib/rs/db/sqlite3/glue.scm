
(define-macro (define-sqlite-glue (name . args) . body)
  `(define-safe-glue
     (,name ,@args)
     type-handler: (<sqlite3-database>
		    (direct-instance? <sqlite3-database>)
		    ("sqlite3 *~a" "((sqlite3 *)OBJ_TO_RAW_PTR(gvec_ref(~a,SLOT(0))))"))
     type-handler: (<sqlite3-statement>
		    (direct-instance? <sqlite3-statement>)
		    ("sqlite3_stmt *~a" "((sqlite3_stmt *)OBJ_TO_RAW_PTR(gvec_ref(~a,SLOT(0))))"))
     properties: ((other-h-files "<sqlite3.h>" "<string.h>")
		  (other-libs "sqlite3")
                  (other-c-files "sqglue.c")
                  (other-include-dirs ,(with-module rs.sys.config (config-value 'sqlite3.include)))
                  (other-lib-dirs ,(with-module rs.sys.config (config-value 'sqlite3.lib))))
     ,@body))
