#|------------------------------------------------------------*-Scheme-*--|
 | File:    pg/safeglue.scm
 |
 |          Copyright (C)1997 Donovan Kolbly <d.kolbly@rosette.com>
 |          as part of the RScheme project, licensed for free use
 |
 | Version: 1.3
 | Date:    1999-01-12 12:28:44
 | Build:   v0.7.3.4-b7u, 2007-05-30
 |
 | Purpose: safe scheme/C glue with PG95-specific types
 `------------------------------------------------------------------------|#

(define-macro (define-pg-glue args . body)
  `(define-safe-glue ,args
     type-handler: (<pg-connection>
		    (direct-instance? <pg-connection>)
		    ("PGconn *~a"
		     "(PGconn *)OBJ_TO_RAW_PTR(gvec_ref(~a,SLOT(1)))"))
     type-handler: (<pg-result>
		    (class-eq? <pg-result>)
		    ("PGresult *~a"
		     "(PGresult *)OBJ_TO_RAW_PTR(gvec_ref(~a,SLOT(0)))"))
     properties: ((other-h-files "gettuple.h" "<libpq-fe.h>")
		  (other-c-files "gettuple.c")
		  (other-lib-dirs "/usr/local/pgsql/lib")
		  (other-local-include-dirs ".")
		  (other-include-dirs "/usr/local/pgsql/include")
		  (other-libs "pq"))
     ,@body))
