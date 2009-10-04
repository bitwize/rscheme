
(define-macro (define-db-glue args . body)
  `(define-safe-glue ,args
     type-handler: (<db> (instance? <db>)
			 ("DB *~a"
			  "(DB *)OBJ_TO_RAW_PTR(gvec_ref(~a,SLOT(1)))"))
     ,@body))
