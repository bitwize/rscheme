
(define-macro (define-rstore-glue args . body)
  `(define-lss-glue ,args
     properties: ((other-h-files "<rscheme/pkgs/rstore/rstore.h>")
		  (other-libs "rstore"))
     type-handler: (<persistent-store> (direct-instance? <persistent-store>)
		       ("RStore *~a"
                        "OBJ_TO_C_PTR( RStore *, gvec_ref(~a, SLOT(0)) )"))
     type-handler: (<persistent-addr> (direct-instance? <persistent-addr>)
                       ("struct PAddrVec *~a"
                        "((struct PAddrVec *)PTR_TO_DATAPTR( ~a ))"))
     ,@body))

