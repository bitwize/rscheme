
(define-macro (define-zlib-glue args . body)
  `(define-safe-glue ,args
     properties: ((other-h-files "<zlib.h>")
		  (other-libs "z"))
     ,@body))

