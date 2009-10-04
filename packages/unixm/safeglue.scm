
(define-macro (define-unix-glue args . body)
  `(define-syscall-glue ,args
     ,@body))
