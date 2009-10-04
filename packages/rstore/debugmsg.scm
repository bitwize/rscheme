;;;
;;;  This is a stop-gap measure until we get logging facilities
;;;  into the core system (on the table for 0.7.4...)
;;;

(define *log-verbose-mode* (delay (and (getenv "RS_LVERBOSE") #t)))

(define-macro (dm . opts)
  (if (number? (car opts))
      `(if (force *log-verbose-mode*)
           (format #t ,(format #f "463-~04dD ~a\n" (car opts) (cadr opts))
                   ,@(cddr opts)))
      `(if (force *log-verbose-mode*)
           (format #t ,(format #f "463-0000D ~a\n" (car opts)) ,@(cdr opts)))))

(define-macro (em num msg . opts)
  `(error ,(format #f "463-~04dE ~a" num msg) ,@opts))

;;
