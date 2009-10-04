
;; debug message

(define *debug-messages-enabled* #t)

(define-syntax (debug-message-enabled? self)
  (and *debug-messages-enabled* (other-message-enabled? self)))

(define (debug-messages-disable #optional major)
  (if major
      (message-table-config (get-message-table-by-id major)
                            type: 'debug
                            disable: #t)
      (set! *debug-messages-enabled* #f)))

(define (debug-messages-enable #optional major)
  (if major
      (message-table-config (get-message-table-by-id major)
                            type: 'debug
                            enable: #t)
      (set! *debug-messages-enabled* #t)))

;;  (dm [type: TYPE] [NUM] FMT ARG ...)

(define-macro (dm . args)
  (bind ((msg args xtra (foo 'debug args))
	 (mn (gensym)))
    `(let ((,mn (alloc-message ,args ,@msg)))
       (if (debug-message-enabled? ,mn)
	   (show-message ,mn (vector ,@args))
	   (values)))))

