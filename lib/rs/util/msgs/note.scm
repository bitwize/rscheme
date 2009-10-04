;;  (note [type: TYPE] [NUM] FMT ARG ...)

(define-macro (note . args)
  (bind ((msg args xtra (foo 'notice args))
	 (mn (gensym)))
    `(let ((,mn (alloc-message ,args ,@msg)))
       (if (other-message-enabled? ,mn)
	   (show-message ,mn (vector ,@args))
	   (values)))))

;(&module (export note))
