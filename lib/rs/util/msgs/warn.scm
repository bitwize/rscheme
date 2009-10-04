
;; warning message

(define-macro (wm . args)
  (bind ((msg args xtra (foo 'warning args))
	 (mn (gensym)))
    `(let ((,mn (alloc-message ,args ,@msg)))
       (if (other-message-enabled? ,mn)
	   (show-message ,mn (vector ,@args))
	   (values)))))
