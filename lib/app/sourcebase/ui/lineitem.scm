(define-generic-function render-work-line-item)

(define-method render-work-line-item ((self <work-request>))
  (format #t "~-5d. ~8a ~a\n"
	  (id (base-request self))
	  (state (base-request self))
	  (title (base-request self))))

(define-method render-work-line-item ((self <fs-change>))
  (format #t "~-5d. ~8a ~a [~a]\n"
	  (id (base-request self))
	  (state (base-request self))
	  (title (base-request self))
	  (name (file-system self))))

(define-method render-work-line-item ((self <code-review>))
  (format #t "~-5d. ~8a ~a (review ~a)\n"
	  (id (base-request self))
	  (state (base-request self))
	  (title (base-request self))
	  (name (group self))))

(define-method render-work-line-item ((self <integration-request>))
  (format #t "~-5d. intgrate ~a (integrate into ~a)\n"
	  (id (base-request self))
	  (title (base-request self))
	  (name (file-system self))))
