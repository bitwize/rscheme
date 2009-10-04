
;;;  Note:  The `unwind-protect' is necessary to ensure that
;;;         the body doesn't leave interrupts disabled by
;;;         throwing an exception...

(define-syntax critical-section
  (syntax-form (() . body)
    (let ((was (os-set-sigenable #f)))
      (unwind-protect
        (begin . body)
        (os-set-sigenable was))
      (values)))
  (syntax-form ((end) . body)
    (let ((was (os-set-sigenable #f)))
      (let-syntax ((end (syntax-form args 
			  (values . args))))
        (unwind-protect
         (begin . body)
         (os-set-sigenable was))))))

