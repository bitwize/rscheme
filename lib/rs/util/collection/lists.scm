(define-method iterator ((self <pair>))
  (let ((p self))
    (lambda ()
      (if (pair? p)
          (let ((x (car p)))
            (set! p (cdr p))
            x)
          (error (make <

(define-method next ((self <pair>))
  (cdr self))

(define-method first ((self <empty-list>))
  (error (make <
