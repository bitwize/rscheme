
;;;

(define-method inexact->exact ((self <size>))
  (make-size (inexact->exact (dx self)) 
	     (inexact->exact (dy self))))

(define-method inexact->exact ((self <point>))
  (make-point (inexact->exact (x self)) 
	      (inexact->exact (y self))))

(define-method inexact->exact ((self <rect>))
  (make-rect (inexact->exact (origin-x self)) 
	     (inexact->exact (origin-y self))
	     (inexact->exact (size-width self))
	     (inexact->exact (size-height self))))

;;;

(define-method ceiling ((self <size>))
  (make-size (ceiling (dx self)) (ceiling (dy self))))

(define-method ceiling ((self <point>))
  (make-size (ceiling (x self)) (ceiling (y self))))

;;;
;;; returns an enclosing (>= in size) rect

(define-method ceiling ((self <rect>))
  (let ((ox (origin-x self))
	(oy (origin-y self))
	(lx (limit-x self))
	(ly (limit-y self)))
    (bbox-rect (floor ox) 
	       (floor oy)
	       (ceiling lx)
	       (ceiling ly))))

;;;

(define-method floor ((self <size>))
  (make-size (floor (dx self)) (floor (dy self))))

(define-method floor ((self <point>))
  (make-size (floor (x self)) (floor (y self))))

;;; returns a contained (<= in size) rect

(define-method floor ((self <rect>))
  (let ((ox (origin-x self))
	(oy (origin-y self))
	(lx (limit-x self))
	(ly (limit-y self)))
    (bbox-rect (ceiling ox) 
	       (ceiling oy)
	       (floor lx)
	       (floor ly))))
