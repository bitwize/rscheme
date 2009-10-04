(define-constant $pi (* 2 (sys:atan 1 0)))

#|
(define-class <rect-complex> (<complex>)
  (rect-real type: <real>)
  (rect-imag type: <real>))

(define-method number-as-string ((self <rect-complex>) radix)
  (string-append (number-as-string (rect-real self) radix)
		 (if (< (rect-imag self) 0)
		     "-"
		     "+")
		 (number-as-string (abs (rect-imag self)) radix)
		 "i"))

(define (make-rectangular (r <real>) (i <real>))
  (make <rect-complex> 
	rect-real: r
	rect-imag: i))

(define-method binary+ ((a <rect-complex>) (b <rect-complex>))
  (make-rectangular (+ (rect-real a) (rect-real b))
		    (+ (rect-imag a) (rect-imag b))))

(define-method binary+ ((a <rect-complex>) (b <real>))
  (make-rectangular (+ (rect-real a) b) (rect-imag a)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;           polar
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-class <polar-complex> (<complex>)
  (polar-magnitude type: <real>)
  (polar-angle type: <real>))

(define (make-polar (m <real>) (a <real>))
  (let* ((a2 (if (negative? m)
		 (+ a (/ $pi 2))
		 a))
	 (n (round (/ a2 (* $pi 2)))))
    (make <polar-complex>
	  polar-magnitude: (abs m)
	  polar-angle: (- a2 (* n (* $pi 2))))))

(define-method number-as-string ((self <polar-complex>) radix)
  (string-append (number-as-string (polar-magnitude self) radix)
		 "@"
		 (number-as-string (rad->deg (polar-angle self)) radix)))

(define (deg->rad x)
  (* x (/ 180 $pi)))

(define (rad->deg x)
  (* x (/ $pi 180)))
|#
