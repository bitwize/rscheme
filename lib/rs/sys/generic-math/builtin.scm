;;;
;;;   This file is organized by method.  
;;;
;;;   It could provide appropriate methods for the built-in number
;;;   types (e.g., <fixnum>, <bignum>, etc.) but there is no inlining
;;;   or compile-time method selection happening that could take
;;;   advantage of that, so we just defer to the base arithmetic in
;;;   the default case.  
;;;
;;;   If the arguments are not comprehended by the base system, and
;;;   a more specific method is not available, the core runtime system
;;;   will signal an error.

(define-method binary+ ((a <number>) (b <number>))
  (base+ a b))

(define-method binary- ((a <number>) (b <number>))
  (base- a b))

(define-method binary* ((a <number>) (b <number>))
  (base* a b))

(define-method binary/ ((a <number>) (b <number>))
  (base/ a b))

(define-method quotient ((a <number>) (b <number>))
  (base-quotient a b))

(define-method remainder ((a <number>) (b <number>))
  (base-remainder a b))

(define-method modulo ((a <number>) (b <number>))
  (base-modulo a b))


;;;

(define-method floor ((n <number>)) (sys:floor n))
(define-method ceiling ((n <number>)) (sys:ceiling n))
(define-method truncate ((n <number>)) (sys:truncate n))
(define-method round ((n <number>)) (sys:round n))

(define-method exp ((a <number>)) (sys:exp a))
(define-method log ((a <number>)) (sys:log a))
(define-method sin ((a <number>)) (sys:sin a))
(define-method cos ((a <number>)) (sys:cos a))
(define-method tan ((a <number>)) (sys:tan a))
(define-method asin ((a <number>)) (sys:asin a))
(define-method acos ((a <number>)) (sys:acos a))
(define-method atan ((a <number>)) (sys:atan a))
(define-method atan ((a <number>) (b <number>)) (sys:atan a b))

(define-method sqrt ((a <number>)) (sys:sqrt a))
(define-method expt ((a <number>) (b <number>)) (sys:expt a b))

;;;

(define-method magnitude ((n <complex>))
  (let ((r (real-part n))
	(i (imag-part n)))
    (sqrt (+ (* r r) (* i i)))))

(define-method magnitude ((n <real>))
  n)

(define-method angle ((n <complex>))
  (atan (imag-part n) (real-part n)))

(define-method angle ((n <real>))
  0)

(define-method expt ((self <rect-complex>) (p <integer>))
  ;; Note: There's a better way to do this using the binomial theorem
  (if (zero? p)
      1
      (if (< p 0)
          (/ (expt self (- p)))
          (* self (expt self (- p 1))))))
      

(define-method expt ((self <rect-complex>) (p <number>))
  (let ((a (expt (magnitude self) p))
        (t (* (angle self) p)))
    (make-rectangular (* a (cos t))
                      (* a (sin t)))))

(define-method sqrt ((self <rect-complex>))
  (let ((a (sqrt (magnitude self)))
        (t (/ (angle self) 2)))
    (make-rectangular (* a (cos t))
                      (* a (sin t)))))


;;;
;;;  some slightly more specialized methods
;;;
;;;  (these should probably be builtin...)
;;;

(define-method floor ((self <mp-rational>))
  (quotient (numerator self) (denominator self)))

(define-constant $half (/ 1 2))

(define-method round ((self <mp-rational>))
  ;; is this exactly right...?  Check CLTL!
  ;; (esp. see case where n is k/2)
  (floor (+ self $half)))

(define-method ceiling ((self <mp-rational>))
  (floor (+ self 1)))
