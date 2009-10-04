(define-class <num-den-rational> (<rational>)
  (num type: <integer>)
  (den type: <integer>))

(define-syntax (make-rational n d)
  (let ((temp d))
    (if (= temp 1)
	n
	(make <num-den-rational>
	      num: n
	      den: temp))))

(define-method number-as-string ((self <num-den-rational>) radix)
  (string-append (number-as-string (num self) radix)
		 "/"
		 (number-as-string (den self) radix)))

;;

(define-method binary/ ((a <integer>) (b <integer>))
  (make-rational a b))

(define-method binary/ ((a <num-den-rational>) (b <integer>))
  (binary* a (make-rational 1 b)))

(define-method binary/ ((a <num-den-rational>) (b <num-den-rational>))
  (binary* a (make-rational (den b) (num b))))

;;

(define-method binary+ ((a <num-den-rational>) (b <num-den-rational>))
  (let* ((d (lcm (den a) (den b)))
	 (n (+ (* (num a) (quotient d (den a)))
	       (* (num b) (quotient d (den b)))))
	 (q (gcd n d)))
    (if (= q d)
	(quotient n q)
	(make-rational (quotient n q) (quotient d q)))))

(define-method binary+ ((a <num-den-rational>) (b <integer>))
  (make-rational (+ (num a) (* b (den a))) (den a)))

(define-method binary+ ((a <integer>) (b <num-den-rational>))
  (make-rational (+ (num b) (* a (den b))) (den b)))

;;;

(define-method binary* ((a <num-den-rational>) (b <integer>))
  (make-rational (* (num a) b) (den a)))

(define-method binary* ((a <integer>) (b <num-den-rational>))
  (make-rational (* a (num b)) (den b)))

(define-method binary* ((a <num-den-rational>) (b <num-den-rational>))
  (let* ((n (* (num a) (num b)))
	 (d (* (den a) (den b)))
	 (q (gcd n d)))
    (make-rational (quotient n q) (quotient d q))))

;;;

(define-method exact->inexact ((x <num-den-rational>))
  (as-float x))

(define-method as-float ((x <rational>))
  (float/ (as-float (numerator x))
	  (as-float (denominator x))))
