
;;;

(define (cube x)
  (* x x x))

(define-method cubert ((self <real>))
  (if (< self 0)
      (- (cubert (- self)))
      (expt self 1/3)))

(define-method cubert ((self <rect-complex>))
  (expt self 1/3))

(define-method x:zero? ((self <number>)) (zero? self))
(define-method x:negative? ((self <number>)) (< self 0))

(define-method x:sqrt ((x <number>))
  (if (< x 0)
      (make-rectangular 0 (x:sqrt (- x)))
      (let* ((s (sqrt x))
             (n (inexact->exact s)))
        (if (= (* n n) x)
            n
            s))))

(define-method x:angle ((self <real>)) 0)
(define-method x:magnitude ((self <real>)) self)

(define-method x:magnitude ((self <rect-complex>))
  (let ((r (real-part self))
	(i (imag-part self)))
    (sqrt (+ (* r r) (* i i)))))


(define-method complex->real ((self <real>)) 
  self)

(define-method complex->real ((self <rect-complex>))
  ;; XXX how do we properly define this tolerance?
  (if (< (* (abs (imag-part self)) 10000.0) (abs (real-part self)))
      (x:magnitude self)
      #f))
      

(define-method x:angle ((self <rect-complex>))
  (atan (imag-part self) (real-part self)))
  
;;;

(define-class <polynomial-root-failure> (<condition>)
  type
  polynomial)

(define-method display-object ((self <polynomial-root-failure>) port)
  (format port "*** Could not compute root(s) of polynomial:\n")
  (format port "       ")
  (let loop ((power (- (length (polynomial self)) 1))
             (coeffs (reverse (polynomial self)))
             (first? #t))
    (if (null? coeffs)
        (format port " = 0\n*** Equation is ~a!\n" (type self))
        (begin
          (if (not first?)
              (if (negative? (car coeffs))
                  (format port " - ~d" (- (car coeffs)))
                  (format port " + ~d" (car coeffs)))
              (format port "~d" (car coeffs)))
          (case power
            ((0) (values))
            ((1) (format port "x"))
            (else (format port "x^~d" power)))
          (loop (- power 1)
                (cdr coeffs)
                #f)))))

;;; Adapted from Winston & Horn, LISP, 3rd. Ed.

(define (compute-linear-roots a b)              ; ax + b
  (if (zero? a)
      (signal (make <polynomial-root-failure>
                    type: (if (zero? b)
                              'homogeneous
                              'inconsistent)
                    polynomial: (list b a)))
      (if (zero? b)
          '(0)
          (list (/ (- b) a)))))

(define (conjugate-pair a b)
  (list (make-rectangular a b)
        (make-rectangular a (- b))))

(define (compute-quadratic-roots a b c)         ; ax^2 + bx + c
  (cond
   ;; simplify choices for numerical accuracy by making
   ;; sure the leading coefficient is positive
   ((x:negative? a) (compute-quadratic-roots (- a) (- b) (- c)))
   ;; it's really a linear equation...
   ((x:zero? a) (compute-linear-roots b c))
   ;; a convenient special case if c == 0
   ((x:zero? c) (cons 0 (compute-linear-roots a b)))
   ;; and now to the quadratic formula...
   (else
    ;; compute the discriminant `d'
    (let ((d (- (* b b) (* 4 a c)))
          (a2 (* 2 a)))
      (cond
       ;; there are two complex roots which are complex conjugates
       ((< d 0)
        (conjugate-pair (/ (- b) a2)
                        (/ (x:sqrt (- d)) a2)))
       ;; both roots are the same
       ((= d 0)
        (let ((r (/ (- b) a2)))
          (list r r)))
       ;; two real roots...  Two cases based on sign of `b'
       ;; so that we achieve best numerical accuracy
       ((< b 0)
        (let ((r (- (x:sqrt d) b)))
          (list (/ r a2)
                (/ (* 2 c) r))))
       (else
        (let ((r (- (+ (x:sqrt d) b))))
          (list (/ (* 2 c) r)
                (/ r (* 2 a))))))))))

(define (compute-cubic-roots a b c d)
  ;; handle some special cases first...
  (cond
   ((< a 0) (compute-cubic-roots (- a) (- b) (- c) (- d)))
   ((zero? a) (compute-quadratic-roots b c d))
   ((zero? d) (cons 0 (compute-quadratic-roots a b c)))
   (else
    ;; general case... Solve for the resolvent roots, first.
    ;; the resolvent is x^2 + (2b^3 - 9abc +27a^2d)x - (3ac-b^2)^3 == 0
    (let ((rroots (compute-quadratic-roots 1
                                           (+ (* 2 b b b)
                                              (* 9 a (- (* 3 a d)
                                                        (* b c))))
                                           (cube (- (* b b) (* 3 a c))))))
      ;(format #t "R roots => ~s\n" rroots)
      (if (real? (car rroots))
           ;; roots of original polynomial are complex...
          (let ((r (cubert (car rroots)))
                (s (cubert (cadr rroots))))
            (let ((sroot (/ (- (+ r s) b) (* a 3)))
                  (real (/ (- (- (/ (+ r s) 2)) b) (* a 3))))
              (if (= r s)
                  (list sroot real real)
                  (let ((imag (/ (* (- r s) 
                                    (/ (sqrt 3) 2)) 
                                 (* a 3))))
                    (list sroot
                          (make-rectangular real imag)
                          (make-rectangular real (- imag)))))))
          ;; roots are real...
          (let ((r (x:magnitude (car rroots)))
                (t (x:angle (car rroots))))
            ;(format #t "  r ~s   t ~s\n" r t)
            (let ((rd (* 2 (cubert r)))
                  (cd (/ (cos (/ t 3)) -2))
                  (sd (/ (* (sin (/ t 3)) (sqrt 3)) 2)))
              (list (/ (- (* -2 rd cd) b) (* 3 a))
                    (/ (- (* rd (+ cd sd)) b) (* 3 a))
                    (/ (- (* rd (- cd sd)) b) (* 3 a))))))))))
