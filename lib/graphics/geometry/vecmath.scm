
(define (point+ (a <point>) (b <size>))
  (make-point (+ (x a) (dx b)) (+ (y a) (dy b))))

(define (point- (a <point>) (b <point>))
  (make-size (- (x a) (x b)) (- (y a) (y b))))

(define (size- (a <size>) (b <size>))
  (make-size (- (dx a) (dx b)) (- (dy a) (dy b))))

(define (size+ (a <size>) (b <size>))
  (make-size (+ (dx a) (dx b)) (+ (dy a) (dy b))))

(define (inner-product (a <size>) (b <size>))
  (+ (* (dx a) (dx b))
     (* (dy a) (dy b))))

(define (size* (v <size>) (n <real>))
  (make-size (* (dx v) n)
	     (* (dy v) n)))

(define-method sign ((self <real>))
  (cond
   ((< self 0)
    -1)
   ((> self 0)
    1)
   (else
    0)))

(define (normalize (v <size>))
  ;; check some important common cases...
  (cond
   ((zero? (dy v))
    (make-size (sign (dx v)) 0))
   ((zero? (dx v))
    (make-size 0 (sign (dy v))))
   (else
    (size* v (/ (sqrt (inner-product v v)))))))

(define (inset-rect (r <rect>) dx dy)
  (make-rect (+ (origin-x r) dx)
   	     (+ (origin-y r) dy)
	     (- (size-width r) (* 2 dx))
	     (- (size-height r) (* 2 dy))))

(define (offset-point (p <point>) dx dy)
  (make <point>
	x: (+ (x p) dx)
	y: (+ (y p) dy)))

(define (offset-rect (r <rect>) dx dy)
  (make-rect (+ (origin-x r) dx)
   	     (+ (origin-y r) dy)
	     (size-width r)
	     (size-height r)))

(define-method scale ((r <rect>) s)
  (make-rect (* (origin-x r) s)
             (* (origin-y r) s)
             (* (size-width r) s)
             (* (size-height r) s)))

(define-method path-length ((self <line>))
  (distance (to self) (from self)))
