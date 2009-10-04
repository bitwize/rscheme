(define-class <geometric> (<object>) :abstract)   ; DUIM calls this a <region>

;;;

(define-class <area> (<geometric>) :abstract)
(define-class <curve> (<geometric>) :abstract)    ; DUIM calls this a <path>

;;;

(define-class <point> (<geometric>)
  (x type: <real> :sealed)
  (y type: <real> :sealed))

(define-constant $zero-point (make <point> x: 0 y: 0))

(define-class <size> (<geometric>)
  (dx type: <real> :sealed)
  (dy type: <real> :sealed))

(define-constant $zero-size (make <size> dx: 0 dy: 0))

(define-method width ((self <size>))
  (dx self))

(define-method height ((self <size>))
  (dy self))

(define-class <line> (<curve>)
  (from type: <point> :sealed)
  (to type: <point> :sealed))

(define-inline (make-point (x <real>) (y <real>))
  (let (((t <point> :trust-me) (make-gvec <point> x y)))
    t))
#|
  (make <point>
	x: x
	y: y)
|#

(define (make-size (dx <real>) (dy <real>))
  (make <size>
	dx: dx
	dy: dy))

;;;

(define (make-line x1 y1 x2 y2)
  (make <line>
	from: (make-point x1 y1)
	to: (make-point x2 y2)))

(define-class <rect> (<area>)
  (origin-x type: <real> :sealed)
  (origin-y type: <real> :sealed)
  (size-width type: <real> :sealed)
  (size-height type: <real> :sealed))

(define-constant $zero-rect (make <rect> 
				  origin-x: 0 origin-y: 0 
				  size-width: 0 size-height: 0))

(define-method x ((self <rect>)) (origin-x self))
(define-method y ((self <rect>)) (origin-y self))
(define-method width ((self <rect>)) (size-width self))
(define-method height ((self <rect>)) (size-height self))

(define-method size ((r <rect>))
  (make-size (size-width r) (size-height r)))

(define-method origin ((r <rect>))
  (make-point (origin-x r) (origin-y r)))


(define-method limit ((r <rect>))
  (make-point (limit-x r) (limit-y r)))


(define (make-rect (x <real>) (y <real>) (w <real>) (h <real>))
  (make <rect>
	origin-x: x
	origin-y: y
	size-width: w
	size-height: h))

(define (make-rect2 (origin <point>) (size <size>))
  (make <rect>
	origin-x: (x origin)
	origin-y: (y origin)
	size-width: (width size)
	size-height: (height size)))

;;; works even if (UR < LL)

(define (bbox-rect (llx <real>) (lly <real>) (urx <real>) (ury <real>))
  (let ((w (- urx llx))
	(h (- ury lly)))
    (make-rect (if (< w 0) urx llx)
	       (if (< h 0) ury lly)
	       (abs w)
	       (abs h))))

#| 
   rectangle (box) corners
|#

(define lower-left origin)
(define upper-right limit)

(define-method lower-right ((r <rect>))
  (make-point (limit-x r) (origin-y r)))

(define-method upper-left ((r <rect>))
  (make-point (origin-x r) (limit-y r)))


(define-method display-object ((self <point>) port)
  (format port "~d,~d" (x self) (y self)))

(define-method display-object ((self <size>) port)
  (format port "~dx~d" (dx self) (dy self)))

(define-method write-object ((self <point>) port)
  (format port "#[<point> ~d ~d]" (x self) (y self)))

(define-method write-object ((self <size>) port)
  (format port "#[<size> ~d x ~d]" (dx self) (dy self)))

(define-method write-object ((self <rect>) port)
  (format port "#[<rect> ~d ~d x ~d ~d]"
	  (x self)
	  (y self)
	  (width self)
	  (height self)))

;;;

(define (limit-x (r <rect>))
  (+ (origin-x r) (size-width r)))

(define (limit-y (r <rect>))
  (+ (origin-y r) (size-height r)))

(define (center-x (r <rect>))
  (+ (origin-x r) (/ (size-width r) 2)))

(define (center-y (r <rect>))
  (+ (origin-y r) (/ (size-height r) 2)))

;;;

(define (rect->values (r <rect>))
  (values (origin-x r) (origin-y r) (size-width r) (size-height r)))

(define (point->values (p <point>))
  (values (x p) (y p)))

(define (size->values (s <size>))
  (values (dx s) (dy s)))

(define (point->size (p <point>))
  (make-size (x p) (y p)))

(define (size->point (s <size>))
  (make-point (dx s) (dy s)))

;;;

(define-method chop ((self <point>))
  (bind ((a b (point->values self))
         (a0 (chop a))
         (b0 (chop b)))
    (if (and (eq? a a0)
             (eq? b b0))
        self
        (make-point a0 b0))))

(define-method chop ((self <size>))
  (bind ((a b (size->values self))
         (a0 (chop a))
         (b0 (chop b)))
    (if (and (eq? a a0)
             (eq? b b0))
        self
        (make-size a0 b0))))

;;;


(define-method equal? ((a <point>) b)
  (and (instance? b <point>)
       (= (x a) (x b))
       (= (y a) (y b))))

(define-method equal? ((a <size>) b)
  (and (instance? b <size>)
       (= (dx a) (dx b))
       (= (dy a) (dy b))))

(define-method equal? ((a <rect>) b)
  (and (instance? b <rect>)
       (= (origin-x a) (origin-x b))
       (= (origin-y a) (origin-y b))
       (= (size-width a) (size-width b))
       (= (size-height a) (size-height b))))

(define-method bbox ((self <point>))
  (make-rect (x self) (y self) 0 0))

(define-method bbox ((self <line>))
  (let* (((p0 <point>) (from self))
         ((p1 <point>) (to self)))
    (let* ((x0 (min (x p0) (x p1)))
           (x1 (max (x p0) (x p1)))
           (y0 (min (y p0) (y p1)))
           (y1 (max (y p0) (y p1))))
      (bbox-rect x0 y0 x1 y1))))
  
(define-method bbox ((self <rect>))
  self)

(define (line-horizontal? (self <line>))
  (= (y (to self)) (y (from self))))

(define (line-vertical? (self <line>))
  (= (x (to self)) (x (from self))))

(define-method normal-on ((self <line>) #optional t)
  (let ((d (normalize (point- (to self) (from self)))))
    (make-size (- (dy d)) (dx d))))

;;;

(define-generic-function rotate)
(define-generic-function cubert)
(define-generic-function x:sqrt)

;;;
;;;  Trim off one or more sides of a rectangle
;;;
;;;     (trim-rect R top: y bottom: y left: x right: x)
;;;
;;;  is equivalent to:
;;;
;;;     (inset-rect R x y)

(define (trim-rect (self <rect>) 
                   #key (top default: #f)
                        (bottom default: #f)
                        (left default: #f)
                        (right default: #f))
  (bind ((x y w h (rect->values self)))
    (if top
        (begin
          (set! h (- h top))))
    (if bottom
        (begin
          (set! y (+ y bottom))
          (set! h (- h bottom))))
    (if left
        (begin
          (set! x (+ x left))
          (set! w (- w left))))
    (if right
        (begin
          (set! w (- w right))))
    (make-rect x y w h)))


