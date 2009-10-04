(define-class <geometric> (<object>) :abstract)

(define-class <point> (<geometric>)
  x
  y)

(define $zero-point (make <point>
			  x: 0
			  y: 0))

(define-class <size> (<geometric>)
  width
  height)

(define-class <area> (<geometric>) :abstract)

(define-class <rect> (<area>)
  origin-x
  origin-y
  size-width
  size-height)

(define (rect-size (r <rect>))
  (make <size>
	width: (size-width r)
	height: (size-height r)))

(define (rect-origin (r <rect>))
  (make <point>
	x: (origin-x r)
	y: (origin-y r)))

(define (make-rect x y w h)
  (make <rect>
	origin-x: x
	origin-y: y
	size-width: w
	size-height: h))

(define-method write-object ((self <rect>) port)
  (format port "#[<rect> ~d,~d ~d*~d]"
	  (origin-x self)
	  (origin-y self)
	  (size-width self)
	  (size-height self)))

(define-method write-object ((self <point>) port)
  (format port "#[<point> ~d,~d]" (x self) (y self)))

(define-method write-object ((self <size>) port)
  (format port "#[<size> ~d*~d]" (width self) (height self)))

(define (limit-x (r <rect>))
  (+ (origin-x r) (size-width r)))

(define (limit-y (r <rect>))
  (+ (origin-y r) (size-height r)))

(define (center-x (r <rect>))
  (+ (origin-x r) (quotient (size-width r) 2)))

(define (center-y (r <rect>))
  (+ (origin-y r) (quotient (size-height r) 2)))

(define (point-in-rect? (r <rect>) (pt <point>))
  (and (>= (x pt) (origin-x r))
       (>= (y pt) (origin-y r))
       (< (x pt) (limit-x r))
       (< (y pt) (limit-y r))))

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

(define (coord-intersect x1 w1 x2 w2)
  (let ((m1 (+ x1 w1))
	(m2 (+ x2 w2)))
    (if (< x1 x2)
	;;
	;;  cases i, ii, iii
	;;
	(if (< m1 x2)
	    ;;
	    ;; case i
	    ;;
	    (values 0 0)
	    ;;
	    ;; cases ii, iii
	    ;;
	    (if (< m1 m2)
		;;
		;; case ii
		;;
		(values x2 (- m1 x2))
		;;
		;; case iii
		;;
		(values x2 w2)))
	;;
	;;  cases iv, v, vi
	;;
	(if (< m1 m2)
	    ;;
	    ;;  case iv
	    ;;
	    (values x1 w1)
	    ;;
	    ;;  cases v, vi
	    ;;
	    (if (< x1 m2)
		;;
		;; case v
		;;
		(values x1 (- m2 x1))
		;;
		;; case vi
		;;
		(values 0 0))))))
		
(define (intersect-rect (accum <rect>) (arg <rect>))
  (bind ((x w (coord-intersect (origin-x accum)
			       (size-width accum)
			       (origin-x arg)
			       (size-width arg)))
	 (y h (coord-intersect (origin-y accum)
			       (size-height accum)
			       (origin-y arg)
			       (size-height arg))))
    (make-rect x y w h)))

    
