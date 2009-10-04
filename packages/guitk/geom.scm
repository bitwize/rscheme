
(define-class <point> (<object>)
  x
  y)

(define-class <size> (<object>)
  width
  height)

(define-class <rect> (<object>)
  (origin '#uninit <point>)
  (size '#uninit <size>))


(define-generic-function max-x)
(define-generic-function max-y)

(define-method width ((self <rect>)) (width (size self)))
(define-method height ((self <rect>)) (height (size self)))
(define-method x ((self <rect>)) (x (origin self)))
(define-method y ((self <rect>)) (y (origin self)))

(define-method max-x ((self <rect>))
    (+ (x (origin self)) (width (size self))))

(define-method max-y ((self <rect>)) 
    (+ (y (origin self)) (height (size self))))

(define (make-rect x y w h)
  (make <rect>
	origin: (make <point> x: x y: y)
	size: (make <size> width: w height: h)))
