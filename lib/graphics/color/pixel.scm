
(define-class <pixel> (<rgb-color>)
  (alpha-component type: <fixnum> :sealed))

(define-method alpha-component ((self <color>))
  65535)

(define-method pixel-alpha ((self <color>))
  (/ (alpha-component self) 65535.0))

(define (color-comp r)
  (min 65535 (max 0 (inexact->exact (truncate (* r 65536))))))

(define-syntax (rgba8->pixel r g b a)
  (make-gvec <pixel>
             (fixnum* r 257)
             (fixnum* g 257)
             (fixnum* b 257)
             (fixnum* a 257)))
             
(define (make-pixel #key (red type: <real> default: 1) 
                         (green type: <real> default: 1) 
                         (blue type: <real> default: 1)
                         (alpha type: <real> default: 1))
  (make <pixel>
    red-component: (color-comp red)
    green-component: (color-comp green)
    blue-component: (color-comp blue)
    alpha-component: (color-comp alpha)))

(define-method alpha-componenet ((self <color>))
  65535)

(define-constant $clear (make-pixel alpha: 0))

(define-method pixel-rgba ((self <color>))
  (values (red-component self)
          (green-component self)
          (blue-component self)
	  65535))

(define-method pixel-rgba ((self <pixel>))
  (values (red-component self)
          (green-component self)
          (blue-component self)
	  (alpha-component self)))
