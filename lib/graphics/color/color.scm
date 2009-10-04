
(define-class <color> (<object>) :abstract)

(define-class <gray-color> (<color>)
  (white-component type: <fixnum> :sealed))

#|
(define-class <hsv-color> (<color>)
  (hue-component type: <fixnum> :sealed)
  (saturation-component type: <fixnum> :sealed)
  (value-component type: <fixnum> :sealed))
|#

(define-class <cmyk-color> (<color>)
  ;; the components are expressed in 0..65535
  (cyan-component type: <fixnum> :sealed)
  (magenta-component type: <fixnum> :sealed)
  (yellow-component type: <fixnum> :sealed)
  (black-component type: <fixnum> :sealed))
  
(define-class <rgb-color> (<color>)
  ;; the components are expressed in 0..65535
  (red-component type: <fixnum> :sealed)
  (green-component type: <fixnum> :sealed)
  (blue-component type: <fixnum> :sealed))

;;;

(define (color? x)
  (instance? x <color>))


(define (color-comp r)
  (min 65535 (max 0 (inexact->exact (round (* r 65535))))))

(define-syntax color
  (syntax-form (g)
    (make <gray-color>
          white-component: (color-comp g)))
  (syntax-form (r g b)
    (make <rgb-color>
          red-component: (color-comp r)
          green-component: (color-comp g)
          blue-component: (color-comp b))))
    
#|
(define-macro color
  (macro-rules ()
    ((_ #key red green blue)
     (if (and (real? red) (>= red 0) (<= red 1)
              (real? green) (>= green 0) (<= green 1)
              (real? blue) (>= blue 0) (<= blue 1))
         (list 'quote
               (make <rgb-color>
                     red-component: (color-comp red)
                     green-component: (color-comp green)
                     blue-component: (color-comp blue)))
         (error "color: 0-1-valued constants required")))
    ((_ #key gray)
     (if (and (real? gray) (>= gray 0) (<= gray 1))
         (list 'quote
               (make <gray-color>
                     white-component: (color-comp gray)))
         (error "color: 0-1-valued constant required" gray)))))
|#

(define-constant $white (color 1))
(define-constant $black (color 0))

(define (mkcolor r g b) ; r,g,b in X coords ie, 0-65535
  (cond
   ((and (= r 0) (= g 0) (= b 0)) $black)
   ((and (= r 65535) (= g 65535) (= b 65535)) $white)
   (else
    (make <rgb-color>
	  red-component: r
	  green-component: g
	  blue-component: b))))

(define (make-rgb #key
                  (red-component default: 0) 
                  (green-component default: 0)
                  (blue-component default: 0))
  (make <rgb-color>
        red-component: red-component
        green-component: green-component
        blue-component: blue-component))
        

(define (make-gray #key (gray type: <real> default: 1))
  (make <gray-color> white-component: (color-comp gray)))

(define (make-color #rest r)
  (if (null? r)
      $black
      (case (car r)
        ((red: green: blue:) (apply make-rgb-color r))
        ((red-component: green-component: blue-component:)
         (apply make-rgb r))
        ((cyan: magenta: yellow: black:) (apply make-cmyk-color r))
        ((gray:) (apply make-gray r))
        (else (error "make-color: unrecognized color spec: ~s" r)))))

(define (make-cmyk-color #key
                         (cyan type: <real> default: 0)
                         (magenta type: <real> default: 0)
                         (yellow type: <real> default: 0)
                         (black type: <real> default: 0))
  (make <cmyk-color>
        cyan-component: (color-comp cyan)
        magenta-component: (color-comp magenta)
        yellow-component: (color-comp yellow)
        black-component: (color-comp black)))

(define (make-rgb-color #key
                        (red type: <real> default: 1) 
                        (green type: <real> default: 1) 
                        (blue type: <real> default: 1))
  (mkcolor (color-comp red)
	   (color-comp green)
	   (color-comp blue)))

(define-method color-rgb-components ((self <gray-color>))
  (let ((w (white-component self)))
    (values w w w)))

(define-method color-rgb-components ((self <rgb-color>))
  (values (red-component self)
          (green-component self)
          (blue-component self)))

(define-method color-rgb-components ((self <cmyk-color>))
  (bind ((r (- 65535 (cyan-component self)))       ; Foley & van Dam p.588
         (g (- 65535 (magenta-component self)))
         (b (- 65535 (yellow-component self)))
         (k (black-component self)))
    (values (max 0 (- r k))
            (max 0 (- g k))
            (max 0 (- b k)))))

(define-method color-cmyk-components ((self <rgb-color>))
  (bind ((r g b (color-rgb-components self))
         (k (min r g b)))
    (values (max 0 (- r k))
            (max 0 (- g k))
            (max 0 (- b k))
            k)))
            
(define-method color-cmyk-components ((self <cmyk-color>))
  (values (cyan-component self)
          (magenta-component self)
          (yellow-component self)
          (black-component self)))

(define-method color-rgb ((self <color>))       ; use lower-level GF...
  (bind ((r g b (color-rgb-components self)))
    (values (/ r 65535.0)
            (/ g 65535.0)
            (/ b 65535.0))))

(define-method color-cmyk ((self <color>))
  (bind ((c m y k (color-cmyk-components self)))
    (values (/ c 65535.0)
            (/ m 65535.0)
            (/ y 65535.0)
            (/ k 65535.0))))

(define-method color-rgb ((self <gray-color>))
  (let ((g (/ (white-component self) 65535.0)))
    (values g g g)))

(define-method color-rgb ((self <rgb-color>))
  (values (/ (red-component self) 65535.0)
          (/ (green-component self) 65535.0)
          (/ (blue-component self) 65535.0)))

;;;

(define-method color-red ((self <color>))
  (bind ((r g b (color-rgb self))) r))

(define-method color-green ((self <color>))
  (bind ((r g b (color-rgb self))) g))

(define-method color-blue ((self <color>))
  (bind ((r g b (color-rgb self))) b))

;;;

(define-method color-red ((self <rgb-color>))
  (/ (red-component self) 65535.0))

(define-method color-green ((self <rgb-color>))
  (/ (green-component self) 65535.0))

(define-method color-blue ((self <rgb-color>))
  (/ (blue-component self) 65535.0))

;;;

(define-method color-red ((self <gray-color>))
  (/ (white-component self) 65535.0))

(define-method color-green ((self <gray-color>))
  (/ (white-component self) 65535.0))

(define-method color-blue ((self <gray-color>))
  (/ (white-component self) 65535.0))

;;;
