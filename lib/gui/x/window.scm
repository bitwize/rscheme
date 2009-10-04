
(define-class <x-drawable> (<x-object>)
  (properties type: <vector> init-value: '#())
  (cache #| type: (union #f <with-state-cache>) |# init-value: #f)
  (drawable-root #|type: <x-drawable>|#))

(define (drawable-screen (self <x-drawable>)) ;; *** extension to CLX
  (get-property (drawable-root self) 'screen))

(define (drawable-display (self <x-drawable>))
  (x-display self))

(define (drawable=? (a <x-drawable>) (b <x-drawable>))
  (eq? a b))

(define (drawable-id (self <x-drawable>))
  (x-id self))

(define (drawable? object)
  (instance? object <x-drawable>))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-class <x-window> (<x-drawable>))

#|

(define (window-visual (self <x-window>))
  (x-id (visual self)))

(define (window-map-state (self <x-window>))
  ...)

(define (window-colormap-installed? (self <x-window>))
  ...)
|#

(define (window-display (self <x-window>))
  (x-display self))

(define (window=? (a <x-window>) (b <x-window>))
  (eq? a b))

(define (window-id (self <x-window>))
  (x-id self))

(define (window? object)
  (instance? object <x-window>))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-class <x-pixmap> (<x-drawable>))

(define (pixmap? x)
  (instance? x <x-pixmap>))

