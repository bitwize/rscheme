;;;

(define-class <x-pixmap-image-rep> (<image-rep>)
  for-drawable
  use-colormap
  (x-pixmap init-value: #f))

(define-method initialize ((self <x-pixmap-image-rep>))
  (next-method)
  (register-for-finalization self))

(define-method finalize ((self <x-pixmap-image-rep>))
  (dm 459 "finalize: ~s" self))

(define (fill-x-pixmap (self <x-pixmap-image-rep>))
  (let* (((im <graphic-image>) (rep-of self))
	 (xi-rep (x-image-rep im (use-colormap self)))
	 (px (create-pixmap width: (image-width im)
			    height: (image-height im)
			    depth: (image-depth (x-image xi-rep))
			    drawable: (for-drawable self)))
	 (gc (create-gcontext drawable: (for-drawable self))))
    (dm 451 "fill pixmap ~s ~dx~dx~d" self 
        (image-width im)
        (image-height im)
        (image-depth (x-image xi-rep)))
    (x-composite xi-rep px gc 0 0)
    (set-x-pixmap! self px)
    px))

(define (get-x-pixmap (self <x-pixmap-image-rep>))
  (or (x-pixmap self)
      (fill-x-pixmap self)))

(define-method x-composite ((self <x-pixmap-image-rep>) win gc x y)
  ;(dm 452 "pixmap composite ~s at ~d,~d" self x y)
  (copy-area (get-x-pixmap self)
	     gc
	     0
	     0
	     (image-width self)
	     (image-height self)
	     win
	     x
	     y))

;;;

(define (x-pixmap-image-rep (self <graphic-image>) drawable colormap)
  ;(dm "x-pixmap-image-rep: ~s ~s" self drawable)
  (or (get-image-rep self
		     <x-pixmap-image-rep>
		     (lambda (r)
		       (eq? (for-drawable r) drawable)))
      (make <x-pixmap-image-rep>
	for-drawable: drawable
	use-colormap: colormap
	rep-of: self)))

(define (image->pixmap (self <graphic-image>) drawable colormap)
  (get-x-pixmap (x-pixmap-image-rep self drawable colormap)))
