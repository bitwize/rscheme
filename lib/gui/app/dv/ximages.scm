
(define (drawable-colormap d)
  (get-property d
		'colormap
		(let ((c (screen-default-colormap (drawable-screen d))))
		  (set-property! d 'colormap c)
		  c)))
      
;;;

(define-class <x-image-rep> (<image-rep>)
  for-cmap
  x-image)

(define-method composite ((self <x-image-rep>) win gc x y)
  (put-image win gc (x-image self) x: x y: y))

(define-method composite ((self <graphic-image>) win gc x y)
  (composite (x-image-rep self (drawable-colormap win)) win gc x y))

(define (x-image-rep (self <graphic-image>) x-cmap)
  (or (get-image-rep self 
		     <x-image-rep>
		     (lambda (r)
		       (eq? (for-cmap r) x-cmap)))
      (make-x-image-rep self x-cmap)))

(define (make-x-image-rep (g <graphic-image>) x-cmap)
  (let ((xi (case (colormap-visual-class x-cmap)
	      ((pseudo-color)
	       (pseudo-color-image-rep-from-true-color g x-cmap))
	      ((true-color)
	       (true-color-image-rep g x-cmap))
	      (else
	       (error "unsupported visual class: ~s" 
		      (colormap-visual-class x-cmap))))))
    (make <x-image-rep>
	  for-cmap: x-cmap
	  rep-of: g
	  x-image: xi)))

(define (find-bpp-and-pad dpy depth)
  (values 24 32))

(define (true-color-image-rep (g <graphic-image>) x-cmap)
  (bind ((depth 24)
	 (bpp pad (find-bpp-and-pad (colormap-display x-cmap) depth))
	 (w (image-width g))
	 (h (image-height g))
	 (data (make-vector (image-height g)))
	 (makepix (color->pixel-proc x-cmap))
	 #|(back (memory-image-rep
		(make-graphic-image from: (read-png-image $bg))))|#
	 (mem (memory-image-rep g)))
    ;;
    (for-each
     (lambda (y)
       (let ((row (make-vector w)))
	 ;(format #t "y = ~d\n" y)
	 (vector-set! data y row)
	 (for-each
	  (lambda (x)
	    (vector-set! row x (makepix (get-pixel mem x y))))
	  (range w))))
     (range h))
    ;;
    (create-image bits-per-pixel: bpp
		  data: data
		  depth: depth
		  width: (image-width g)
		  height: (image-height g))))

(define (get-dithering-proc cmap)
  (let ((c (get-property cmap 'dither-info #f)))
    (if (not c)
	(begin
	  (set! c (cons (get-color-map cmap) (make-color-table)))
	  (set-property! cmap 'dither-info c)))
    (let ((color-vec (car c))
	  (cache (cdr c)))
      (lambda (color x y)
	(let ((dither-pattern (table-lookup cache color)))
	  (if (not dither-pattern)
	      (begin
		(set! dither-pattern (compute-dithering color-vec color))
		(table-insert! cache color dither-pattern)))
	     (vector-ref dither-pattern 
			 (+ (bitwise-and x 3)
			    (* (bitwise-and y 3) 4))))))))

(define (pseudo-color-image-rep-from-true-color (g <graphic-image>) x-cmap)
  (let ((bpp 8)
	(depth 8)
	(w (image-width g))
	(h (image-height g))
	(data (make-vector (image-height g)))
	(dithered-pixel (get-dithering-proc x-cmap))
	(mem (memory-image-rep g)))
    ;;
    (for-each
     (lambda (y)
       (let ((ymod4 (modulo y 4))
	     (row (make-vector w)))
	 ;(format #t "y = ~d\n" y)
	 (vector-set! data y row)
	 (for-each
	  (lambda (x)
	    (vector-set! row x (dithered-pixel (get-pixel mem x y)
					       (modulo x 4)
					       ymod4)))
	  (range w))))
     (range h))
    ;;
    (create-image bits-per-pixel: bpp
		  data: data
		  depth: depth
		  data: data
		  width: (image-width g)
		  height: (image-height g))))


(define (get-color-map cmap)
  (let ((v (list->vector (range 256))))
    (vector-map cons
		(query-colors cmap 
			      (list->vector (range 256))
			      result-type: <vector>)
		v)))
