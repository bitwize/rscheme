
(define-macro (do-times (var cnt) . body)
  (let ((loopvar (gensym))
	(cntvar (gensym)))
    `(let ((,cntvar ,cnt))
       (let ,loopvar (((,var <fixnum>) 0))
	    (if (eq? ,var ,cntvar)
		(values)
		(begin
		  (begin ,@body)
		  (,loopvar (add1 ,var))))))))

(define (tiff-color-map->color-table img k)
  (let* ((v (get-tag-array img k 'color-map))
	 (d (quotient (vector-length v) 3))
	 (d2 (* d 2))
	 (pixs (make-vector d)))
    (for-each
     (lambda (i)
       (vector-set! pixs
		    i
		    (make-color red: (/ (vector-ref v i) 65535)
				green: (/ (vector-ref v (+ i d)) 65535)
				blue: (/ (vector-ref v (+ i d2)) 65535))))
     (range d))
    pixs))

;; supply 16 pixels for each color value, representing a 4x4 dithering

(define (get-color-map cmap)
  (let ((v (query-colors cmap 
			 (list->vector (range 256))
			 result-type: <vector>)))
    (vector-map cons v (list->vector (range (vector-length v))))))

(define *tmp* #f) ;; hack
(define *pre* (make-color-table)) ;; hack II

(define (alloc-dithered-pixel cmap color)
  (or (table-lookup *pre* color)
      (let* ((cv (or *tmp*
		     (begin
		       (set! *tmp* (get-color-map cmap))
		       *tmp*)))
	     (dith (compute-dithering cv color)))
	(format #t "computed dithering for: ~s\n" color)
	(table-insert! *pre* color dith)
	dith)))

  
#|
  (let ((cv (get-property cmap
			  'color-vector
			  (get-color-map cmap))))
    (compute-dithering cv color)))
|#

#|
  ;; forget dithering for now...
  (let ((c (alloc-color cmap color)))
    (make-vector 16 c))
|#

(define (alloc-exact-pixel cmap color)
  (or (table-lookup *pre* color)
      (let* ((cv (or *tmp*
		     (begin
		       (set! *tmp* (get-color-map cmap))
		       *tmp*)))
	     (dith (compute-dithering cv color)))
	(format #t "computed dithering for: ~s\n" color)
	(table-insert! *pre* color dith)
	dith)))

(define (alloc-dithered-pixel-map cmap color-tbl)
  (vector-map (curry alloc-dithered-pixel cmap) color-tbl))

(define (alloc-exact-pixel-map cmap color-tbl)
  (vector-map (curry alloc-exact-pixel cmap) color-tbl))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   procedures for building X images from 
;;;   RGB-color TIFF subimages
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; promote an 8-bit color sample to 16-bits
;;; (a cheap implementation of (s/255)*65535)

(define-syntax (promote-color-8->16 s)
  (let (((t <fixnum>) s))
    (fixnum+ (logical-shift-left t 8) t)))

(define (rgb-tiff->rgb-x cmap img k depth)
  (bind ((w (get-tag-scalar img k 'image-width))
	 (h (get-tag-scalar img k 'image-length))
	 (data (make-vector h))
	 (mkpx (rgb->pixel-proc cmap))
	 (bpp pad (find-bpp-and-pad (colormap-display cmap) depth)))
    ;
    (for-each-row 
     img k
     (lambda (y r)
       (let ((row (make-vector w)))
	 (vector-set! data y row)
	 (do-times (x w)
	   (vector-set! 
	    row
	    x
	    (mkpx (promote-color-8->16 (bvec-ref r (* x 3)))
		  (promote-color-8->16 (bvec-ref r (+ 1 (* x 3))))
		  (promote-color-8->16 (bvec-ref r (+ 2 (* x 3))))))))))
    ;
    ; NOTE! We need to be able to create an image with, 
    ;       eg, depth=24 and bits-per-pixel=32
    ;       (breeze (MkLinux) likes that)
    (create-image bits-per-pixel: bpp
		  depth: depth
		  data: data
		  width: w
		  height: h)))

;;;
;;;  find a pixmap format (ie, bits-per-pixel and scanline-pad)
;;;  that we can use on the given display at the given depth
;;;  

(define (find-bpp-and-pad dpy deep)
  (let loop ((formats (display-pixmap-formats dpy)))
    (if (null? formats)
	#f
	(if (= (depth (car formats)) deep)
	    (values (bits-per-pixel (car formats))
		    (scanline-pad (car formats)))
	    (loop (cdr formats))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   procedures for building X images from 
;;;   color-mapped TIFF subimages
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (cm-tiff->cm-x cmap img k depth)
  (make-image-using-clut
   cmap img k 
   (lambda ()
     (values depth depth alloc-dithered-pixel-map))))

(define (cm-tiff->rgb-x cmap img k depth)
  (make-image-using-clut
   cmap img k
   (lambda ()
     (values depth depth alloc-exact-pixel-map))))

;;;

(define (make-image-using-clut cmap img k proc)
  (bind ((w (get-tag-scalar img k 'image-width))
	 (h (get-tag-scalar img k 'image-length))
	 (data (make-vector h))
	 (color-tbl (tiff-color-map->color-table img k))
	 (bpp depth alloc-clut (proc))
	 (clut (alloc-clut cmap color-tbl)))
    ;
    (for-each-row 
     img k
     (lambda (y r)
       (let ((row (make-vector w))
	     ((ydith <fixnum>) (* 4 (modulo y 4))))
	 (vector-set! data y row)
	 (do-times (x w)
	   (let ((dith (vector-ref clut (bvec-ref r x))))
	     (vector-set! row x 
			  (vector-ref dith 
				      (fixnum+ ydith 
					       (bitwise-and x 3)))))))))
    ;
    (create-image bits-per-pixel: bpp
		  data: data
		  depth: depth
		  data: data
		  width: w
		  height: h)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (find-subimage-with-photometric (tiff <tiff-image>) pm)
  (let ((n (number-of-subimages tiff)))
    (let loop ((k 0))
      (if (< k n)
	  (if (eq? (get-tag-scalar tiff k 'photometric-interpretation) pm)
	      k
	      (loop (+ k 1)))
	  #f))))

;; PhotometricInterpretation

(define-constant $tiff-rgb 2)
(define-constant $tiff-palette-color 3)

;;

(define (tiff->x-image (self <tiff-image>) cmap depth)
  (case (colormap-visual-class cmap)
    ((pseudo-color)
     ;; look for a color-mapped source image
     (let ((cm-k (find-subimage-with-photometric self $tiff-palette-color)))
       (if cm-k
	   (cm-tiff->cm-x cmap self cm-k depth)
	   ;; otherwise, an RGB source image can be dithered internally
	   (let ((rgb-k (find-subimage-with-photometric self $tiff-rgb)))
	     (if rgb-k
		 (rgb-tiff->cm-x cmap self rgb-k depth)
		 (error "tiff->x-image: can't find a subimage to use"))))))
    ((true-color)
     ;; look for an RGB color source image
     (let ((rgb-k (find-subimage-with-photometric self $tiff-rgb)))
       (if rgb-k
	   (rgb-tiff->rgb-x cmap self rgb-k depth)
	   ;; otherwise, a color mapped image is easily done
	   (let ((cm-k (find-subimage-with-photometric self
						       $tiff-palette-color)))
	     (if cm-k
		 (cm-tiff->rgb-x cmap self rgb-k depth)
		 (error "tiff->x-image: can't find a subimage to use"))))))
    (else
     (error "tiff->x-image: visual class `~s' not supported"
	    (colormap-visual-class cmap)))))

;;;

(define-class <tiff-image-rep> (<image-rep>)
  underlying-image)

(define-method make-image-rep ((self <tiff-subimage>) (in <graphic-image>))
  (make <tiff-image-rep>
        rep-of: in
        underlying-image: self))

(define-method copy-to-memory-image-rep ((self <tiff-image-rep>) 
					 (mem <memory-image-rep>))
  (let ((data (data mem))
	(w (image-width (rep-of self)))
	(k 0))
    (for-each-pixel
     (underlying-image self)
     (lambda (y x c)
       (bind ((r g b a (pixel-rgba c)))
	 (bvec-set! data k (logical-shift-right r 8))
	 (bvec-set! data (+ k 1) (logical-shift-right g 8))
	 (bvec-set! data (+ k 2) (logical-shift-right b 8))
	 (bvec-set! data (+ k 3) (logical-shift-right a 8))
	 (set! k (+ k 4)))))))
