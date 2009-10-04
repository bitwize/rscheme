
(define-class <color-list> (<object>)
  (name type: <symbol>)
  (color-specs type: <vector>)
  (colormap-key type: <symbol>))

(define-method to-string ((self <color-list>))
  (to-string (name self)))

(define-class <bound-colors> (<object>)
  (colors type: <color-list>) ;; contains a vector of color specs
  (pixels type: <vector>))    ;; a parallel array of pixel values

(define *color-lists* '())

(define-method initialize ((self <color-list>))
  (set! *color-lists* (cons self *color-lists*)))

(define-macro (define-color-list name . cspecs)
  `(define ,name
     (make <color-list>
       name: ',name
       color-specs: (vector
		     ,@(map 
			(lambda (cs)
			  (cond
			   ((string->color cs)
			    `(string->color ,cs))
			   ((string=? cs "black")
			    '$black)
			   ((string=? cs "white")
			    '$white)
			   (else
			    cs)))
			cspecs))
       colormap-key: ',(symbol-append "bound-colors." name))))

(define-color-list *bw* "black" "white")

;;;

(define-macro (get-bound-colors cmap name)
  (let ((key (symbol-append "bound-colors." name)))
    `(get-property ,cmap ',key (make-bound-colors ,cmap ,name))))

(define (bind-one-color cmap cspec bw)
  (handler-case
   (let ((pix (alloc-color cmap cspec)))
     (dm "  ~s => ~s" cspec pix)
     pix)
   ((<x-error>)
    (let ((dark? (or (string? cspec)
		     (< (color-value cspec) 0.5))))
      (wm "Couldn't bind ~a color: ~a" (if dark? "dark" "light") cspec)
      (vector-ref (pixels (force bw)) (if dark? 0 1))))))

(define (make-bound-colors cmap (list <color-list>))
  (dm "binding colors: ~s" (name list))
  (let* ((bw (delay (get-bound-colors cmap *bw*)))
	 (b (make <bound-colors>
	      colors: list
	      pixels: (vector-map
		       (lambda (cs)
			 (bind-one-color cmap cs bw))
		       (color-specs list)))))
    (set-property! cmap (colormap-key list) b)
    b))

(define (color-value (c <color>))  ; from HSV model
  (max (color-red c)
       (color-green c)
       (color-blue c)))

(define (init-default-colormap-pixels s)
  (let ((c (screen-default-colormap s)))
    (set-property! c 
		   (colormap-key *bw*)
		   (make <bound-colors>
		     colors: *bw*
		     pixels: (vector (screen-black-pixel s)
				     (screen-white-pixel s))))
    (for-each
     (lambda (ent)
       (if (not (eq? ent *bw*))
	   (make-bound-colors c ent)))
     *color-lists*)
    c))

;;;
#|

  This could be made much more effective (but slower)
  by going in two passes -- in the first pass, we
  find out how many colors are available.  I'm not sure
  how to do it, but we might be able to do it by allocating
  as many new colors as we can (allocate them in "private" mode?).
  Those pixels are then one that are available.

  In the second pass, we reference all the already-allocated
  colors (to keep them from being changed on us), as we do now.

  Then, we make maximum use of the available colors (but we
  reallocate them in shared mode) by finding places in the visual
  cube that are weak in colors and allocating them there.

  Here's how you could find weak places (VERY EXPENSIVE):  For
  each point in color space, compute the weighted proximity function.
  (3d gaussian weighting in XYZ color space).  Choose the smallest
  weight and allocate that point.  Repeat (note that you have to
  recompute after each allocation) for each available color cell.

  CHEAPER -- approximate using a 3d grid (8^3=512, 16^3=4096).  Pick
  the cell with smallest weight (includes self and neighbors).

  CHEAPEST -- use a 6^3=216 grid and find the zeros
|#

(define (slurp-colormap)
  (let* ((cmap (screen-default-colormap (on-screen (current-client))))
	 (vc (colormap-visual-class cmap)))
    (dm 103 "default colormap visual class: ~s" vc)
    (if (eq? vc 'pseudo-color)
	(let ((n 0))
	  (handler-case
	   (let loop ()
	     (bind ((pix x c (alloc-color cmap
					  (make-color
					   red: (/ (random 65535) 65535.0)
					   green: (/ (random 65535) 65535.0)
					   blue: (/ (random 65535) 65535.0)))))
		   (set! n (+ n 1))
		   (loop)))
	   ((<condition>)))
	  (dm 104 " allocated ~d new colors" n)
	  (vector-for-each
	   (lambda (ent)
	     (alloc-color cmap (car ent)))
	   (get-color-map cmap))))))

;;; one observation reveals that an 8x8x8 cube is only 22% occupied
;;; (compared to the theoretical 50%), so there are lots of zeros
;;; a 6x6x6 cube is 41% occupied (compared to the theoretical 100%)

(define (build-color-cube)
  (let* ((k 6)
	 (vec (make-vector (* k k k) '()))
	 (q (inexact->exact (ceiling (/ 65536 k)))))
    (vector-for-each
     (lambda (ent)
       (bind ((r g b (color-rgb-components (car ent)))
	      (ri (* k k (quotient r q)))
	      (gi (* k (quotient g q)))
	      (bi (quotient b q))
	      (i (+ ri gi bi)))
	 (vector-set! vec i (cons ent (vector-ref vec i)))))
     (get-color-map (screen-default-colormap (on-screen (current-client)))))
    vec))
