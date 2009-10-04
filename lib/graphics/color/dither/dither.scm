;;;  a slightly optimized implementation...

;;;  given an array of pairs of colors and pixel-values, 
;;;   i.e., (color . pixel-value) pairs,
;;;  compute a 4x4 matrix (flattened into a 16-element vector)
;;;  of pixel-values approximating the given color

(define (compute-dithering (colors <vector>) color)
  (bind ((dith (make-vector 16))
	 (r g b (color-rgb-components color))
	 (r (* r 16))
	 (g (* g 16))
	 (b (* b 16)))
    (vector-for-each
     (lambda (left k)
       (bind ((col px (nearest-color-and-pixel colors r g b left)))
	 (vector-set! dith k px)
	 (bind ((dr dg db (color-rgb-components col)))
	   (set! r (- r dr))
	   (set! g (- g dg))
	   (set! b (- b db)))))
     '#(16 15 14 13 12 11 10 9 8 7 6 5 4 3 2 1)
     '#(0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15))
    dith))

;;;  compute the "distance" between two colors
;;;
;;;  the color weights are taken from the defaults
;;;  in the `tiff2bw' program

#|

(define (sqr a)
  (* a a))

(define-inline (color-distance c1 r2 g2 b2)
  (bind ((r1 g1 b1 (color-rgb-components c1)))
    (+ (sqr (* 0.28 (- r2 r1)))
       (sqr (* 0.59 (- g2 g1)))
       (sqr (* 0.11 (- b2 b1))))))

;;;
;;;  given an array of (color . pixel) pairs, find the color
;;;  and pixel for the
;;;  one "closest" to the given color
;;;

(define (nearest-color-and-pixel (colors <vector>) r g b left)
  (let ((r (/ r left))
	(g (/ g left))
	(b (/ b left)))
    (let loop ((best 0)
	       (i 1)
	       (best-r (color-distance (car (vector-ref colors 0)) r g b)))
      (if (< i (vector-length colors))
	  (let ((r (color-distance (car (vector-ref colors i)) r g b)))
	    (if (< r best-r)
		(loop i (+ i 1) r)
		(loop best (+ i 1) best-r)))
	  (values (car (vector-ref colors best))
		  (cdr (vector-ref colors best)))))))
|#

(define-glue (nearest-color-and-pixel colors r g b left)
{
  double r_value = fx2int(r) / (float)fx2int(left);
  double g_value = fx2int(g) / (float)fx2int(left);
  double b_value = fx2int(b) / (float)fx2int(left);
  int i, n;
  obj best = FALSE_OBJ;
  double best_dist = 0.0;

  n = SIZEOF_PTR( colors );

  for (i=0; i<n; i+=SLOT(1))
    {
      obj p = gvec_ref( colors, i );
      obj c = pair_car( p );
      double d_r = 0.28 * (r_value - fx2int( gvec_ref( c, SLOT(0) ) ));
      double d_g = 0.59 * (g_value - fx2int( gvec_ref( c, SLOT(1) ) ));
      double d_b = 0.11 * (b_value - fx2int( gvec_ref( c, SLOT(2) ) ));
      double dist = (d_r * d_r) + (d_g * d_g) + (d_b * d_b);
      if (i == 0 || (dist < best_dist))
        {
	  best = p;
	  best_dist = dist;
	}
    }
  REG0 = pair_car( best );
  REG1 = pair_cdr( best );
  RETURN(2);
})
