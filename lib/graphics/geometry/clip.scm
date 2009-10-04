(define (point-in-rect? (r <rect>) (pt <point>))
  (and (>= (x pt) (origin-x r))
       (>= (y pt) (origin-y r))
       (< (x pt) (limit-x r))
       (< (y pt) (limit-y r))))

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
	    (values)
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
		(values))))))

(define (rects-intersect? (r1 <rect>) (r2 <rect>))
  (and (intersect-rect r1 r2) #t))

		
(define (intersect-rect (accum <rect>) (arg <rect>))
    ;; a slight change to the protocol; returns #f
    ;; if the rectangles do not intersect.  Before,
    ;; we would return a 0-sized rectangle
  (bind ((x w (coord-intersect (origin-x accum)
			       (size-width accum)
			       (origin-x arg)
			       (size-width arg))))
    (if x
        (bind ((y h (coord-intersect (origin-y accum)
                                     (size-height accum)
                                     (origin-y arg)
                                     (size-height arg))))
          (if y
              (make-rect x y w h)
              #f))
        #f)))

;;; build a rect which countains all the given points

(define (union-points lst)
  (if (null? lst)
      #f
      (let loop ((l (cdr lst))
                 (minx (x (car lst)))
                 (maxx (x (car lst)))
                 (miny (y (car lst)))
                 (maxy (y (car lst))))
        (if (null? l)
            (bbox-rect minx miny maxx maxy)
            (let (((p <point>) (car l)))
              (loop (cdr l)
                    (min minx (x p))
                    (max maxx (x p))
                    (min miny (y p))
                    (max maxy (y p))))))))

(define (union-rects lst)
  (if (null? lst)
      #f
      (let loop ((l (cdr lst))
                 (minx (origin-x (car lst)))
                 (miny (origin-y (car lst)))
                 (maxx (limit-x (car lst)))
                 (maxy (limit-y (car lst))))
        (if (null? l)
            (bbox-rect minx miny maxx maxy)
            (let (((r <rect>) (car l)))
              (loop (cdr l)
                    (min minx (origin-x r))
                    (min miny (origin-y r))
                    (max maxx (limit-x r))
                    (max maxy (limit-y r))))))))

(define (union-rect (a <rect>) (b <rect>))
  (bbox-rect (min (origin-x a) (origin-x b))
	     (min (origin-y a) (origin-y b))
	     (max (limit-x a) (limit-x b))
	     (max (limit-y a) (limit-y b))))

;;;
;;;  find where two line segments (A..B) and (C..D) intersect
;;;
;;;  returns #f if they do not intersect,
;;;  and three values if they do:
;;;    (1) the intersection point
;;;    (2) the handedness (one of (pl = Potentially Leaving,
;;;                                pe = Potentially Entering))
;;;    (3) the intersection parameter (on a-->b; 0=a, 1=b)
;;;
;;;  [see Foley, van Dam, et.al., 2nd Ed., p.119]

(define (line-intersect (a <point>) (b <point>) (c <point>) (d <point>))
  (bind ((p0 h0 t0 (clip-to-segment a b c (find-perp c d a))))
    (if (and p0 (>= t0 0) (<= t0 1))
        (bind ((p1 h1 t1 (clip-to-segment c d a (find-perp a b c))))
          (if (and p1 (>= t1 0) (<= t1 1))
              (values p0 h0 t0)
              #f))
        #f)))

;;;
;;;  Find where the infinite line a-->b intersects
;;;  the line that contains c and has normal n.
;;;  Returns the intersection point on a-->b, 
;;;  the symbol pe for a potentially entering
;;;  line and pl for a potentially leaving line,
;;;  and the intersection parameter on a-->b
;;;  (0=a, 1=b).
;;;

(define (clip-to-segment (a <point>) (b <point>) (c <point>) (n <size>))
  (let ((den (inner-product n (point- a b)))
	(num (inner-product n (point- a c))))
    (if (= den 0)
	#f
	(let ((t (/ num den)))
	  (values
	   (point+ a (size* (point- b a) t))
	   (if (< den 0) 'pl 'pe)
	   t)))))

(define (clip-to-rect (a <point>) (b <point>) (box <rect>))
  (let ((x0 (origin-x box))
	(y0 (origin-y box))
	(x0w (limit-x box))
	(y0h (limit-y box))
	(best-pe #f)
	(best-pe-t #f)
	(best-pl #f)
	(best-pl-t #f))
    ;
    (define (submit1 p1 norm)
      (bind ((pt dir t (clip-to-segment a b p1 norm)))
	(format #t "submit ~a - ~a -> ~a ~a ~a\n" p1 norm pt dir t)
	(case dir
	  ((pe)
	   (if (or (not best-pe-t)
		   (> t best-pe-t))
	       (begin
		 (set! best-pe pt)
		 (set! best-pe-t t))))
	  ((pl)
	   (if (or (not best-pl-t)
		   (< t best-pl-t))
	       (begin
		 (set! best-pl pt)
		 (set! best-pl-t t)))))))
    ;
    (submit1 (make-point x0 y0) (make-size -1 0))
    (submit1 (make-point x0 y0h) (make-size 0 1))
    (submit1 (make-point x0w y0h) (make-size 1 0))
    (submit1 (make-point x0w y0) (make-size 0 -1))
    ;
    (values best-pe best-pl)))

;;;
;;;  find the perpendicular to a line P...R that is more toward
;;;  Q than not
;;;

(define (find-perp (p <point>) (r <point>) (q <point>))
  (let ((b (normalize (point- q p)))
	(a (normalize (point- r p))))
    (normalize (size- b (size* a (inner-product a b))))))


;;;

