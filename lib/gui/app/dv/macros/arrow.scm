
(define $tip-setback 0.5)  ;; arrow tip setback

(define $arrow-length 6)
(define $arrow-width 1.5)

;;;

(define $curvy-radius 5)
(define $curvy-curviness 2.5) ;; how do I make this generate an arc?

(define (curvy-path-with-arrow points)
  (let* ((out (make-dequeue))
	 (v (list->vector points))
	 (n (vector-length v))
	 (fpu (normalize (point- (vector-ref v (- n 2))
				 (vector-ref v (- n 1))))))
	 ;
    (define (pp . opts)
      (dequeue-push-back! out (cons 'path-point opts))
      (values))
    ;
    (define (plain-point i)
      (pp x: (x (vector-ref v i))
	  y: (y (vector-ref v i))))
    ;
    ;  set the final point back some to make
    ;  room for the arrowtip setback (1/2 the 
    ;  line width of the target frame) and the
    ;  arrow length
    ;
    (vector-set! v (- n 1) (point+ (vector-ref v (- n 1))
				   (size* fpu (+ $tip-setback $arrow-length))))
    ;
    (plain-point 0)
    ;
    (for-each 
     (lambda (i)
       (let* ((pre (vector-ref v (- i 1)))
	      (pt (vector-ref v i))
	      (post (vector-ref v (+ i 1)))
	      (au (normalize (point- pre pt)))
	      (bu (normalize (point- post pt)))
	      (apt (point+ pt (size* au $curvy-radius)))
	      (bpt (point+ pt (size* bu $curvy-radius)))
	      (ahand (size* au (- $curvy-curviness)))
	      (bhand (size* bu (- $curvy-curviness))))
	 (pp x: (x apt)
	     y: (y apt)
	     out-dx: (dx ahand)
	     out-dy: (dy ahand))
	 (pp x: (x bpt)
	     y: (y bpt)
	     in-dx: (dx bhand)
	     in-dy: (dy bhand))))
     (cdr (range (- n 1))))
    ;
    (plain-point (- n 1))
    ;;
    ;(format #t "fpu => ~s\n" fpu)
    (let* ((ep (last points))
	   (ap1 (point+ ep (size* fpu $tip-setback)))
	   (ap2 (point+ ap1 (size* fpu $arrow-length)))
	   (ap3 (point+ ap2 (size* (rotate fpu 90) $arrow-width)))
	   (ap4 (point+ ap2 (size* (rotate fpu -90) $arrow-width)))
	   (arrow `((path-point x: ,(x ap1) y: ,(y ap1))
		    (path-point x: ,(x ap3) y: ,(y ap3))
		    (path-point x: ,(x ap4) y: ,(y ap4)))))
      `(group
	(path
	 subpaths: ((subpath points: ,(vector->list (dequeue-state out)))))
	(path
	 fill-color: black
	 stroke-color: none
	 subpaths: ((subpath
		     closed?: #t
		     points: ,arrow)))))))

(define (simple-line x0 y0 x1 y1 opts)
  `(path stroke-color: black
         ,@opts
         subpaths: ((subpath points: ((path-point x: ,x0 y: ,y0)
                                      (path-point x: ,x1 y: ,y1))))))

(define (hline-with-arrow from to dir #optional dash)
  (let* ((x1 (x from))
	 (y1 (y from))
	 (x2 (x to))
	 (y2 (y to))
	 (x3 (- x2 (* dir $tip-setback)))
	 (x4 (- x3 (* dir $arrow-length))))
    `(group
      ,(simple-line x1 y1 x4 y2 (if dash `(dashing: ,dash) '()))
      (path 
       fill-color: black
       subpaths: ((subpath
		   closed?: #t
		   points: ((path-point x: ,x3
					y: ,y2)
			    (path-point x: ,x4
					y: ,(- y2 $arrow-width))
			    (path-point x: ,x4
					y: ,(+ y2 $arrow-width)))))))))

(define (vline-with-arrow from to dir #optional dash)
  (let* ((x1 (x from))
	 (y1 (y from))
	 (x2 (x to))
	 (y2 (y to))
	 (y3 (- y2 (* dir $tip-setback)))
	 (y4 (- y3 (* dir $arrow-length))))
    `(group
      ,(simple-line x1 y1 x2 y4 (if dash `(dashing: ,dash) '()))
      (path
       fill-color: black
       subpaths: ((subpath
		   closed?: #t
		   points: ((path-point x: ,x2
					y: ,y3)
			    (path-point x: ,(- x2 $arrow-width)
					y: ,y4)
			    (path-point x: ,(+ x2 $arrow-width)
					y: ,y4))))))))

