
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;  Expensive...

(define (fixnumify (x <number>))
  (cond
   ((fixnum? x) x)
   ((inexact? x) (inexact->exact x))
   (else (truncate x))))

(define-method points->bvec ((points <vector>))
  (let* ((n (vector-length points))
	 ((pbuf <byte-vector>) (bvec-alloc <byte-vector> (* n 2))))
    (let loop ((i 0)
               (j 0))
      (if (< j n)
	  (begin
	    (xbo-write-s2 pbuf i (fixnumify (vector-ref points j)))
	    (loop (+ i 2) (+ j 1)))
	  (values pbuf (div2 n))))))

(define-method points->bvec ((points <list>))
  (let* ((n (length points))
	 ((pbuf <byte-vector>) (bvec-alloc <byte-vector> (* n 2))))
    (let loop ((p points)
	       (i 0))
      (if (null? p)
	  (values pbuf (div2 n))
	  (begin
	    (xbo-write-s2 pbuf i (fixnumify (car p)))
	    (loop (cdr p) (+ i 2)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (clear-area (window <x-drawable>) #key (x default: 0)
		                               (y default: 0)
					       (width default: #f)
					       (height default: #f)
					       (exposures? default: #f))
  (internal-send
   (x-display window)
   (make-buffer u1: 61 ; ClearArea
		u1: (if exposures? 1 0)
		u2: 4
		u4: (x-id window)
		s2: x
		s2: y
		u2: (or width 0)
		u2: (or height 0))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   lines
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (draw-line (drawable <x-drawable>)
		   (gcontext <x-gcontext>)
		   (x1 <fixnum>)
		   (y1 <fixnum>)
		   (x2 <fixnum>)
		   (y2 <fixnum>)
		   . opt)
  (let ((relative? (if (null? opt)
		       #f
		       (car opt))))
    (internal-send
     (x-display drawable)
     (make-buffer u1: 65 ;; PolyLine
		  u1: (if relative? 1 0)
		  u2: (+ 3 2)
		  u4: (x-id drawable)
		  u4: (x-id gcontext)
		  s2: x1
		  s2: y1
		  s2: x2
		  s2: y2))
    (values)))


(define (draw-lines (drawable <x-drawable>)
		    (gcontext <x-gcontext>)
		    (points <sequence>)
		    #key (relative? default: #f)
		         (fill? default: #f)
			 (shape default: 'complex))
  (bind ((pbuf n (points->bvec points)))
    (internal-send
     (x-display drawable)
     (vector
      (if fill?
	  (make-buffer u1: 69 ;; FillPoly
		       u1: 0
		       u2: (+ 4 n)
		       u4: (x-id drawable)
		       u4: (x-id gcontext)
		       u1: (case shape
			     ((complex) 0)
			     ((non-convex) 1)
			     ((convex) 2)
			     (else 
			      (error "draw-lines: unknown shape: ~s"
				     shape)))
		       u1: (if relative? 1 0)
		       u2: 0)
	  (make-buffer u1: 65 ;; PolyLine
		       u1: (if relative? 1 0)
		       u2: (+ 3 n)
		       u4: (x-id drawable)
		       u4: (x-id gcontext)))
      pbuf))
    (values)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   rectangles
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (draw-rectangle (drawable <x-drawable>)
			(gcontext <x-gcontext>)
			(x <fixnum>)
			(y <fixnum>)
			(width <fixnum>)
			(height <fixnum>)
			. opt)
  (let ((fill? (if (null? opt)
		   #f
		   (car opt))))
    (internal-send
     (x-display drawable)
     (make-buffer u1: (if fill? 
			  70  ;; PolyFillRectangle
			  67) ;; PolyRectangle
		  u1: 0
		  u2: (+ 3 2)
		  u4: (x-id drawable)
		  u4: (x-id gcontext)
		  s2: x
		  s2: y
		  s2: width
		  s2: height))
    (values)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   points
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (draw-point (drawable <x-drawable>)
		    (gcontext <x-gcontext>)
		    (x <fixnum>)
		    (y <fixnum>))
  (internal-send
   (x-display drawable)
   (make-buffer u1: 64 ; PolyPoint
		u1: 0
		u2: 4
		u4: (x-id drawable)
		u4: (x-id gcontext)
		s2: x
		s2: y)))

(define (draw-points (drawable <x-drawable>)
		     (gcontext <x-gcontext>)
		     (points <sequence>)
		     #rest opt)
  (bind ((relative? (if (null? opt)
			#f
			(car opt)))
	 (pbuf n (points->bvec points)))
    (internal-send
     (x-display drawable)
     (vector
      (make-buffer u1: 64 ; PolyPoint
		   u1: (if relative? 1 0)
		   u2: (+ 3 n)
		   u4: (x-id drawable)
		   u4: (x-id gcontext))
      pbuf))))

		   
       
       
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   arcs
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (radians->x-angle r)
  (inexact->exact (round (* r 3666.92988883726854))))

;; N[64 180 / Pi, 30 ] = 3666.9298888372685361150819081

(define (draw-arc (drawable <x-drawable>)
                  (gcontext <x-gcontext>)
                  (x <fixnum>)
                  (y <fixnum>)
                  (w <fixnum>)
                  (h <fixnum>)
                  (angle1 <real>)
                  (angle2 <real>)
		  #optional fill?)
  (let ((arcs (make-buffer u2: x
                           u2: y
                           u2: w
                           u2: h
                           u2: (radians->x-angle angle1)
                           u2: (radians->x-angle angle2))))
    (internal-send
     (x-display drawable)
     (vector (make-buffer u1: (if fill?
                                  71    ; PolyFillArc
                                  68)   ; PolyArc
                          u1: 0
                          u2: 6
                          u4: (x-id drawable)
                          u4: (x-id gcontext))
             arcs))))

                          
    
