
;;;

(define (unfilter filt-type data offset line-bytes bpp prev)
  (let ((l (bvec-alloc <byte-vector> line-bytes)))
    ;
    (define (Prior x)
      (if (and prev (>= x 0))
	  (bvec-ref prev x)
	  0))
    ;
    (case filt-type
      ((0)
       (bvec-copy l 0 data offset line-bytes))
      ((1)
       (begin
	 ; this could be improved
	 (define (Raw x)
	   (if (< x 0)
	       0
	       (bvec-ref l x)))
	 (define (Sub x)
	   (bvec-ref data (+ offset x)))
	 (do-times (x line-bytes)
	  (bvec-set! l x (bitwise-and (+ (Sub x) (Raw (- x bpp))) #xFF)))))
      ((2)
       (begin
	 (define (Up x)
	   (bvec-ref data (+ offset x)))
	 (do-times (x line-bytes)
	  (bvec-set! l x (bitwise-and (+ (Up x) (Prior x)) #xFF)))))
      ((3)
       (begin
	 (define (Raw x)
	   (if (< x 0)
	       0
	       (bvec-ref l x)))
	 (define (Average x)
	   (bvec-ref data (+ offset x)))
	 (do-times (x line-bytes)
	   (bvec-set! l x (bitwise-and
			   (+ (Average x)
			      (quotient (+ (Raw (- x bpp)) (Prior x)) 2))
			   #xFF)))))
      ((4)
       (begin
	 (define (Raw x)
	   (if (< x 0)
	       0
	       (bvec-ref l x)))
	 (define (Paeth x)
	   (bvec-ref data (+ offset x)))
	 (do-times (x line-bytes)
	  (bvec-set! l x (bitwise-and
			  (+ (paeth-predictor (Raw (- x bpp))
					      (Prior x)
					      (Prior (- x bpp)))
			     (Paeth x))
			  #xFF)))))
      (else
       (error "unrecognized filter-type: ~s" filt-type)))
    l))

(define (paeth-predictor a b c)
  (let* ((p (- (+ a b) c))
	 (pa (abs (- p a)))
	 (pb (abs (- p b)))
	 (pc (abs (- p c))))
    (if (and (<= pa pb)
	     (<= pa pc))
	a
	(if (<= pb pc)
	    b
	    c))))
