(define call/cc call-with-current-continuation)

(define-macro (do-times (var num) . body)
  (let* ((lim (gensym))
	 (dtl (symbol-append lim "/" var "-loop")))
    `(let ((,lim ,num))
       (let ,dtl ((,var 0))
         (if (< ,var ,lim)
	     (begin
	       ,@body
	       (,dtl (+ 1 ,var)))
	     (values))))))

(define (first-row w)
  (let ((s (make-vector (+ 1 (* w 2)))))
    (do-times (i w)
      (vector-set! s (* i 2) (list 'v i))
      (vector-set! s (+ 1 (* i 2)) (list 'h i)))
    (vector-set! s (* w 2) (list 'v w))
    s))

(define (skip-top rowspec i get-generator display-hline display-corner)
  (let ((out (make-dequeue)))
    (let loop ((j 0)
	       (w 0))
      (if (< j (vector-length rowspec))
	  (let ((e (vector-ref rowspec j)))
	    (case (car e)
	      ((h)
	       (if (= w 0)
		   (bind ((gen cell (get-generator i (cadr e))))
		     (dequeue-push-back! out gen)
		     (loop (+ j 1) (- (cf-horz-span (cell-format cell)) 1)))
		   (loop (+ j 1) (- w 1))))
	      ((v) 
	       (if (= w 0)
		   (dequeue-push-back! out e))
	       (loop (+ j 1) w))
	      (else
	       (dequeue-push-back! out e)
	       (assert (= w 0))
	       (loop (+ j 1) 0))))
	  (dequeue-state out)))))

(define (draw-top rowspec i get-generator display-hline display-corner)
  (let ((out (make-dequeue)))
    (let loop ((j 0)
	       (w 0))
      (if (< j (vector-length rowspec))
	  (let ((e (vector-ref rowspec j)))
	    (case (car e)
	      ((h)
	       (display-hline i (cadr e))
	       (if (= w 0)
		   (bind ((gen cell (get-generator i (cadr e))))
		     (dequeue-push-back! out gen)
		     (loop (+ j 1) (- (cf-horz-span (cell-format cell)) 1)))
		   (loop (+ j 1) (- w 1))))
	      ((v) 
	       (display-corner i (cadr e))
	       (if (= w 0)
		   (dequeue-push-back! out e))
	       (loop (+ j 1) w))
	      (else
	       (set-car! e (call/cc (car e)))
	       (dequeue-push-back! out e)
	       (assert (= w 0))
	       (loop (+ j 1) 0))))
	  (begin
	    (newline)
	    (dequeue-state out))))))

(define (draw-bottom rowspec i display-hline display-corner)
  (let loop ((j 0)
	     (w 0))
    (if (< j (vector-length rowspec))
	(let ((e (vector-ref rowspec j)))
	  (case (car e)
	    ((h)
	     (display-hline i (cadr e))
	     (if (= w 0)
		 (loop (+ j 1) 0)
		 (loop (+ j 1) (- w 1))))
	    ((v) 
	     (display-corner i (cadr e))
	     (loop (+ j 1) w))
	    (else
	     (call/cc (car e))
	     (assert (= w 0))
	     (loop (+ j 1) 0))))
	(newline))))

(define (draw-body-lines rowspec i nlines display-vline)
  ;
  (do-times (i (- nlines 1))
    (vector-for-each
     (lambda (e)
       (case (car e)
	 ((h)
	  (display "???"))
	 ((v) 
	  (display-vline i (cadr e)))
	 (else
	  (set-car! e (call/cc (car e)))
	  (values))))
     rowspec)
    (newline))
  ;
  (let ((out (make-dequeue)))
    (let loop ((j 0))
      (if (< j (vector-length rowspec))
	  (let ((e (vector-ref rowspec j)))
	    (case (car e)
	      ((h)
	       (if (> nlines 0)
		   (display "???"))
	       (loop (+ j 1)))
	      ((v) 
	       (if (> nlines 0)
		   (display-vline i (cadr e)))
	       (dequeue-push-back! out e)
	       (loop (+ j 1)))
	      (else
	       (if (> nlines 0)
		   (set-car! e (call/cc (car e))))
	       (if (= (cadr e) 1)
		   (let ((w (cf-horz-span (cell-format (caddr e)))))
		     (dequeue-push-back! out (list 'h (cadddr e)))
		     (do-times (i (- w 1))
		       (dequeue-push-back! out (list 'v (+ (cadddr e) 1 i)))
		       (dequeue-push-back! out (list 'h (+ (cadddr e) 1 i)))))
		   (begin
		     (set-car! (cdr e) (- (cadr e) 1))
		     (dequeue-push-back! out e)))
	       (loop (+ j 1)))))
	  (begin
	    (if (> nlines 0)
		(newline))
	    (dequeue-state out))))))

(define (content-generator cell i j row-seps col-seps row-hs col-ws)
  (let* ((v-span (cf-vert-span (cell-format cell)))
	 (h-span (cf-horz-span (cell-format cell)))
	 (box-h (+ (reduce
		    + 0
		    (vector->list
		     (subvector row-hs i (+ i v-span))))
		   (reduce
		    + 0
		    (vector->list
		     (subvector row-seps (+ i 1) (+ i v-span))))))
	 ; could definitely cache this as a fn. of h-span!
	 (box-w (+ (reduce
		    + 0 
		    (vector->list
		     (subvector col-ws j (+ j h-span))))
		   (reduce
		    + 0
		    (vector->list
		     (subvector col-seps (+ j 1) (+ j h-span))))))
	 (box (build-cell-box cell box-w box-h)))
    (lambda (done)
      (vector-for-each
       (lambda (l)
	 (display l)
	 (set! done (call/cc done)))
       box))))

(define (draw-matrix h w cells 
		     row-seps col-seps 
		     col-ws row-hs 
		     h-borders v-borders)
  ;;
  (define (get-generator i j)
    (let ((c (vector-ref (vector-ref cells i) j)))
      (values (list
	       (content-generator c i j row-seps col-seps row-hs col-ws)
	       (cf-vert-span (cell-format c))
	       c
	       j)
	      c)))
  ;;
  (define (display-hline i j)
    (let ((s (if (< j w)
		 (vector-ref (vector-ref h-borders i) j)
		 0)))
      (display (make-string (vector-ref col-ws j)
			    (style->border-char s s 0 0)))))
  ;;
  (define (display-vline i j)
    (if (> (vector-ref col-seps j) 0)
	(let ((s (if (< i h)
		     (vector-ref (vector-ref v-borders i) j)
		     0)))
	  (display (style->border-char 0 0 s s)))))
  ;;
  (define (display-corner i j)
    ;(format #t "k[~d,~d]" i j)
    (if (or (> (vector-ref row-seps i) 0)
	    (> (vector-ref col-seps j) 0))
	(let ((ls (if (> j 0)
		      (vector-ref (vector-ref h-borders i) (- j 1))
		      0))
	      (rs (if (< j w)
		      (vector-ref (vector-ref h-borders i) j)
		      0))
	      (ts (if (> i 0)
		      (vector-ref (vector-ref v-borders (- i 1)) j)
		      0))
	      (bs (if (< i h)
		      (vector-ref (vector-ref v-borders i) j)
		      0)))
	  (display (style->border-char ls rs ts bs)))))
  ;;
  (let loop ((rowspec (first-row w))
	     (i 0))
    (if (< i h)
	(loop (draw-body-lines
	       (if (= 0 (vector-ref row-seps i))
		   (skip-top rowspec i
			     get-generator display-hline display-corner)
		   (draw-top rowspec i
			     get-generator display-hline display-corner))
	       i
	       (vector-ref row-hs i)
	       display-vline)
	      (+ i 1))
	(begin
	  (if (> (vector-ref row-seps i) 0)
	      (draw-bottom rowspec i display-hline display-corner))
	  (values)))))

