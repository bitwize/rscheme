(define (load-pcixf-table in-area (path <string>) (c <<class>>))
  (define (pdup item)
    (area-clone in-area (object-class item) item))

  (format #t "~a: ~s\n" path c)
  
  (set! s (file->string path))
  (let ((cols (columns)))
    (format #t "~d columns\n" (length cols))
    (if (not (eq? (+ 1 (length cols)) ;; +1 is for `forward' ptr
		  (length (slot-descriptors c))))
	(error "~a: not conformal with ~s" path c))
    (let ((m (make-meta cols)))
      (if (not (find-first-card-type #\D))
	  '()
	  (let loop ((r '())
		     (i 0))
	    (if (eq? (current-card-type) #\D)
		(begin
		  #|(format #t "\n~05x ==> ~s"
			  *current-card-offset*
			  (get-field *current-card-offset* 30))|#
		  (if (eq? (remainder i 10) 0)
		      (display i))
		  (write-char #\.)
		  (flush-output-port (current-output-port))
		  (loop (pdup (cons (area-make-gvec* 
				     in-area
				     c 
				     #f ;; forward
				     (map pdup 
					  (get-row 
					   (current-card-data) 
					   m)))
				    r))
			(+ i 1)))
		(begin
		  (newline)
		  (reverse r))))))))
		  