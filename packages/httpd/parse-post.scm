(define (replace+! str)
  (let loop ((i 0))
    (let ((x (string-search str #\+ i)))
      (if x
	  (begin
	    (string-set! str x #\space)
	    (loop (+ x 1)))
	  str))))
	  
(define (de-escape-str ent offset)
  (let loop ((i offset)
	     (r '()))
    (let ((n (string-search ent #\% i)))
      (if n
	  (loop (+ n 3)
		(cons (string
		       (integer->char
			(string->number (substring 
					 ent 
					 (+ n 1) 
					 (+ n 3))
					16)))
		      (cons (substring ent i n) r)))
	  (if (null? r)
	      (substring ent i)
	      (apply string-append 
		     (reverse (cons (substring ent i) r))))))))

(define (parse-POST-args data)
  (map (lambda (ent)
	 (let ((x (string-search ent #\=)))
	   (if x
	       (cons (string->symbol (substring ent 0 x))
		     (replace+! (de-escape-str ent (+ x 1))))
	       (error "badly formatted POST arg: ~s" data))))
       (string-split data #\&)))
  
