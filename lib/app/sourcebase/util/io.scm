(define (with-lines-from-file file proc)
  (let ((p (open-input-file file)))
    (let loop ()
      (let ((l (read-line p)))
        (if (eof-object? l)
	    (close-input-port p)
	    (begin
	      (proc l)
	      (loop)))))))
	    
(define-method print ((self <<class>>))
   (let ((s (format #f "Class ~a" (class-name self))))
     (display s)
     (newline)
     (display (make-string (string-length s) #\=))
     (newline)
     (format #t "superclasses: ~j\n" (map class-name (superclasses self)))
     (format #t "heap type: ~d (~a)" (heap-type self)
	    (let ((t (assq (heap-type self) '((0 gvec)
					    (1 bvec)))))
	    (if t
		(cadr t)
		'unknown)))
     (newline)
     (format #t "image mode: ~s (~a)" (image-mode self)
      		(let ((t (assq (image-mode self) '((4 template)))))
		   (if t
		       (cadr t)
		       'unknown)))
     (newline)
     (format #t "instance size: ~d slots\n" (instance-size self))
     (display "Slots\n=====\n")
     (let* ((namelen (lambda (x) (string-length (symbol->string (name x)))))
            (all (sort (map cdr (all-slots self))
		      (lambda (a b)
		        (< (index a) (index b)))))
	    (fw (apply max (map namelen all))))
    (for-each (lambda ((sd <slot-descriptor>))
         	   (format #t " ~a~a ~a (slot ~-2d) type: ~s"
			(name sd)
			(make-string (- fw (namelen sd)) #\space)
		   	(if (memq sd (direct-slots self))
			    "   direct"
			    "inherited")
			(index sd)
			(class-name (type-restriction sd)))
		   (case (initialization-mode sd)
		     ((required) (display " **required"))
		     ((optional) (format #t " init-value: ~s" (init-value sd)))
		     (else (format #t " [init-mode: ~s]" (initialization-mode sd))))
		    (newline))
		all))))

(define (print-columns headers lines)
    (let* ((linedata (map (lambda (line)
			   (map (lambda (item)
			   	   (if (string? item)
				       item
				       (with-output-to-string
				         (lambda ()
					   (display item)))))
				line))
			 lines))
	  (num-cols (length (car headers)))
	  (col-widths (reduce max
			      (map (curry map string-length) 
				   (append headers linedata))))
	  (print-row (row-writer (current-output-port) col-widths)))
      (for-each print-row headers)
      (print-row (map (rcurry make-string #\-) col-widths))
      (for-each print-row linedata)
      (values)))
	  

(define (row-writer port (widths <pair>))
  (let ((first (row-writer* port 0 widths)))
    (lambda (data)
      (first 0 data))))
  
(define (row-writer* port (at <fixnum>) (widths <pair>))
   (if (null? (cdr widths))
       ;;
       ;; prints the rightmost column
       ;;
       (lambda ((x <fixnum>) (data <pair>))
          (if (not (string=? (car data) ""))
	      (begin
		(space port (fixnum- at x))
	        (write-string port (car data))))
	  (output-port-write-char port #\newline))
	;;
	;; compute how to print the remaining columns
	;;
        (let ((rest (row-writer* port (+ at (car widths) 1) (cdr widths))))
         (lambda ((x <fixnum>) (data <pair>))
	  (let (((str <string>) (car data)))
	    (if (eq? (string-length str) 0)
	        (rest x (cdr data))
		(begin
		  (space port (fixnum- at x))
		  (write-string port str)
		  (rest (fixnum+ at (string-length str)) (cdr data)))))))))

(define (space port (n <fixnum>))
  (let loop (((left <fixnum>) n))
   (cond
     ((eq? left 0) 0)
     ((fixnum>=? left 35)
	(write-string port "                                   ")
	(loop (fixnum- left 35)))
     ((fixnum>=? left 20)
	(write-string port "                    ")
	(loop (fixnum- left 20)))
     ((fixnum>=? left 10)
	(write-string port "          ")
	(loop (fixnum- left 10)))
     (else
	(write-string port
	              (gvec-ref '#(#f 
				    " " 
				    "  " 
				    "   " 
				    "    " 
				    "     "
				    "      " 
				    "       " 
				    "        " 
				    "         ")
				 left))))))
