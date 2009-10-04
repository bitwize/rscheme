
(define (parse-property-value (prop <property>) (str <string>))
   (or (eval-property-value (property-value-type prop) str)
       (error "~a: ~s is not a valid value"
              (name prop) str)))

	  
(define-method property-value-string-form ((self <property-value>))
  (name self))

(define-method property-value-string-form ((self <symbol>))
  (symbol->string self))

(define-method property-value-string-form ((self <string>))
  self)

(define-method property-value-string-form ((self <number>))
  (number->string self))

(define-method property-value-string-form ((self <number>))
  (number->string self))

;;;

(define (eval-property-value ptx str)
  (cond
    ((symbol? ptx)
       (case ptx
        ((<integer>) 
	   (let ((n (string->number str)))
	     (if (integer? n)
	         n
		 #f)))        
	((<number>) 
	   (string->number str))
	((<string>) 
	   str)
	((<symbol>)
	   (let* ((s (open-input-string str))
		  (first (read s))
		  (second (read s)))
	     (if (and (symbol? first)
	              (eof-object? second))
	         first
		 #f)))
	((<user>) 
	   (table-lookup (user-table *application*) str))
	((<group>) 
	   (table-lookup (group-table *application*) str))
	((<file-system>) 
	   (table-lookup (file-system-table *application*) str))
	((<change-request>)
	   (let ((k (string->number str)))
	      (and (fixnum? k)
		    (table-lookup (change-request-table *application*) k))))
	(else
	  (error "[internal] invalid property type expr: ~s" ptx))))
    ((pair? ptx)
        (case (car ptx)
	   ((enum)
	      (let loop ((i (cdr ptx)))
	         (if (null? i)
		     #f
		     (if (string=? (property-value-string-form (car i)) str)
			 (car i)
			 (loop (cdr i))))))
	   ((list)
	      (let ((x (map (curry eval-property-value (cadr ptx))
	                    (string-split str #\space))))
	 	(if (every? identity x)
		    x
		    #f)))
	   ((or)
	      (let loop ((i (cdr ptx)))
	        (if (null? i)
		    #f
		    (or (eval-property-value (car ptx) str)
		        (loop (cdr i))))))
	   (else
		(error "[internal] invalid property type expr: ~s" ptx))))
    (#t
	(error "[internal] invalid property type expr: ~s" ptx))))

;;

