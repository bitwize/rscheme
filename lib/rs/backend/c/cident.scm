;; remove special characters
;;
;; used for converting scheme representations into those
;; palatable to the OS in the form of file names and palatable
;; to C in the form of identifiers

(define (make-c-identifier (str <string>))
  (letrec ((result (make-seq))
	   (initial (lambda (s)
		      (if (null? s)
			  "squiggle"
			  (if (valid-initial? (car s))
			      (begin
				(seq-add! result (car s))
				(saw-valid (cdr s)))
			      (initial (cdr s))))))
	   (saw-valid (lambda (s)
			(if (null? s)
			    (list->string (seq->list result))
			    (if (valid-non-initial? (car s))
				(begin
				  (seq-add! result (car s))
				  (saw-valid (cdr s)))
				(saw-invalid (cdr s))))))
	   (saw-invalid (lambda (s)
			  (if (null? s)
			      (list->string (seq->list result))
			      (if (valid-non-initial? (car s))
				  (begin
				    (seq-add! result #\_)
				    (seq-add! result (car s))
				    (saw-valid (cdr s)))
				  (saw-invalid (cdr s)))))))
    (initial (string->list str))))

(define (valid-initial? ch)
  (char-alphabetic? ch))

(define (valid-non-initial? ch)
  (or (char-alphabetic? ch) 
      (char-numeric? ch)
      (eq? ch #\_)))

(define (string-upcase str)
  (list->string (map char-upcase (string->list str))))

