
(define *number-recognizers* '#())

(define (add-number-recognizer! proc)
  (set! *number-recognizers* 
	(vector-append *number-recognizers* (vector proc)))
  proc)

(define (parse-number (str <string>) (default-radix <integer>))
  (let loop ((i 0))
    (if (< i (vector-length *number-recognizers*))
	(let ((proc (vector-ref *number-recognizers* i)))
	  (or (proc str default-radix)
	      (loop (+ i 1))))
	#f)))

(add-number-recognizer! string->number)

,(use regex)

(add-number-recognizer!
 (let ((pat (reg-expr->proc '(entire (seq (let num (+ hex-digit))
					  #\/
					  (let den (+ hex-digit)))))))
   (lambda (str radix)
     (bind ((s e num den (pat str)))
       (and s
	    (/ (parse-number num radix)
	       (parse-number den radix)))))))

(define-class <complex*> (<complex>)
  (real type: <number>)
  (imaginary type: <number>))

(define-method write-object ((self <complex*>) port)
  (if (negative? (imaginary self))
      (format port "~d~di" (real self) (imaginary self))
      (format port "~d+~di" (real self) (imaginary self))))


(add-number-recognizer!
 (let ((pat (reg-expr->proc '(entire (seq (let re (+ (or #\/ #\. hex-digit)))
					  (let si (or #\+ #\-))
					  (let im (+ (or #\/ #\. hex-digit)))
					  #\i)))))
   (lambda (str radix)
     (bind ((s e re si im (pat str)))
       (and s
	    (make <complex*>
		  real: (parse-number re radix)
		  imaginary: ((if (string=? si "-")
				  -
				  identity)
			      (parse-number im radix))))))))


