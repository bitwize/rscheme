;;
;;  this is a very direct translation of the
;;
;;  Mathematica Package Miscellaneous/Calendar.m
;;

(define (my-digits-init n lst path)
  ;(format #t "my-digits-init: ~d (...~d...) ~s\n" n (length lst) path)
  (if (null? lst)
      (list n)
      (bind ((q r ((car lst) n path)))
	(cons q
	      (my-digits 
	       r
	       (cdr lst)
	       (append path (list q)))))))

(define (my-digits n lst path)
  ;(format #t "my-digits: ~d (...~d...) ~s\n" n (length lst) path)
  (let ((md (my-digits-init n lst path)))
    ;(format #t "  md => ~s\n" md)
    (if (eq? (last md) 0)
	(increment-last (my-digits-init (- n 1) lst path))
        md)))

(define (increment-last (lst <pair>))
  (if (null? (cdr lst))
      (cons (+ (car lst) 1) '())
      (cons (car lst) (increment-last (cdr lst)))))

(define (my-quotient n lst)
  ;(format #t "my-quotient: ~d ~s\n" n lst)
  (if (eq? (length lst) 1)
      (values (+ 1 (quotient n (car lst)))
	      (modulo n (car lst)))
      (let loop ((s (car lst))
		 (c (car lst))
		 (lst (cdr lst))
		 (q 1)
		 (r n))
	;(format #t "\t\tloop: ~d ~s ~d (~d ~d)\n" s lst c q r)
	(if (> n s)
	    (let ((nxt (car lst)))
	      (loop (+ s nxt)
		    nxt
		    (cdr lst)
		    (+ q 1)
		    (- r c)))
	    (values q r)))))

#|
(define (myq-cent n)
  (if (<= n 36524)
      (values 1 n)
      (if (<= n 73048)
	  (values 2 (- n 36524))
	  (if (<= n 109572)
	      (values 3 (- n 73048))
	      (values 4 (- n 109572))))))

(define (my-years n path)
  (if (<= n 365)
      (values 1 n)
      (if (<= n 730)
	  (values 2 (- n 365))
	  (if (<= n 1095)
	      (values 3 (- n 730))
	      (values 4 (- n 1095))))))
|#

#|
        1996-12-31      729024  Years[ 1461 (5 4 24) ]
        2000-12-31      730485  Years[ 1460 (6 1 25) ]
        2004-12-31      731946  Years[ 1461 (6 1 1) ]

|#

(define (gregorian-four-centuries n path)
  ;; 146097 is the number of days in four centuries
  (values (+ 1 (quotient (- n 1) 146097))
          (+ 1 (modulo (- n 1) 146097))))

(define (gregorian-century n path)
  ;; 36524 is the number of days in one century
  (format #t "Century[ ~s ~s ]\n" n path)
  (values
   (+ (quotient (- n 1) 36524) 1)
   (+ (modulo (- n 1) 36524) 1)))

(define (gregorian-four-years n path)
  (format #t "FourYears[ ~s ~s ]\n" n path)
  ;; 1460 is the number of days in four years that 
  ;; *does not* include a leap year
  (my-quotient 
   n
   (if (eq? (cadr path) 4)
       ;; we are in the last century of a 4-century cycle, so
       ;; the last 4 years *does* include a leap year
       '(1461 1461 1461 1461 1461
         1461 1461 1461 1461 1461
         1461 1461 1461 1461 1461
         1461 1461 1461 1461 1461
         1461 1461 1461 1461            1461)
       ;; we are in a middle century of a 4-century cycle, so
       ;; the last 4-year block *does not* include a leap year,
       ;; and hence has only 1460 days
       '(1461 1461 1461 1461 1461
         1461 1461 1461 1461 1461
         1461 1461 1461 1461 1461
         1461 1461 1461 1461 1461
         1461 1461 1461 1461            1460))))

(define (gregorian-years n path)
  (format #t "Years[ ~s ~s ]\n" n path)
  (my-quotient 
   n
   (if (and (eq? (cadr path) 4)
            (not (eq? (caddr path) 25)))
       ;; A four-year with a leap year (always the last)
       '(365 365 365 366)
       ;; A four-year without a leap year
       '(365 365 365 365))))

(define (gregorian-months n path)
  (if (< n 1)
      (values 1 n)
      (my-month-quotient n (apply leap-path? path))))

(define gregorian-calendar
 (list 
  ;; GregorianFourCenturies
  gregorian-four-centuries
  ;; GregorianCentury
  gregorian-century
  ;; FourYears
  gregorian-four-years
  ;; Years
  gregorian-years
  ;; Months
  gregorian-months))


(define (leap-path? (car-path <fixnum>) 
		    (cadr-path <fixnum>)
		    (caddr-path <fixnum>)
		    (cadddr-path <fixnum>))
  (eq? (fixnum- (fixnum-quotient cadddr-path 4)
		(fixnum* (fixnum- 1
				  (fixnum-quotient cadr-path 4))
			 (fixnum-quotient caddr-path 25))) 
       1))

#|

(define (day->ymd day)
  (bind ((c4 c y4 y m d (list->values (my-digits day gregorian-calendar '()))))
    (values (+ 1 
	       (* (- c4 1) 400) 
	       (* (- c 1) 100)
	       (* (- y4 1) 4) 
	       (- y 1))
	    m
	    d)))
|#
