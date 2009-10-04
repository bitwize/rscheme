
;;
;; returns 5 values, which are the "digits" of the
;; year in variable base (400 100 4) and #t if it's a leap year
;; 

(define (year-parts (y <fixnum>))
  (bind ((quad-cent (quotient (sub1 y) 400))
	 (n1 (remainder (sub1 y) 400))
	 (cent (quotient n1 100))
	 (n2 (remainder n1 100))
	 (yr (quotient n2 4))
	 (n3 (remainder n2 4)))
    (values quad-cent cent yr n3
	    (if (eq? n3 3)
		(if (eq? yr 24)
		    (eq? cent 3)
		    #t)
		#f))))
