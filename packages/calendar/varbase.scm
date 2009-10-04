#|
(define (yp y)
  (letrec ((quad-cent (quotient y 400))
	   (n1 (remainder y 400))
	   (cent (quotient n1 100))
	   (n2 (remainder n1 100))
	   (yr (quotient n2 4))
	   (n3 (remainder n2 4)))
    (values quad-cent cent yr n3)))


(define (digit-stripper expr parts digits)
  (if (pair? parts)
      (let ((quot (gensym))
	    (rem (gensym))
	    (v (car parts)))
	`(let (((,quot <fixnum>) (fixnum-quotient ,expr ,(car parts)))
	       ((,rem <fixnum>) (remainder ,expr ,(car parts))))
	   ,(digit-stripper rem (cdr parts) (append digits (list quot)))))
      `(values ,@digits ,expr)))
|#

#|
;;
;;  returns the digits breakdown in variable base (pre-multiplied)
;;
;;   e.g., (digit-breakdown 12345 (1000 100 10)) => 12 3 4 5
;;   e.g., (digit-breakdown 12345 (200 50 2)) => 61 2 22 1

(define-rewriter (digit-breakdown form)
  ;;
  (define (digit-stripper expr parts digits)
    (if (pair? parts)
	(let ((quot (gensym))
	      (rem (gensym))
	      (v (car parts)))
	  `(let ((,quot (fixnum-quotient ,expr ,(car parts)))
		 (,rem (remainder ,expr ,(car parts))))
	     ,(digit-stripper rem (cdr parts) (append digits (list quot)))))
	`(values ,@digits ,expr)))
  ;;
  (digit-stripper (cadr form) (caddr form) '()))
|#
