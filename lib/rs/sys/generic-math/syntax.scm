(define-syntax +
  (syntax-form ()
    0)
  (syntax-form (a)
    a)
  (syntax-form (a b)
    (binary+ a b))
  (syntax-form (a b . more)
    (binary+ a (+ b . more)))
  (else gm-full+))

(define-syntax *
  (syntax-form ()
    1)
  (syntax-form (a)
    a)
  (syntax-form (a b)
    (binary* a b))
  (syntax-form (a b . more)
    (binary* a (* b . more)))
  (else gm-full*))


(define-syntax -
  (syntax-form ()
    0)
  (syntax-form (a)
    (binary- 0 a))
  (syntax-form (a b)
    (binary- a b))
  (syntax-form (a b . more)
    (- (binary- a b) . more))
  (else gm-full-))

(define-syntax /
  (syntax-form ()
    1)
  (syntax-form (a)
    (binary/ 1 a))
  (syntax-form (a b)
    (binary/ a b))
  (syntax-form (a b . more)
    (/ (binary/ a b) . more))
  (else gm-full/))

(define (gm-full+ . args)
  (let loop ((x args) 
	     (sum 0))
    (if (null? x)
	sum
	(loop (cdr x)
	      (binary+ sum (car x))))))

(define (gm-full- arg1 . args)
  (if (null? args)
      (binary- 0 arg1)
      (let loop ((x (cdr args))
		 (diff (binary- arg1 (car args))))
	(if (null? x)
	    diff
	    (loop (cdr x) (binary- diff (car x)))))))

(define (gm-full* . args)
  (let loop ((x args) (prod 1))
    (if (null? x)
	prod
	(loop (cdr x) (binary* prod (car x))))))

(define (gm-full/ arg1 . args)
  (if (null? args)
      (binary/ 1 arg1)
      (let loop ((x (cdr args))
		 (quotient (binary/ arg1 (car args))))
	(if (null? x)
	    quotient
	    (loop (cdr x) (binary/ quotient (car x)))))))
