;; library for smoothing over some of the diffs between
;; Dylan & Scheme

(define <double> <float>)
(define pair cons)
(define rest cdr)

(define (ne? x y) (not (equal? x y)))

(define-generic-function size)
(define-generic-function element)
(define-generic-function set-element!)

(define-method size ((self <list>))
    (length self))
    
(define-method size ((self <vector>))
    (vector-length self))
    
(define-method size ((self <string>))
    (string-length self))
    
(define (aref thing . elem)
    (let loop ((t thing) (e elem))
	(if (null? (cdr e))
	    (element t (car e))
	    (loop (element t (car e)) (cdr e)))))

(define (aref-setter new-value thing . elem)
    (let loop ((t thing) (e elem))
	(if (null? (cdr e))
	    (set-element! t (car e) new-value)
	    (loop (element t (car e)) (cdr e)))))

;; x[1,2] := 3  ===> (set-aref! x 1 2 3)

(define (set-aref! target next-1 next-2 . rest)
  (let loop ((t target)
	     (n1 next-1)
	     (n2 next-2)
	     (r rest))
    (if (null? r)
	(set-element! t n1 n2)
	;; pick a new target
	(loop (element t n1) n2 (cdr r)))))

(define (element-setter new-value thing elem)
    (set-element! thing elem new-value))

(define-method set-element! ((self <vector>) index value)
    (vector-set! self index value)
    value)

(define-method set-element! ((self <pair>) index value)
    (set-car! (list-tail self index) value)
    value)

(define-method set-element! ((self <string>) index value)
    (string-set! self index value)
    value)

(define-method element ((self <vector>) index)
    (vector-ref self index))

(define-method element ((self <pair>) index)
    (list-ref self index))

(define-method element ((self <string>) index)
    (string-ref self index))

(define-method element ((self <table>) key)
    (table-lookup self key))

(define-method set-element! ((self <table>) key value)
    (table-insert! self key value)
    value)
    
(define (first self)
    (element self 0))

(define (second self)
    (element self 1))

(define (third self)
    (element self 2))

(define (set-first! p v)
  (set-car! p v))

(define (set-rest! p v)
  (set-cdr! p v))

(define (head x)
    (if (null? x)
	'()
	(car x)))

(define (tail x)
    (if (null? x)
	'()
	(cdr x)))



;;  (do-set proc value target z z z)  ==> (proc target z z z value)
;;
;; for example,
;;   x.foo := 10
;; becomes
;;   (do-set set-foo! x 10)
;;                            <== this order is used to comply w/
;;                                Dylan evaluation order rqmts
;; which should be
;;   (set-foo! x 10)

(define (do-set proc value . args)
    (apply proc (append args (list value)))
    value)

