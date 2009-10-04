
(check (values->list (vector->values '#())) '())
(check (values->list (vector->values '#(x))) '(x))
(check (values->list (vector->values '#(x y))) '(x y))
(check (values->list (vector->values '#(x y z))) '(x y z))
(check (values->list (vector->values '#(x y z 1))) '(x y z 1))

;-----------------------------------------------------------------------

(define (all-rq a b)                 (list a b))
(define (all-opt #optional a b)      (list a b))
(define (all-key #key a b)           (list a b))
(define (all-rest #rest a)           (list a))

(define (mixed-ro a #optional b)      (list a b))
(define (mixed-rk a #key b)           (list a b))
(define (mixed-rr a #rest b)          (list a b))
(define (mixed-ok #optional a #key b) (list a b))
(define (mixed-or #optional a #rest b)(list a b))

(define (mixed-rok a #optional b #key c)  (list a b c))
(define (mixed-ror a #optional b #rest c) (list a b c))
(define (mixed-rkr a #rest b #key c)      (list a b c))
(define (mixed-rokr a #optional b #rest c #key d) (list a b c d))

;-----------------------------------------------------------------------

(check (all-opt) '(#f #f))
(check (all-opt 1) '(1 #f))
(check (all-opt 1 2) '(1 2))
(expect-to-fail (all-opt 1 2 3))

;..................................................

(check (all-rq 1 2) '(1 2))
(expect-to-fail (all-rq 1))
(expect-to-fail (all-rq 1 2 3))

;..................................................

(check (mixed-ro 1) '(1 #f))
(check (mixed-ro 1 2) '(1 2))
(expect-to-fail (mixed-ro))
(expect-to-fail (mixed-ro 1 2 3))

;..................................................

(check (mixed-rk 1 b: 2) '(1 2))
(expect-to-fail (mixed-rk 1))    ; undefaulted keywords are required
