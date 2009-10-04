#|------------------------------------------------------------*-Scheme-*--|
 | File:    modules/lowscm/mapping.scm
 |
 |          Copyright (C)1997 Donovan Kolbly <d.kolbly@rscheme.org>
 |          as part of the RScheme project, licensed for free use.
 |          See <http://www.rscheme.org/> for the latest information.
 |
 | File version:     1.11
 | File mod date:    2006-01-28 16:51:48
 | System build:     v0.7.3.4-b7u, 2007-05-30
 | Owned by module:  low-scheme
 |
 | Purpose:          Functions to apply other functions over lists
 `------------------------------------------------------------------------|#

;;
;;

(%strategy ccode

(define-inline (map1 proc list)
  (if (null? list)
      '()
      (let (((first <pair>) (cons (proc (car list)) '())))
	(let loop ((l (cdr list))
		   ((dest <pair>) first))
	  (if (null? l)
	      first
	      (let ((next (cons (proc (car l)) '())))
		(set-cdr! dest next)
		(loop (cdr l) next)))))))

(define-inline (map2 proc l1 l2)
  (let loop ((l1 l1)
	     (l2 l2))
    (if (null? l1)
	'()
	(cons (proc (car l1) (car l2)) 
	      (loop (cdr l1) (cdr l2))))))

#|
(define (map1g (proc <single-dispatch-gf>) list)
  (if (pair? list)
      (cons (proc (car list)) (map1g proc (cdr list)))
      (if (null? list)
	  '()
	  (type-error 'map 1 list "not a list"))))
|#
)

;; unsafe-map-c[ad]r are unsafe with regard to the list
;; and with regard to the contents of the list.
;; that is, they require that `lst' be a proper list,
;; and each of the elements of `lst' to be a pair.

(define (unsafe-map-car list)
    (if (null? list)
	'()
	(cons (car (car list)) (unsafe-map-car (cdr list)))))

(define (unsafe-map-cdr list)
    (if (null? list)
	'()
	(cons (cdr (car list)) (unsafe-map-cdr (cdr list)))))

(define (all-pairs? lst)
  (let loop ((l lst))
    (if (null? l)
	#t
	(if (pair? (car l))
	    (loop (cdr l))
	    #f))))

(define (unsafe-car x)
    (car x))

(define (unsafe-cdr x)
    (cdr x))

(define (mapn proc lists)
    (if (all-pairs? lists)
	(cons (apply* (unsafe-map-car lists) proc)
	      (mapn proc (unsafe-map-cdr lists)))
	'()))


(define-inline map
  (nlambda
   ((proc lst) (map1 proc lst))
   ((proc l1 l2) (map2 proc l1 l2))
   ((proc #rest lists) (mapn proc lists))))

(%strategy ccode
(define (for-each1 (proc <function>) list)
  (if (pair? list)
      (let loop (((list <pair>) list))
	(if (pair? (cdr list))
	    (begin
	      (proc (car list))
	      (loop (cdr list)))
	    (proc (car list))))
      (values)))

(define (for-each2 (proc <function>) l1 l2)
  (if (and (pair? l1)
           (pair? l2))
      (let loop (((l1 <pair>) l1)
                 ((l2 <pair>) l2))
	(if (and (pair? (cdr l1))
                 (pair? (cdr l2)))
	    (begin
	      (proc (car l1) (car l2))
	      (loop (cdr l1) (cdr l2)))
	    (proc (car l1) (car l2))))
      (values))))

(define (for-eachn proc lists)
  (if (all-pairs? lists)
      (let loop ((lists lists))
        (let ((next (unsafe-map-cdr lists)))
          (if (all-pairs? next)
              (begin
                (apply* (unsafe-map-car lists) proc)
                (loop next))
              (apply* (unsafe-map-car lists) proc))))
      (values)))

(define for-each
  (nlambda
   ((proc a) (for-each1 proc a))
   ((proc a b) (for-each2 proc a b))
   ((proc #rest r) (for-eachn proc r))))

(%strategy ccode
(define (any? (predicate <function>) list)
    (let loop ((x list))
	(if (pair? x)
	    (if (predicate (car x))
		#t
		(loop (cdr x)))
	    #f)))

(define (every? (predicate <function>) list)
    (let loop ((x list))
	(if (pair? x)
	    (if (predicate (car x))
		(loop (cdr x))
		#f)
	    #t)))

(define (dequeue-for-each (proc <function>) q)
  (let ((n (dequeue-count q)))
    (let loop (((i <fixnum>) 0))
      (if (eq? i n)
          (values)
          (let ((item (dequeue-ref q i)))
            (proc item)
            (loop (add1 i)))))))

(define (dequeue-state-as-list q)
  (let ((n (dequeue-count q)))
    (let loop (((i <fixnum>) n)
               (r '()))
      (if (eq? i 0)
          r
          (loop (sub1 i) (cons (dequeue-ref q (sub1 i)) r))))))

)
