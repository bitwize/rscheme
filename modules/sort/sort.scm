#|------------------------------------------------------------*-Scheme-*--|
 | File:    modules/sort/sort.scm
 |
 |          Copyright (C)1997 Donovan Kolbly <d.kolbly@rscheme.org>
 |          as part of the RScheme project, licensed for free use.
 |          See <http://www.rscheme.org/> for the latest information.
 |
 | File version:     1.7
 | File mod date:    1997-11-29 23:10:34
 | System build:     v0.7.3.4-b7u, 2007-05-30
 | Owned by module:  sort
 |
 | Purpose:          heap sort function for lists and vectors
 `------------------------------------------------------------------------|#

; a sort facility, using heapsort
;
; adapted from the HeapSort algorithm
; presented in:
;	Algorithms, 2nd ed.
;	Robert Sedgewick
;	Addison Wesley

(%strategy ccode
(define (sort:down-heap (heap <vector>)
			(N <fixnum>)
			compare-fn 
			(k0 <fixnum>))
  (let ((v (gvec-ref heap k0))
	((heap <vector>) heap)
	((N/2 <fixnum>) (div2 N)))
    (let loop (((k <fixnum>) k0)
	       ((j <fixnum>) (add1 (mul2 k0))))
      (if (fixnum<? k N/2)
	  (begin
	    (if (fixnum<? (add1 j) N)
		(if (compare-fn (gvec-ref heap j)
				(gvec-ref heap (add1 j)))
		    (set! j (add1 j))))
	    (if (compare-fn v (gvec-ref heap j))
		(begin
		  (gvec-set! heap k (gvec-ref heap j))
		  (loop j (add1 (mul2 j))))
		(begin
		  (gvec-set! heap k v)
		  (values))))
	  (begin
	    (gvec-set! heap k v)
	    (values)))))))

(define (sort:heap-sort-1 heap (N <fixnum>) compare-fn)
  (let loop1 ((k (div2 (add1 N))))
    (if (fixnum>? k 0)
	(begin
	  (sort:down-heap heap N compare-fn (sub1 k))
	  (loop1 (sub1 k))))))

(define (sort:heap-sort->list (heap <vector>) (N <fixnum>) compare-fn)
  (sort:heap-sort-1 heap N compare-fn)
  (let loop2 ((result '()) 
	      ((n <fixnum>) (sub1 N)))
    (if (eq? n -1)
	result
	(let ((top (gvec-ref heap 0)))
	  (gvec-set! heap 0 (gvec-ref heap n))
	  (sort:down-heap heap n compare-fn 0)
	  (loop2 (cons top result) (sub1 n))))))

(define (sort:heap-sort->vector (heap <vector>) 
				(N <fixnum>) 
				compare-fn
				(result <vector>))
  (sort:heap-sort-1 heap N compare-fn)
  (let loop2 (((n <fixnum>) (sub1 N)))
    (if (eq? n -1)
	result
	(begin
	  (gvec-set! result n (gvec-ref heap 0))
	  (gvec-set! heap 0 (gvec-ref heap n))
	  (sort:down-heap heap n compare-fn 0)
	  (loop2 (sub1 n))))))

(define (vector-sort! (vector <vector>) compare-fn)
  (sort:heap-sort->vector (clone vector)
			  (vector-length vector)
			  compare-fn
			  vector))

(define (sort collection compare-fn)
  (if (vector? collection)
      (sort:heap-sort->vector (clone collection) 
			      (vector-length collection) 
			      compare-fn
			      (make-vector (vector-length collection)))
      (let ((heap (list->vector collection)))
	(sort:heap-sort->list heap (vector-length heap) compare-fn))))
