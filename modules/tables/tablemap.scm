#|------------------------------------------------------------*-Scheme-*--|
 | File:    modules/tables/tablemap.scm
 |
 |          Copyright (C)1997 Donovan Kolbly <d.kolbly@rscheme.org>
 |          as part of the RScheme project, licensed for free use.
 |          See <http://www.rscheme.org/> for the latest information.
 |
 | File version:     1.5
 | File mod date:    1997-11-29 23:10:34
 | System build:     v0.7.3.4-b7u, 2007-05-30
 | Owned by module:  tables
 |
 | Purpose:          map over hash table contents
 `------------------------------------------------------------------------|#

(define-syntax ($bucket-size) 8)

(define-syntax (bucket-hash bucket ix)
    (gvec-ref bucket (fixnum+ ix 2)))
    
(define-syntax (bucket-key bucket ix)
    (gvec-ref bucket (fixnum+ ix (fixnum+ 2 ($bucket-size)))))
    
(define-syntax (bucket-value bucket ix)
    (gvec-ref bucket (fixnum+ ix (fixnum+ 2 (fixnum* 2 ($bucket-size))))))

(define (bucket-for-each (bucket <table-bucket>) proc)
  (let buckets-loop (((b <table-bucket>) bucket))
    (let loop (((i <fixnum>) 0))
      (if (fixnum<? i ($bucket-size))
	  (begin
	    (if (bucket-hash b i)
		(proc (bucket-hash b i)
		      (bucket-key b i)
		      (bucket-value b i)))
	    (loop (add1 i)))
	  (if (bucket-overflow b)
	      (buckets-loop (bucket-overflow b))
	      (values))))))

;; counts the # entries in a bucket

(define (bucket-count b)
    (let loop (((i <fixnum>) 0) ((n <fixnum>) 0))
	(if (fixnum<? i ($bucket-size))
	    (loop (add1 i)
	          (if (bucket-hash b i)
		      (add1 n)
		      n))
	    n)))

(define-method table-for-each ((self <table>) (proc <closure>))
  (for-each
   (lambda (bucket)
     (bucket-for-each bucket proc))
   (table-chains self)))

;; returns a pair of the length of a chain in # buckets 
;; and the # entries along the chain

(define (chain-length chain)
    (let loop (((i <fixnum>) 0) ((n <fixnum>) 0) (b chain))
	(if b
	    (loop (add1 i) 
		  (fixnum+ n (bucket-count b))
		  (bucket-overflow b))
	    (cons i n))))

;; returns a vector:
;;   [0] is a vector of the size of each bucket chain
;;   [1] is the total # of (unique) chains
;;   [2] is a list of the lengths of the unique chains

(define (table-stats (self <table>))
    (let ((chains (table-chains self)))
	(vector
	    (vector-map chain-length (directory self))
	    (length chains)
	    (map chain-length chains))))
