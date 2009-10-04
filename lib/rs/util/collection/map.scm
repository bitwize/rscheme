
;;; renamed to `map1x' to avoid conflict with `map1' macro

(define-method map1x ((self <empty-list>) proc)
  '())

(define-method map1x ((self <pair>) proc)
  (let (((first <pair>) (cons (proc (car self)) '())))
    (let loop ((l (cdr self))
	       ((dest <pair>) first))
      (if (null? l)
	  first
	  (let ((next (cons (proc (car l)) '())))
	    (set-cdr! dest next)
	    (loop (cdr l) next))))))

(define-method map1x ((self <vector>) (proc <function>))
  (let (((r <vector>) (make-vector (vector-length self)))
	((n <fixnum>) (vector-length self)))
    (let loop (((i <fixnum>) 0))
      (if (fixnum<? i n)
	  (begin
	    (vector-set! r i (proc (vector-ref self i)))
	    (loop (add1 i)))
	  r))))

(define-method map1x ((self <sequence>) proc)
  (let loop ((s (initial-state self)))
    (if s
	(cons (proc (current-element self s))
	      (loop (next-state self s)))
	'())))

;;;

(define (map2x (proc <function>) seq1 seq2)
  (let loop ((s1 (initial-state seq1))
	     (s2 (initial-state seq2)))
    (if (and s1 s2)
	(cons (proc (current-element seq1 s1)
		    (current-element seq2 s2))
	      (loop (next-state seq1 s1)
		    (next-state seq2 s2)))
	'())))

(define map
  (nlambda
   ;; highly specialized implementations for a single sequence
   ((proc seq1)
    (map1x seq1 proc))
   ;; slightly specialized implementation for two sequences
   ((proc seq1 seq2)
    (sequence-as (map2x proc seq1 seq2) (object-class seq1)))
   ;; degenerate case
   ((proc)
    '())
   ;; generic (and slow) implementation
   ((proc #rest seqs)
    (sequence-as
     (let loop ((ss (map1x seqs initial-state)))
       (if (every-not-false? ss)
	   (cons (apply proc (map2x current-element seqs ss))
		 (loop (map2x next-state seqs ss)))
	   '()))
     (object-class (car seqs))))))

;;;
