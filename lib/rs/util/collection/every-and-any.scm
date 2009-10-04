
(define-syntax (nop x)
  x)

(define-macro (define-sequence-mapper name #key base-case 
		                                compose 
						(finally default: nop))
  (let ((triv-name (symbol-append name "/1")))
    `(begin
       ;
       (define-method ,triv-name ((self <empty-list>) proc)
	 ,base-case)
       ;
       (define-method ,triv-name ((self <pair>) (proc <function>))
	 (let loop ((s self))
	   (if (pair? s)
	       (,compose (proc (car s))
			 (loop (cdr s)))
	       ,base-case)))
       ;
       (define-method ,triv-name ((self <vector>) (proc <function>))
	 (let (((n <fixnum>) (vector-length self)))
	   (let loop (((i <fixnum>) 0))
	     (if (fixnum<? i n)
		 (,compose (proc (vector-ref self i))
			   (loop (add1 i)))
		 ,base-case))))
       ;
       (define-method ,triv-name ((self <sequence>) proc)
	 (let loop ((s (initial-state self)))
	   (if s
	       (,compose (proc (current-element self s))
			 (loop (next-state self s)))
	       ,base-case)))
       ;
       (define ,name
	 (nlambda
	  ;; highly specialized implementations for a single sequence
	  ((proc seq1)
	   (,triv-name seq1 proc))
	  ;; slightly specialized implementation for two sequences
	  ((proc seq1 seq2)
	   (let loop ((s1 (initial-state seq1))
		      (s2 (initial-state seq2)))
	     (if (and s1 s2)
		 (,compose (proc (current-element seq1 s1)
				 (current-element seq2 s2))
			   (loop (next-state seq1 s1)
				 (next-state seq2 s2)))
		 ,base-case)))
	  ;; degenerate case
	  ((proc)
	   ,base-case)
	  ;; generic (and slow) implementation
	  ((proc #rest seqs)
	   (let loop ((ss (map initial-state seqs)))
	     (if (every-not-false? ss)
		 (,compose (apply proc (map current-element seqs ss))
			   (loop (map next-state seqs ss)))
		 ,base-case))))))))

;;;

(define-sequence-mapper every?
  base-case: #t
  compose: and)

(define-sequence-mapper any?
  base-case: #f
  compose: or)

;;;

(define (every-not-false? lst)
  (let loop ((l lst))
    (if (null? l)
	#t
	(if (car l)
	    (loop (cdr l))
	    #f))))
