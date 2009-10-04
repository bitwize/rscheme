
;;;

(define for-each
  (nlambda 
   ;; specialized implementation for a single sequence
   ((proc seq1)
    (let ((s (initial-state seq1)))
      (if s
	  (let loop ((s s))
	    (let ((n (next-state seq1 s)))
	      (if n
		  (begin
		    (proc (current-element seq1 s))
		    (loop n))
		  (proc (current-element seq1 s)))))
	  (values))))
   ;; specialized implementation for two sequences
   ((proc seq1 seq2)
    (let ((s1 (initial-state seq1))
	  (s2 (initial-state seq2)))
      (if (and s1 s2)
	  (let loop ((s1 s1)
		     (s2 s2))
	    (let ((n1 (next-state seq1 s1))
		  (n2 (next-state seq2 s2)))
	      (if (and n1 n2)
		  (begin
		    (proc (current-element seq1 s1)
			  (current-element seq2 s2))
		    (loop n1 n2))
		  (proc (current-element seq1 s1)
			(current-element seq2 s2)))))
	  (values))))
   ;; degenerate case
   ((proc)
    (values))
   ;; generic (slow) implementation
   ((proc #rest seqs)
    (let ((ss (map initial-state seqs)))
      (if (every-not-false? ss)
	  (let loop ((ss ss))
	    (let ((nn (map next-state seqs ss)))
	      (if (every-not-false? nn)
		  (begin
		    (apply proc (map current-element seqs ss))
		    (loop nn))
		  (apply proc (map current-element seqs ss)))))
	  (values))))))
    
;;;

