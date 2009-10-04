
(define (common a b)
  (let ((m (min (string-length a)
		(string-length b))))
    (let loop ((i 0))
      (if (< i m)
	  (if (char=? (string-ref a i)
		      (string-ref b i))
	      (loop (+ i 1))
	      i)
	  i))))

;;
;; names must be sorted
;;  

(define (find-abbreviations names min)
  (let loop ((prev #f)
	     (n names)
	     (r '()))
    (if (null? n)
	(reverse r)
	(let* ((s (car n))
	       (max-common-len (max (if prev 
					(common prev s)
					0)
				    (if (pair? (cdr n))
					(common (cadr n) s)
					0)
				    min)))
	  (loop s (cdr n) (cons (abbrev-list s max-common-len) r))))))
	    
(define (abbrev-list (s <string>) max-common-len)
  (let loop ((i (- (string-length s) 1))
	     (abrevs '()))
    ;(format #t "abbrev[~d/~d] of ~s: ~s\n" i max-common-len s (substring s 0 i))
    (if (<= i max-common-len)
	abrevs
	(loop (- i 1) (cons (substring s 0 i) abrevs)))))

;;
