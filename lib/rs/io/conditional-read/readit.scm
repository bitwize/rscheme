
(define (scan-conditional port ch polarity)
  (bind ((feature fline (read port))
	 (subseq sline (read port))
	 (answer (eval-feature feature)))
    (if (if polarity answer (not answer))
	(make-token '<literal> subseq sline)
	(input-port-scan-token port))))

(define (scan-sharp-plus port ch)
  (scan-conditional port ch #t))

(define (scan-sharp-minus port ch)
  (scan-conditional port ch #f))

(vector-set! *sharp-scanners*
	     (char->integer #\+) 
	     scan-sharp-plus)
(vector-set! *sharp-scanners* 
	     (char->integer #\-)
	     scan-sharp-minus)
