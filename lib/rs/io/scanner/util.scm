;;

(define-method collect-tok ((self <input-port>) 
			    (intok <char-set>) 
			    (pre <vector>))
  (let loop (((v <vector>) pre)
	     ((n <fixnum>) (vector-length pre)))
    (let ((ch (input-port-peek-char self)))
      (if (and (char? ch)
	       (table-lookup intok ch))
	  (if (< n (vector-length v))
	      (begin
		(vector-set! v n (input-port-read-char self))
		(loop v (add1 n)))
	      (loop (vector-append 
		     v
		     (vector (input-port-read-char self)
			     0 0 0 0 0 0 0 0 0 0 0 0 0 0))
		    (add1 n)))
	  (vector-slice v 0 n)))))

;;

;; 53 bogomips
