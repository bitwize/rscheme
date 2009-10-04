
(define-method scan-number ((self <input-port>) 
			    (ch <char>) 
			    (start <text-location>))
  (let* ((n (collect-tok self *num-continued* (vector ch)))
	 (str (vector->string n))
	 (len (location- (location self) start)))
    ;; check for some special cases that start out looking like numbers
    (cond
     ((string=? str ".")
      (make <dot-token>
	    location: start
	    lexeme-length: len))
     ((member str '("..." "+" "-"))
      (make <identifier-token>
	    data: (string->symbol str)
	    location: start
	    lexeme-length: len))
     (else
      (let ((n (parse-number str 10)))
	(if n
	    (make <numeric-token>
		  data: n
		  location: start
		  lexeme-length: len)
	    (signal (make <invalid-numeric-token>
			  source: self
			  position: start
			  content: str))))))))

			  


