
(define-method scan-curly ((self <input-port>) 
			   (ch <char>) 
			   (start <text-location>))
  (let ((content (open-output-string)))
    (if (scan-curly* self content)
	(make <curly-braced-token>
	      location: start
	      lexeme-length: (location- (location self) start)
	      data: (close-output-port content))
	(signal (make <unterminated-curly-braced-token>
		      source: self
		      location: start)))))

(define (scan-curly* port dest)
  (let loop ()
    (let ((ch (input-port-read-char port)))
      (if (char? ch)
	  (case ch
	    ((#\{) 
	     (output-port-write-char dest #\{)
	     (if (scan-curly* port dest)
		 (begin
		   (output-port-write-char dest #\})
		   (loop))
		 #f))
	    ((#\}) #t)
	    ((#\") 
	     (if (scan-curly*string port dest)
		 (loop)
		 #f))
	    (else
	     (output-port-write-char dest ch)
	     (loop)))
	  #f))))

(define (scan-curly*string port dest)
  (let loop ()
    (let ((ch (input-port-read-char port)))
      (if (char? ch)
	  (case ch
	    ((#\") 
	     (output-port-write-char dest #\")
	     #t)
	    ((#\\)
	     (output-port-write-char dest #\\)
	     (let ((ch (input-port-read-char port)))
	       (if (char? ch)
		   (begin
		     (output-port-write-char dest ch)
		     (loop))
		   #f)))
	    (else
	     (output-port-write-char dest ch)
	     (loop)))
	  #f))))
