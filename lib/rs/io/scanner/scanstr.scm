(define (my-scan-str-get-esc-number p pre-seq radix digit-set)
  (let* ((loc (location p))
	 (num (collect-tok p digit-set pre-seq)))
    (if (= (vector-length num) 0)
	(signal (make <missing-string-escape-number>
		      source: p
		      position: (location p)))
	(let ((n (string->number (vector->string num) radix)))
	  (if (< n 65536)
	      (integer->char n)
	      (signal (make <string-escape-number-too-big>
			    source: p
			    position: loc
			    data: n)))))))

(define (my-scan-str-escape p ch add! start)
  (case (char-downcase ch)
    ((#\n) (add! #\newline))
    ((#\t) (add! #\tab))
    ((#\") (add! #\"))
    ((#\\) (add! #\\))
    ((#\x) (add! (my-scan-str-get-esc-number p '#() 16 *hex-digit*)))
    ((#\') (add! #\'))
    ((#\.)) ;; a character escape NOP, useful for terminating numeric escapes
    ((#\v) (add! (integer->char 11)))
    ((#\b) (add! (integer->char 8)))
    ((#\r) (add! (integer->char 13)))
    ((#\f) (add! (integer->char 12)))
    ((#\a) (add! (integer->char 7)))
    ((#\?) (add! #\?))
    ((#\c) (let ((nxt (read-char p)))
	     (if (char? nxt)
		 (let ((code (char->integer (char-upcase nxt))))
		   (if (and (>= code 64) (< code 96))
		       (add! (integer->char (- code 64)))
		       (signal (make <invalid-string-escape>
				     string-escape: (vector #\\ ch nxt)
				     source: p
				     location: (previous-location p)))))
		 (signal (make <unterminated-string-token>
			       source: p
			       position: start)))))
    (else 
     (if (char-numeric? ch)
	 (my-scan-str-get-esc-number p '#() 8 *octal-digit*)
	 (signal (make <invalid-string-escape>
		       string-escape: (vector #\\ ch)
		       source: p
		       location: (previous-location p)))))))

(define (my-scan-string* p start)
  (letrec ((accum (open-output-string))
	   (add! (lambda (ch)
		   (output-port-write-char accum ch))))
    (let loop ()
      (let ((c (read-char p)))
	(if (char? c)
	    (if (eq? c #\\)
		;; an escape character.. process it
		(let ((c (read-char p)))
		  (if (char? c)
		      (begin
			(my-scan-str-escape p c add! start)
			(loop))
		      (signal (make <unterminated-string-token>
				    source: p
				    position: start))))
		;; not an escape character...
		(if (eq? c #\")
		    ;; but the end of the string, so return it
		    (close-output-port accum)
		    ;; not an escape or the EOS, so a regular char
		    (begin
		      (add! c)
		      (loop))))
	    ;; not a character at all; must be EOF
	    (signal (make <unterminated-string-token>
			  source: p
			  position: start)))))))
	 
(define-method scan-string-token ((self <input-port>)
				  (ch <char>)
				  (start <text-location>))
  (let ((str (my-scan-string* self start)))
    (make <string-token>
	  data: str
	  location: start
	  lexeme-length: (location- (location self) start))))
