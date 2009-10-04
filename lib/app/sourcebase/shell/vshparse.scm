(define (vsh-read port)
   (bind ((typ val lin (vsh-scan-token port)))
     (case typ
       ((eof) val)
       ((open) (let ((val (vsh-read port))
                     (r (vsh-read port)))
                 (cons val (if (eof-object? r)
		               '()
			       r))))
       ((close) '())
       ((word) (let ((r (vsh-read port)))
                 (cons val (if (eof-object? r)
		               '()
			       r))))
       ((eol delim) '()))))

(define (vsh-scan-token port)
  (if (eq? (peek-char port) #\()
      (bind ((datum line (read port)))
         (values 'word datum line))
      (vsh-scan-token* port)))

(define (vsh-scan-token* port)
  (let ((line (input-port-line-number port))
        (ch (read-char port)))
    (if (eof-object? ch)
        (values 'eof ch line)
	(case ch
	    ((#\newline) (values 'eol #f line))
	    ((#\#) (let loop ()
	             (let ((ch (read-char port)))
		       (if (or (eq? ch #\newline)
		               (eof-object? ch))
			   (vsh-scan-token port)
			   (loop)))))
	    ((#\space #\tab) (vsh-scan-token port))
	    ((#\[) (values 'open #t line))
	    ((#\]) (values 'close #t line))
	    ((#\;) (values 'delim #t line))
	    ((#\") (values 'word
	    		   (list->string
			    (call-with-list-extending
			     (lambda (add)
			       (let loop ()
			       (let ((ch (read-char port)))
			         (if (eq? ch #\")
				     #t
				     (begin
					(if (eq? ch #\\)
					    (set! ch (read-char port)))
					(add ch)
					(loop))))))))
			    line))
	    (else (values 'word
	    		  (list->string
			   (call-with-list-extending
			    (lambda (add)
			     (add ch)
			     (let loop ()
			       (let ((ch (peek-char port)))
			         (if (char-wordbreak? ch)
				     #t
				     (begin
					(if (eq? ch #\\)
					    (read-char port))
					(add (read-char port))
					(loop))))))))
			line))))))

(define (char-wordbreak? ch)
  (or (char-whitespace? ch)
      (eq? ch #\[)
      (eq? ch #\])
      (eq? ch #\;)
      (eof-object? ch)))
      
      