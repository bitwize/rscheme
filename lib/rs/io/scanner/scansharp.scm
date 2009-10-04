
(define-method scan-sharp ((self <input-port>) 
			   (ch <char>) 
			   (start <text-location>))
  (let ((sh-ch (input-port-read-char self)))
    (if (char? sh-ch)
	(let ((ent (table-lookup (sharp-table self) sh-ch)))
	  (if ent
	      (ent self sh-ch start)
	      (signal (make <invalid-character-after-sharp>
			    source: self
			    character: sh-ch
			    position: start))))
	(signal (make <unterminated-sharp>
		      source: self
		      position: start)))))

;;;

(define-method scan-open-vector ((self <input-port>)
				 (ch <char>)
				 (start <text-location>))
  (make <open-vector-token>
	location: start))

;;;

(define-method scan-id-sharp ((self <input-port>)
			      (ch <char>)
			      (start <text-location>))
  (bind ((id (collect-tok self *id-continued* (vector ch)))
	 (len (location- (location self) start))
	 (str (list->string (vector->list id)))
	 (ent (table-lookup (sharp-id-table self) str)))
    (if (pair? ent)
	(make <boolean-token>
	      location: start
	      lexeme-length: len
	      data: (car ent))
	(if ent
	    (make <syntactic-keyword-token>
		  location: start
		  lexeme-length: len
		  data: ent)
	    ;; it's not a boolean, and it's not a syntactic keyword...
	    ;; it might be a number like #xFFC0 or #b10110
	    (let ((n (case (char-downcase (string-ref str 0))
		       ((#\x) (parse-number (substring str 1) 16))
		       ((#\b) (parse-number (substring str 1) 2))
		       ((#\o) (parse-number (substring str 1) 8))
		       ((#\d) (parse-number (substring str 1) 10))
		       (else #f))))
	      (if n
		  (make <numeric-token>
			data: n
			location: start
			lexeme-length: len)
		  (signal (make <invalid-sharp>
				sharp-lexeme: str
				lexeme-length: len
				source: self
				location: start))))))))

;;;

(define-method scan-long-comment ((self <input-port>)
				  (pipe-ch <char>)
				  (start <text-location>))
  (skip-long-comment self start)
  (scan self))

(define (skip-long-comment (self <input-port>)
			   (start <text-location>))
  (letrec ((main-text (lambda ()
			(let ((ch (input-port-read-char self)))
			  (if (char? ch)
			      (if (eq? ch #\|)
				  (saw-pipe)
				  (if (eq? ch #\#)
				      (saw-sharp)
				      (main-text)))
			      (hit-eof)))))
	   (saw-pipe (lambda ()
			(let ((ch (input-port-read-char self)))
			  (if (char? ch)
			      (if (eq? ch #\#)
				  #t ;; all done
				  (if (eq? ch #\|)
				      (saw-pipe)
				      (main-text)))
			      (hit-eof)))))
	   (saw-sharp (lambda ()
			(let ((ch (input-port-read-char self)))
			  (if (char? ch)
			      (if (eq? ch #\#)
				  (saw-sharp)
				  (begin
				    (if (eq? ch #\|)
					(skip-long-comment 
					 self
					 (location+ (location self) -2)))
				    (main-text)))
			      (hit-eof)))))
	   (hit-eof (lambda ()
		      (signal (make <unterminated-long-comment>
				    source: self
				    position: start)))))
    (main-text)))

;;;

(define (make-sharp-table (port-class <<class>>))
  (let ((t (make-char-table)))
    ;;
    (insert-all! t scan-char-spec port-class '#(#\\))
    (insert-all! t scan-open-vector port-class '#(#\())
    (insert-all! t scan-long-comment port-class '#(#\|))
    (insert-all! t scan-id-sharp port-class (key-sequence *id-initial*))
    ;;
    t))
