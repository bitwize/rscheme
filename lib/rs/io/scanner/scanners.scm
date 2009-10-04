(define (scan-open-paren port (ch <char>) (start <text-location>))
  (make <open-paren-token>
	location: start))

(define (scan-close-paren port (ch <char>) (start <text-location>))
  (make <close-paren-token>
	location: start))

(define (scan-open-sqbracket port (ch <char>) (start <text-location>))
  (make <open-sqbracket-token>
	location: start))

(define (scan-close-sqbracket port (ch <char>) (start <text-location>))
  (make <close-sqbracket-token>
	location: start))

(define (scan-backquote port (ch <char>) (start <text-location>))
  (make <backquote-token>
	location: start))

(define (scan-quote port (ch <char>) (start <text-location>))
  (make <quote-token>
	location: start))

(define (scan-unquote port (ch <char>) (start <text-location>))
  (if (eq? (input-port-peek-char port) #\@)
      (let ((at (input-port-read-char port)))
	(make <unquote-splicing-token>
	      location: start
	      lexeme-length: 2))
      (make <unquote-token>
	    location: start)))

(define-method scan-identifier ((self <input-port>) 
				(ch <char>) 
				(start <text-location>))
  (bind ((id (collect-tok self *id-continued* (vector ch)))
	 (str (list->string (vector->list id))))
    (if (eq? (input-port-peek-char self) #\:)
	(begin
	  (input-port-read-char self)
	  (make <keyword-token>
		data: (string->keyword str)
		location: start
		lexeme-length: (location- (location self) start)))
	(make <identifier-token>
	      data: (string->symbol str)
	      location: start
  	      lexeme-length: (location- (location self) start)))))

(define-method scan-flag ((self <input-port>)
			  (ch <char>)
			  (start <text-location>))
  (bind ((id (collect-tok self *id-continued* '#()))
	 (str (list->string (vector->list id))))
    (make <flag-token>
	  data: (string->flag str)
	  location: start
	  lexeme-length: (location- (location self) start))))

(define-method scan-whitespace ((self <input-port>)
				(ch <char>)
				(start <text-location>))
  (let ((spc (collect-tok self *whitespace* '#())))
    (scan self)))

(define-method scan-line-comment ((self <input-port>)
				  (ch <char>)
				  (start <text-location>))
  (read-line self)
  (scan self))
