
;; we've seen "#\" so far

(define *char-names* (make-table string-ci=? string-ci->hash))

(table-insert! *char-names* "cr" #\cr)
(table-insert! *char-names* "lf" #\newline)
(table-insert! *char-names* "newline" #\newline)
(table-insert! *char-names* "tab" #\tab)
(table-insert! *char-names* "space" #\space)
(table-insert! *char-names* "ff" #\ff)

(define-method scan-char-spec ((self <input-port>)
			       (chslash <char>)
			       (start <text-location>))
  (let ((ch (input-port-read-char self)))
    (if (table-lookup *id-initial* ch)
	(let ((ch-chars (collect-tok self *id-continued* (vector ch)))
	      (len (location- (location self) start)))
	  (make <char-token>
		location: start
		lexeme-length: len
		data: (char-spec->data self start ch-chars)))
	;; "#\" isn't followed by an identifier-like thing, so it
	;; must be something special like "#\."
	(make <char-token>
	      location: start
	      lexeme-length: 3
	      data: ch))))

(define (char-spec->data port start (ch-chars <vector>))
  (if (eq? (vector-length ch-chars) 1)
      ;; if we only collected one char, then it's something
      ;; of the form "#\X"
      (vector-ref ch-chars 0)
      (let ((ch-name (vector->string ch-chars)))
	(if (and (char-ci=? (string-ref ch-name 0) #\x)
		 (string->number (substring ch-name 1) 16))
	    ;; if it starts with an x and the rest of the string is a valid
	    ;; hex number, then it is something of the form "#\x41"
	    (let ((code (string->number (substring ch-name 1) 16)))
	      (if (and (>= code 0) (< code 65536))
		  (integer->char code)
		  (signal (make <char-literal-out-of-range>
				source: port
				position: start
				char-code: code))))
	    ;; otherwise, it's presumable a character name of some sort
	    (let ((char (table-lookup *char-names* ch-name)))
		(if char
		    char
		    (signal (make <invalid-char-name>
				  char-name: ch-name
				  source: port
				  position: start))))))))
		    
