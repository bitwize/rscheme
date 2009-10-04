
(define-class <html-input-port> (<input-port>)
  underlying-input-port)

(define-class <html-output-port> (<output-port>)
  underlying-output-port
  (tag-stack init-value: '())
  (properties init-value: '#()))

(define (format-html fmt . args)
  (let ((u (underlying-output-port (current-output-port))))
    (apply format u fmt args)))

(define-method output-port-write-char ((self <html-output-port>) ch)
  (if (memq ch '(#\< #\> #\&))
      (display (cdr (assq ch '((#\< . "&lt;")
			       (#\> . "&gt;")
			       (#\M-space . "&nbsp;") ;; #\u(NO-BREAK SPACE)
			       (#\& . "&amp;"))))
	       (underlying-output-port self))
      (output-port-write-char (underlying-output-port self) ch)))

(define (with-output-to-html-string thunk)
  (let ((s (open-output-string)))
    (with-output-to-port
	(make <html-output-port>
	      underlying-output-port: s)
      thunk)
    (close-output-port s)))

(define-method escape-html ((self <string>))
  (let* ((s (open-output-string))
         (h (make <html-output-port>
                  underlying-output-port: s)))
    (write-string h self)
    (close-output-port s)))

(define-method output-port-control ((self <html-output-port>) attrib value)
  (let ((xl (assq attrib '((bold "b") 
			   (emphasis "em")
			   (header-1 "H1")
			   (header-2 "H2")
			   (paragraph "p")
			   (horz-rule "hr")
			   (line-break "br")
			   (unnumbered-list "UL")
			   (numbered-list "OL")
			   (list-item "LI")
			   (header-3 "H3")
			   (header-4 "H4")
			   (header-5 "H5")
			   (header-6 "H6")
			   (code "code")
			   (preformatted "pre")
			   (address "ADDRESS")
			   (title "TITLE")
			   (html "HTML")
			   (header "HEAD")
			   (body "BODY")))))
    (if xl
	(format (underlying-output-port self)
		"<~a~a>"
		(if value "" "/")
		(cadr xl))
	(case attrib
	  ((ref)
	   (if value
	       (format (underlying-output-port self)
		       "<A HREF=~s>"
		       value)
	       (display "</A>" (underlying-output-port self))))
	  ((input-form)
	   (if value
	       (format (underlying-output-port self)
		       "<form method=\"~a\" action=~s>"
		       (car value)
		       (cadr value))
	       (display "</form>" (underlying-output-port self))))
	  ((option)
	   (format (underlying-output-port self) "<option")
	   (if (car value)
	       (format (underlying-output-port self) " value=\"~a\"" 
		       (car value)))
	   (if (cadr value)
	       (format (underlying-output-port self) " selected"))
	   (format (underlying-output-port self) ">"))
	  ((select)
	   (if value
	       (format (underlying-output-port self)
		       "<select name=\"~a\"~a>" 
		       (car value)
		       (if (pair? (cdr value))
			   (format #f " size=~d" (cadr value))
			   ""))
	       (format (underlying-output-port self) "</select>")))
	  ((input-field)
	   (format (underlying-output-port self)
		   "<input name=\"~a\" type=\"~a\""
		   (car value)
		   (cadr value))
	   (if (memq 'length: (cddr value))
	       (format (underlying-output-port self)
		       " size=~d" (cadr (memq 'length: (cddr value)))))
	   (if (memq 'default: (cddr value))
	       (format (underlying-output-port self)
		       " value=~s" (cadr (memq 'default: (cddr value)))))
	   (output-port-write-char (underlying-output-port self) #\>))
	  (else
	   (error "output-port-control: attrib ~s invalid" attrib))))))


;; uses <CR><LF> as the delimiter
;; (can also optimize to grab lines wholesale out of buffer,
;;  but do that later)

(define-method input-port-read-char ((self <html-input-port>))
  (input-port-read-char (underlying-input-port self)))

(define-method input-port-peek-char ((self <html-input-port>))
  (input-port-peek-char (underlying-input-port self)))

(define-method input-port-read-line ((self <html-input-port>))
  (let loop ((r '()))
    (let ((ch (read-char self)))
      (if (eof-object? ch)
	  (if (null? r)
	      ch
	      (list->string (reverse r)))
	  (if (eq? ch (integer->char 13))
	      (let ((ch (read-char self)))
		(if (eq? ch (integer->char 10))
		    (list->string (reverse r))
		    (error "low-level error: <CR> not followed by <LF>\n")))
	      (if (eq? ch (integer->char 10))
		  ;; assume it's just \n delimited, then
		  (list->string (reverse r))
		  (loop (cons ch r))))))))


(define (skip-whitespaces (str <string>) (i <fixnum>))
  (let loop ((i i))
    (if (and (< i (string-length str))
	     (char-whitespace? (string-ref str i)))
	(loop (+ i 1))
	i)))
