
(define (string-upcase str)
  (list->string (map char-upcase (string->list str))))

(define (string-downcase str)
  (list->string (map char-downcase (string->list str))))

(define-class <html-input-port> (<input-port>)
  underlying-input-port)

(define-class <html-output-port> (<output-port>)
  underlying-output-port)

(define-method output-port-write-char ((self <html-output-port>) ch)
  (if (memq ch '(#\< #\> #\&))
      (display (cdr (assq ch '((#\< . "&lt;")
			       (#\> . "&gt;")
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

(define-method output-port-control ((self <html-output-port>) attrib value)
  (let ((xl (assq attrib '((bold "b") 
			   (emphasis "em")
			   (header-1 "H1")
			   (header-2 "H2")
			   (paragraph "p")
			   (horz-rule "hr")
			   (unnumbered-list "UL")
			   (numbered-list "NL")
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
	  ((input-field)
	   (if (null? (cddr value))
	       (format (underlying-output-port self)
		       "<input name=\"~a\" type=\"~a\">"
		       (car value)
		       (cadr value))
	       (format (underlying-output-port self)
		       "<input name=\"~a\" type=\"~a\" length=~d>"
		       (car value)
		       (cadr value)
		       (caddr value))))
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


(define-syntax (unnumbered-list . items)
  (output-port-control (current-output-port) 'unnumbered-list #t)
  (begin . items)
  (output-port-control (current-output-port) 'unnumbered-list #f)
  (newline))

(define-syntax (numbered-list . items)
  (output-port-control (current-output-port) 'numbered-list #t)
  (begin . items)
  (output-port-control (current-output-port) 'numbered-list #f)
  (newline))

(define-syntax (header-1 . items)
  (output-port-control (current-output-port) 'header-1 #t)
  (begin . items)
  (output-port-control (current-output-port) 'header-1 #f)
  (newline))

(define-syntax (header-2 . items)
  (output-port-control (current-output-port) 'header-2 #t)
  (begin . items)
  (output-port-control (current-output-port) 'header-2 #f)
  (newline))

(define-syntax (title . items)
  (output-port-control (current-output-port) 'title #t)
  (begin . items)
  (output-port-control (current-output-port) 'title #f)
  (newline))

(define-syntax (bold . items)
  (output-port-control (current-output-port) 'bold #t)
  (begin . items)
  (output-port-control (current-output-port) 'bold #f))

(define-syntax (code . items)
  (output-port-control (current-output-port) 'code #t)
  (begin . items)
  (output-port-control (current-output-port) 'code #f))

(define-syntax (preformatted . items)
  (output-port-control (current-output-port) 'preformatted #t)
  (begin . items)
  (output-port-control (current-output-port) 'preformatted #f))

(define-syntax (list-item . items)
  (output-port-control (current-output-port) 'list-item #t)
  (begin . items)
  (newline))

(define-syntax (html . things)
  (with-output-to-port
      (make <html-output-port>
	    underlying-output-port: (current-output-port))
    (lambda ()
      (output-port-control (current-output-port) 'html #t)
      (begin . things)
      (output-port-control (current-output-port) 'html #f)))
  (newline)
  'text/html)

(define-syntax (html-body . things)
  (output-port-control (current-output-port) 'body #t)
  (begin . things)
  (output-port-control (current-output-port) 'body #f))

(define-syntax (html-header . things)
  (output-port-control (current-output-port) 'header #t)
  (begin . things)
  (output-port-control (current-output-port) 'header #f))

(define (par)
  (output-port-control (current-output-port) 'paragraph #t)
  (newline))

(define (horz-rule)
  (output-port-control (current-output-port) 'horz-rule #t)
  (newline))

(define-syntax (hyperlink (place) . things)
  (output-port-control (current-output-port)
		       'ref
		       place)
  (begin . things)
  (output-port-control (current-output-port)
		       'ref
		       #f))

(define-syntax (input-form (method action) . body)
  (output-port-control (current-output-port) 'input-form (list (mquote method)
							 action))
  (begin . body)
  (output-port-control (current-output-port) 'input-form #f))

(define-syntax (input-field name length)
  (output-port-control (current-output-port)
		       'input-field
		       (list (mquote name) 'text length)))

