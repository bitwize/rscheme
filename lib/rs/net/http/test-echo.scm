#!/usr/local/bin/rs -script

,(use html)

(define (get-cgi-body)
  (let ((len (string->number (getenv "CONTENT_LENGTH"))))
    (read-string (current-input-port) len)))

(define (main args)
  (display "Content-type: text/html\n\n")
  (let ((f (parse-post-content (get-cgi-body))))
    (html
     (unnumbered-list
	(for-each
	  (lambda (k)
	(list-item (display (car k))
	  (display " --> ")
	  (code (display (cdr k))))
	(newline))
	 f)))))

