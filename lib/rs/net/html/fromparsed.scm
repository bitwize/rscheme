;;;
;;;  write out HTML that is from parsed HTML
;;;  (as from rs.net.html.parse's `parse-html')

(define (display-parsed-html html)
  (for-each display-parsed-html* html))

(define (display-parsed-html* html)
  (cond
   ((string? html)
    (display html))
   ((symbol? html)
    ;; entities are not really implemented yet, but give it a shot
    (format (underlying-output-port (current-output-port))
	    "&~a;"
	    html))
   ((pair? html)
    (with-tag (car html) 
	      (cadr html)
	      (lambda ()
		(display-parsed-html (cddr html)))))
   (else
    (write html))))
