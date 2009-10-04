(define (rnote-responder remains query data)
  (set-env "RNOTE_DIR" "/cmvc/tmp/docs")
  (export-environ)
  (if (pair? remains)
      (cond
       ((string=? (car remains) "query")
	(let ((data (parse-POST-args data)))
	  (let ((a (assq 'query data)))
	    (log-entry "Query: ~s\n" (cdr a))
	    (let ((p (open-input-process 
		      (format #f "rsh argent RNOTE_DIR=/cmvc/tmp/docs bin/Rnote -query '~a' -html" (cdr a)))))
	      (log-entry "reply is ~d lines\n"
			 (copy-lines p (current-output-port)))
	      (if (not (close-input-port-ok? p))
		  (error "bummer"))
	      'text/html))))
       ((string->number (car remains))
	(log-entry "View: ~s\n" (car remains))
	(let ((p (open-input-process 
		  (format #f "/u/donovan/bin/Rnote -view ~d -html" 
			  (string->number (car remains))))))
	  (copy-lines p (current-output-port))
	  (if (not (close-input-port-ok? p))
	      (error "bummer"))
	  'text/html))
       (else
	(error "bummer2")))
      (error "bummer3")))
