
(define (no-home-page-handler (who <person>) path query data)
  (html
   (title (format #t "~a's Home Page" (full-name who)))
   (header-1 (format #t "~a's Home Page" (full-name who)))
   (display (full-name who))
   (display " hasn't prepared a more interesting home page\n")
   (display "for you to enjoy, so this default is being provided.\n")
   (par)
   (format #t "I will send mail to ~a and tell ~a you're interested.\n"
	   (first-name who)
	   (if (eq? (gender who) 'female)
	       "her"
	       "him"))
   (let ((p (open-output-process
	     (format #f "mail -s \"interest in ~s\" donovan@tkg.com"
		     (user-id who)))))
     (format p "There is some interest in ~a.\n" (full-name who))
     (with-output-to-port p
       (lambda ()
	 (display "    Query:\n" p)
	 (for-each (lambda (qi)
		     (format #t "\t~a: ~s\n" (car qi) (cdr qi)))
		   query)
	 (if data
	     (begin
	       (display "\n    Data:\n")
	       (print data))
	     (display "\n    <No Data>\n"))))
     (close-output-port p))
   (par)))

(define (directory-assistance-handler who path query data)
  (html
   (title (display "Directory Assistance"))
   (header-1 (display "Directory Assistance"))
   (format #t "The user `~a' is not recognized.  " who)
   (display "For help in finding the person you are trying to reach, ")
   (display "contact ")
   (mail-to "Postmaster@TKG.com" "postmaster@tkg.com")
   (par)
   (header-2 (display "Did you mean, perhaps?"))
   (unnumbered-list
    (for-each (lambda (alt)
		(list-item 
		 (output-port-control (current-output-port)
				      'ref
				      (string-append
				       "/~"
				       (user-id alt)
				       "/"
				       (string-join #\/ path)))
		 (display (full-name alt))
		 (output-port-control (current-output-port) 'ref #f)))
	      *people*))))
