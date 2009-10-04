
(define (real-html-page-dispatch path)
  (console "real-page: ~s\n" path)
  (let* ((pathn (string->file (string-join #\/ path)))
	 (type (assoc (extension pathn)
		     '(("gif" image/gif)
		       ("jpg" image/jpeg)
		       ("html" text/html)))))
    (if type
	(let ((sub (append-path *root-path* pathn))
	      (t (cadr type)))
	  (console "Checking subfile of [root]: ~a\nfull path: ~a\n"
		   (string-join "/" path) 
		   sub)
	  (let ((s (stat (pathname->os-path sub))))
	    (if s
		(begin
		  (console " File exists (~d bytes), will return type ~s\n"
			   (stat-size s)
			   t)
		  (case t
		    ((text/html)
		     (console " ** will run through pagefilter **\n")
		     (filtered-html-page path pathn))
		    ((image/gif image/jpeg)
		     (display (file->string (pathname->os-path sub)))
		     t)))
		(error/url-not-found *query*))))
	(error/url-not-found *query*))))
