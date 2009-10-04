(define (filtered-html-page path (pathname <file-name>))
  (console "filtering page: ~s\n" path)
  (let ((in (open-input-process 
	     (format #f "cd /u/donovan/net/www_clone ; pagefilter -n ~a < ~a"
		     (make <file-name>
			   file-directory: (file-directory pathname)
			   filename: (filename pathname)
			   extension: #f)
		     pathname))))
    (let loop ()
      (let ((l (read-line in)))
	(if (eof-object? l)
	    (begin
	      (close-input-port in)
	      'text/html)
	    (begin
	      (write-string (current-output-port) l)
	      (newline)
	      (loop)))))))
