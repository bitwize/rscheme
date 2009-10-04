
(define *scandir-cache* (make-table string=? string->hash))

(define (cached-cmvc-scandir (release <string>) (dir <directory-name>))
  (let* ((key (string-append release
			     " "
			     (pathname->string dir)))
	 (t (table-lookup *scandir-cache* key)))
    (if (and t (time<? (time) (car t)))
	(cdr t)
	(let ((data (cmvc-scandir release dir)))
	  (table-insert! *scandir-cache* 
			 key
			 (cons (time+interval (time)
					      (seconds->interval 3600))
			       data))
	  data))))

;;

(define (scandir-cmd (release <string>)
		     (dir <directory-name>))
  (let ((d (dir->string dir "/" root-name)))
    (if (string=? d "/")
	(format #f 
		"Report -view FileView -where \"releaseName = '~a'\" -raw"
		release
		d)
	(format #f 
		"Report -view FileView -where \"releaseName = '~a' and pathName like '~a%'\" -raw"
		release
		d))))

(define (cmvc-scandir (release <string>)
		      (dir <directory-name>))
  (console "(cmvc-scandir release: ~s dir: ~s)\n" release dir)
  (let* ((cmd (scandir-cmd release dir))
	 (p (open-input-process cmd)))
    (console "(cmd: ~s)" cmd)
    ;(write cmd) (newline)
    (if p
	(let ((items (read-cmvc-dir-report p dir)))
	  (if (close-input-port-ok? p)
	      items
	      #f))
	(internal-error
	 "Could not launch Report process: ~a\n"
	 (errorstr (errno))))))
    
(define (read-cmvc-dir-report port dir-path)
  (let ((n (length (steps dir-path))))
    (console "n = ~s (~s)\n" n (steps dir-path))
    (let loop ((lst '()))
      (let ((line (read-line port)))
	(if (eof-object? line)
	    lst
	    (let ((parts (list->vector (string-split line #\|))))
	      (let ((path (string->file (vector-ref parts 7)))
		    (component (vector-ref parts 2))
		    (vers (vector-ref parts 3)))
		(console "~-10a ~s\n" component path)
		(let ((t (list-tail (if (file-directory path)
					(steps (file-directory path))
					'())
				    n)))
		  (if (null? t)
		      ;; it's a file in the same directory, so return it
		      (loop (cons (cons (file-within-dir path) parts)
				  lst))
		      ;; it's a file in a subdirectory.. see if we've
		      ;; seen it already
		      (let* ((subdir-name (car t))
			     (a (assoc subdir-name lst)))
			(if a
			    (begin
			      ;; increment the N-SUB-FILES counter
			      (set-car! (cdr a) (+ (cadr a) 1))
			      ;; increment the N-DIRECT-SUB-FILES counter
			      ;; if appropriate
			      (if (null? (cdr t))
				  (set-car! (cddr a) (+ (caddr a) 1)))
			      (loop lst))
			    (loop (cons (list subdir-name 1 1) lst)))))))))))))
