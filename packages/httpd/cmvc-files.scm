
(define (file-responder remains query data)
  (console "file-responder: (remains: ~s)\n" remains)
  (if (< (length remains) 3)
      (huh? remains)
      (if (member (car remains) '("view" "get" "lock"))
	  (let ((act (string->symbol (car remains))))
	    (if (and (eq? act 'view)
		     (string=? (last remains) ""))
		(set! act 'list))
	    (file-action-responder act
				   (cadr remains) ;; release
				   (string-join #\/ (cddr remains))
				   query))
	  (huh? remains))))

(define (file-action-responder action release file-path query)
  (console "file-action (~s) release ~s, path ~s\n" action release file-path)
  (fluid-let ((*login* (check-cmvc-authority query 'file action))
	      (*cmvc-release* release))
    (case action
      ((view) (file-view-responder release file-path))
      ((list) (file-list-responder release file-path))
      ((lock) (file-lock-responder release file-path))
      ((get) (file-get-responder release file-path))
      (else (error "internal error: invalid file action: ~s" action)))))

(define (file-get-responder release path)
  (let ((p (open-input-process 
	    (format #f "File -extract ~s -release ~s -stdout" path release))))
    (copy-lines p (current-output-port))
    (if (not (close-input-port-ok? p))
	(cmvc-access-error
	 (lambda ()
	   (format #t 
		   "File ~s doesn't exist or you aren't authorized to get it"
		   path))))
    'text/plain))

(define (file-lock-responder release path)
  (if (eq? 0 (system (format #f "File -lock ~s -release ~s" path release)))
      (let ((p (open-input-process 
		(format #f "File -extract ~s -release ~s -stdout -nokeys" 
			path 
			release))))
	(copy-lines p (current-output-port))
	'text/plain)
      (cmvc-access-error
       (lambda ()
	 (display "File ")
	 (bold
	  (format #t "~s" path))
	 (display " does not exist, or you do not ")
	 (display "have permission to lock it.")))))

(define (file-view-responder release path)
  (html
   (title (display "CMVC File Query"))
   (header-1 (display "CMVC File Query"))
   (let* ((cmd (format #f "File -view ~s -release ~s -long" path release))
	  (p (open-input-process cmd)))
     (par)
     (display "Command: ")
     (code (display cmd))
     (newline)
     (if (not p)
	 (internal-error
	  "Could not launch File process: ~a\n"
	  (errorstr (errno))))
     ;;
     (par)
     (hyperlink ((format #f "/cmvc/~a/file/get/~a/~a" 
			 *cmvc-family* 
			 release 
			 path))
		(format #t "[text of this file]"))
     (par)
     (hyperlink ((format #f "/cmvc/~a/file/lock/~a/~a"
			 *cmvc-family* 
			 release
			 path))
		(format #t "[locked version of this file]"))
     (par)
     (horz-rule)
     ;;
     (preformatted
      (copy-lines p (current-output-port)))
     (if (not (close-input-port-ok? p))
	 (cmvc-access-error
	  (lambda ()
	    (display "File ")
	    (bold
	     (format #t "~a" path))
	    (display " does not exist, or you do not ")
	    (display "have permission to view it")))))))

(define (file-list-responder release path)
  (let ((dir (string->dir path)))
     (let ((elems (cached-cmvc-scandir release dir)))
       (with-output-to-port *console-output-port*
	 (lambda ()
	   (format #t "scandir result:\n")
	   (print elems)))
       (if (not elems)
	   (internal-error
	    "Could not scan cmvc directory (err ~a)\n"
	    (errorstr (errno)))
	   (scanned-dir->html elems release dir)))))

(define (scanned-dir->html entries release dir)
  (html
   (title (display "CMVC Directory Query"))
   (header-1 (display "CMVC Directory Query"))
   ;;
   (format #t "~d entries in " (length entries))
   (code (display dir))
   (write-char #\.)
   (par)
   ;;
   (if (not (null? (steps dir)))
       (begin
	 (par)
	 (hyperlink ((format #f "/cmvc/~a/file/view/~a/~a"
			     *cmvc-family* 
			     release
			     (append-dirs dir (string->dir ".."))))
		      (format #t "[parent directory]"))))
   (par)
   (horz-rule)
   ;;
   (let ((max-len (apply max (map string-length (map car entries)))))
     (preformatted
      (for-each 
       (lambda (entry)
	 (let ((name (car entry))
	       (regular? (vector? (cdr entry))))
	   (hyperlink ((if regular?
			   name
			   (string-append name "/")))
		      (display name)
		      (if (not regular?)
			  (write-char #\/)))
	   (display (make-string (+ 5 (- max-len 
					 (string-length name)
					 (if regular? 0 1)))
				 #\space))
	   (if regular?
	       (show-file-info entry)
	       (show-dir-info entry))
	   (newline)))
       (sort entries
	     (lambda (a b)
	       (string<? (car a) (car b)))))))))

(define (show-file-info entry)
  (let ((vec (cdr entry)))
    (let ((component (vector-ref vec 2))
	  (vers (vector-ref vec 8))
	  (locked (vector-ref vec 12)))
      (format #t "~10a (version ~a)" component vers)
      (if (not (string=? locked ""))
	  (begin
	    (display "  ")
	    (bold (format #t "locked by ~a" locked)))))))

(define (show-dir-info entry)
  (let ((num-files-rec (cadr entry))
	(num-files (caddr entry)))
    (format #t "DIR        (~d files, ~d recursively)" 
	    num-files
	    num-files-rec)))
