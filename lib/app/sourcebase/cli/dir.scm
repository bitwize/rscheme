
(define $dir-subcommand
  (let-syntax ((cmd (syntax-form (name proc)
		      (cons (mquote name)
			    (lambda args
			      (apply proc args))))))
    (list (cmd unlock handle-unlock-request)
	  (cmd lock handle-d-lock-request)
	  (cmd create handle-mkdir-request)
	  (cmd inspect handle-inspect-request)
	  (cmd link handle-dlink-request)
	  (cmd delete handle-dunlink-request)
	  (cmd checkin handle-d-checkin-request))))

(define (handle-dir-request args req inp out (u <user>))
  (bind ((pwd (get-pwd-arg req))
	 (proc sub-args (parse-subcommand req $dir-subcommand 'dir)))
    (proc (map (lambda (f)
		 (fs-append-path pwd (string->fs-path f)))
	       (append args sub-args))
	  req inp out u)))

(define (handle-dunlink-request args req inp out u)
  (let ((fs (get-fspace-arg req))
	(verbose? (assq 'verbose req))
	(reasons (get-reasons-arg req)))
    (for-each
     (lambda (p)
       (unlink-dir fs p reasons)
       (if verbose?
	   (client-print-message out
				 (format #f "~a removed\n"
					 (fs-path->string p)))))
     args)))

(define (handle-dlink-request args req inp out u)
  (let ((fs (get-fsys-arg req))
	(to (map (let ((pwd (get-pwd-arg req)))
		   (lambda (f)
		     (fs-append-path pwd (string->fs-path f))))
		 (cdr (assq 'to req))))
	(reas (get-reasons-arg req))
	(tofs (string->filesystem (get-exactly-one req 'to-filespace)))
	(rem (get-remarks-arg req inp out)))
    ;;
    (if (not (eq? (length args) (length to)))
	(service-error 761 "not equal number of <dir1>'s and <dir2>'s"))
    ;;
    (for-each (lambda (from-dir to-dir)
		(cross-link-node-rec fs
				     from-dir
				     tofs
				     to-dir
				     reas
				     rem))
	      args
	      to)))

(define (handle-d-lock-request args req inp out u)
  (let ((fs (get-fsys-arg req))
	(verbose? (assq 'verbose req)))
    (for-each
     (lambda (p)
       (node-lock fs p u)
       (if verbose?
	   (client-print-message out
				 (format #f "~a locked\n"
					   (fs-path->string p)))))
     args)))

(define (handle-d-checkin-request args req inp out u)
  (let ((fs (get-fsys-arg req))
	(reasons (get-reasons-arg req))
	(comment (get-remarks-arg req inp out)))
    (for-each (lambda (p)
		(directory-delta fs p u reasons comment))
	      args)))

(define (handle-mkdir-request args req inp out u)
  (let ((fs (get-fsys-arg req))
	(g (get-group-arg req))
	(reasons (get-reasons-arg req)))
    (for-each (lambda (p)
		(make-directory fs p u g reasons))
	      args)))
