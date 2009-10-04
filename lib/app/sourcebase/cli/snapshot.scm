(define $snap-subcommand
  (let-syntax ((cmd (syntax-form (name proc)
		      (cons (mquote name)
			    (lambda args
			      (apply proc args))))))
    (list (cmd create handle-snap-create-request)
	  (cmd extend handle-snap-extend-request)
	  (cmd inspect handle-snap-inspect-request)
	  (cmd commitdate handle-snap-commitdate-request)
	  (cmd commit handle-snap-commit-request))))

(define (handle-snap-request args req inp out (u <user>))
  (bind ((fs (get-fsys-arg req))
	 (proc sub-args (parse-subcommand req $snap-subcommand 'snap)))
    (proc (map (lambda (snap)
		 (or (table-lookup (snapshot-table fs) snap)
		     (service-error 702 "~a: not a snapshot in filesys ~a"
				    snap
				    (name fs))))
	       args)
	  sub-args
	  req
	  inp
	  out
	  u)))

(define (handle-snap-inspect-request args sub-args req inp out u)
  (let ((ren (rendition-proc <snapshot> req render-full)))
    (for-each
     (lambda (s)
       (client-print-message
	out
	(with-output-to-string
	  (lambda ()
	    (ren s)))))
     args)))

(define (handle-snap-commit-request args sub-args req inp out u)
  (for-each snapshot-commit args))

(define (handle-snap-extend-request args sub-args req inp out u)
  (if (pair? sub-args)
      (service-error 701 "unexpected arguments to `--snap --extend'")
      (let ((crs (get-reasons-arg req)))
	(for-each
	 (lambda ((s <snapshot>))
	   (snapshot-extend s crs))
	 args))))

(define (handle-snap-create-request args sub-args req inp out u)
  (if (pair? args)
      (service-error 701 "unexpected arguments to `--snap --create'")
      (if (assq 'current req)
	  (make-current-snapshot (car sub-args) 
				 (get-fsys-arg req)
				 (list (cons 'state 'active)))
	  (if (assq 'based req)
	      (let* ((a (cadr (assq 'based req)))
		     (fs (get-fsys-arg req))
		     (basis (or (table-lookup (snapshot-table fs) a)
				(service-error 
				 702 
				 "~a: not a snapshot of filesystem ~a"
				 a (name fs)))))
		(make-based-snapshot 
		 (car sub-args)
		 fs
		 (list (cons 'state 'active)
		       (cons 'extend '())
		       (cons 'basis basis))
		 basis))
	      (if (assq 'empty req)
		  (let* ((fs (get-fsys-arg req)))
		    (make-empty-snapshot 
		     (car sub-args)
		     fs
		     (list (cons 'state 'active)
			   (cons 'extend '()))))
		  (service-error
		   703
		   "`--snap --create' missing subcommand `--current',\n`--based', or `--empty'"))))))

(define (handle-snap-commitdate-request args sub-args req inp out (u <user>))
  (for-each 
   (lambda ((s <snapshot>))
     (let ((c (assq 'committed (properties s))))
       (if c
	   (client-print-message
	    out
	    (time->string (timestamp (cdr c)) "%Y-%m-%d %H:%M:%S\n"))
	   (service-error 708 "snapshot ~s not committed" s))))
   args))
