(define $file-subcommand
  (let-syntax ((cmd (syntax-form (name proc)
		      (cons (mquote name)
			    (lambda args
			      (apply proc args))))))
    (list (cmd unlock handle-unlock-request)
	  (cmd checkin handle-ci-request)
	  (cmd inspect handle-inspect-request)
	  (cmd checkout handle-co-request)
	  (cmd tar handle-tar-request)
	  (cmd create handle-fcreate-request)
	  (cmd delete handle-f-rm-request)
	  (cmd rename handle-f-mv-request)
	  ;(cmd modify handle-f-ch-request)
	  (cmd diverge handle-f-diverge-request)
	  (cmd link handle-flink-request))))

(define (absolute-paths req paths)
  (let ((pwd (get-pwd-arg req)))
    (map (lambda (f)
	   (fs-append-path pwd (string->fs-path f)))
	 paths)))

(define (handle-file-request args req inp out (u <user>))
  (bind ((pwd (get-pwd-arg req))
	 (proc sub-args (parse-subcommand req $file-subcommand 'file)))
    (proc (absolute-paths req (append args sub-args))
	  req
	  inp
	  out
	  u)))

(define (handle-inspect-request args req inp out (u <user>))
  (let* ((fs (get-fspace-arg req)))
    (for-each
     (lambda ((p <fs-absolute-path>))
       (client-print-message
	out
	(with-output-to-string
	  (lambda ()
	    (render-full (find-version fs p) fs)))))
     args)))

(define (handle-unlock-request args req inp out (u <user>))
  (let* ((fs (get-fsys-arg req))
	 (verbose? (assq 'verbose req)))
    (for-each
     (lambda (p)
       (let ((cv (node-unlock fs p u)))
	 (client-chmod out
		       (fs-path->string p)
		       (bitwise-and (permissions cv) #o555))
	 (if verbose?
	     (client-print-message out
				   (format #f "~a unlocked\n"
					   (fs-path->string p))))))
     args)))

(define (handle-ci-request args req inp out (u <user>))
  (let* ((rem (get-remarks-arg req inp out))
	 (fs (get-fsys-arg req))
	 (reasons (get-reasons-arg req))
	 (timestamp (let ((a (assq 'timestamp req)))
		      (if a
			  (string->time (get-exactly-one req 'timestamp))
			  #f)))
	 (from-stdin? (assq 'stdin req))
	 (diverge? (and (assq 'diverge req) #t))
	 (other-fss (let ((a (assq 'other-fs req)))
		      (if a
			  (map string->filesystem (cdr a))
			  '())))
	 (verbose? (and (assq 'verbose req) #t)))
    (for-each
     (lambda (p)
	 (bind ((new-text stat (if from-stdin?
				   (values (client-snarf-stdin inp out)
					   #o644)
				   (client-upload inp
						  out 
						  (fs-path->string p))))
		(v (find-version* fs p))
		(v2 #f))
	   (format #t "~s\n" p)
	   (format #t "new text: ~d lines / ~d chars, mode #o~o\n"
		   (length (string-split new-text #\newline))
		   (string-length new-text)
		   stat)
	   (if v
	       (set! v2 (file-delta fs ;; filesystem
				    p  ;; path
				    u  ;; user
				    (string->shared-content 
				     new-text
				     (contents v))
				    reasons
				    rem
				    timestamp
				    other-fss
				    diverge?))
	       (set! v2 (current-version
			 (make-file fs
				    p
				    u
				    (get-group-arg req)
				    (string->content new-text)
				    reasons
				    rem
				    timestamp))))
	   ;;
	   ;; a huge violation of the API protocol, modifying a DB
	   ;; object outside the scope of the API layer...
	   ;;
	   (set-permissions! v2 stat)
	   ;;
	   (if (not from-stdin?)
	       (client-chmod out
			     (fs-path->string p)
			     (bitwise-and stat #o555)))
	   (if verbose?
	       (client-print-message out
				     (format #f "~a: ~d bytes\n"
					     (fs-path->string p)
					     (string-length new-text))))))
     args)))
	 
(define (handle-co-request args req inp out (u <user>))
  (let* ((lock? (assq 'lock req))
	 (fs (get-fspace-arg req))
	 (verbose? (assq 'verbose req))
	 (to-stdout? (assq 'stdout req))
	 (vers (and (assq 'version req)
		    (get-exactly-one req 'version)))
	 (extra-keys (extra-keys-proc fs req))
	 (do-expand? (if lock?
			  #f
			  (if (assq 'nokeys req)
			      #f
			      #t))))
    (for-each
     (lambda (p)
       (if verbose?
	   (client-print-message out 
				 (format #f "file: ~a\n" 
					 (fs-path->string p))))
       (let* ((v (if lock?
		     (node-lock fs p u)
		     (find-version fs p)))
	      (v (if vers
		     (value
		      (string->leaf (versions (versioned-object v)) vers))
		     v))
	      (txt (if do-expand?
		       (expanded-file-content fs p v 
					      (extra-keys p v))
		       (content->string (contents v)))))
	 (if to-stdout?
	     (client-print-message out txt)
	     (client-download inp 
			      out
			      (fs-path->string p) 
			      (if lock?
				  (permissions v)
				  (bitwise-and (permissions v) #o555))
			      txt))))
     args)))

(define (handle-tar-request args req inp out (u <user>))
  (let ((tmpf (build-tar-file args req u))
	(to-stdout? (assq 'stdout req)))
    (let ((txt (file->string tmpf)))
      (format #t "sending ~d bytes...\n" (string-length txt))
      (if to-stdout?
	  (client-print-message out txt)
	  (client-download-tar out txt)))))

(define (build-tar-file args req (u <user>))
  (let* ((fs (get-fspace-arg req))
	 (do-expand? (not (assq 'nokeys req)))
	 (restrict-mode? (not (assq 'keep-mode req)))
	 (tmpf (format #f "/tmp/temp.~d.tar.gz" (random)))
	 (tmpp (open-output-process (string-append "gzip -c > " tmpf)))
	 (tarf (open-blocked-port tmpp 512))
	 (snap? (instance? fs <snapshot>))
	 (extra-keys (extra-keys-proc fs req))
	 (fsys (if snap? (versioned-object fs) fs))
	 (default-uid (id (owner fsys))))
    ;;
    (define (archive p)
       (let ((v (find-version fs p)))
	 (format #t "a ~a: ~s\n" (fs-path->string p) v)
	 (if (instance? v <file-version>)
	     (let ((txt (if do-expand?
			    (let ((xtra (extra-keys p v)))
			      ;(format #t "Extra keys: ~s\n" xtra)
			      (expanded-file-content fs p v xtra))
			    (content->string (contents v)))))
	       (write-one-tar-file 
		tarf
		(substring (fs-path->string p) 1)
		(if restrict-mode?
		    (bitwise-and (permissions v) #o555)
		    (permissions v))
		(if (and (not snap?)
			 (active-checkout (versioned-object v)))
		    (id (user (active-checkout (versioned-object v))))
		    default-uid)
		(id (group (versioned-object v)))
		(modification-time v)
		txt))
	     (begin
	       (for-each (lambda ((ent <pair>))
			   (archive (fs-append-path 
				     p 
				     (string->fs-path (car ent)))))
			 (contents v))
	       (format #t "  ~d entries in ~a\n"
		       (length (contents v))
		       (fs-path->string p))))))
    ;;
    (if (pair? args)
	(for-each archive args)
	(archive (get-pwd-arg req)))
    ;;
    (format #t "~d bytes writtten so far...\n" (bytes-written tarf))
    (flush-to-blocks tarf 20)
    (close-output-port tmpp)	;; this blocks, waiting for gzip to exit
    tmpf))


(define (handle-flink-request args req inp out u)
  (let ((fs (get-fsys-arg req))
	(to (map (let ((pwd (get-pwd-arg req)))
		   (lambda (f)
		     (fs-append-path pwd (string->fs-path f))))
		 (cdr (assq 'to req))))
	(reas (get-reasons-arg req))
	(tofs (string->filesystem (get-exactly-one req 'to-filespace))))
    ;;
    (if (not (eq? (length args) (length to)))
	(service-error 761 "not equal number of <file1>'s and <file2>'s"))
    ;;
    (for-each (lambda (from-f to-f)
		(cross-link-node fs
				 from-f
				 tofs
				 to-f
				 reas))
	      args
	      to)))



;; ****NOTE****
;; there is no automatic support (yet) for noticing in other fs's
;; when a file gets changed.  hence, it is possible to have
;; a different state in the "current" filesystem than in an
;; otherwise consistent snapshot


(define (handle-f-diverge-request args req inp out u)
  (let ((fs (get-fsys-arg req))
	(r (get-reasons-arg req)))
    ;; no support yet for diverging but keeping a few (non-one)
    ;; set of FS's common
    (for-each (lambda (f)
		(diverge-node (list (cons fs f)) r))
	      args)))

(define (handle-fcreate-request args req inp out u)
  (let ((fs (get-fsys-arg req))
	(grp (get-group-arg req))
	(from-stdin? (assq 'stdin req))
	(binary? (assq 'binary req))
	(rem (get-remarks-arg req inp out))
	(reasons (get-reasons-arg req))
	(timestamp (let ((a (assq 'timestamp req)))
		     (if a
			 (string->time (get-exactly-one req 'timestamp))
			 #f))))
    (for-each (lambda (p)
		(bind ((text stat (if from-stdin?
				      (values (client-snarf-stdin inp out) 
					      #o644)
				      (client-upload inp 
						     out
						     (fs-path->string p))))
		       (content (string->content text binary?)))
		  (set-permissions! 
		   (current-version
		    (make-file fs p u grp content reasons rem timestamp))
		   stat)
		  (if (not from-stdin?)
		      (client-chmod out
				    (fs-path->string p)
				    (bitwise-and stat #o555)))))
	      args)))

(define (handle-f-rm-request args req inp out u)
  (let ((fs (get-fspace-arg req))
	(verbose? (assq 'verbose req))
	(reasons (get-reasons-arg req)))
    (for-each
     (lambda (p)
       (unlink-file fs p reasons)
       (if verbose?
	   (client-print-message out
				 (format #f "~a removed\n"
					 (fs-path->string p)))))
     args)))

(define (handle-f-mv-request args req inp out u)
  (let ((fs (get-fspace-arg req))
	(reasons (get-reasons-arg req))
	(verbose? (assq 'verbose req))
	(to-path (assq 'to req)))
    ;;
    (if (not to-path)
	(service-error 702 "`--file --rename' missing `--to' argument"))
    ;;
    (if (not (= (length (cdr to-path)) (length args)))
	(service-error 
	 702
	 "rename: not same number of source and to paths (~d from, ~d to)" 
	 (length args)
	 (length (cdr to-path))))
    ;;
    (for-each
     (lambda (p t)
       (rename-node fs p t reasons)
       (if verbose?
	   (client-print-message out
				 (format #f "~a => ~a\n"
					 (fs-path->string p)
					 (fs-path->string t)))))
     args
     (absolute-paths req (cdr to-path)))))

#|
(define (handle-f-ch-request args req inp out u)
  (let ((fs (get-fspace-arg req))
	(reasons (get-reasons-arg req))
	(mode (assq 'mode req))
	(rem (if (assq 'remarks req)
		 (get-remarks-arg req inp out)
		 #f))
	(group (assq 'group req)))
    ;;
    (if (and (null? versioned-props)
	     (null? stable-props))
	(service-error 
	 702
	 "`--file --modify' missing sub-argument to modify")
	(begin
	  (if (not (null? versioned-props))
	      (for-each (lambda (p)
			  (if (active-checkout (find-node fs p))
			      (service-error 703
					     "~a: file is locked"
					     (fs-path->string p))))
			args))
	  (let ((props (append versioned-props stable-props)))
	    (for-each (lambda (p)
			(node-modify fs p reasons rem props))
		      args))))))
|#
