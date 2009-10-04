(define $filesystem-subcommand
  (let-syntax ((cmd (syntax-form (name proc)
		      (cons (mquote name)
			    (lambda args
			      (apply proc args))))))
    (list (cmd create handle-fs-create-request)
	  (cmd inspect handle-fs-inspect-request)
	  (cmd diverge handle-fs-diverge-request)
	  (cmd policy handle-fs-policy-request))))

(define (handle-filesystem-request args req inp out (u <user>))
  (bind ((proc sub-args (parse-subcommand req
					  $filesystem-subcommand 
					  'filesystem)))
    (proc (map string->filesystem args) req inp out u)))

(define (handle-fs-create-request fss req inp out u)
  (cond
   ((assq 'link req)
    (make-shared-filesystem (get-exactly-one req 'name)
                            (let ((a (assq 'owner req)))
                              (if a
                                  (string->user (cadr a))
                                  u))
                            (get-group-arg req)
                            (string->filesystem
                             (cadr (assq 'link req)))))
   (else
    (make-filesystem (get-exactly-one req 'name)
                     (let ((a (assq 'owner req)))
                       (if a
                           (string->user (cadr a))
                           u))
                     (get-group-arg req)))))

(define (handle-fs-policy-request fss req inp out u)
  (let ((choice (if (assq 'on req)
		    (if (assq 'off req)
			(service-error 705 "`--on' and `--off' both specified")
			#t)
		    (if (assq 'off req)
			#f
			(service-error 706 "neither `--on' nor `--off' specified"))))
	(policies (map string->symbol (cdr (assq 'policy req)))))
    (for-each
     (lambda ((fs <file-system>))
       (for-each (lambda (p)
		   (set-filesystem-policy fs p choice))
		 policies))
     fss)))

(define (handle-fs-diverge-request fss req inp out u)
  (for-each
   (lambda ((f <file-system>))
     (diverge-fs f (string->filesystem (get-exactly-one req 'from))))
   fss))

(define (handle-fs-inspect-request fss req inp out u)
  (for-each
   (lambda ((f <file-system>))
     (case *client-version*
       ((1) (client-print-message
             out
             (with-output-to-string
               (lambda ()
                 (render-full f)))))
       ((2) (client-response-structure out
                                       name: (name f)))))
   fss))
