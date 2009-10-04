(define $user-subcommands '(view report login))

(define (handle-flush-request args req inp out (u <user>))
  (if (super-user? u)
      (flush-mail-queue)
      (service-access-denied "flush" "mail-queue" #f)))

(define (handle-eval-request args req inp out (u <user>))
  (if (super-user? u)
      (let ((t (open-client-tee out)))
	(with-output-to-port
	    t
	  (lambda ()
	    (if (assq 'load req)
		(for-each load (cdr (assq 'load req))))
	    (for-each
	     (lambda (a)
	       (let ((r (eval (read (open-input-string 
				     (immediate-or-snarf-stdin a inp out)))
			      *self*)))
		 (format #t "value := ~#*@60s\n" r)))
	     args)))
	(close-output-port t))
      (service-access-denied "eval" "system" #f)))

(define (handle-user-request args req inp out (u <user>))
  (if (null? args)
      ;; <user> class operation
      (user-class-operation req inp out u)
      ;; <user> instance operation
      (let ((op (user-instance-operation req inp out u)))
	(for-each op (map string->user args)))))

(define (user-class-operation req inp out (u <user>))
  (if (super-user? u)
      (let ((nm (get-exactly-one req 'name))
	    (full (get-exactly-one req 'fullname))
	    (email (get-exactly-one req 'email)))
	(make-user nm full email '()))
      (service-access-denied "create" "user" #f)))

(define (parse-site site)
  (let ((i (string-search site #\@)))
    (if i
	(cons (substring site (+ i 1))
	      (substring site 0 i))
	(service-error 120
		       "site specification `~a' invalid, expected user@host"
		       site))))

(define (add-login-site (u <user>) host login)
  (let ((h (assoc host (remote-hosts u))))
    (if h
	(begin
          (set-cdr! h (cons login (cdr h)))
          (values))
	(set-remote-hosts! u (cons (list host login) (remote-hosts u))))))

(define (remove-login-site (u <user>) host login)
  (let ((h (assoc host (remote-hosts u))))
    (if h
	(let ((m (member login (cdr h))))
	  (if m
	      (let ((r (delq! (car m) (cdr h))))
		(if (null? r)
		    ;; list for this host is now empty... drop it
		    (set-remote-hosts! u (delq! h (remote-hosts u)))
		    ;; still some others for this host
		    (begin
                      (set-cdr! h r)
                      (values)))))))))

(define (user-instance-operation req inp out (usr <user>))
  (cond
   ((assq 'modify req)
    (cond
     ((assq 'site req)
      (let ((adds (if (assq 'add req)
		      (map parse-site (cdr (assq 'add req)))
		      '()))
	    (dels (if (assq 'remove req)
		      (map parse-site (cdr (assq 'remove req)))
		      '())))
	(lambda ((u <user>))
	  (if (or (super-user? usr)
		  (eq? u usr))
	      (begin
		;; update remote-host's entries
		(for-each (lambda ((p <pair>))
			    (remove-login-site u (car p) (cdr p)))
			  dels)
		(for-each (lambda ((p <pair>))
			    (add-login-site u (car p) (cdr p)))
			  adds))
	      (service-access-denied "modify-site" "user" (name u))))))
     (else
      (service-error 125 "missing `--user --modify' subcommand"))))
   ;; not a modify request...
   (else
    (let ((ren (rendition-proc <user> req render-full)))
      (lambda ((u <user>))
	(client-print-message
	 out
	 (with-output-to-string
	   (lambda ()
	     (ren u)))))))))
