(define (mkdir-cmd (p <string>))
  (make-directory (current-filesystem *vsh-state*)
	            (fs-append-path (current-path *vsh-state*)
	     		            (string->fs-path p))
		    (user *vsh-state*)
		    (group *vsh-state*)
		     (reasons *vsh-state*)))

;; take a snapshot
;; usage: snap name [-a key=value...]

(define (snap . args)
   (let ((attribs  (let ((a (member "-a" (cdr args))))
		    (if a
		        (parse-kvlist (cdr a))
			'()))))
    (make-current-snapshot (car args)
			   (current-filespace *vsh-state*) 
			   attribs)))

(define (checkin-dir msg arg)
   (directory-delta (current-filesystem *vsh-state*)
   		    (fs-append-path (current-path *vsh-state*)
		   		    (string->fs-path arg))
		    (user *vsh-state*)
		    msg))

(define (checkin . args)
  (let ((msg (if (string=? (car args) "-m")
  		 (let ((m (cadr args)))
		    (set! args (cddr args))
		    m)
		 #f)))
  (define (ci path)
	(make-file (current-filesystem *vsh-state*)
		   (fs-append-path (current-path *vsh-state*)
		   		   (string->fs-path path))
		   (user *vsh-state*) 
		   (group *vsh-state*)
		   (string->content (file->string (local-file path)))
		   (reasons *vsh-state*)
		   msg
		   #f))

   (define (cirec path)
      (format #t "cirec: ~a (local ~a)\n" path (local-file path))
      (let ((b (stat (local-file path))))
         (if b
	    (if (stat-directory? b)
		(begin
		    (mkdir-cmd path)
		    (for-each cirec 
				(map (lambda (a)
					(string-append path "/" a))
				    (cddr (scandir path)))))
		(ci path))
	     (error "~a: missing local file" path))))

   (if (string=? (car args) "-F")
       (with-lines-from-file (cadr args) ci)
       (if (string=? (car args) "-R")
           (for-each cirec (cdr args))
	    (for-each ci args)))))

       
(define (checkout . args)
)


(define (vi file)
  (let* ((tmp-file (string-append "/tmp/.vsh." (number->string (random))))
         (path (fs-append-path (current-path *vsh-state*)
	 		       (string->fs-path file)))
	 (fs (current-filesystem *vsh-state*))
	 (usr (user *vsh-state*))	       
	 (nodev (and (find-version* fs path) 
	 	     (node-lock fs path usr)))
	 (text #f))
    (if (instance? nodev <directory-version>)
        (begin
	  (node-unlock fs path usr)
	  (error "~a: is a directory" file)))
    (if nodev
	(set! text (content->string (contents nodev)))
	(begin
	  (format #t "~a: new file\n" file)
	  (set! text "")))
    ;;
    (call-with-output-file
      tmp-file
      (lambda (port)
        (write-string port text)))
    ;;
    (system (string-append "vi " tmp-file))
    ;; check it back in
    (let ((new-text (file->string tmp-file)))
	(if (string=? new-text text)
	    (begin
		(format #t "~a: no changes\n" file)
		(node-unlock fs path usr))
	    (if nodev
		(let ((nc (string->shared-content new-text (contents nodev))))
		    (format #t "~a: checking in changes\n" file)
		    (file-delta fs path usr nc (reasons *vsh-state*) #f #f))
		(make-file fs path usr (group *vsh-state*)
			   (string->content new-text)
			   (reasons *vsh-state*) 
			   #f
			   #f))))
    ;;			
    (system (string-append "rm -f " tmp-file))
    (values)))

(define (unlock (file <string>))
  (node-unlock (current-filesystem *vsh-state*)
			   (fs-append-path (current-path *vsh-state*)
			 		   (string->fs-path file))
			    (user *vsh-state*))
  (values))
  
(define (lock (file <string>))
  (node-lock (current-filesystem *vsh-state*)
			   (fs-append-path (current-path *vsh-state*)
			 		   (string->fs-path file))
			    (user *vsh-state*))
  (values))

(define (mkfs (fs <string>))
   (make-filesystem fs (user *vsh-state*) (group *vsh-state*))
   (values))

(define (mkuser (login <string>) (fullname <string>) (email <string>))
   (make-user login fullname email '()))

(define (mkgroup (name <string>) (parent <string>) (lead <string>))
   (make-group name (list (string->group parent)) (string->user lead)))

(define (req action . args)
  (case (string->symbol (substring action 1))
    ((open)
       (if (not (eq? (length args) 4))
           (error "usage: req -open title group summary remarks"))
       (let ((cr (make-change-request (car args)
       				      (string->group (cadr args))
				      (if (string=? (caddr args) "-")
				          #f
					  (caddr args))
				      (if (string=? (cadddr args) "-")
				          #f
					  (cadddr args))
				      '())))
	  (format #t "Created new change request: ~d\n" (id cr))))
    ((view)
       (for-each (lambda (r)
       		   (print (table-lookup (change-request-table *application*) (string->number r))))
		args))
    (else (error "usage: req -open|-view args..."))))
    
