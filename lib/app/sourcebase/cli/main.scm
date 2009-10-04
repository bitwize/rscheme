(define *network-log* #f)

(define (set-network-log! (port <output-port>))
  (set! *network-log* port))

#|
(define-method write-string ((self <fd-output-port>) (str <string>))
  (format #t "writing: ~#*@60s\n" str)
  (fd-write (file-descriptor self)
	    str
	    0
	    (string-length str)))
|#

(define $command-table 
  (let-syntax ((cmd (syntax-form (name proc)
		      (cons (mquote name)
			    (lambda args
			      (apply proc args))))))
    (list (cmd file handle-file-request)
	  (cmd ls handle-ls-request)
	  (cmd dir handle-dir-request)
	  (cmd report handle-report-request)
	  (cmd Group handle-group-request)
	  (cmd user handle-user-request)
	  (cmd changereq handle-cr-request)
	  (cmd flush handle-flush-request)
	  (cmd filesystem handle-filesystem-request)
	  (cmd snap handle-snap-request)
	  (cmd eval handle-eval-request)
          ;(cmd exec handle-exec-request)
	  (cmd sync handle-sync-request)
          )))

(define (write-network-log . args)
  (if *network-log*
      (begin
	(write args *network-log*)
	(newline *network-log*)
	(flush-output-port *network-log*))))

;;

(define-class <service-error> (<condition>)
  msg-code
  msg-args
  default-msg-text)

(define (service-message out code text . args)
  (write-network-log 'service-message code text args)
  (client-print-message
   out
   (string-append (format #f "srv-~03d " code)
		  (apply format #f text args))))
  
(define (service-error code text . args)
  (write-network-log 'service-error code text args)
  (signal (make <service-error>
		msg-code: code
		msg-args: args
		default-msg-text: text)))

(define (service-access-denied operation class-name instance-name)
  (if instance-name
      (service-error 599 "Access denied
You do not have sufficient authority to perform the ~a
operation on the ~a ~a\n" operation class-name instance-name)
      (service-error 598 "Access denied
You do not have sufficient authority to perform the ~a
operation on the ~a class\n" operation class-name)))



(define-method display-object ((self <service-error>) port)
  (format port "SRV-~03d " (msg-code self))
  (apply format port (default-msg-text self) (msg-args self))
  (newline port))

(define (check-identity peer-host socket-fd)
  (bind ((ident-resp ident-info (remote-port-owner socket-fd)))
    (if (string-ci=? ident-resp "USERID")
	(let ((f (string-split ident-info " : ")))
	  (if (and (= (length f) 2)
		   (string-ci=? (car f) "UNIX"))
	      (cadr f)
	      "unknown"))
	"unknown")))

;;; this, obviously, is the main entry point for connecting clients

(define (start-talking peer inp out sockfd)
  (format out "SRV-1 (~a ~a)\n" 
	  (name *application*)
	  (application-version *application*))
  (flush-output-port out)
#|
too slow...
  (let ((remote-user (string-append (check-identity peer sockfd) "@" peer)))
    (format #t "socket ~d, remote user is: <~a>\n" sockfd remote-user))
|#
  (let ((hello (read-line inp)))
    (cond
     ((eof-object? hello)
      (client-exit out 3))
     ((and (> (string-length hello) 6)
	   (or (string=? (substring hello 0 5) "CLI-1")
               (string=? (substring hello 0 5) "CLI-2")))
      (if (eq? (service-state *application*) 'available)
	  (let ((client-version (cadr (assoc (substring hello 0 5)
                                             '(("CLI-1" 1)
                                               ("CLI-2" 2))))))
	    (handle-service-request peer inp out client-version)
	    ;; this is just about the best approach in terms
	    ;; of stability -- we flush our output so
	    ;; the user gets to see the results ASAP,
	    (flush-output-port out)
	    ;; but we commit before sending the EXIT command,
	    ;; so the client doesn't exit with status 0 unless
	    ;; we successfully commit
	    (auto-commit)
	    (client-exit out 0))
	  (service-error 1 "System is unavailable")))
     ((string=? hello "\r")
      (format out "telnet to this server is not available\r\n"))
     (else
      (format #t "saying hello? ~s\n" hello)
      (service-error 3 "Client incompatibility")))))

(define *do-auto-commit* #f)

(define (set-auto-commit! flag)
  (set! *do-auto-commit* flag))

(define (auto-commit)
  (if *do-auto-commit*
      (let ((n (num-dirty-pages *pstore*)))
	(if (= n 0)
	    (format #t " no changes to commit\n")
	    (format #t " ~s pages: commit-record: '~s\n" n (commit *pstore*))))
      (format #t " [auto-commit disabled]\n")))

;;; manual `commit'

(define (handle-sync-request args req inp out (u <user>))
  (let ((cr (commit *pstore*)))
    (client-print-message out (format #f "New commit record: ~s\n" cr))))

(define (auto-rollback)
  (rollback *pstore*))

(define (rollback ps)
  (format #t "================ rolling back database =============\n")
  (let ((n (num-dirty-pages ps)))
    (format #t "Number of dirty pages: ~d\n" n)
    (if (> n 0)
	(begin
	  (format #t "*** can't handle dirty pages... giving up\n")
	  (error "rollback: can't handle dirty pages\n")))
    (format #t "================ rollback complete =============\n")))

(define (printbt c next-h)
  (with-output-to-port
      (current-error-port)
    (lambda ()
      (format #t "********** error detected **********\n~a\n" c)
      (format #t "************************************\n")
      (format #t "************ Continuation backtrace:\n")
      (with-module repl
	(ccbt))))
  (next-h))

(define (app-server peer inp out sockfd)
  (handler-case
   ;;
   (handler-bind (<condition> printbt)
     (start-talking peer inp out sockfd))
   ;;
   ((<service-error> condition: c)
    (format #t "======== ~s =======\n~a" c c)
    (client-print-error
     out
     (call-with-output-string
      (lambda (strp)
	(display c strp))))
    (flush-output-port out)
    (auto-rollback)
    (client-exit out 1)
    #f)
   ((<condition> condition: c)
    (format #t "======== ~s =======\n~a" c c)
    (client-print-error
     out
     (call-with-output-string
      (lambda (strp)
	(format strp "SRV-002 Internal error...\n~a" c))))
    (flush-output-port out)
    (auto-rollback)
    (client-exit out 2)
    #f)))

;;
;;  client RPCs
;;

(define (fixnum->32 (val <fixnum>))
  (string
   (integer->ascii-char  (bitwise-and (logical-shift-right val 24) #xFF))
   (integer->ascii-char  (bitwise-and (logical-shift-right val 16) #xFF))
   (integer->ascii-char  (bitwise-and (logical-shift-right val 8) #xFF))
   (integer->ascii-char  (bitwise-and val #xFF))))

(define-syntax (fixnum->16 val)
  (let (((i <fixnum>) val))
    (string
     (integer->ascii-char 
      (bitwise-and (logical-shift-right i 8) #xFF))
     (integer->ascii-char 
      (bitwise-and i #xFF)))))

(define-syntax (fixnum->8 val)
  (string (integer->ascii-char (bitwise-and val #xFF))))


(define-syntax (client-write-cmd out . strs)
  (let* ((o out)
	 (m (find-method write-string (list o ""))))
    (letrec-syntax ((render (syntax-form ())
			    (syntax-form (item . more)
			      (m o item)
			      (render . more))))
      (render . strs))))

(define (client-print-error out text)
  (client-write-cmd out "P" (fixnum->32 (string-length text)) text "."))

(define (client-response-structure out #rest info)
  (let ((n (quotient (length info) 2))
        (write-string (find-method write-string (list out "test"))))
    ;;
    (define (write-item x)
      (cond
       ((string? x)
        (write-string out "S")
        (write-string out (fixnum->16 (string-length x)))
        (write-string out x))
       ((list? x)
        (write-string out "L")
        (write-string out (fixnum->16 (length x)))
        (for-each write-item x))
       ((fixnum? x)
        (if (< x 65536)
            (write-string out (string-append "i" (fixnum->16 x)))
            (write-string out (string-append "I" (fixnum->32 x)))))
       ((table? x)
        (write-string out "R")
        (write-string out (fixnum->8 n))
        (let loop ((i (key-sequence x)))
          (if (pair? i)
              (let ((head (car i)))
                (write-string out (fixnum->16 (string-length head)))
                (write-string out head)
                (write-item (table-lookup x head))
                (loop (cdr i))))))))
    ;;
    (let ((t (make-string-table)))
      (let loop ((i info))
        (if (null? i)
            (begin
              (write-string out "R")
              (write-item t)
              (write-string out "."))
            (begin
              (table-insert! t
                             (symbol->string (keyword->symbol (car i))) 
                             (cadr i))
              (loop (cddr i))))))))

;;

(define (client-print-message out (text <string>))
  (client-write-cmd out "p" (fixnum->32 (string-length text)) text "."))

(define (client-exit out code)
  (client-write-cmd out "x" (fixnum->16 code) "."))

(define (client-download-tar out (text <string>))
  (client-write-cmd
   out
   "T"
   (fixnum->32 (string-length text))
   text
   "."))

(define (client-chmod out (path <string>) (mode <fixnum>))
  (client-write-cmd
   out
   "m"
   (fixnum->8 (string-length path))
   path
   (fixnum->16 mode)
   "."))
  
(define (client-download inp out
			 (path <string>) 
			 (permissions <fixnum>)
			 (text <string>))
  (format #t "download ~a (chmod ~03o)\n" path permissions)
  (client-write-cmd
   out
   "d"
   (fixnum->8 (string-length path))
   path
   (fixnum->16 permissions)
   (fixnum->32 (string-length text))
   text
   ".")
  (flush-output-port out)
  (let ((rc (read-char inp)))
    (case rc
      ((#\d) ;; downloaded ok
       #t)
      ((#\e) ;; error
       (service-error 501 "Client-side error writing ~a: ~a"
		      path
		      (client-read-str inp)))
      (else
       (error "protocol error: bad download return ~s" rc)))))


(define (client-upload inp out path)
  (client-write-cmd out "u" (fixnum->8 (string-length path)) path ".")
  (flush-output-port out)
  (let ((rc (read-char inp)))
    (case rc
      ((#\f) ;; file follows
       (let ((stat (client-read-16 inp))
	     (text (client-read-text inp)))
	 (values text (bitwise-and #o777 stat))))
      ((#\e) ;; error
       (service-error 500 "Client-side error accessing ~a: ~a"
		      path
		      (client-read-str inp)))
      (else
       (error "protocol error: bad upload return ~s" rc)))))

(define (client-snarf-stdin inp out)
  (client-write-cmd out "-.")
  (flush-output-port out)
  (let ((rc (read-char inp)))
    (case rc
      ((#\-) (client-read-text inp))
      (else (error "protocol error: bad snarf stdin return: ~s" rc)))))

;;
;;  utility functions for reading data from the client
;;

(define (client-read-8 inp)
  (char->integer (read-char inp)))

(define (client-read-16 inp)
  (+ (* (client-read-8 inp) #x100)
     (client-read-8 inp)))

(define (client-read-32 inp)
  (+ (* (client-read-8 inp) #x1000000)
     (* (client-read-8 inp) #x10000)
     (* (client-read-8 inp) #x100)
     (client-read-8 inp)))

(define (client-read-str inp)
  (read-string inp (client-read-16 inp)))

(define (client-read-text inp)
  (let ((txt (read-string inp (client-read-32 inp))))
    (write-network-log 'client-read-text txt)
    txt))

(define (client-read-klist inp)
  (map (lambda (i)
	 (cons (string->symbol (client-read-str inp))
	       (map (lambda (j)
		      (client-read-str inp))
		    (range (client-read-16 inp)))))
       (range (client-read-8 inp))))

;;
;;  actually handle a service request...
;;


(define *standard-vsh-variables* (make-table string=? string->hash))

(define (get-exactly-one req key)
  (let ((a (assq key req)))
    (if a
	(if (pair? (cdr a))
	    (if (pair? (cddr a))
		(service-error 12
			      "Too many values for `--~a'; expected one" 
			      key)
		(cadr a))
	    (service-error 11 "Missing value for `--~a'" key))
	(service-error 11 "Missing `--~a' flag" key))))

(define (immediate-or-snarf-stdin (str <string>) inp out)
  (if (string=? str "-")
      (client-snarf-stdin inp out)
      str))

(define (get-remarks-arg req inp out)
  (immediate-or-snarf-stdin (get-exactly-one req 'remarks) inp out))

(define (get-reasons-arg req)
  (let ((r (assq 'request req)))
    (if r
	(map string->changereq (cdr r))
	'())))

(define (get-fsys-arg req)
  (let ((fs (get-fspace-arg req)))
    (if (instance? fs <file-system>)
	fs
	(service-error 12 "A file-system is required, not snapshot ~a" fs))))

(define (get-fspace-arg req)
  (let* ((fs (get-exactly-one req 'filespace))
	 (i (string-search fs $version-delim)))
    (if i
	(string->snapshot (string->filesystem (substring fs 0 i)) 
			  (substring fs (+ i 1)))
	(string->filesystem fs))))

(define (get-group-arg req)
  (string->group (get-exactly-one req 'group)))

(define (get-pwd-arg req)
  (string->fs-path (get-exactly-one req 'pwd)))

(define (get-user-arg req peer)
  (let ((u (string->user (get-exactly-one req 'login))))
    (let ((h (assoc peer (remote-hosts u))))
      (if (and h (member (get-exactly-one req 'logname) (cdr h)))
	  u
	  (service-error 15 "Unauthorized access: ~a@~a as ~a"
			(get-exactly-one req 'logname)
			peer
			(name u))))))

#|
  (let loop ((cmd #f)
	     (r req))
    (if (null? r)
	(if cmd
	    (let ((u (get-user-arg req peer))
		  (proc (cdr (assq (car cmd) $command-table))))
	      (fluid-let ((*user* u))
		(proc (cdr cmd) req inp out u)))
	    (service-error 4 "No command"))
	(if (assq (caar r) $command-table)
	    (if cmd
		(service-error 5
			       "Multiple commands `--~a' and `--~a' at least"
			       (car cmd)
			       (caar r))
		(loop (car r) (cdr r)))
	    (loop cmd (cdr r)))))
|#

(define (handle-command req inp out peer client-version)
  (bind ((proc args (parse-subcommand req $command-table #f)))
    (let ((u (get-user-arg req peer)))
      (fluid-let ((*user* u)
                  (*client-version* client-version))
	(proc args req inp out u)))))

(define (parse-subcommand req cmdtable basecmd)
  (let loop ((cmd #f)
	     (r req))
    (if (null? r)
	(if cmd
	    (let ((proc (cdr (assq (car cmd) cmdtable))))
	      (values proc (cdr cmd)))
	    (if basecmd
		(service-error 8 "Missing subcommand to `--~a'" basecmd)
		(service-error 4 "No command")))
	(if (assq (caar r) cmdtable)
	    (if cmd
		(if basecmd
		    (service-error 
		     8
		     "Multiple subcommands to `--~a' (`--~a' and `--~a' at least)"
		     basecmd
		     (car cmd)
		     (caar r))
		    (service-error 5
				   "Multiple commands `--~a' and `--~a' at least"
				   (car cmd)
				   (caar r)))
		(loop (car r) (cdr r)))
	    (loop cmd (cdr r))))))

(define (handle-service-request peer inp out client-version)
  (let ((req (client-read-klist inp)))
    (write-network-log 'handle-service-request peer req)
    (print req)
    (handle-command req inp out peer client-version)))

;;
;;

(define (read-str str)
  (with-input-from-string str read))

;;

(define (cli-properties req optional required)
  (let ((r '()))
    ;;
    (define (parse-one prop-name)
      (let (((p <property>) (table-lookup 
			     (property-table *application*)
			     (symbol->string prop-name))))
	(if (assq prop-name req)
	    (begin
	      (set! r (cons (cons p (parse-property-value 
				     p
				     (string-join
				      #\space
				      (cdr (assq prop-name req)))))
			    r))
	      #t)
	    (if (default-value p)
		(begin
		  (set! r (cons (cons p (default-value p)) r))
		  #t)
		#f))))
    ;;
    (for-each parse-one optional)
    (for-each (lambda (p)
		(if (not (parse-one p))
		    (service-error 496 "missing `--~a' argument" p)))
	      required)
    r))
