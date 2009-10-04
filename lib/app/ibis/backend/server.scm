,(use rs.sys.threads.manager)

(define-thread-var *session*)

;;;

(define-class <session> (<object>)
  (infobase)
  (socket-peer)
  (socket-filedes)
  (session-thread)
  (input-port)
  (output-port)
  (start-time init-function: time)
  (last-time init-function: time)
  (auth-info init-value: '()))

(define-class <request> (<object>)
  (in-session type: <session>)
  (keylist type: <list>))

(define-thread-var *request*)

(define (sigpipe)
  (format (current-error-port) "*** Got a SIGPIPE\n"))

(define (sigint)
  (format (current-error-port) "*** Got a SIGINT\n")
  (note 2119 "*** Exiting on SIGINT")
  (process-exit 2))

(define (server-daemon #key port)
  (let* ((fd (inet-server port))
	 (svc (make-service fd)))
    ;;
    (with-module
        start
      (register-interrupt-handler! 'c-signal handle-c-signal)
      (register-c-signal-handler! 'SIGPIPE sigpipe)
      (register-c-signal-handler! 'sigint sigint))
    ;;
    (note 2111 "Server daemon running on port ~d" port)
    (let loop ()
      (bind ((fd peer (get-next-client svc))
             (session (kick-off-session peer fd)))
        ;; single-threaded for now, because I'm
        ;; not sure the system can actually handle concurrency
        ;; against the object database
        (thread-join (session-thread session))
        (loop)))))

(define (kick-off-session peer fd)
  (letrec ((s (make <session>
                    infobase: (current-information-base)
                    socket-peer: peer
                    socket-filedes: fd
                    session-thread: (make-thread
                                     (lambda ()
                                       (run-session* s)
                                       (shutdown-session s))
                                     (format #f "server~a" peer))
                    input-port: (open-mbox-input-port fd)
                    output-port: (open-queued-output fd))))
    (thread-resume (session-thread s))
    s))

(define (run-session* (self <session>))
  (note 2121 "Client from ~a" (socket-peer self))
  (thread-let ((*session* self))
    (handler-case
     (run-session self)
     ;;
     ((<service-error> condition: c)
      (client-print-error (output-port self)
                          (call-with-output-string
                           (lambda (strport)
                             (display c strport))))
      (flush-output-port (output-port self))
      (auto-rollback)
      (client-exit (output-port self) 1))
     ;;
     ((<condition> condition: c)
      (format #t "*** service from ~s failed ***\n" (socket-peer self))
      (display c)
      (format #t "~a\n" (make-string 60 #\=))
      (with-module repl (apply-backtrace c))
      (format #t "~a\n" (make-string 60 #\=))
      ;
      (client-print-error (output-port self) 
                          "SRV-002 Internal server error\n")
      (client-print-error (output-port self)
                          (call-with-output-string
                           (lambda (strport)
                             (display c strport))))
      (flush-output-port (output-port self))
      (auto-rollback)
      (client-exit (output-port self) 2)))))

;;;

(define (shutdown-session (self <session>))
  (close-input-port (input-port self))
  (close-output-port (output-port self))
  (fd-close (socket-filedes self))
  (note 2139 "Session ~a connected for ~d seconds"
        (socket-peer self)
        (interval->seconds (time-time (time) (start-time self)))))

;;;**********************************************************************
;;;
;;;   Session runtime
;;;
;;;**********************************************************************

(define (run-session (self <session>))
  (format (output-port self) "SRV-1 (IBIS ~a)\n" *application-version*)
  (flush-output-port (output-port self))
  ;;
  (let ((hello (read-line (input-port self))))
    (cond
     ((eof-object? hello)
      (format #t "Connection lost waiting for hello\n"))
     ((and (> (string-length hello) 6)
	   (string=? (substring hello 0 5) "CLI-1"))
      ;(format #t "/// ~s\n" hello)
      (command-line-interaction self))
     ((string=? hello "\r")
      (manual-telnet-session self))
     (else
      (wm 2127 "Bad hello form from ~a: ~s" (socket-peer self) hello)))))

(define (manual-telnet-session (self <session>))
  (wm 2126 "Apparent telnet access from ~a" (socket-peer self))
  (format (output-port self) "login: ")
  (flush-output-port (output-port self))
  (let ((who (chomp-cr (read-line (input-port self)))))
    (format (output-port self) "*** Welcome to the IBIS server, ~s\r\n" who)
    ;
    (format (output-port self) "*** manual 'telnet' is not supported!\r\n")
    ;
    (format (output-port self) "*** 'bye, ~s\r\n" who)))

(define (chomp-cr str)
  (if (string? str)
      (let ((n (string-length str)))
        (if (and (> n 0)
                 (char=? (string-ref str (- n 1)) #\cr))
            (substring str 0 (- n 1))
            str))
      str))

;;;

(define (command-line-interaction (self <session>))
  ;(client-print-message (output-port self) 
  ;(format #f "IBIS ~a\n" *application-version*))
  (let loop ((i 1))
    (let ((req (client-read-klist (input-port self))))
      (note 2131 "Request [~d]" i)
      (for-each (lambda (r)
                  (note 2132 "    ~a => ~s" (car r) (cdr r)))
                req)
      ;
      (process-command self req)
      (if (assq '*cont req)
          (loop (+ i 1))
          (begin
            (note 2138 "Processed ~d requests for ~a" i (socket-peer self))
            (auto-commit)
            (client-exit (output-port self) 0))))))

(define (get-socket-username socket-fd)
  (bind ((ident-resp ident-info (remote-port-owner socket-fd)))
    (if (string-ci=? ident-resp "USERID")
	(let ((f (string-split ident-info " : ")))
	  (if (and (= (length f) 2)
		   (string-ci=? (car f) "UNIX"))
	      (cadr f)
	      "unknown"))
	"unknown")))

;;;**********************************************************************
;;;
;;;   CLI marshalling
;;;
;;;**********************************************************************


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
  (let ((o out))
    (letrec-syntax ((render (syntax-form ())
			    (syntax-form (item . more)
			      (write-string o item)
			      (render . more))))
      (render . strs))))

(define (client-print-error out text)
  (client-write-cmd out "P" (fixnum->32 (string-length text)) text "."))

(define (client-print-message out (text <string>))
  (client-write-cmd out "p" (fixnum->32 (string-length text)) text "."))

(define (client-exit out code)
  (client-write-cmd out "x" (fixnum->16 code) "."))

;;;
;;; request that the client extract `text' as a tar file
;;;

(define (client-download-tar out (text <string>))
  (client-write-cmd
   out
   "T"
   (fixnum->32 (string-length text))
   text
   "."))

;;;
;;;  request that the client change file permissions
;;;

(define (client-chmod out (path <string>) (mode <fixnum>))
  (client-write-cmd
   out
   "m"
   (fixnum->8 (string-length path))
   path
   (fixnum->16 mode)
   "."))

;;;
;;;  request that the client store a file
;;;

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
    ;(write-network-log 'client-read-text txt)
    txt))

(define (client-read-klist inp)
  (map (lambda (i)
	 (cons (string->symbol (client-read-str inp))
	       (map (lambda (j)
		      (client-read-str inp))
		    (range (client-read-16 inp)))))
       (range (client-read-8 inp))))

(define (get-stdin (self <request>))
  (client-snarf-stdin (input-port (in-session self))
                      (output-port (in-session self))))

;;;**********************************************************************
;;;
;;;   Services
;;;
;;;**********************************************************************

(define (get-access-host (u <user>) (s <session>) remote-u)
  (let ((hostname #f)
        (hostaddr (inet-addr->string
                   (inet-socket-addr-parts (socket-peer s))))
        (hlist (access-host-list u)))
    ;(format #t "hostaddr ~s list ~s\n" hostaddr hlist)
    (let loop ((hlist hlist))
      (if (pair? hlist)
          (let* ((a (car hlist))
                 (isaddr? (char-numeric? (string-ref (host a) 0))))
            (if (and (not isaddr?)
                     (not hostname))
                (set! hostname (inet-socket-hostname (socket-peer s))))
            (if (and (string=? (if isaddr? hostaddr hostname) (host a))
                     (string=? (user a) remote-u))
                (begin
                  (note 2122 "Accessing ~a from host ~a" 
                        (name u)
                        (host a))
                  a)
                (loop (cdr hlist))))
          (service-error 5 "User ~s not allowed access from user ~s on host ~a~a"
                         (name u)
                         remote-u
                         hostaddr
                         (if hostname
                             (format #f " (~a)" hostname)
                             ""))))))

(define (check-access-host-user (u <user>) (s <session>) (remote-u <string>))
  (let ((h (get-access-host u s remote-u)))
    (if (run-ident-check? h)
        (let ((real-u (get-socket-username (socket-filedes s))))
          (note 2123 "Real user (per ident) is ~s" real-u)
          #t)
        #t)))

(define-method get-user-object ((self <request>))
  (cdr (assq 'user (auth-info (in-session self)))))

(define (authenticate component priviledge)
  (let* ((s *session*)
         (u (cdr (assq 'user (auth-info s)))))
    ;; translate from a <string> object to a <user> object
    ;; 
    (if (eq? 1 (check-authority component 
                                priviledge
                                (cdr (assq 'cache (auth-info s)))))
        #t
        (let ((super (assq 'super (auth-info s))))
          (if super
              (begin
                (if (not (cdr super))
                    (begin
                      (format #t "User ~s is invoking super-user priviledges\n"
                              (name u))
                      (set-cdr! super #t)))
                #t)
              (service-error 4 "User ~s not authenticated for `~s' access to ~s"
                             (name u)
                             priviledge
                             (name component)))))))

(define (init-auth-info (self <session>) request)
  (let* ((remote-u (get-single-value '*logname))   ; who are they coming from?
         (local-u (get-single-value 'login remote-u)) ;" are they trying to be?
         (user-o (or (table-lookup (user-index (current-information-base))
                                   local-u)
                     (service-error 3 "No such user ~s" local-u))))
    ;; check to make sure we're on their access list
    (check-access-host-user user-o self remote-u)
    ;;
    (let ((alist (list (cons 'user user-o)
                       (cons 'remote-user remote-u)
                       (cons 'cache (explode-authority-vector 
                                     (authority-vector user-o))))))
      (set-auth-info! self (if (and (super-user? user-o)
                                    (assq 'su request))
                               (append alist
                                       (list (cons 'super #f)))
                               alist)))))

#|
  superceded by the upcoming `rs.util.reload' module...

(define *server-commands-mtime* #f)

(define (reload-commands)
  (let ((mt (stat-mtime (stat "server-commands.scm"))))
    (if (or (not *server-commands-mtime*)
            (time>? mt *server-commands-mtime*))
        (begin
          (set! *server-commands-mtime* mt)
          (load "server-commands.scm")))))

|#

(define (process-command (self <session>) request)
  ;;(reload-commands)
  ;;
  (thread-let ((*request* (make <request>
                                keylist: request
                                in-session: self)))
    (if (null? (auth-info self))
        (begin
          (init-auth-info self request)
          (authenticate (root-domain (current-information-base)) 'login)))
    ;;
    (let loop ((lst *command-list*))
      (if (null? lst)
          (service-error 404 "No action specified")
          (if (command-supplied? (caar lst) request)
              (let ((proc ((cdar lst))))
                (with-transaction
                 (make <ibase-transaction>
                       user: (cdr (assq 'user (auth-info self))))
                 (lambda ()
                   (with-notification
                    (lambda ()
                      (proc *request*))))))
              (loop (cdr lst)))))))

(define (command-supplied? pattern request)
  (if (symbol? pattern)
      (and (assq pattern request) #t)
      (every? (lambda (n)
                (and (assq n request) #t))
              pattern)))

;;;  Retrieve a single value from the current request;
;;;  If more than one value was supplied, signal a session error
;;;  If no value was supplied (and no optional arg is supplied
;;;  to this procedure), then signal a session error

(define (get-single-value key . opt)
  (let ((a (assq key (keylist *request*))))
    (if a
        (if (pair? (cdr a))
            (if (pair? (cddr a))
                (service-error 103 "Multiple value supplied for `--~a'; expected only one" key)
                (cadr a))
            (service-error 104 "No value supplied for `--~a'; expected one"
                           key))
        (if (pair? opt)
            (car opt)
            (service-error 104 "No `--~a' supplied; expected flag with value"
                           key)))))

(define (ibisd store #rest options)
  (with-system-state
   (open-ibis-store store)
   (lambda ()
     (apply server-daemon options))))
