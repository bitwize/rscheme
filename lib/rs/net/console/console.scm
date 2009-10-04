
(define (parse-socket-spec str)
  (let ((lst (string-split str #\:)))
    (if (= (length lst) 1)
        (string->number (car lst))
        (make-inet-socket-addr (string->inet-addr (car lst))
                               (string->number (cadr lst))))))

;;;
;;;  `login-hook' is an optional procedure of two
;;;  arguments:  
;;;
;;;    -  the first argument is a <console-connsection> object
;;;       with is in the post-authentication state (i.e., has
;;;       it's authentication slot filled in)
;;;
;;;    -  the second argument is a procedure which should be invoked
;;;       to execute the REPL interaction.  If the procedure takes
;;;       an optional `environment:' keyword argument to supply the
;;;       environment for the REPL interaction

(define (start-console-server listen realm logbase #key
                              (login-hook default: #f))
  (let ((s (open-server-socket (parse-socket-spec listen)))
        (f (and logbase (open-log-fileset basefile: logbase))))
    (thread-resume
     (make-thread
      (lambda ()
        (if f
            (with-message-dest
             (log-file-message-dest f)
             (lambda ()
               (note 801 "console server start on ~s" listen)
               (console s realm (or login-hook
                                    (lambda (cnx body)
                                      (body))))))
            (begin
              (note 802 "console server start on ~s" listen)
              (console s realm (or login-hook
                                   (lambda (cnx body)
                                     (body)))))))
      "consoled"))))

(define-class <console-connection> (<object>)
  realm
  socket
  (io-port init-value: #f)
  (user init-value: #f)
  (authentication init-value: #f)
  telnet-session)

(define (console s realm login-hook)
  (let loop ()
    (let ((c (accept-client s)))
      (note 751 "Console client: ~s" c)
      (thread-resume
       (make-thread
        (lambda ()
          (handler-case
           (handle-console-connection
            (make <console-connection>
                  realm: realm
                  socket: c
                  telnet-session: (open-telnet-session c))
            login-hook)
           ((<condition> condition: e)
            (note 752 "Console died: ~a" e)))
          (close c))
        "console"))
      (loop))))

(define (get-console-login (self <console-connection>))
  (let ((o (output-port (telnet-session self)))
        (i (input-port (telnet-session self))))
    (format o "Authenticate to ~s:\n" (name (realm self)))
    (let loop ((tries 0))
      (format o "login: ")
      (flush-output-port o)
      (let ((u (read-line i)))
        (note 701 "user login ~s" u)
        (if (need-password? (realm self) u)
            (begin
              ;; tell the other side we're going to echo; hopefully,
              ;; then they won't echo the password
              (telnet-declare-option (telnet-session self) 'echo #t)
              (format o "password: ")
              (flush-output-port o)
              (let ((p (read-line i)))
                (telnet-declare-option (telnet-session self) 'echo #f)
                (let ((auth (authenticate (realm self) u p)))
                  (or auth
                      (begin
                        (format o "\n")
                        (wm 781 "password check failed for ~s" u)
                        (if (< tries 3)
                            (loop (+ tries 1))
                            (begin
                              (format o "*** Authentication failed\nBye.\n")
                              #f)))))))
            (let ((auth (authenticate (realm self) u #f)))
              (or auth
                  (if (< tries 3)
                      (loop (+ tries 1))
                      (begin
                        (wm 782 "user check failed for ~s" u)
                        (format o "** Authentication failed\nBye.\n")
                        #f)))))))))
              
    
(define (handle-console-connection (self <console-connection>) login-hook)
  (let ((auth (if (realm self)
                  (get-console-login self)
                  #t)))
    (if auth
        (begin
          (set-authentication! self auth)
          (note 708 "user authenticated: ~s" auth)
          (login-hook self
                      (lambda (#key (environment 
                                     default: (get-default-envt self)))
                        (handle-console-interaction 
                         self
                         environment)))))))

(define (handle-console-interaction (self <console-connection>) envt)
  (let ((i (input-port (telnet-session self)))
        (o (output-port (telnet-session self)))
        (tt (telnet-get-terminal-type (telnet-session self))))
    (note 702 "terminal type ~s" tt)
    (format o "\n(terminal type is ~s)\n" tt)
    ;;
    (for-each (lambda (env)
                (format o "  env.~s[~s] = ~s\n"
                        (caar env)
                        (cadar env)
                        (cadr env)))
              (telnet-get-environment (telnet-session self)))
    ;;
    (telnet-repl self envt)))

(define (get-default-envt (self <console-connection>))
  (top-level-envt (get-module 'user)))

(define (telnet-repl (self <console-connection>) envt)
  (with-edit-port
   (input-port (telnet-session self))
   (output-port (telnet-session self))
   (output-port (telnet-session self))
   (lambda ()
     (note 709 "cmdloop started")
     (cmd-loop envt "telnet[~d]=>"))))


#|

    (let loop ((user #f))
      (format o "? ")
      (flush-output-port o)
      (let ((l (read i)))
        (format #t "console => ~s\n" l)
        (if (string? l)
            (let ((l (car (string-split l #\cr))))
              (if (member l '("quit" "exit" "bye" "logout"))
                  (if user
                      (format o "bye, ~a.\n" user)
                      (format o "bye.\n" user))
                  (if user
                      (begin
                        (format o "ok, ~a.\n" user)
                        (loop user))
                      (begin
                        (format o "hi, ~a.\n" l)
                        (loop l))))))))))
|#
