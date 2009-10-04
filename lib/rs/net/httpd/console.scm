
(define (start-console listen realm)
  (let ((s (open-server-socket (parse-socket-spec listen))))
    (thread-resume
     (make-thread
      (lambda ()
        (console s realm))
      "consoled"))))

(define-class <console-connection> (<object>)
  realm
  socket
  (io-port init-value: #f)
  (user init-value: #f)
  (authentication init-value: #f)
  telnet-session)
  
(define (console s realm)
  (let loop ()
    (let ((c (accept-client s)))
      (format #t "Console client: ~s\n" c)
      (thread-resume
       (make-thread
        (lambda ()
          (handler-case
           (handle-console-connection
            (make <console-connection>
                  realm: realm
                  socket: c
                  telnet-session: (open-telnet-session c)))
           ((<condition> condition: e)
            (format #t "Console died: ~a\n" e)))
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
        ;; tell the other side we're going to echo; hopefully,
        ;; then they won't echo the password
        (telnet-declare-option (telnet-session self) 'echo #t)
        (format o "password: ")
        (flush-output-port o)
        (let ((p (read-line i)))
          (telnet-declare-option (telnet-session self) 'echo #f)
          (format #t "authentication ~s ~s\n" u p)
          (let ((auth (authenticate (realm self) u p)))
            (or auth
                (begin
                  (format o "\n")
                  (if (< tries 3)
                      (loop (+ tries 1))
                      (begin
                        (format o "*** Authentication failed\nBye\n.")
                        #f))))))))))

(define (handle-console-connection (self <console-connection>))
  (let ((auth (get-console-login self)))
    (if auth
        (begin
          (set-authentication! self auth)
          (handle-console-interaction self)))))

(define (handle-console-interaction (self <console-connection>))
  (let ((i (input-port (telnet-session self)))
        (o (output-port (telnet-session self)))
        (tt (telnet-get-terminal-type (telnet-session self))))
    (format o "\n(terminal type is ~s)\n" tt)
    ;;
    (for-each (lambda (env)
                (format o "  env.~s[~s] = ~s\n"
                        (caar env)
                        (cadar env)
                        (cadr env)))
              (telnet-get-environment (telnet-session self)))
    ;;
    (telnet-repl self)))

(define (telnet-repl (self <console-connection>))
  (let ((in-module (get-module 'user)))
    (with-edit-port
     (input-port (telnet-session self))
     (output-port (telnet-session self))
     (output-port (telnet-session self))
     (lambda ()
       (cmd-loop (top-level-envt in-module) "telnet[~d]=>")))))


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
