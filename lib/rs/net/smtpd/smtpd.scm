#|
        http://www.networksorcery.com/enp/protocol/smtp.htm
|#

;;;

(define-class <smtp-message> (<object>)
  (properties init-value: '#())
  (envelope-sender type: <string>)
  (envelope-recipients type: <list>)
  (content type: <vector>))

(define-class <smtp-endpoint> (<object>)
  (properties init-value: '#())
  (name type: <string>)
  (reject-message init-value: #f))      ; e.g., ((551 5 1 6) "no fwd address")

(define-class <smtp-domain> (<object>)
  (properties init-value: '#())
  (name type: <string>)
  users)

(define-class <smtp-server> (<object>)
  name
  port
  rcpt-domains
  (smtp-realm init-value: #f)
  (smtp-auth-required? init-value: #f)
  (injector init-value: #f))

(define-thread-var *smtp-server* #f)

(define-class <smtp-client-connection> (<object>)
  socket
  start-time
  last-time
  input-port
  (envelope-sender init-value: #f)
  (envelope-recipients init-value: '())
  (auth-user init-value: #f)
  (exit-hook init-value: #f)
  (peer-hello init-value: #f))

(define (clear-envelope! (self <smtp-client-connection>))
  (set-envelope-sender! self #f)
  (set-envelope-recipients! self '()))
  
(define (new-smtp-connection sock)
  (let* ((t (time))
         (p (make <bounded-line-input-port>
                  underlying-input-port: (input-port sock)))
         (c (make <smtp-client-connection>
                  socket: sock
                  start-time: t
                  last-time: t
                  input-port: p)))
    (thread-resume
     (make-thread
      (lambda ()
        (handler-case
         (process-smtp-connection c)
         ((<condition> condition: e)
          (format #t "~a FAILED: ~a\n" (peer sock) e)))
        (close sock))
      (~ "smtp:~a" (to-string (peer sock)))))))

(define (run-smtp-server (self <smtp-server>))
  (thread-let ((*smtp-server* self))
    (let ((s (open-server-socket (port self))))
      ;;
      (let loop ()
        (new-smtp-connection (accept-client s))
        (loop)))))

(define smtp-request-pattern (reg-expr->proc '(entire
                                               (seq
                                                (save (+ alpha))
                                                (? (seq
                                                    #\space
                                                    (save (+ any))))))))

(define (read-request cnx)
  (bind ((l (read-line (input-port cnx))))
    (if (string? l)
        (bind ((s e verb parm (smtp-request-pattern l)))
          (format #t "<<< ~a\n" l)
          (values verb parm))
        (values))))

;;; `code' is the core + extended status code,
;;; e.g.,  '(250 2 1 0)

(define (send-response cnx (code <list>) (lines <vector>))
  (assert (> (vector-length lines) 0))
  ;;
  (let ((port (output-port (socket cnx)))
        (pre (~ "~03d" (car code)))
        (n (vector-length lines)))
    ;;
    (let loop (((i <fixnum>) 0))
      (let ((l (if (and (eq? i 0) (pair? (cdr code)))
                   (string-append
                    (string-join #\. (map number->string (cdr code)))
                    " "
                    (vector-ref lines i))
                   (vector-ref lines i)))
            ((i <fixnum>) (add1 i)))
        ;;
        (if (eq? i n)
            (begin
              (format #t ">>> ~a ~a\n" pre l)
              (format port "~a ~a\r\n" pre l)
              (flush-output-port port))
            (begin
              (format #t ">>> ~a-~a\n" pre l)
              (format port "~a-~a\r\n" pre l)
              (loop i)))))))

(define (respond cnx (code <list>) fmt . args)
  (send-response cnx code (vector (apply format #f fmt args))))


(define *smtp-verb-table* (make-string-ci-table))

(define (process-smtp-connection (self <smtp-client-connection>))
  (respond self '(220) "~a NO UCE ESMTP" (name *smtp-server*))
  (call-with-current-continuation
   (lambda (exit)
     (set-exit-hook! self exit)
     ;;
     (let loop ()
       (bind ((verb parm (read-request self)))
         (if verb
             (let ((exec (table-lookup *smtp-verb-table* verb)))
               ;(format #t "Request ~s: ~s\n" verb exec)
               (if exec
                   ((value exec) self parm)
                   (respond self '(500 5 5 2) "syntax error"))
               (loop))
             (format #t "Unexpected client shutdown\n")))))))

(define (process-helo (self <smtp-client-connection>) parm)
  (send-response self '(250) (vector (name *smtp-server*)))
  (format #t "HELO <~a>\n" parm)
  (set-peer-hello! self (list "HELO" parm))
  (clear-envelope! self))

(define *extended-smtp-features* '())

(define (register-extended-smtp-feature name ehlo commands)
  (cond
   ((assoc name *extended-smtp-features*)
    => (lambda (m)
         (set! *extended-smtp-features* (delq m *extended-smtp-features*)))))
  ;;
  (set! *extended-smtp-features* (cons (list name ehlo)
                                       *extended-smtp-features*))
  (for-each
   (lambda (cmd)
     (table-insert! *smtp-verb-table* (car cmd) (cadr cmd)))
   commands)
  (values))

(define (process-ehlo (self <smtp-client-connection>) parm)
  (set-peer-hello! self (list "EHLO" parm))
  (send-response self
                 '(250)
                 (vector-append 
                  (vector (name *smtp-server*))
                  '#("ENHANCEDSTATUSCODES"
                     "PIPELINING"
                     "VRFY"
                     "8BITMIME")
                  (list->vector 
                   (map cadr *extended-smtp-features*))))
  (clear-envelope! self))

(define (process-noop (self <smtp-client-connection>) parm)
  (respond self '(250 2 0 0) "ok"))

(define (process-rset (self <smtp-client-connection>) parm)
  (respond self '(250 2 0 0) "ok")
  (clear-envelope! self))

(define (process-quit (self <smtp-client-connection>) parm)
  (respond self '(221 2 0 0) "~a saying goodbye" (name *smtp-server*))
  ((exit-hook self)))

(define mail-from-pattern (reg-expr->proc '(prefix
                                            (seq (or #\F #\f)
                                                 (or #\R #\r)
                                                 (or #\O #\o)
                                                 (or #\M #\m)
                                                 #\:))))

(define mail-to-pattern (reg-expr->proc '(prefix
                                          (seq (or #\T #\t)
                                               (or #\O #\o)
                                                 #\:))))

(define (process-mail (self <smtp-client-connection>) parm)
  (call-with-current-continuation
   (lambda (return)
     ;;
     (if (and (smtp-auth-required? *smtp-server*)
              (not (auth-user self)))
         (begin
           (respond self '(530 5 7 1) "SMTP-AUTH required")
           (return)))
     ;;
     (bind ((s e (mail-from-pattern parm)))
       (if e
           (bind ((addr e (parse-encoded-box-part parm e))
                  (extra (if e (trim-whitespace (substring parm e)) "")))
             (if (or (not addr)
                     (not (valid-email-address? addr)))
                 (begin
                   (respond self '(501 5 1 7)
                            "malformatted address; try <foo@bar.com>")
                   (return)))
             ;;
             (format #t "\tMAIL ADDR: ~s\n" addr)
             ;;
             (if (> (string-length extra) 0)
                 (begin
                   (format #t "\tEXTRA: ~s\n" extra)
                   (if (not (or (string-ci=? extra "BODY=7BIT")
                                (string-ci=? extra "BODY=8BITMIME")))
                       (begin
                         (respond self '(555 5 5 2)
                                  "junk after envelope address not permitted")
                         (return)))))
             ;;
             (set-envelope-sender! self addr)
             (respond self '(250 2 1 0) "originator <~a> ok" addr))
           (respond self '(501 5 5 2)
                    "malformatted address; try FROM:<...>"))))))

(define (process-vrfy (self <smtp-client-connection>) parm)
  (call-with-current-continuation
   (lambda (return)
     (bind ((addr e (parse-encoded-box-part parm 0))
            (extra (if e (trim-whitespace (substring parm e)) "")))
       (if (or (not addr)
               (not (valid-email-address? addr)))
           (begin
             (respond self '(501 5 1 3) "malformed address; try <foo@bar.com>")
             (return)))
       ;;
       (if (> (string-length extra) 0)
           (begin
             (respond self '(501 5 5 2) "junk after address not permitted")
             (return)))
       ;;
       (bind ((a relay? (validate-recipient addr self return)))
         (if relay?
             (respond self '(551 5 1 2) "recipient ~s is not a local user" a)
             (respond self '(250) "~a" a)))))))

  
(define (process-rcpt (self <smtp-client-connection>) parm)
  (call-with-current-continuation
   (lambda (return)
     (if (not (envelope-sender self))
         (begin
           (respond self '(503 5 5 1) "need MAIL before RCPT")
           (return)))
     ;;
     (bind ((s e (mail-to-pattern parm)))
       (if e
           (bind ((addr e (parse-encoded-box-part parm e))
                  (extra (if e (trim-whitespace (substring parm e)) "")))
             (if (or (not addr)
                     (not (valid-email-address? addr)))
                 (begin
                   (respond self '(501 5 1 3) 
                            "malformatted address; try <foo@bar.com>")
                   (return)))
             ;;
             ;(format #t "\tRCPT ADDR: ~s\n" addr)
             ;;
             (if (> (string-length extra) 0)
                 (begin
                   ;(format #t "\tEXTRA: ~s\n" extra)
                   (if (not (or (string-ci=? extra "BODY=7BIT")
                                (string-ci=? extra "BODY=8BITMIME")))
                       (begin
                         (respond self '(555 5 5 2)
                                  "junk after envelope address not permitted")
                         (return)))))
             ;;
             (bind ((rcpt relay? (validate-recipient addr self return)))
               (set-envelope-recipients! 
                self 
                (cons rcpt (envelope-recipients self)))
               ;;
               (if relay?
                   (respond self '(250 2 1 5) "recipient ~a ok" rcpt)
                   (respond self '(250 2 1 5) "recipient ~a ok" rcpt))))
           (respond self '(501 5 5 2)
                    "malformatted address; try TO:<...>"))))))

(define-generic-function smtp-user-relay-ok?)

(define (validate-recipient (addr <string>) self return)
  (bind ((k (string-search addr #\@))
         (user domain (if k
                          (values (substring addr 0 k)
                                  (substring addr (+ k 1)))
                          (values addr (name *smtp-server*)))))
    ;;
    (cond
     ((table-lookup (rcpt-domains *smtp-server*) domain)
      => (lambda ((d <smtp-domain>))
           (cond
            ((table-lookup (users d) user)
             => (lambda ((endpoint <smtp-endpoint>))
                  (if (reject-message endpoint)
                      (begin
                        (send-response self
                                       (car (reject-message endpoint))
                                       (cadr (reject-message endpoint)))
                        (return))
                      (values addr #f))))
            (else
             (respond self '(550 5 1 1) "mailbox ~s does not exist" addr)
             (return)))))
     (else
      (if (and (auth-user self)
               (smtp-user-relay-ok? (auth-user self)))
          (values addr #t)
          (begin
            (respond self '(551 5 7 1) "relay service not available")
            (return)))))))

(define (process-data (self <smtp-client-connection>) parm)
  (if parm
      (respond self '(501 5 5 2) "no parameters allowed on DATA")
      (if (null? (envelope-recipients self))
          (respond self '(503 5 5 1) "need RCPT before DATA")
          (begin
            (respond self '(354) "go ahead")
            (process-encoded-message self)))))

(define (process-encoded-message (self <smtp-client-connection>))
  (let ((q (make-dequeue)))
    (let loop ()
      (let ((l (read-line (input-port self))))
        (cond
         ((not (string? l))
          (format #t "unexpected disconnect during DATA\n")
          ((exit-hook self)))
         ;;
         ((string=? l ".")
          (inject
           self
           (make <smtp-message>
                 properties: (vector 'server *smtp-server*
                                     'hello (peer-hello self)
                                     'peer (peer (socket self)))
                 envelope-sender: (envelope-sender self)
                 envelope-recipients: (envelope-recipients self)
                 content: (dequeue-state q)))
          (clear-envelope! self))
         ;;
         (else
          (if (and (> (string-length l) 0)
                   (char=? (string-ref l 0) #\.))
              (dequeue-push-back! q (substring l 1))
              (dequeue-push-back! q l))
          (loop)))))))

(table-insert! *smtp-verb-table* "EHLO" (& process-ehlo))
(table-insert! *smtp-verb-table* "HELO" (& process-helo))
(table-insert! *smtp-verb-table* "RSET" (& process-rset))
(table-insert! *smtp-verb-table* "NOOP" (& process-noop))
(table-insert! *smtp-verb-table* "MAIL" (& process-mail))
(table-insert! *smtp-verb-table* "RCPT" (& process-rcpt))
(table-insert! *smtp-verb-table* "DATA" (& process-data))
(table-insert! *smtp-verb-table* "QUIT" (& process-quit))
(table-insert! *smtp-verb-table* "VRFY" (& process-vrfy))

(define (parse-encoded-box-part (str <string>) (i <fixnum>))
  (let ((p (open-input-string str))
        (q (open-output-string)))
    ;;
    (define (err why)
      (format #t "ERROR: ~a\n" str)
      (format #t "       ~a^ ~a\n"
              (make-string (max 0 (- (buffered-input-posn p) 1)) #\space)
              why)
      (values))
    ;;
    (set-buffered-input-posn! p i)
    ;;
    (letrec ((initial (lambda ()
                        (let ((ch (read-char p)))
                          (cond
                           ((eof-object? ch)
                            (err "EOF in initial"))
                           ((char=? ch #\<)
                            (skiproute))
                           (else
                            (err "Missing open '<'"))))))
             ;;
             (skiproute (lambda ()
                          (let ((ch (peek-char p)))
                            (cond
                             ((eof-object? ch)
                              (err "EOF in skiproute"))
                             ((char=? ch #\@)
                              (let loop ()
                                (let ((ch (read-char p)))
                                  (if (eof-object? ch)
                                      (err "EOF in route")
                                      (if (char=? ch #\:)
                                          (main)
                                          (loop))))))
                             (else
                              (main))))))
             ;;
             (main (lambda ()
                     (let ((ch (read-char p)))
                       (cond
                        ((eof-object? ch)
                         (err "EOF in main"))
                        ((char=? ch #\\)
                         (write-char (read-char p) q)
                         (main))
                        ((char=? ch #\")
                         (inquote))
                        ((char=? ch #\>)
                         (values (get-output-string q)
                                 (buffered-input-posn p)))
                        (else
                         (write-char ch q)
                         (main))))))
             (inquote (lambda ()
                        (let ((ch (read-char p)))
                          (cond
                           ((eof-object? ch)
                            (err "EOF in quoted part"))
                           ((char=? ch #\\)
                            (write-char (read-char p) q)
                            (inquote))
                           ((char=? ch #\")
                            (main))
                           (else
                            (write-char ch q)
                            (inquote)))))))
      ;;
      (initial))))

(define-class <smtpd-error> (<condition>)
  (code type: <list>)
  (message type: <vector>))

(define (inject (self <smtp-client-connection>) (msg <smtp-message>))
  (let ((f (injector *smtp-server*)))
    (if f
        (handler-case
         (let ((token (f msg)))
           (respond self '(250 2 6 0) "ok ~a" token))
         ((<smtpd-error> condition: c)
          (send-response self (code c) (message c)))
         ((<condition> condition: c)
          (format #t "***ERROR*** ~a\n" c)
          (respond self '(554 5 3 0) "internal server error")))
        (respond self '(570 5 3 5) "this server does not accept mail"))))
