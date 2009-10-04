,(use rs.sys.threads.manager
      syscalls
      rs.util.properties
      regex
      tables)

(define *channel-regex* (reg-expr->proc '(entire (seq (or #\# #\&)
                                                      (+ (not 
                                                          (or #\space
                                                              #\bel
                                                              #\nul
                                                              #\cr
                                                              #\lf
                                                              #\,)))))))

(define *nickname-regex* (reg-expr->proc '(entire (seq alpha
                                                       (* (or alpha
                                                              digit
                                                              #\-
                                                              #\[
                                                              #\]
                                                              #\\
                                                              #\`
                                                              #\^
                                                              #\{
                                                              #\}))))))


(define (valid-channel-name? str) (and (*channel-regex* str) #t))
(define (valid-nick-name? str) (and (*nickname-regex* str) #t))

(load "numerics.scm")

(define (writev+ port vec)
  (let ((f (open-output-string)))
    (writev f vec)
    (let ((s (get-output-string f)))
      (format #t "<<< ~a\n" (substring s 0 (- (string-length s) 2)))
      (writev port vec))))

(define-class <irc-network> (<object>)
  (properties init-value: '#() type: <vector>)
  (name type: <string>)
  (channel-index type: <string-table>)
  (nickname-index type: <string-table>)
  (server-index type: <string-table>))

(define-class <irc-server> (<object>)
  (properties init-value: '#() type: <vector>)
  (name type: <string>)
  (container type: <irc-network>)
  (next-hop init-value: #f)
  (connections type: <object-table> init-function: make-object-table))

(define-thread-var *irc-network* #f)
(define-thread-var *irc-server* #f)

(define (make-irc-network name)
  (make <irc-network>
        name: name
        server-index: (make-string-table)
        channel-index: (make-string-table)
        nickname-index: (make-string-table)))

(set! *irc-network* (make-irc-network "RSchemeNet"))

;;;

(define-class <irc-user> (<object>)
  (properties init-value: '#() type: <vector>)
  (nickname type: <string>)
  (server type: <irc-server>)
  (socket init-value: #f)
  (channels type: <list> init-value: '())
  (username type: <string> init-value: "")
  (hostname type: <string> init-value: "")
  (realname type: <string> init-value: ""))


(define-class <irc-channel> (<object>)
  (properties init-value: '#() type: <vector>)
  (name type: <string>)
  (operators type: <list> init-value: '())
  (members type: <list> init-value: '())
  (ctime type: <time>)
  (mtime type: <time>)
  (mode-private? type: <boolean> init-value: #f)
  (mode-secret? type: <boolean> init-value: #f)
  (mode-invite-only? type: <boolean> init-value: #f)
  (mode-topic-protect? type: <boolean> init-value: #f)
  (mode-moderated? type: <boolean> init-value: #f)
  (mode-inside-only? type: <boolean> init-value: #t)
  (password init-value: #f))


(define (get-channel channel)
  (let ((ch (table-lookup (channel-index *irc-network*) channel)))
    (or ch
        (let* ((t (time))
               (ch (make <irc-channel>
                         ctime: t
                         mtime: t
                         name: channel)))
          (table-insert! (channel-index *irc-network*) channel ch)
          ch))))

(define-class <irc-connection> (<object>)
  (properties init-value: '#() type: <vector>)
  socket
  start-time
  last-time
  input-port
  (farside init-value: #f))     ; an <irc-user> or an <irc-server>

;;

(define ircmsg (reg-expr->proc '(seq
                                 (? (save (seq #\: (+ (not #\space)) 
                                               (+ #\space))))
                                 (save (or (+ alpha)
                                           (seq digit digit digit))))))

(define ircparm (reg-expr->proc '(seq (+ #\space)
                                      (or (seq #\: (save (* (not (or #\nul
                                                                     #\cr
                                                                     #\lf)))))
                                          (save (seq (not #\:)
                                                     (* (not (or #\space
                                                                 #\nul
                                                                 #\cr
                                                                 #\lf)))))))))

(define (crack-irc-parms str k)
  (let ((q (make-dequeue)))
    (let loop ((k k))
      (bind ((s e trailing middle (ircparm str k)))
        (if s
            (if trailing
                (begin
                  (dequeue-push-back! q trailing)
                  (dequeue-state q))
                (begin
                  (dequeue-push-back! q middle)
                  (loop e)))
            (dequeue-state q))))))
            
(define (parse-irc-message line)
  (bind ((s e prefix cmd (ircmsg line))
         (parms (crack-irc-parms line e)))
    (values prefix cmd parms)))


#|
(define-class <irc-message> (<object>)
  ...)

(read-line (input-port c))

(define (read-irc-message sock)
  (input-port-read-max (input-port sock))
|#

;;;

(define-class <irc-message-input-port> (<buffered-input-port>)
  (underlying-input-port type: <input-port>))

(define-method provide-more-input ((self <irc-message-input-port>))
  (input-port-read-max (underlying-input-port self) 512))

(define-method more-input-ready? ((self <irc-message-input-port>))
  (more-input-ready? (underlying-input-port self)))

(define-method input-port-read-line ((self <irc-message-input-port>))
  (let loop ((try 0))
    (bind ((i (buffered-input-posn self))
           (b (buffered-input-buffer self))
           (s e (cr-or-lf b i)))
      (if s
          (begin
            (set-buffered-input-posn! self e)
            (substring (buffered-input-buffer self) i s))
          (if (= try 0)
              (let ((more (provide-more-input self)))
                (if more
                    (begin
                      (set-buffered-input-posn! self 0)
                      (set-buffered-input-buffer! 
                       self
                       (string-append (substring b i) more))
                      (loop (add1 try)))
                    (begin
                      (set-buffered-input-posn! self 0)
                      (set-buffered-input-buffer! self "")
                      b)))
              (error "Could not read message within message limit"))))))

(define cr-or-lf (reg-expr->proc '(+ (or #\nul #\cr #\lf))))

;;;

(define *all-connections* '())

(define (ircd)
  (thread-resume (make-thread (lambda ()
                                (run-ircd server-name: "irc1.rscheme.com"
                                          port: 6666))
                              "ircd(aa)"))
  (thread-resume (make-thread (lambda ()
                                (run-ircd server-name: "irc2.rscheme.com"
                                          port: 6667))
                              "ircd(bb)")))

(define (run-ircd #key server-name port)
  (let ((s (open-server-socket port))
        (srv (make <irc-server>
                   name: server-name
                   container: *irc-network*)))
    (table-insert! (server-index *irc-network*) server-name srv)
    ;;
    (thread-let ((*irc-server* srv))
      (let loop ()
        (new-ircd-connection (accept-client s))
        (loop)))))

(define (new-ircd-connection c)
  (let* ((t (time))
         (x (make <irc-connection>
                  socket: c
                  input-port: (make <irc-message-input-port> 
                                    underlying-input-port: (input-port c))
                  last-time: t
                  start-time: t))
         (s *irc-server*))
    (table-insert! (connections s) x x)
    ;;
    (thread-resume
     (make-thread
      (lambda ()
        ;;
        (handler-case
         (process-irc-connection x)
         ((<condition> condition: e)
          (format #t "~a FAILED: ~a\n" (peer c) e)))
        ;;
        (close c)
        (table-remove! (connections s) x))
      (~ "irc//~a" (to-string (peer c)))))))

(define (rats c)
  (write-string (output-port (socket c)) 
                ":localhost 403 :localhost: No such channel\r\n")
  (flush-output-port (output-port (socket c))))

(define (motd c)
  (let ((p (output-port (socket c)))
        (me (name *irc-server*)))
    ;;
    (RPL_MOTDSTART p me: me)
    (for-each
     (lambda (l)
       (RPL_MOTD p me: me line: l))
     '("This is a message"
       "of the day!"))
    (RPL_ENDOFMOTD p me: me)
    (flush-output-port p)))

(define (process-irc-connection (c <irc-connection>))
  (let loop ()
    (bind ((line (read-line (input-port c))))
      (format #t ">>> ~a\n" line)
      (if (string? line)
          (bind ((prefix cmd parms (parse-irc-message line)))
            (let ((fn (table-lookup *irc-command-table* cmd)))
              (set-last-time! c (time))
              (if fn
                  ((value fn) c prefix parms)
                  (format #t "unknown ~s ~s ~s\n" prefix cmd parms))
              (loop)))))))
          

(define (irc-set-password (self <irc-connection>) prefix parms)
  (set-property! self 'password (vector-ref parms 0)))

(define (irc-set-nickname (self <irc-connection>) prefix parms)
  (let ((nick (vector-ref parms 0))
        (p (output-port (socket self))))
    ;;
    (if (not (farside self))
        (let ((u (make <irc-user>
                       nickname: nick
                       socket: (socket self)
                       server: *irc-server*
                       hostname: (to-string (peer (socket self))))))
          (set-farside! self u)
          (table-insert! (nickname-index *irc-network*) nick u)))
    ;;
    (set-property! self 'nick-name nick)))

;; USER donovan star.westgate.xynthesis.com irc1.rscheme.com :Donovan Kolbly

(define (irc-set-username (self <irc-connection>) prefix parms)
  (let (((u <irc-user>) (farside self)))
    ;;
    (set-username! u (vector-ref parms 0))
    (set-hostname! u (vector-ref parms 1))
    (set-realname! u (vector-ref parms 3))

    (set-property! self 'user-name (vector-ref parms 0))
    (set-property! self 'host-name (vector-ref parms 1))
    (set-property! self 'server-name (vector-ref parms 2))
    (set-property! self 'real-name (vector-ref parms 3))
    ;;
    (motd self)))

(define (irc-join (self <irc-connection>) prefix parms)
  (let* ((channel (vector-ref parms 0))
         (ch (get-channel channel))
         (p (output-port (socket self)))
         (me (name *irc-server*))
         (u (farside self)))
    ;;
    (if (null? (operators ch))
        (set-operators! ch (list u)))
    (set-members! ch (cons u (members ch)))
     ;;
    ;; JOIN
    (format p ":~a!~~~a@~a JOIN :~a\r\n"
            (get-property self 'nick-name)
            (get-property self 'user-name)
            (get-property self 'host-name)
            channel)
    ;; RPL_TOPIC
    (format p ":localhost 332 ~a :The topic is open for debate\r\n"
            channel)
    ;; RPL_NAMREPLY
    (RPL_NAMREPLY p 
                  me: me 
                  nickname: (nickname u)
                  pub-or-secret: (if (mode-secret? ch) "@" "=")
                  channel: (name ch)
                  members: (string-join 
                            #\space
                            (map (lambda ((u <irc-user>))
                                   (string-append
                                    (if (memq u (operators ch)) "@" "")
                                    (nickname u)))
                                 (members ch))))
    (RPL_ENDOFNAMES p 
                    me: me 
                    nickname: (nickname u)
                    channel: (name ch))
    ;;
    (flush-output-port p)))

(define (irc-ping (self <irc-connection>) prefix parms)
  (values))

(define (irc-private-message (self <irc-connection>) prefix parms)
  (format #t "MESSAGE ~s\n" parms))

(define (irc-quit (self <irc-connection>) prefix parms)
  (format #t "BYE\n"))


(define (irc-who (self <irc-connection>) prefix parms)
  (bind ((search op (vector->values parms))
         (p (output-port (socket self)))
         (me (name *irc-server*)))
    ;;
    (for-each
     (lambda (u)
       (let ((is-channel-op? #t)
             (has-voice? #t))
         (RPL_WHOREPLY p 
                       me: me
                       you: (get-property self 'nick-name)
                       channel: search
                       username: (get-property u 'user-name)
                       hostname: (get-property u 'host-name)
                       servername: (get-property u 'server-name)
                       nickname: (get-property u 'nick-name)
                       status: (string-append
                                (if (get-property u 'away? #f) "G" "H")
                                (if (get-property u 'oper? #f) "*" "")
                                (cond
                                 (is-channel-op? "@")
                                 (has-voice? "+")
                                 (else "")))
                       hopcount: 0
                       realname: (get-property u 'real-name))))
     (list self))
    ;;
    (RPL_ENDOFWHO p 
                  me: me
                  name: search
                  mask: "*")
    ;;
    (flush-output-port p)))

(define (irc-mode (self <irc-connection>) prefix parms)
  (if (char=? (string-ref (vector-ref parms 0) 0) #\#)
      (irc-mode/channel self prefix parms)))

(define (inform-mode sender recipient ch)
  (let ((p (output-port (socket recipient))))
    ;;
    (MODE:SET p
              me: (name sender)
              subject: (name ch)
              set: (string-append
                    (if (mode-private? ch) "p" "")
                    (if (mode-secret? ch) "s" "")
                    (if (mode-invite-only? ch) "i" "")
                    (if (mode-topic-protect? ch) "t" "")
                    (if (mode-moderated? ch) "m" "")
                    (if (mode-inside-only? ch) "n" "")))
    (flush-output-port p)))
  
                     
(define (irc-mode/channel (self <irc-connection>) prefix parms)
  (let ((ch (get-channel (vector-ref parms 0)))
        (p (output-port (socket self))))
    ;;
    (if (> (vector-length parms) 1)
        (let ((modes (string->list (substring (vector-ref parms 1) 1)))
              (set (eq? (string-ref (vector-ref parms 1) 0) #\+)))
          (for-each
           (lambda (s)
             (case s
               ((#\p) (set-mode-private?! ch #t))
               ((#\s) (set-mode-secret?! ch #t))
               ((#\i) (set-mode-invite-only?! ch #t))
               ((#\t) (set-mode-topic-protect?! ch #t))
               ((#\m) (set-mode-moderated?! ch #t))
               ((#\n) (set-mode-inside-only?! ch #t))))
           set)
          (for-each (lambda (m)
                      (inform-mode (farside self) m ch))
                    (members ch)))
        (inform-mode *irc-server* (farside self) ch))))

(define *irc-command-table* (make-string-ci-table))

(table-insert! *irc-command-table* "PASS" (& irc-set-password))
(table-insert! *irc-command-table* "NICK" (& irc-set-nickname))
(table-insert! *irc-command-table* "USER" (& irc-set-username))
(table-insert! *irc-command-table* "JOIN" (& irc-join))
(table-insert! *irc-command-table* "PING" (& irc-ping))
(table-insert! *irc-command-table* "PRIVMSG" (& irc-private-message))
(table-insert! *irc-command-table* "QUIT" (& irc-quit))
(table-insert! *irc-command-table* "WHO" (& irc-who))
(table-insert! *irc-command-table* "MODE" (& irc-mode))

#|
(table-lookup (server-index *irc-network*) "irc1.rscheme.com")
(car (value-sequence (connections %)))

(define pp (output-port (socket %)))

(format pp ":alice JOIN #foo\r\n")
(flush-output-port pp)

(format pp ":@sam JOIN #foo\r\n")
(flush-output-port pp)

(format pp ":+bob JOIN #foo\r\n")
(flush-output-port pp)

(format pp ":donovan JOIN #foo\r\n")
(flush-output-port pp)

(format pp ":irc1.rscheme.com MODE #foo -t\r\n")
(flush-output-port pp)

(format pp ":irc1.rscheme.com MODE #foo +nt\r\n")
(flush-output-port pp)

(format pp ":alice PRIVMSG #foo :Howdy back atcha\r\n")
(flush-output-port pp)

(let ((ch (get-channel "#foo"))
      (u (make <irc-user>
               nickname: "alice"
               server: (table-lookup (server-index *irc-network*) 
                                     "irc1.rscheme.com"))))
  (set-members! ch (cons u (members ch))))

|#

