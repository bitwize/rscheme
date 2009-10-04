;;;
;;;   A POP server for RScheme
;;;

(define-message-table rs.net.popd 544)

(define-generic-function get-pop-mailbox)

(define (make-pop-token)
  (string-append (to-string (make-uuid)) "@" (hostname)))

(define-class <pop3-session> (<object>)
  socket
  (state init-value: 'authorization)
  (token init-function: make-pop-token)
  (mailbox type: <vector> init-value: '#())
  (realm type: <auth-realm>)
  (authdest init-value: #f))

(define-class <normal-quit> (<condition>))


(define (terminate (self <pop3-session>))
  (signal (make <normal-quit>)))


(define (pop-server #key 
                    (port default: 110)
                    (log default: #f)
                    (authdest default: #f)
                    (realm type: <auth-realm>))
  (let ((sock (open-server-socket port)))
    (if log
        (with-message-dest
         (log-file-message-dest log)
         (lambda ()
           (note 300 "Start on ~a" port)
           (pop-server* sock realm authdest)))
        (begin
          (note 301 "Start on ~a" port)
          (pop-server* sock realm authdest)))))

(define (pop-server* sock realm authdest)
  (let loop ()
    (let ((c (accept-client sock)))
      (note 402 "Connection from ~s" (peer c))
      (thread-resume
       (make-thread
        (lambda ()
          (handler-case
           (pop-session (make <pop3-session> 
                              socket: c
                              realm: realm
                              authdest: authdest))
           ((<normal-quit>)
            (note 409 "Normal QUIT"))
           ((<condition> condition: e)
            (wm 408 "*** Failed: ~a" e)))
          (close c))
        "popd"))
      (loop))))

(define (send (self <pop3-session>) msg . args)
  (let ((str (apply format #f msg args))
        (p (output-port (socket self))))
    ;(dm 600 "<< ~a" str)
    (write-string p str)
    (write-string p "\r\n")
    (flush-output-port p)))

(define (send-stuffed (self <pop3-session>) (line <string>))
  (if (and (> (string-length line) 0)
           (char=? (string-ref line 0) #\.))
      (send self ".~a" line)
      (send self "~a" line)))

(define (read-cmd (self <pop3-session>))
  (let ((cmd (read-line (input-port (socket self)))))
    (if (eof-object? cmd)
        (error "Unexpected EOF"))
    (if (and (> (string-length cmd) 1)
             (char=? (string-ref cmd (- (string-length cmd) 1))
                     #\cr))
        (set! cmd (substring cmd 0 (- (string-length cmd) 1))))
    (dm 700 ">> ~a" cmd)
    (bind ((s e kwd args (match-command cmd)))
      (dm 701 "client command: ~s args: ~s" kwd args)
      (values kwd
              (if args 
                  (string-split args spaces)
                  '())))))

(define (pop-session (self <pop3-session>))
  ;; send the greeting
  (send self "+OK POP3 server ready <~a>" (token self))
  (let loop ()
    (bind ((cmd args (read-cmd self)))
      (let ((c (table-lookup *pop-cmd-table* cmd)))
        (if c
            (if (pair? c)
                (if (not (memq (state self) (car c)))
                    (send self "-ERR wrong state")
                    (apply (value (cdr c)) self args))
                (apply (value c) self args))
            (send self "-ERR what? ~s" cmd))
        (loop)))))

(define (quit-cmd (self <pop3-session>))
  (let ((n 0))
    (vector-for-each
     (lambda (m)
       (if (deleted? m)
           (begin
             (commit-deletion m)
             (set! n (+ n 1)))))
     (mailbox self))
    (note 711 "Quit (~d deleted)" n)
    (send self "+OK (~d deleted)" n)
    (terminate self)))
  
(define match-command
  (reg-expr->proc '(seq (save (+ (or digit alpha)))
                        (? (seq (+ #\space)
                                (save (+ (seq (* space)
                                              (+ (not space)))))))
                        (* space))))

(define spaces (reg-expr->proc '(+ space)))

;;;

(define-class <pop3-user> (<object>)
  (properties type: <vector> init-value: '#())
  name)

(define-generic-function password ((self <pop3-user>)))
(define-generic-function get-mailbox ((self <pop3-user>)))

(define-class <mailbox-entry> (<object>) :abstract
  (id type: <fixnum> init-value: 0)
  (deleted? init-value: #f))

;(define-generic-function size ((self <mailbox-entry>)))
(define-generic-function mtime ((self <mailbox-entry>)))
(define-generic-function uid ((self <mailbox-entry>)))

(define-generic-function commit-deletion ((self <mailbox-entry>)))
(define-generic-function mailbox-entry-content ((self <mailbox-entry>)))

;;;

(define (uidl-cmd self . opt)
  (if (null? opt)
      (begin
        (send self "+OK")
        (vector-for-each
         (lambda (m)
           (if (not (deleted? m))
               (send self "~d ~a" (id m) (uid m))))
         (mailbox self))
        (send self "."))
      (select-msg self
                  (car opt)
                  (lambda (m)
                    (send self "+OK ~d ~a" (id m) (uid m))))))

(define (list-cmd self . opt)
  (if (null? opt)
      (begin
        (send self "+OK")
        (vector-for-each 
         (lambda (m)
           (if (not (deleted? m))
               (send self "~d ~d" (id m) (size m))))
         (mailbox self))
        (send self "."))
      (select-msg self (car opt)
                  (lambda (m)
                    (send self "+OK ~d ~d" (id m) (size m))))))

(define (select-msg (self <pop3-session>) arg proc)
  (let ((i (string->number arg)))
    (if (or (< i 1)
            (> i (+ 1 (vector-length (mailbox self)))))
        (send self "-ERR no msg ~d" i)
        (let ((m (vector-ref (mailbox self) (- i 1))))
          (if (deleted? m)
              (send self "-ERR ~d was deleted" i)
              (proc m))))))
  
(define (retr-cmd self msg)
  (select-msg
   self
   msg
   (lambda (m)
     (send self "+OK ~d octets" (size m))
     (for-each
      (lambda (l)
        (send-stuffed self l))
      (string-split (mailbox-entry-content m) #\newline))
     (send self "."))))

(define (top-cmd self msg n)
  (let ((n (string->number n)))
    (select-msg
     self
     msg
     (lambda (m)
       (send self "+OK")
       (let loop ((l (string-split (mailbox-entry-content m) #\newline))
                  (i 0))
         (if (and (pair? l) (< i n))
             (begin
               (send-stuffed self (car l))
               (loop (cdr l) (+ i 1)))
             (send self ".")))))))

(define (dele-cmd self msg)
  (select-msg
   self
   msg
   (lambda (m)
     (set-deleted?! m #t)
     (send self "+OK"))))

(define (noop-cmd self)
  (send self "+OK"))

(define (rset-cmd self)
  (vector-for-each
   (lambda (m)
     (set-deleted?! m #f))
   (mailbox self))
  (send self "+OK so marked"))

(define (apop-cmd (self <pop3-session>) name digest)
  (let ((u (chap-authenticate 
            (realm self)
            name
            digest
            (lambda (pass)
              (md5-digest 
               (string-append "<" (token self) ">" pass)))))
        (client (peer (socket self))))
    ;;
    (if (not u)
        (begin
          (wm 724 "~a failed authentication as ~s" client name)
          (send self "-ERR failed authentication"))
        (begin
          (set-state! self 'transaction)
          (set-mailbox! self (get-pop-mailbox u))
          (note 722 "~a authenticated as ~s" client name)
          (if (authdest self)
              (with-message-dest
               (authdest self)
               (lambda ()
                 (note 722 "~a authenticated as ~s" client name))))
          (send self "+OK")))))

(define (stat-cmd (self <pop3-session>))
  (let (((v <vector>) (mailbox self)))
    (let loop ((i 0)
               (sum 0))
      (if (= i (vector-length v))
          (send self "+OK ~d ~d" (vector-length v) sum)
          (loop (+ i 1) (+ sum (size (vector-ref v i))))))))

;;;

(define *pop-cmd-table* (make-string-ci-table))

(table-insert! *pop-cmd-table* "quit" (& quit-cmd))
(table-insert! *pop-cmd-table* "apop" (& apop-cmd))
(table-insert! *pop-cmd-table* "stat" (cons '(transaction) (& stat-cmd)))
(table-insert! *pop-cmd-table* "uidl" (cons '(transaction) (& uidl-cmd)))
(table-insert! *pop-cmd-table* "list" (cons '(transaction) (& list-cmd)))
(table-insert! *pop-cmd-table* "retr" (cons '(transaction) (& retr-cmd)))
(table-insert! *pop-cmd-table* "noop" (cons '(transaction) (& noop-cmd)))
(table-insert! *pop-cmd-table* "rset" (cons '(transaction) (& rset-cmd)))
(table-insert! *pop-cmd-table* "top" (cons '(transaction) (& top-cmd)))
(table-insert! *pop-cmd-table* "dele" (cons '(transaction) (& dele-cmd)))
