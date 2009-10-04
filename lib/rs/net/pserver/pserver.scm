
(define-message-table rs.net.pserver 543)

(define-class <pserver-family> (<object>)
  name)

(define-class <root-binding> (<object>)
  (family type: <pserver-family>)
  filesystem
  path-to-top)

(define-class <pserver-connection> (<object>)
  (properties init-value: '#())
  realm
  socket
  messages
  (auth-token init-value: #f)
  (arg-accum init-value: '())
  (global-options init-value: '())
  (inventory init-value: #f))

(define (can-modify-client? (self <pserver-connection>))
  (not (member "-n" (global-options self))))

;;;
;;;  In debug mode, all messages go to stdout
;;;

(define *debug-mode* #t)

(define (start-cvs-server #key 
                          (port default: 2401)
                          (logfile default: "/tmp/pserver_log")
                          realm
                          family)
  (pserver-add-family family)
  (let* ((s (open-server-socket port))
         (log (open-log-fileset basefile: logfile)))
    (thread-resume
     (make-thread
      (lambda ()
        (note 100 "pserver start on ~s" port)
        (pserver s log realm))
      "pserverd"))))

(define (pserver sock log realm)
  (with-message-dest
   (if *debug-mode*
       (current-output-port)
       (log-file-message-dest log))
   (lambda ()
     (let loop ((i 0))
       (let ((c (accept-client sock)))
         (note 101 "client[~d] ~s" i c)
         (thread-resume
          (make-thread
           (lambda ()
             (handler-case
              (let ((cnx (make <pserver-connection>
                               socket: c
                               realm: realm
                               messages: (make-message-queue))))
                (with-message-dest
                 (if *debug-mode*
                     (current-output-port)
                     (messages cnx))
                 (lambda ()
                   (handle-pserver-client cnx))))
              ((<condition> condition: e)
               (wm 198 "client[~d] died: ~a" i e)))
             (note 190 "closing client[~d]" i)
             (close c))))
         (loop (+ i 1)))))))

(define (handle-pserver-client (self <pserver-connection>))
  (note 501 "start")
  (let ((a (pserver-authentication-handshake self)))
    (if a
        (begin
          (set-auth-token! self a)
          (process-requests self)))))
            
(define (process-requests (self <pserver-connection>))
  (let loop ()
    (let ((l (read-line (input-port (socket self)))))
      (dm 610 "request ~s" l)
      (if (string? l)
          (bind ((brk (string-search l #\space))
                 (word rest (if brk
                                (values (substring l 0 brk)
                                        (substring l (+ brk 1)))
                                (values l ""))))
            (dispatch-request self word rest)
            (flush-output-port (output-port (socket self)))
            (loop))))))

;;;
