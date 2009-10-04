(define-message-table rs.net.smtp 445)

(define-class <smtp-connection> (<object>)
  host
  rpc-gateway)

;;

(define-class <smtp-error> (<condition>)
  connection
  request
  reply)

(define-method display-object ((self <smtp-error>) port)
  (format port "SMTP Request ~s to ~s failed:\n  ~s\n"
          (apply format #f (request self))
          (host (connection self))
          (reply self)))
;;;

(define smtp-reply-pattern (reg-expr->proc
                            '(seq (save (seq digit digit digit))
                                  (save (or #\space #\-))
                                  (save (* (not (or #\cr #\lf)))))))


(define (smtp-connect #key 
                      remote-host
                      local-hostname
                      (local-port default: #f))
  (let ((sock (open-client-socket (make-inet-socket-addr (string->hostaddr remote-host)
                                                         25)
                                  local-port)))
    ;;
    (define (rpc msg . args)
      (case msg
        ((close)
         (dm 1209 "smtp-close ~s" remote-host)
         (close sock))
        ((chunk)
         (dm 1202 "> ~s" (car args))
         (write-string (output-port sock) (car args)))
        (else
         (let* ((req (apply format #f msg args))
                (reqlines (string-split req #\newline)))
           (if (string=? (last reqlines) "")
               (set! reqlines (reverse! (cdr (reverse! reqlines)))))
           ;;
           (for-each (lambda (l)
                       (dm 1203 "> ~a" l)
                       (format (output-port sock) "~a\r\n" l))
                     reqlines)
           (flush-output-port (output-port sock)))
         ;;
         (let loop ((r '())
                    (rc #f))
           (let ((l (read-line (input-port sock))))
             (dm 1204 "< ~a" l)
             (if (not (string? l))
                 (reverse! r)
                 (bind ((s e code cont rest (smtp-reply-pattern l)))
                   (if s
                       (if (string=? cont "-")
                           (loop (cons rest r) 
                                 (or rc (string->number code)))
                           (cons (or rc (string->number code))
                                 (reverse! (cons l r))))
                       (em 1238 "Bad SMTP reply: ~s" l)))))))))
    ;;
    (let ((l (read-line (input-port sock))))
      (dm 1200 "smtp-connect ~s" remote-host)
      (dm 1201 "< ~a" l))
    (rpc "EHLO ~a\n" local-hostname)
    rpc))
    
(define-method close ((self <smtp-connection>))
  ((rpc-gateway self) 'close))

(define (open-smtp #key 
                   (host type: <string>)
                   (local-hostname default: (hostname))
                   (local-port default: #f))
  (make <smtp-connection>
        host: host
        rpc-gateway: (smtp-connect remote-host: host
                                   local-hostname: local-hostname
                                   local-port: local-port)))

(define (rpc-and-check-ok* (self <smtp-connection>) expect fmt . args)
  (let ((reply (apply (rpc-gateway self) fmt args)))
    (if (eq? (car reply) expect)
        (values)
        (signal (make <smtp-error>
                      connection: self
                      request: (cons fmt args)
                      reply: reply)))))

(define (rpc-and-check-ok (self <smtp-connection>) fmt . args)
  (apply rpc-and-check-ok* self 250 fmt args))

;;;
;;;  Note that `headers' is of the form:
;;;
;;;     (key value value ...)
;;;
;;;  where multiple values will appear on multiple lines in the envelope

(define (send-one-email (cnx <smtp-connection>) (self <rfc822-message>))
  (let ((rpc (rpc-gateway cnx)))
    (rpc-and-check-ok 
     cnx
     "MAIL FROM:<~a>" (to-string (sender self)))
                    
    (for-each (lambda (r)
                (rpc-and-check-ok cnx "RCPT TO:<~a>" (to-string r)))
              (recipients self))
    (rpc-and-check-ok* cnx 354 "DATA")
    ;;
    (for-each (lambda ((h <pair>))
                (rpc 'chunk (~ "~a: ~a\r\n" (car h) (cadr h)))
                (for-each (lambda (follow)
                            (rpc 'chunk (string-append "\t" follow "\r\n")))
                          (cddr h)))
              (headers self))
    (rpc 'chunk "\r\n")
    ;;
    (for-each (lambda ((l <string>))
                (if (and (> (string-length l) 0)
                         (char=? (string-ref l 0) #\.))
                    (rpc 'chunk "."))
                (rpc 'chunk (string-append l "\r\n")))
              (string-split (contents self) #\newline))
    (rpc-and-check-ok cnx ".")))

(define (time->rfc-822 (t <time>))
  (time->string t "%d %b %Y %H:%M:%S %z"))

