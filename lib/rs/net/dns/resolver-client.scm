,(use unixm
      regex
      tables)

;;;

(define-class <resolver-client> (<object>)
  (socket init-value: #f)
  (domain-search-path type: <list>)
  (name-servers type: <list>)
  (outbound-mbox type: <mailbox>)
  (response-timeout type: <interval>)
  ;;
  (cache type: <string-table>))

(define (make-default-resolver-client)
  (let ((h (string-split (hostname) #\.)))
    (make <resolver-client>
          domain-search-path: (list (string-join #\. (cdr h)))
          name-servers: (list (make-inet-socket-addr 
                               (make-inet-addr "127.0.0.1")
                               53)))))
        
(define (read-resolv-conf #optional (file default: "/etc/resolv.conf"))
  (let ((pat (reg-expr->proc '(prefix 
                               (seq
                                (save 
                                 (or "nameserver"
                                     "domain"
                                     "search"))
                                (+ space)
                                (save (+ any))))))
        (spaces (reg-expr->proc '(+ space)))
        (sp '())
        (ns '()))
    ;;
    (call-with-input-file
        file
      (lambda (port)
        (let loop ()
          (let ((l (read-line port)))
            (if (string? l)
                (bind ((s e key value (pat l)))
                  (if s
                      (cond
                       ;;
                       ((string=? key "domain")
                        (set! sp (list (trim-whitespace value))))
                       ;;
                       ((string=? key "search")
                        (set! sp (string-split value spaces)))
                       ;;
                       ((string=? key "nameserver")
                        (set! ns (cons value ns)))))
                  (loop)))))))
    ;;
    (make <resolver-client>
          domain-search-path: sp
          name-servers: (map (lambda (h)
                               (make-inet-socket-addr (make-inet-addr h) 53))
                             (reverse! ns)))))

;;;

(define (host-lookup (self <resolver-client>)
                     #key 
                     (name default: #f)
                     (address default: #f)
                     (type default: #f)
                     (class default: 'IN))
  (let* ((q (if name
                (make-query name (or type 'A) class)
                (make-query (make-reverse-lookup address)
                            (or type 'PTR) 
                            class)))
         (m (query->message q))
         (str (msg->bs m))
         (sock (open-udp-socket)))
    ;;
    (let retry-loop ((n 3))
      (if (> n 0)
          (let server-loop ((servers (name-servers self)))
            (if (null? servers)
                (retry-loop (- n 1))
                (begin
                  (send-to sock str 0 (string-length str) #f (car servers))
                  (recv-from sock 

(make-inet-addr 
    
    

(define (make-reverse-lookup address)
  (string-append
   (string-join 
    #\.
    (reverse
     (string-split (inet-addr->string address) #\.)))
   ".in-addr.arpa."))


;;;

(define-class <pending-query> (<object>)
  (pending-attempts type: <list>)
  (last-xmit type: <time>)
  (message type: <string>)
  (query type: <query>)
  (response type: <mailbox>)
  (lock type: <semaphore>))

;;;

(define (resolver-inbound-thread (self <resolver-client>))
;;;

(define (resolver-outbound-thread (self <resolver-client>))
  (let loop ()
    (let* (((q <pending-query>) (receive-message! (outbound-mbox self)))
           (t (time)))
      ;;
      (let ((next-attempt #f))
        (with-semaphore
         (lock cur)
         ;;
         (if (null? (pending-attempts q))
             (send-message! (response cur) #f)
             (begin
               (set! next-attempt (car (pending-attempts q)))
               (set-pending-attempts! q (cdr (pending-attempts q)))
               (send-message! (outbound-mbox self) q))))
        ;;
        (if next-attempt
            (begin
              (send-to sock 
                       (message q) 0 (string-length (message q))
                       #f 
                       next-attempt)
              (set-last-xmit! q t))))
      (if (mailbox-has-data? (outbound-mbox self))
          (let ((lastt (last-xmit (dequeue-ref (outbound-mbox self) 0)))
                (nextt (time+interval lastt retry-delay))
                (dts (interval->seconds (time-time nextt t))))
            (thread-sleep (max dts 0.01))))
      (loop))))
