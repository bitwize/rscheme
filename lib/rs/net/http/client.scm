(define (strip-cr str)
  (if (string? str)
      (let (((n <fixnum>) (string-length str)))
        (if (and (fixnum>? n 0)
                 (char=? (string-ref str (sub1 n)) #\cr))
            (substring str 0 (sub1 n))
            str))
      str))

(define-class <http-client> (<object>)
  (name init-value: #f)
  (last-use-time init-function: time)
  headers
  socket)

(define (stale? (self <http-client>) #optional (t default: (time)))
  (> (interval->seconds (time-time t (last-use-time self))) 20))

(define-method close ((self <http-client>))
  (close (socket self))
  (set-socket! self #f))

(define (open-http-client #key 
                          server 
                          (port default: 80)
                          (method default: 'http))
  (let ((h (if (eq? port 80)
               server
               (~ "~a:~d" server port))))
    (make <http-client>
          headers: `((host ,h)
                     (user-agent "rscheme")
                     (pragma "no-cache")
                     (connection "keep-alive")
                     (keep-alive "30")
                     (accept "*/*"))
          socket: (open-client-socket
                   (make-inet-socket-addr (gethostbyname server) port)))))
  
(define (send-http-request (self <http-client>) method uri extra)
  (let ((p (output-port (socket self))))
    (format p "~a ~a HTTP/1.1\r\n" method uri)
    (for-each
     (lambda (h)
       (format p "~a: ~a\r\n" (car h) (cadr h)))
     (append (headers self) extra))
    (format p "\r\n")))

(define-class <http-client-response> (<object>)
  (properties init-value: '())
  (client type: <http-client>)
  response-code
  response-msg)

(define-class <http-non-success-response> (<condition>) :abstract
  request-type
  response
  response-content)

(define-class <http-error-response> (<http-non-success-response>))

(define-class <http-redirect-response> (<http-non-success-response>))

(define-method display-object ((self <http-redirect-response>) port)
  (format port "*** {~a} HTTP Redirect to: ~s\n"
          (machine-bits->string self)
          (get-property (response self) 'location)))

(define (check-put-success (self <http-client-response>))
  (check-response-ok "PUT" self))

(define (check-post-success (self <http-client-response>))
  (check-response-ok "POST" self))

(define (check-response-ok what (self <http-client-response>))
  (case (response-code self)
    ((200) self)
    ((302) (signal (make <http-redirect-response>
                         request-type: what
                         response: self
                         response-content: (read-content self))))
    (else
     (signal (make <http-error-response>
                   request-type: what
                   response: self
                   response-content: (read-content self))))))

(define-method display-object ((self <http-error-response>) port)
  (format port "*** ~a failed: ~a ~a\n" 
                (request-type self)
                (response-code (response self) )
                (response-msg (response self))))

(define-method open-content ((self <http-client-response>))
  (let ((p (input-port (socket (client self)))))
    (if (has-property? self 'content-length)
        (let ((n (get-property self 'content-length)))
          (make-fixed-length-input-port p (string->number n)))
        p)))

(define-method read-content ((self <http-client-response>))
  (if (has-property? self 'content-length)
      (let ((n (get-property self 'content-length)))
        (read-string (input-port (socket (client self))) (string->number n)))
      ;; without an explicit content-length, we read to the end of the socket
      (port->string (input-port (socket (client self))))))

(define (read-http-response (self <http-client>))
  (let* ((p (input-port (socket self)))
         (head (strip-cr (read-line p))))
    (if (eof-object? head)
        (error "unexpected EOF")
        (bind ((code msg (parse-response-header head)))
          (if code
              (make <http-client-response>
                    client: self
                    properties: (http-read-headers p)
                    response-code: code
                    response-msg: msg)
              (begin
                (display (port->string p))
                (error "bad response line: ~s" head)))))))
;;;

(define response-fmt (reg-expr->proc '(entire
                                       (seq "HTTP/1.1"
                                            (+ space)
                                            (save (+ digit))
                                            (+ space)
                                            (save (+ printable))))))

(define (parse-response-header (l <string>))
  (bind ((s e code msg (response-fmt l)))
    (if s
        (values (string->number code) msg)
        (values))))

(define (http-post (self <http-client>) #key 
                   data
                   uri
                   (content-type default: #f))
  (send-http-request 
   self 
   "POST"
   uri
   (append (if content-type
               `((content-type ,content-type))
               '())
           `((content-length ,(to-string (string-length data))))))
  ;;
  (let ((dest (output-port (socket self))))
    (write-string dest data)
    (flush-output-port dest)
    (set-last-use-time! self (time))
    (check-response-ok "POST" (read-http-response self))))


(define (chunked-copy-between-ports src dst #optional visit)
  (let loop ((n 0))
    (let ((chunk (input-port-read-max src 8192)))
      (if (eof-object? chunk)
          n
          (begin
            (write-string dst chunk)
            (if visit (visit chunk))
            (loop (+ n (string-length chunk))))))))
        
(define (http-put (self <http-client>) #key 
                  source
                  uri
                  (content-type default: #f)
                  (content-length default: #f))
  (send-http-request 
   self 
   "PUT"
   uri
   (append (if content-type
               `((content-type ,content-type))
               '())
           (if content-length
               `((content-length ,(to-string content-length)))
               '())))
  ;;
  (let* ((dest (output-port (socket self)))
         (n (chunked-copy-between-ports source dest)))
    (flush-output-port dest)
    (set-last-use-time! self (time))
    (if (not content-length)
        (close-output-port dest))
    ;;
    (check-response-ok "PUT" (read-http-response self))))


(define (http-get (self <http-client>) #key uri)
  (send-http-request self "GET" uri '())
  (flush-output-port (output-port (socket self)))
  (set-last-use-time! self (time))
  (check-response-ok "GET" (read-http-response self)))

(define (add-http-client-header (self <http-client>)
                                key
                                value)
  (set-headers! self (cons (cons key value) (headers self)))
  (values))

(define (update-cookies-from (self <http-client-response>)
                             ;; the session can be supplied explicitly
                             ;; if we want to transfer the cookies from
                             ;; one connection to another
                             #key (session default: (client self)))
  ;;
  (let ((h (headers session)))
    (cond
     ((get-property self 'set-cookie #f)
      => (lambda (cookie)
           (cond
            ((assq 'cookie h)
             => (lambda (oc)
                  (set-cdr! oc cookie)
                  (values)))
            (else
             (set-headers! session (cons (list 'cookie cookie) h)))))))))
