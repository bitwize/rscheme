

(define-class <soap-binding> (<object>)
  target-url
  target-port
  target-host
  global-namespace
  local-prefix
  (soap-action init-value: "http://www.rscheme.org/"))

(define (make-soap-binding #rest r)
  (apply (with-module objsys make-instance) <soap-binding> r))
         
(define-thread-var *soap-session* #f)

(define-class <soap-session> (<object>)
  (binding type: <soap-binding>)
  (extra-headers init-value: '())
  connect-time
  connection-expire-time
  connection)

(define (with-soap-session (self <soap-session>) thunk)
  (thread-let ((*soap-session* self))
    (thunk)))

(define (soap-session-live? (self <soap-session>))
  (if (connection self)
      (if (time>? (time) (connection-expire-time self))
          (begin
            (close self)
            #f)
          #t)
      #f))
      
(define-method close ((self <soap-session>))
  (close (connection self))
  (set-connection! self #f))

(define-method finalize ((self <soap-session>))
  (if (connection self)
      (close self)))

(define *soap-keep-alive* (seconds->interval 30))

(define-method open-soap-session ((self <soap-binding>))
  (let* ((p (open-client-socket (to-string (gethostbyname (target-host self)))
                                (target-port self)))
         (t (time))
         (s (make <soap-session>
                  binding: self
                  connect-time: t
                  extra-headers: (list
                                  (cons 'keep-alive 
                                        (to-string 
                                         (interval->seconds 
                                          *soap-keep-alive*)))
                                  '(connection . "keep-alive"))
                  connection-expire-time: (time+interval t *soap-keep-alive*)
                  connection: p)))
    (register-for-finalization s)
    s))
  
;;;

#|
(define (t0)
  (make <soap-binding>
        target-url: "/s/soap/catia"
        target-host: "localhost"
        target-port: 7998
        global-namespace: "http://xynthesis.com/soap/axis/1.0"
        local-prefix: 'axis))
|#

(define (encode-soap-request (b <soap-binding>) request)
  (string-append
   (sxml->string
    (pretty-printify-xml
     `(env:Envelope
       (@ (env:encodingStyle "http://schemas.xmlsoap.org/soap/encoding/"))
       (env:Body
        ,request))))
   "\n"))

(define-class <soap-transaction> (<object>)
  session
  (send-time init-value: #f)
  (rcv-time init-value: #f)
  (request init-value: "")
  (response-code init-value: -1)
  (response-msg init-value: "")
  (response-headers init-value: '())
  (response init-value: ""))

(define (send-soap-request (self <soap-transaction>) request)
  (let* ((s (session self))
         (b (binding s))
         (p (output-port (connection s)))
         (body (encode-soap-request b request)))
    (set-request! self body)
    (format p "POST ~a HTTP/1.1\r\n" (target-url b))
    (format p "Accept-Encoding: identity\r\n")
    (format p "Content-length: ~d\r\n" (string-length body))
    (format p "SOAPAction: ~s\r\n" (soap-action b))
    (for-each
     (lambda (h)
       (format p "~C: ~a\r\n" (car h) (cdr h)))
     (extra-headers s))
    (format p "\r\n")
    (write-string p body)
    (flush-output-port p)
    (let ((t (time)))
      (set-send-time! self t)
      (set-connection-expire-time! s (time+interval t *soap-keep-alive*)))))

(define (read-soap-reply (self <soap-transaction>))
  (let* ((s (session self))
         (b (binding s))
         (p (input-port (connection s)))
         (head (strip-cr (read-line p))))
    (if (eof-object? head)
        (error "unexpected EOF")
        (begin
          (set-rcv-time! self (time))
          (bind ((code msg (parse-response-header head)))
            (if code
                (begin
                  ;(format #t "response ~s ~s\n" code msg)
                  (set-response-code! self code)
                  (set-response-msg! self msg)
                  (let ((h (http-read-headers p)))
                    (set-response-headers! self h)
                    (cond
                     ((assq 'content-length h)
                      => (lambda (cl)
                           (set-response! self (read-string p (string->number (cdr cl)))))))))
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
        

(define-class <soap-error> (<condition>)
  transaction
  faultstring
  detailmessage)

(define-method display-object ((self <soap-error>) port)
  (let* ((t (transaction self))
         (b (binding (session t))))
    ;;
    (format port "*** SOAP Fault From ~a:~a\n" (target-host b) (target-port b))
    (format port "    ~a\n" (faultstring self))
    (format port "    ~a\n" (detailmessage self))))

(define (interpret-soap-reply (self <soap-transaction>))
  (let ((content-type (cdr (assq 'content-type (response-headers self))))) 
    (if (and (eq? (response-code self) 200)
             (string=? content-type "text/xml"))
        (let* ((d (sxml:strip-whitespace
                   (string->sxml (response self) *soap-local-namespaces*)
                   '(enc:string)))
               (b (xpath () d "env:Envelope/env:Body")))
          ;; must have exactly one body for us to grok it...
          (if (not (= (length b) 1))
              (error "Malformed SOAP response")
              (let ((f (xpath () (car b) "env:Fault")))
                (if (pair? f)
                    (begin
                      (signal
                       (make <soap-error>
                             transaction: self
                             detailmessage: (xpath-str (car f) "detail/axis:faultDetails/message")
                             faultstring: (xpath-str (car f) "faultstring"))))
                    (car (select sxml:element? (sxml:children (car b))))))))
        (signal (make <soap-error>
                      transaction: self
                      detailmessage: (~ "Not a valid SOAP reponse: code ~s, content-type ~s"
                                        (response-code self)
                                        content-type)
                      faultstring: "Bad Response Type")))))
                  


(define (update-cookies! (self <soap-transaction>))
  (cond
   ((assq 'set-cookie (response-headers self))
    => (lambda (cp)
         (let ((s (session self)))
           (cond
            ((assq 'cookie (extra-headers s))
             => (lambda (oc)
                  (set-cdr! oc (cdr cp))
                  (values)))
            (else
             (set-extra-headers! s (cons (cons 'cookie (cdr cp))
                                         (extra-headers s))))))))))

;;;

(define (soap-rpc req)
  (let ((t (make <soap-transaction> session: *soap-session*)))
    (send-soap-request t req)
    (read-soap-reply t)
    (update-cookies! t)
    (interpret-soap-reply t)))

  
