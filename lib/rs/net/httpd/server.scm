;;;------------------------------------------------------------


(define (parse-socket-spec str)
  (let ((lst (string-split str #\:)))
    (if (= (length lst) 1)
        (string->number (car lst))
        (make-inet-socket-addr (string->inet-addr (car lst))
                               (string->number (cadr lst))))))

;;;
;;;  service-bindings ::= (service-binding ...)
;;;  service-binding ::= (label server-port webspace logfile)

;;;  server-port ::= <inet-socket-addr>
;;;               |  <fixnum>                   ; port number
;;;               |  <string>                   ; "ip:port"
;;;               |  <server-socket>
;;;
;;;    e.g.,
;;;                      ("A" 8080 (make-web-space) "/tmp/a.log")

(define-class <http-service-binding> (<object>)
  (properties   type: <vector>          init-value: '#())
  (label        type: <string>)
  (server-port  type: <promise>)
  (webspace     type: <web-space>)
  (logfile      type: <promise>)
  (method-table type: <hash-table>))

(define (delayed-open-server-port portspec)
  (define (fixname (s <server-socket>))
    (set-name! s portspec)
    s)
  ;;
  (cond
   ((string? portspec)
    (let ((p (parse-socket-spec portspec)))
      (if (not p)
          (error "Invalid server port spec: ~s" portspec))
      (delay (fixname (open-server-socket p)))))
   ((fixnum? portspec)
    (delay (fixname (open-server-socket portspec))))
   ((instance? portspec <inet-socket-addr>)
    (delay (fixname (open-server-socket portspec))))
   (else
    (error "Can't handle server port spec: ~s" portspec))))

(define (delayed-open-logspec label logspec)
  (cond
   ((string? logspec)
    (let* ((p (string->file logspec))
           (parent (pathname->os-path (file-directory p)))
           (sp (stat parent))
           (sf (stat logspec)))
      ;;
      (if (not sp)
          (error "http[~s]: log directory ~s does not exist" label parent))
      (if (not (stat-directory? sp))
          (error "http[~s]: log directory ~s is not a directory" label parent))
      (if (and sf (not (stat-file? sf)))
          (error "http[~s]: log base file ~s is not a file" label logspec))
      ;;
      (delay
        (open-log-fileset basefile: logspec))))
   ;;
   ((eq? logspec 'stdout)
    (let ((p (current-output-port)))    ; capture *our* current output port
      (delay p)))
   ;;
   ((instance? logspec <message-dest>)
    (delay logspec))
   ;;
   ((instance? logspec <log-fileset>)
    (delay logspec))
   ;;
   (else
    (error "http[~s]: log spec ~s is not valid" label logspec))))

(define (build-internal-service-binding spec)
  (if (or (not (list? spec))
          (not (>= (length spec) 4)))
      (error "bad http service binding: ~s" spec))
  ;;
  (bind ((label port webspace logfile (list->values spec))
         (bdg (make <http-service-binding>
                    label: label
                    webspace: webspace
                    logfile: (delayed-open-logspec label logfile)
                    method-table: *method-table*
                    server-port: (if (instance? port <server-socket>)
                                     (delay port)
                                     (delayed-open-server-port port)))))
    ;;
    (let loop ((rest (list-tail spec 4)))
      (if (null? rest)
          bdg
          (if (or (not (pair? (cdr rest)))
                  (not (symbol? (car rest))))
              (error "invalid property list in: ~s" spec)
              (begin
                (set-property! bdg (car rest) (cadr rest))
                (loop (cddr rest))))))))

(define (add-http-service-method (self <http-service-binding>)
                                 (name <string>)
                                 (proc <function>))
  (if (eq? (method-table self) *method-table*)
      (set-method-table! self (hash-table-copy *method-table*)))
  ;;
  (table-insert! (method-table self) name proc)
  (values))


;;;
;;;  Note that all sockets are created before this procedure
;;;  returns, so the caller can setuid away from a priviledged 
;;;  user

(define (start-http-server service-bindings)
  (assert (list? service-bindings))
  ;; build the internal representation of the service binding...
  (let ((sb (map build-internal-service-binding 
                 service-bindings)))
    ;; bind and start them all
    (for-each
     (lambda ((b <http-service-binding>))
       (let ((s (force (server-port b)))
             (l (force (logfile b))))
         ;;
         (thread-resume
          (make-thread
           (lambda ()
             (server* b s l))
           (string-append "server:" (label b))))))
     sb)
    sb))

(define-class <http-connection> (<object>)
  (properties init-value: '#() type: <vector>)
  messages
  socket
  log-fileset
  (http-binding init-value: #f)
  (num-requests init-value: 0))

(define-method peer ((self <http-connection>))
  (peer (socket self)))

(define-method log-file-message-dest ((self <output-port>))
  self)

(define-method log-file-message-dest ((self <http-connection>))
  (log-file-message-dest (log-fileset self)))

(define-thread-var *log-file-message-dest* #f)
(define-thread-var *http-connection* #f)

(define (current-http-connection)
  *http-connection*)

(define (underlying-http-socket (self <http-connection>))
  (socket self))

(define-macro (note* id msg . args)
  `(with-message-dest
    *log-file-message-dest*
    (lambda ()
      (note ,id ,msg ,@args))))

(define (handle-http-socket-connection space i c logs verbose? 
                                       #key
                                       (http-binding default: #f)
                                       (start-hook default: #f)
                                       (end-hook default: #f))
  (let ((t (time)))
    (thread-resume
     (make-thread
      (lambda ()
        (let ((cnx (make <http-connection>
                         messages: (if verbose?
                                       (log-file-message-dest logs)
                                       (make-message-queue tail: 100))
                         log-fileset: logs
                         http-binding: http-binding
                         socket: c)))
          (set-property! cnx 'arrival-time t)
          ;;
          (if http-binding
              (set-property! cnx 'http-binding http-binding))
          ;;
          (thread-let ((*http-connection* cnx))
            (if start-hook (start-hook))
            (handler-case
             (thread-let ((*log-file-message-dest* 
                           (log-file-message-dest logs)))
               (with-message-dest
                (messages cnx)
                (lambda ()
                  (handle-client-connection cnx space))))
             ((<condition> condition: e)
              (with-message-dest
               (messages cnx)
               (lambda ()
                 (wm 599 "Client ~s died: ~a" c e)))
              ;;
              (with-output-to-log-file
               logs
               (lambda ()
                 (print (messages cnx))
                 (let ((s (get-property e 'stack #f)))
                   (if s
                       (begin
                         (format #t "-----------------------------------\n")
                         (print (vm-continuation-reg s))
                         (format #t "-----------------------------------\n")
                         (with-module repl
                           (apply-backtrace* 
                            (vm-dynamic-state-reg s)))))))))))
          (close c)
          (if end-hook (end-hook))))
      (format #f "clnt#~d(~a)" i (peer c))))))

(define (server* (bdg <http-service-binding>) s logs)
  ;;
  (with-message-dest
   (log-file-message-dest logs)
   (lambda ()
     (note 100 "server ~s start on ~s" (label bdg) (name s))))
  ;;
  (let loop ((i 0))
    (handle-http-socket-connection (webspace bdg)
                                   i
                                   (accept-client s)    ;;--> <os-error>
                                   logs 
                                   (get-property bdg 'verbose #f)
                                   http-binding: bdg)
    (loop (+ i 1))))


(define-class <http-request> (<object>)
  (properties init-value: '())
  web-space
  request-number
  (client type: <http-connection>))

(define-method read-content ((self <http-request>))
  ;;; XXX should handle case of content-length not specified, too
  (if (has-property? self 'content-length)
      (let ((n (get-property self 'content-length)))
        (read-string (input-port (socket (client self))) (string->number n)))
      ;; without an explicit content-length, we read to the end of the socket
      (port->string (input-port (socket (client self))))))

(define-method open-content ((self <http-request>))
  (if (has-property? self 'content-length)
      (let ((n (get-property self 'content-length)))
        (make-fixed-length-input-port (input-port (socket (client self)))
                                      (string->number n)))
      (input-port (socket (client self)))))
                                

(define-method peer ((self <http-request>))
  (peer (client self)))

(define-method log-file-message-dest ((self <http-request>))
  (log-file-message-dest (client self)))

(define-class <http-response> (<object>)
  (properties init-value: '#())
  keep-alive?
  (status-code init-value: #f)
  (reason-phrase init-value: #f)
  (header-fields init-value: '())
  (request type: <http-request>)
  (header-sent? init-value: #f)
  (content init-value: #f))

(define-method response->request ((self <http-response>))
  (request self))

(define-method request-message-buffer ((self <http-request>))
  (messages (client self)))

(define-method content-length ((self <http-response>))
  (let ((a (assq 'content-length (header-fields self))))
    (if a
        (cdr a)
        (if (content self)
            (content-length (content self))
            #f))))

(define-method log-file-message-dest ((self <http-response>))
  (log-file-message-dest (request self)))

(define-method set-result! ((self <http-response>) code phrase)
  (set-status-code! self code)
  (set-reason-phrase! self phrase))

(define (handle-client-connection (c <http-connection>) (space <web-space>))
  (let ((n (client-connection-loop c space)))
    (if n
        (note 601 "Processed ~d requests" n)
        (note 609 "Bailed out"))))

(define-class <httpd-bail> (<condition>)
  response-code
  response-msg)

;;; this is used to signal a failure detected during server processing
;;; before a proper response can be formulated

(define (httpd-bail code msg . args)
  (signal
   (make <httpd-bail>
         response-code: code
         response-msg: (apply format #f msg args))))

(define (bailout-response (self <httpd-bail>) c space)
  (let ((rsp (make <http-response>
                   request: (make <http-request>
                                  web-space: space
                                  client: c
                                  request-number: 666666
                                  properties: '())
                   status-code: (response-code self)
                   keep-alive?: #f
                   reason-phrase: (response-msg self)
                   header-fields: '((connection . "close")))))
    ;;
    (set-content-and-type!
     rsp
     (sxml->content
      (pretty-printify-xml
       `(html
         (head
          (title ,(response-msg self)))
         (h1 ,(to-string (response-code self)))
         (h2 ,(response-msg self))
         ;;
         (p "This server had trouble processing your request.")
         ;;
         (hr)))))
    (flush-http-response rsp)))
        

(define (client-connection-loop c (space <web-space>))
  (handler-case
   (let ((tx (cons #f #f)))
     (let loop ((i 1))
       (let ((h (parse-request-header c tx)))
         (if h
             (let* ((req (make <http-request>
                               web-space: space
                               client: c
                               request-number: i
                               properties: h))
                    (rsp (compute-http-response req)))
               (flush-http-response rsp)   ; make sure response has been sent
               (note 602 "Request took ~a" (time-time (time) (car tx)))
               (if (keep-alive? rsp)
                   (loop (+ i 1))
                   i))
             (- i 1)))))
   ((<httpd-bail> condition: e)
    (bailout-response e c space)
    #f)))
   

(define-method uri ((self <http-request>))
  (get-property self 'request-path '()))

(define-method keep-alive? ((self <http-request>))
  (string-ci=? (get-property self 'connection "close") "keep-alive"))

;;

(define (compute-http-response (req <http-request>))
  (let ((rsp (make <http-response>
                   request: req
                   keep-alive?: (keep-alive? req)
                   status-code: #f)))
    (dm 140 "URI ~s" (uri req))
    ;;
    (if (with-module repl *trace-apply*)
        (set-apply-trace-flag! #t))
    ;;
    (handler-case
     (dispatch-uri (root-uri-node (web-space req)) (uri req) rsp)
     ((<condition> condition: err)
      (generate-error-response/internal rsp err)))
    ;;
    (with-message-dest
     (log-file-message-dest req)
     (lambda ()
       (note 199 "(~a) URI ~s => ~s ~a" 
             (peer req) 
             (uri req)
             (status-code rsp)
             (or (content-length rsp) "n/a"))))
    ;;
    rsp))

;;;

(define-method clear-header-field! ((self <http-response>) key)
  (set-header-fields! self (delq (assq key (header-fields self))
                                 (header-fields self))))
  
(define-method set-header-field! ((self <http-response>) key value)
  (if (and (eq? key 'connection) (string=? value "close")) 
      (set-keep-alive?! self #f))
  ;;
  (let ((a (assq key (header-fields self))))
    (if a
        (error "Header field already set: ~s (was ~s, now ~s)" 
               key (cdr a) value)) ; error out? replace? append?
    (set-header-fields! self (cons (cons key value)
                                   (header-fields self)))))

(define-class <redirection> (<object>)
  (target type: <string>))


(define (generate-redirect-response (self <http-response>) new-uri)
  (if (instance? new-uri <redirection>)
      (set! new-uri (target new-uri)))
  (note* 302 "Redirect to ~s" new-uri)
  (set-result! self 302 "Found")
  (set-header-field! self 'location new-uri)
  (set-content!
   self
   (call-with-output-string
    (lambda (p)
      (format p "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01//EN\"\n")
      (format p "          \"http://www.w3.org/TR/html4/strict.dtd\">\n")
      (format p "<html><head><title>302 Found</title></head>\n")
      (format p "<body>\n")
      (format p "<h1>Redirected</h1>\n")
      (format p "This document has moved <a href=\"~a\">here</a>.\n" new-uri)
      (format p "</body></html>\n")))))


(define (generate-error-response/client-error (self <http-response>)
                                              msg . args)
  (note* 400 "Client request error")
  (set-keep-alive?! self #f)
  (set-result! self 400 "Client request error")
  ;;
  (clear-header-field! self 'content-type)
  (set-header-field! self 'content-type "text/html")
  (set-content!
   self
   (call-with-output-string
    (lambda (p)
      (format p "<html><title>Client Request Error</title>\n")
      (format p "<body>\n")
      (format p "<h1>Client Request Error</h1>\n")
      (format p "<p>\n")
      (apply format p msg args)
      (format p "</body></html>\n")))))
  
(define (generate-error-response/internal (self <http-response>)
                                          (err <condition>))
  (note* 500 "Server processing failure")
  (set-keep-alive?! self #f)
  (set-result! self 500 "Server processing failure")
  (clear-header-field! self 'content-type)
  ;;
  (let ((f (get-property (web-space (request self))
                         'error-page/internal
                         generate-error-response/internal/default)))
    (f self err)))
;;

(define (http-error-context->sxml (self <http-request>))
  (let ((msgs (messages (client self))))
    ;;
    (append
     `((peer ,(to-string (peer self)))
       (properties
        ,@(map (lambda (p)
                 `(property (@ (key ,(to-string (car p))))
                            ,(~ "~s" (cdr p))))
               (properties self))))
     (if (output-port? msgs)
         '()
         `((messages 
            ,(with-output-to-string
               (lambda ()
                 (print msgs)))))))))
;;

(define (generate-error-response/internal/default (self <http-response>)
                                                  (err <condition>))
  (set-header-field! self 'content-type "text/html")
  (set-content!
   self
   (call-with-output-string
    (lambda (p)
      (format p "<html><title>Server Processing Failure</title>\n")
      (format p "<body>\n")
      (format p "<h1>Server Processing Failure</h1>\n")
      (format p "<p>\n")
      (format p "The server encountered an internal error processing your\n")
      (format p "request to URI <tt><b>~a</b></tt>.\n"
              (string-join "/" (uri (request self))))
      (format p "<p>\n")
      (format p "The error condition was:\n")
      (format p "<hr><pre>\n~a</pre><hr>"
              (escape-html
               (call-with-output-string
                (lambda (port)
                  (display err port)))))
      (let ((s (get-property err 'stack #f))
            (msgs (messages (client (request self)))))
        ;;
        (if (not (output-port? msgs))
            (begin
              (format p "Here's what was happening...\n<pre>")
              (write-string p
                            (escape-html
                             (with-output-to-string
                               (lambda ()
                                 (print msgs)))))
              (format p "</pre>\n<hr/>\n")))
        ;;
        (if s
            (begin
              (format p "Here is where it happened in the code...\n<pre>\n")
              (write-string p
                            (escape-html
                             (with-output-to-string
                               (lambda ()
                                 (print (vm-continuation-reg s))))))
              (format p "</pre>\n")
              (format p "<hr>\nHere is the call stack\n<pre>\n")
              (write-string p
                            (escape-html
                             (with-output-to-string
                               (lambda ()
                                 (with-module repl
                                   (apply-backtrace* 
                                    (vm-dynamic-state-reg s)))))))
              (format p "</pre>\n"))))
      (format p "</body></html>\n")))))

(define (generate-error-response/not-found/default (self <http-response>))
  (set-header-field! self 'content-type "text/html")
  (set-content! self
                (call-with-output-string
                 (lambda (p)
                   (format p "<html><title>No such thing</title>\n")
                   (format p "<h1>No such thing</h1>\n")
                   (format p "</html>\n")))))
  
(define (generate-error-response/not-found (self <http-response>))
  (note* 404 "Not found")
  (set-result! self 404 "No such web object")
  ;;
  (clear-header-field! self 'content-type)
  (let ((f (get-property (web-space (request self)) 
                         'error-page/not-found 
                         generate-error-response/not-found/default)))
    (f self)))
    
(define (generate-error-response/listing-denied (self <http-response>))
  (note* 403 "Forbidden")
  (set-result! self 403 "Forbidden")
  ;;
  (clear-header-field! self 'content-type)
  (set-header-field! self 'content-type "text/html")
  (set-content! 
   self
   (call-with-output-string
    (lambda (p)
      (format p "<html><title>403 Directory Listing Denied</title>\n")
      (format p "<h1>Directory Listing Denied</h1>\n")
      (format p "Sorry, charlie\n")
      (format p "</html>\n")))))

;;;

(define (parse-fields->table body)
  (let ((tbl (make-string-table)))
    (for-each
     (lambda (f)
       (let ((x (string-search f #\=)))
         (if x
             (table-insert! tbl 
                            (substring f 0 x) 
                            (form-decode (substring f (+ x 1))))
             (table-insert! tbl f ""))))
     (string-split body #\&))
    tbl))

(define (parse-multi-fields->table body)
  (let ((tbl (make-string-table)))
    (for-each
     (lambda (f)
       (let* ((x (string-search f #\=))
              (key (if x (substring f 0 x) f))
              (entry (table-lookup tbl key))
              (value (if x
                         (list (form-decode (substring f (+ x 1))))
                         (cons #t '()))))
         ;;
         (if entry
             (append! entry value)
             (table-insert! tbl key value))))
     (string-split body #\&))
    tbl))

(define-method dispatch-uri ((self <uri-post-form>) path rsp)
  (dm 106 "uri-post-form ~s: path ~s" self path)
  (if (eq? (get-property (request rsp) 'request-type) 'post)
      (let* ((fields (parse-fields->table (read-content (request rsp))))
             (actuals (map 
                       (lambda (parm)
                         (case (cadr parm)
                           ((field)
                            (list (car parm) (table-lookup fields (caddr parm))))
                           ((property)
                            (list (car parm) 
                                  (get-property (request rsp) 
                                                (caddr parm) 
                                                #f)))))
                       (parameters self))))
        (set-property! (request rsp) '%form-fields fields) 
        (call-uri-script rsp (procedure self) 
                         (cons rsp (apply append actuals))))
      (error "not using POST method to access ~s" self)))

(define-method dispatch-uri ((self <uri-dynamic-directory>) path rsp)
  (dm 105 "uri-dynamic-directory ~s: path ~s" self path)
  (if (or (null? path)
          (string=? (car path) ""))
      (if (as-leaf-node self)
          (dispatch-uri (as-leaf-node self) '() rsp)
          (generate-error-response/listing-denied rsp))
      (let ((item ((binder self) (car path) rsp)))
        (if item
            (begin
              (set-property! (request rsp) (property-name self) item)
              (dispatch-uri (contents self) (cdr path) rsp))
            (generate-error-response/not-found rsp)))))

(define-method dispatch-uri ((self <uri-pattern-directory>) path rsp)
  (dm 104 "uri-pattern-directory ~s: path ~s" self path)
  (if (or (null? path) (string=? (car path) ""))
      (generate-error-response/listing-denied rsp)
      (let loop ((p (contents self)))
        (if (null? p)
            (generate-error-response/not-found rsp)
            (let ((item ((cadar p) path)))
              (if item
                  (let ((target (caddar p)))
                    (if (procedure? target)
                        (target path rsp item)
                        (dispatch-uri target (cdr path) rsp)))
                  (loop (cdr p))))))))

(define-method dispatch-uri ((self <uri-union-node>) path rsp)
  (dm 107 "uri-union ~s: path ~s" self path)
  (let loop ((l (members self)))
    (if (null? l)
        (generate-error-response/not-found rsp)
        (if (can-handle? (car l) path)
            (dispatch-uri (car l) path rsp)
            (loop (cdr l))))))

(define-method can-handle? ((self <uri-directory>) path)
  (if (or (null? path) (and (equal? path '(""))
                            (not (table-lookup (contents self) ""))))
      (and (as-leaf-node self) #t)
      (table-key-present? (contents self) (car path))))
  
(define-method dispatch-uri ((self <uri-directory>) path rsp)
  (dm 101 "uri-directory ~s: path ~s" self path)
  (if (or (null? path) (and (equal? path '(""))
                            (not (table-lookup (contents self) ""))))
      (if (as-leaf-node self)
          (dispatch-uri (as-leaf-node self) '() rsp)
          (if (permit-listing? self)
              (generate-directory-listing self rsp)
              (generate-error-response/not-found rsp)))
      (let ((next (table-lookup (contents self) (car path))))
        (dm 102 "  next(~s) => ~s" (car path) next)
        (if next
            (dispatch-uri next (cdr path) rsp)
            (generate-error-response/not-found rsp)))))

(define-method can-handle? ((self <uri-disk-dir>) path)
  (let loop ((p path)
             (base (disk-file-path self))
             (sb (stat (disk-file-path self))))
    (if (null? p)
        #t
        (if (equal? p '(""))
            (or (stat (string-append base "/index.html"))
                (stat (string-append base "/Welcome.html"))
                #f)
            (if (member (car p) '("." ".." ""))
                #f
                (let* ((s (string-append base "/" (car p)))
                       (sb (stat s)))
                  (if sb
                      (loop (cdr p) s sb)
                      #f)))))))

(define-method dispatch-uri ((self <uri-disk-dir>) path rsp)
  (let loop ((p path)
             (base (disk-file-path self))
             (sb (stat (disk-file-path self))))
    (if (null? p)
        (dm 113 "uri-disk-dir: path () base ~s" base)
        (dm 112 "uri-disk-dir: path ~s base ~s" (car p) base))
    (if (null? p)
        (if (stat-file? sb)
            (let ((mt (guess-mime-type base)))
              (if mt
                  (set-header-field! rsp 'content-type mt))
              (set-content! rsp (file->content base)))
            (generate-redirect-response 
             rsp
             (string-join #\/ (append (uri (request rsp)) '("")))))
        (if (equal? p '(""))
            (if (stat (string-append base "/index.html"))
                (loop '("index.html") base sb)
                (if (stat (string-append base "/Welcome.html"))
                    (loop '("Welcome.html") base sb)
                    (if (permit-listing? self)
                        (if (string=? base (disk-file-path self))
                            (generate-directory-listing self rsp)
                            (generate-directory-listing (make <uri-disk-dir>
                                                          disk-file-path: (string-append 
                                                                           base
                                                                           "/"))
                                                        rsp))
                        (generate-error-response/listing-denied rsp))))
            ;; reject "--//--", "--/./--", and "--/../--"
            (if (member (car p) '("." ".." ""))
                (generate-error-response/listing-denied rsp)
                (let* ((s (string-append base "/" (car p)))
                       (sb (stat s)))
                  (if sb
                      (loop (cdr p) s sb)
                      (generate-error-response/not-found rsp))))))))

(define-method set-content-and-type! ((self <http-response>) content 
                                      #optional type)
  (if type
      (set-header-field! self 'content-type type))
  (set-content! self content))

(define-method dispatch-uri ((self <uri-literal-node>) path rsp)
  (if (null? path)
      (begin
        (if (mime-type self)
            (set-header-field! rsp 'content-type (mime-type self)))
        (set-content! rsp (contents self)))
      (generate-error-response/not-found rsp)))

(define-method dispatch-uri ((self <uri-disk-node>) path rsp)
  (if (null? path)
      (let* ((fn (disk-file-path self)))
        (let ((ims (cond
                    ((get-property (request rsp) 'if-modified-since #f)
                     => string->time)
                    (else #f)))
              (t (stat-mtime (stat fn))))
          (if (and ims (time>=? ims t))
              (set-result! rsp '304 "Not Modified")
              (let ((data (file->content fn)))
                (note* 201 "Found ~s (~d bytes)" fn (content-length data))
                (if (mime-type self)
                    (set-header-field! rsp 'content-type (mime-type self)))
                ;; see if we can get it to cache...
                (if *static-max-age*
                    (set-header-field! rsp 'cache-control 
                                       (~ "max-age=~d" *static-max-age*)))
                (set-header-field! 
                 rsp
                 'last-modified 
                 (time->string t "%a, %d %b %Y %H:%M:%S GMT" #f))
                (set-content! rsp data)))))
      (generate-error-response/not-found rsp)))

(define *static-max-age* 60)
(define (clear-static-max-age) (set! *static-max-age* #f))
(define (set-static-max-age! (a <fixnum>)) (set! *static-max-age* a))

(define (call-uri-script rsp proc args)
  (handler-case
   (bind ((content type (apply proc args)))
     (if (instance? content <redirection>)
         (begin
           (note* 206 "Found script (redirecting => ~a)" (target content))
           (generate-redirect-response rsp content))
         (begin
           (note* 202 "Found script (returning ~d bytes)" 
                  (content-length content))
           (if type
               (set-header-field! rsp 'content-type type))
           (set-content! rsp content))))
   ((<condition> condition: err)
    (generate-error-response/internal rsp err))))
  
(define (redirection (to <string>))
  (make <redirection> target: to))


(define-method dispatch-uri ((self <simple-script-node>) path rsp)
  (call-uri-script rsp (procedure self) (list path (request rsp))))

(define-method dispatch-uri ((self <simple-rsp-script-node>) path rsp)
  (call-uri-script rsp (procedure self) (list path rsp)))

(define-method dispatch-uri ((self <complete-script-node>) path rsp)
  ((procedure self) path rsp))

#|
  (handler-case
   (bind ((content type ((procedure self) path (request rsp))))
     (note* 202 "Found script (returning ~d bytes)" (string-length content))
     (set-header-field! rsp 'content-type (or type "text/html"))
     (set-content! rsp content))
   ((<condition> condition: err)
    (generate-error-response/internal rsp err))))
|#
(define-method dispatch-uri ((self <uri-redirect-proc>) path rsp)
  (let ((t ((target-uri self) path rsp)))
    (generate-redirect-response 
     rsp 
     (if (null? path)
         t
         (string-append t "/" (string-join #\/ path))))))
    
(define-method dispatch-uri ((self <uri-redirect>) path rsp)
  (generate-redirect-response 
   rsp 
   (if (null? path)
       (target-uri self)
       (string-append (target-uri self) "/" (string-join #\/ path)))))

(define-method generate-directory-listing ((self <uri-directory>) rsp)
  (let ((links (map (lambda (p)
                      (let ((k (car p))
                            (v (cdr p)))
                        `(link (@ (name ,k)
                                  (type ,(if (directory? v)
                                             "dir"
                                             "leaf")))
                               (title ,(format #f "~s" v))
                               (class ,(symbol->string
                                        (class-name
                                         (object-class v)))))))
                    (sort (map cons
                               (key-sequence (contents self))
                               (value-sequence (contents self)))
                          (lambda (a b)
                            (string<? (car a) (car b)))))))
    (note* 203 "Found directory (~d entries)" (length links))
    (set-content! rsp
                  (xsl-transform
                   "uridir.xsl"
                   `(node (path ,(string-join "/" (uri (request rsp))))
                          (links ,@links))))))


(define-method generate-directory-listing ((self <uri-disk-dir>) rsp)
  (let ((links (map (lambda (k)
                      `(link (@ (name ,k))
                             (title ,k)
                             (class ,"file-or-dir")))
                    (sort (select (lambda (n)
                                    (not (member n '("." ".."))))
                                  (scandir (disk-file-path self)) )
                          string<?))))
    (note* 204 "Found disk directory (~d entries)" (length links))
    (set-content! rsp
                  (xsl-transform
                   "uridir.xsl"
                   `(node (path ,(string-join "/" (uri (request rsp))))
                          (links ,@links))))))
