
(define *show-transaction* #t)
(define *log-file* #f)

(define (make-time-string)
  (time->string (time)
		"%A, %d-%b-%y %H:%M:%S GMT"
		#f))

(define (log-entry . args)
  (if *log-file*
      (begin
	(write args *log-file*)
	(newline *log-file*))))

(define (xaction msg . args)
  (if *show-transaction*
      (apply format (fluid-ref *console-output-port*) msg args)))

(define (summary info)
  (log-entry 'summary info)
  (let ((p (fluid-ref *console-output-port*)))
    (format p
	    "--- ~s ~s ~s\n"
	    (let ((i (assq '*client-machine* info)))
	      (if i (cdr i) '<missing>))
	    (let ((i (assq 'request-type info)))
	      (if i (cdr i) '<missing>))
	    (let ((i (assq 'request-path info)))
	      (if i (cdr i) '<missing>)))
    (for-each (lambda (i)
		(format p "\t~s => ~s\n" (car i) (cdr i)))
	      info)))

(define (start-service port stop)
  (let ((fd (socket-create (socket-address-family->integer 
			    'address-family/internet)
			   (socket-type->integer 'socket-type/stream)
			   0)))
    (format #t "socket fd => ~s\n" fd)

    (if (not (socket-bind/inet fd port))
	(error "socket-bind/inet failed (~d ~s)" (errno) (errorstr (errno))))

    (if (not (socket-listen fd 3))
	(error "socket-listen failed (~d ~s)" (errno) (errorstr (errno))))

    (handle-signal 'SIGPIPE sigpipe)
    (handle-signal 'SIGINT 
		   (lambda ()
		     (fd-close fd)
		     (format #t "closed socket fd => ~s\n" fd)
		     (stop)))
    fd))

(define (handle-signal name thunk)
  (let ((signum (cadr (assq name (vector->list *unix-signals*)))))
    (register-interrupt-handler! (setup-signal-handler signum) thunk)))

(define *handler-exit* #f)
(define *request-info* '())
(define *client-fd* -1)

;; report an HTTP error (code number `code')
;; with a text of `msg'
;;
;; the `thunk' is called to generate additional explanatory text
;;

(define (http-error-handler code msg thunk)
  (let* ((more-msg (if thunk 
		       (with-output-to-html-string thunk) 
		       ""))
	 (header (with-output-to-string
		   (lambda ()
		     (print-error-header code msg more-msg)))))
    ;;
    (fd-write *client-fd*
	      header 
	      0
	      (string-length header))
    ;;
    (fd-write *client-fd*
	      more-msg
	      0
	      (string-length more-msg))
    ;;
    (summary (cons* (cons 'disposition 'http-error)
		    (cons 'code code)
		    (cons 'msg msg)
		    *request-info*))
    (*handler-exit* 'http-error)))

(define (scheme-signal-handler args)
  (summary (cons* (cons 'disposition 'scheme-signal)
		  (cons 'args args)
		  *request-info*))
  (*handler-exit* 'scheme-signal))

(define (accept-client fd)
  (bind ((client-fd at-address (socket-accept fd)))
    (if client-fd
	(bind ((ip-addr names (host-address->name at-address))
	       (peer-name (if (pair? names)
			      (car names)
			      at-address))
	       (base-info (list (cons '*client-machine* peer-name))))
	  (log-entry 'connect-from peer-name)
	  (xaction "connection fd => ~s (from ~a)\n" client-fd peer-name)
	  (let ((r (call-with-current-continuation
		    (lambda (exit)
		      (fluid-let ((*handler-exit* exit)
				  (*request-info* base-info)
				  (*client-fd* client-fd)
				  (*signal-handler* scheme-signal-handler))
			(handle-client client-fd base-info)
			#f)))))
	    (if r
		(begin
		  (log-entry 'disconnect-from peer-name r)
		  (xaction "client from ~a disconnected; REASON: ~s\n"
			   peer-name
			   r))))
	  (fd-close client-fd))
	(format #t "accept failed\n"))))

(define (sigpipe)
  (if *handler-exit*
      (*handler-exit* 'SIGPIPE)))


(define (handle-client client-fd pre-hdr)
  (xaction "handling client fd ~d, pre-hdr ~s\n" client-fd pre-hdr)
  (bind ((inp (make <html-input-port>
		    underlying-input-port: (make <fd-input-port>
						 file-descriptor: client-fd)))
	 (hdr (parse-request-header inp pre-hdr))
	 (data (parse-request-data inp hdr)))
    (set! *request-info* hdr)
    (log-entry 'request-info *request-info*)
    (bind ((reply reply-type (construct-reply hdr data))
	   (reply-header (construct-reply-header hdr reply reply-type)))
      (fd-write client-fd reply-header 0 (string-length reply-header))
      (if (memq (cdr (assq 'request-type hdr)) '(GET POST))
	  (fd-write client-fd reply 0 (string-length reply)))
      (summary (cons* (cons 'disposition 'ok)
		      (cons 'reply-type reply-type)
		      (cons 'reply-size (string-length reply))
		      hdr)))))

(define (parse-request-data inp headers)
  (if (assq 'content-length headers)
      (list->string
       (map (lambda (i) (read-char inp)) 
	    (range (string->number (cdr (assq 'content-length headers))))))
      #f))

(define (parse-request-header inp pre-hdr)
  (let ((req (read-line inp)))
    (if (eof-object? req)
	(error "no request line"))
    (let ((parts (string-split req " ")))
      (let loop ((i 0) (p parts))
	(if (pair? p)
	    (begin
	      (xaction "REQ[~d]: ~s\n" i (car p))
	      (loop (+ i 1) (cdr p)))))
      (if (not (eq? (length parts) 3))
	  (error "bad request line: ~s" parts))
      (let ((type (string->symbol (string-upcase (list-ref parts 0))))
	    (path (list-ref parts 1))
	    (version (list-ref parts 2)))
	(xaction "request type = ~s, path = ~s, version = ~s\n"
		type path version)
	(let loop ((h (cons* (cons 'request-version version)
			     (cons 'request-path (string-split path "/"))
			     (cons 'request-type type)
			     pre-hdr)))
	  (let ((line (read-line inp)))
	    (xaction "in headers => ~s\n" line)
	    (if (or (eof-object? line)
		    (eq? (string-length line) 0))
		(let ((rev (reverse h)))
		  (if *show-transaction*
		      (begin
			(format #t "----headers-----------\n")
			(print rev)))
		  rev)
		(let ((x (string-search line ": ")))
		  (if x
		      (loop (cons (cons (string->symbol
					 (string-downcase (substring line 0 x)))
					(substring line (skip-whitespaces line (+ x 2))))
				  h))
		      (loop (cons (cons 'unknown line) h)))))))))))

(define (construct-reply-header request reply reply-type)
  (with-output-to-string
    (lambda ()
      (print-reply-header reply reply-type))))

(define (construct-reply request data)
  (let* ((rtype #f)
	 (rtext (with-output-to-string
		  (lambda ()
		    (set! rtype (print-result request data))))))
    (values rtext rtype)))

(define $PRODUCT "X-RScheme-HTTPD/1.0")

(define (print-reply-header result result-type)
  (format #t "HTTP/1.0 200 Document follows\n")
  (format #t "MIME-Version: 1.0\n")
  (format #t "Server: ~a\n" $PRODUCT)
  (format #t "Date: ~a\n" (make-time-string))
  (format #t "Content-Type: ~a\n" result-type)
  (format #t "Content-Length: ~d\n" (string-length result))
  (format #t "Last-Modified: ~a\n" (make-time-string))
  (format #t "X-RScheme-ObjectId: ~d\n"
	  (+ 10000 (modulo (random) 90000)))
  (newline))

(define (print-error-header code msg more-msg)
  (format #t "HTTP/1.0 ~d ~a\n" code msg)
  (format #t "MIME-Version: 1.0\n")
  (format #t "Server: X-Exp-RSscheme/1.0\n")
  (format #t "Date: ~a\n" (make-time-string))
  (format #t "Content-Type: text/html\n")
  (format #t "Content-Length: ~d\n" (string-length more-msg))
  (newline))
  

(define (run port)
  (if (eq? (getuid) 1004)
      (set! *log-file* (open-output-append-file "/u/rscheme/rswebd/log"))
      (begin
	(format #t "*** WARNING *** Not running as `rswebd', log is stdout")
	(set! *log-file* (current-output-port))))
  (call-with-current-continuation
   (lambda (exit)
     (let ((fd (start-service port exit)))
       (let loop ()
	 (accept-client fd)
	 (loop))))))

;(start-service 7800)

(define (error/url-not-found query)
  (http-error-handler 
   404
   "URL Not Found"
   (lambda ()
     (html
      (html-header
       (title (format #t "Error: URL not found")))
      (html-body
       (header-1 (format #t "Error: URL not found"))
       (format #t "The requested URL,")
       (preformatted
	(display (string-join #\/ (cdr (assq 'request-path query)))))
       (format #t "is not available from this server at this time.")
       (par)
       (horz-rule)
       (hyperlink ("http://kali.tkg.com:8080/~server/")
		  (display $PRODUCT)))))))
