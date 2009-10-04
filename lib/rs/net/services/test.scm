,(use rs.net.httpd
      rs.util.logfile)

(define *root* (make-root-service))

;;;

(define *httpd* (make <container-system-service>
                      active-instances: (make-dequeue)
                      name: "httpd"
                      parent: *root*))

(define *www.rscheme.org/space* (make-web-space))

(define (rscheme-start) 
  (httpd-service-start
   `((web-space . ,*www.rscheme.org/space*)
     (socket-spec . 8082)
     (logs . ,(open-log-fileset basefile: "testn"))
     (verbose? . #t))))

(define (httpd-service-start config)
  (let ((s (open-server-socket (cdr (assq 'socket-spec config))))
        (logs (log-file-message-dest (cdr (assq 'logs config))))
        (z (current-service)))
    ;;
    (set-property! z 'socket s)
    ;;
    (set-status! (current-service) (~ "run (socket fd ~d)" (filedes s)))
    (let loop ((i 0))
      (let ((c (accept-client s))
            (ix #f))
        (handle-http-socket-connection (cdr (assq 'web-space config))
                                       i
                                       c
                                       logs
                                       (cdr (assq 'verbose? config))
                                       start-hook: (lambda ()
                                                     (set! ix 
                                                           (start-instance z)))
                                       end-hook: (lambda ()
                                                   (stop-instance ix))))
      (loop (+ i 1)))))

(define (httpd-service-stop)
  (let ((s (get-property (current-service) 'socket #f)))
    (close s)))

(define (httpd-service-reinit)
  (values))     ; a nop

;;;

(define *www.rscheme.org* (make <procedural-system-service>
                                active-instances: (make-dequeue)
                                name: "rscheme.org"
                                parent: *httpd*
                                startup-procedure: rscheme-start
                                shutdown-procedure: httpd-service-stop
                                reinit-procedure: httpd-service-reinit))

;;;

(define (all-services root)
  ;;
  (define (rec node path)
    (let ((p (if path 
                 (string-append path "/" (name node))
                 (name node))))
      (cons (cons (if path p "/") node)
            (apply append (map (lambda (sub)
                                 (rec sub p))
                               (children node))))))
  ;;
  (rec root #f))
  
(define (list-services root)
  (for-each
   (lambda (t)
     (let* ((name (car t))
            (svc (cdr t))
            (fin (num-instances-finished svc))
            (start (num-instances-started svc))
            (last (last-instance-started svc)))
       (format #t "~20a  ~a [~a] (~d/~d)"
               name
               (machine-bits->string svc)
               (service-status svc)
               (- start fin)
               start)
       (if last
           (format #t " ~a" (time->string last "%Y-%m-%d %H:%M:%S %Z")))
       (if (and (instance? svc <procedural-system-service>)
                (manager-thread svc))
           (format #t " mgr(~a)"
                   (thread-time (manager-thread svc))))
       (format #t " inst(~a)" (instance-time-accum svc))
       (newline)))
   (all-services root)))

(define (service-status/html)
  `(table
    (@ (border "1"))
    (tr
     (th (@ (rowspan "2")) "Name")
     (th (@ (rowspan "2")) "Status")
     (th (@ (rowspan "2")) "Mgr Time")
     (th (@ (colspan "4")) "Instances")
     (th (@ (rowspan "2")) "Action"))
    (tr
     (th "Last")
     (th "Current")
     (th "Total")
     (th "Time"))
    ,@(map
       (lambda (t)
         (let* ((name (car t))
                (svc (cdr t))
                (fin (num-instances-finished svc))
                (start (num-instances-started svc))
                (last (last-instance-started svc)))
           `(tr
             (td ,name)
             (td ,(service-status svc))
             (td ,(if (instance? svc <procedural-system-service>)
                      (if (manager-thread svc)
                          (~ "~a" (thread-time (manager-thread svc)))
                          "-")
                      '(i "n/a")))
             (td ,(if last
                      (time->string last "%Y-%m-%d %H:%M:%S %Z")
                      "-"))
             (td ,(to-string (- start fin)))
             (td ,(to-string start))
             (td ,(~ "~a" (instance-time-accum svc)))
             (td (form (@ (method "POST")
                          (action ,(~ "/status/action/~a"
                                      (machine-bits->string svc))))
                       (input (@ (name "what")
                                 (type "submit")
                                 (class "button")
                                 (value "Start")))
                       (input (@ (name "what")
                                 (type "submit")
                                 (class "button")
                                 (value "Reinit")))
                       (input (@ (name "what")
                                 (type "submit")
                                 (class "button")
                                 (value "Stop"))))))))
       (all-services *root*))))

(define (status-page path req)
  (sxml->content
   `(html
     (head
      (title "Server Status Report"))
     (h1 "Server Status Report")
     (p
      (b "path = ") (code ,(~ "~s" path)))
     (hr)
     ,(service-status/html)
     (hr)
     (p
      "All's well that ends well"))))

(define *rscheme-top* (make-uri-directory))

(uri-link-add! *www.rscheme.org/space* "" *rscheme-top*)

(uri-link-add! *rscheme-top* "status" (make-uri-simple-script (& status-page)))
