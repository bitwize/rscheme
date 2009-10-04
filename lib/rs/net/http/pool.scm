,(use tables)

(define-class <per-server-pool> (<object>)
  (lock)
  (connector type: <function>)
  (busy init-value: '())
  (available init-value: '()))

(define-method name ((self <per-server-pool>))
  (name (lock self)))

(define *pool-lock* (make-semaphore "http-server-pool" 1))
(define *cnxn-pool* (make-string-table))

(define (make-uri-connector items)
  (cond
   ((string=? (car items) "http")
    (if (= (length items) 3)
        (let ((port (string->number (caddr items))))
          (lambda ()
            (open-http-client server: (cadr items) port: port)))
        (lambda ()
          (open-http-client server: (cadr items)))))
   (else
    (error "make-uri-connector: can't handle method `~s'" (car items)))))
        

(define (url-pool-clean)
  (for-each 
   (lambda (at)
     (let ((psp (table-lookup *cnxn-pool* at)))
       (if psp
           (with-semaphore
            (lock psp)
            (begin
              (for-each close (available psp))
              (set-available! psp '()))))))
   (key-sequence *cnxn-pool*))
  (values))

(define (psp-get (at <string>))
  (let ((h (table-lookup *cnxn-pool* at)))
    (or h
        (with-semaphore
         *pool-lock*
         (or (table-lookup *cnxn-pool* at)
             (let ((new (make <per-server-pool>
                              connector: (make-uri-connector
                                          (car (string->url at)))
                              lock: (make-semaphore at 1))))
               (table-insert! *cnxn-pool* at new)
               new))))))

(define (pool-get (at <string>))
  (let ((t (time)))
    (let loop ()
      (let ((c (pool-get1 at)))
        (if (stale? c t)
            (begin
              (close c)
              (pool-dropped c)
              (loop))
            c)))))

(define (pool-get1 (at <string>))
  (let (((psp <per-server-pool>) (psp-get at)))
    (with-semaphore
     (lock psp)
     (if (null? (available psp))
         (let ((c ((connector psp))))
           (set-name! c at)
           (set-busy! psp (cons c (busy psp)))
           c)
         (let ((c (car (available psp))))
           (set-busy! psp (cons c (busy psp)))
           (set-available! psp (cdr (available psp)))
           c)))))

(define (pool-dropped item)
  (let (((psp <per-server-pool>) (psp-get (name item))))
    (assert (memq item (busy psp)))
    ;;
    (with-semaphore
     (lock psp)
     (set-busy! psp (delq item (busy psp))))
    (values)))

(define (pool-put item)
  (let (((psp <per-server-pool>) (psp-get (name item))))
    (assert (memq item (busy psp)))
    ;;
    (with-semaphore
     (lock psp)
     (begin
       (set-busy! psp (delq item (busy psp)))
       (set-available! psp (append (available psp) (list item)))))
    (values)))

;;;

(define (url-get (url <string>))
  (bind ((a (string->url url))
         (p (pool-get (url->string (list (car a) '(""))))))
    (dynamic-wind
        #f
        (lambda ()
          (let ((rsp (http-get p uri: (if (equal? (cadr a) '(""))
                                          "/"
                                          (string-join #\/ (cadr a))))))
            (values (read-content rsp) 
                    (append
                     ;; ---[ last-modified ]---
                     (cond
                      ((not (has-property? rsp 'last-modified))
                       '())
                      ((string->time (get-property rsp 'last-modified))
                       => (lambda (t)
                            (list (cons 'last-modified t))))
                      (else
                       '()))
                     ;; ---[ content-type ]---
                     (cond
                      ((has-property? rsp 'content-type)
                       (list (cons 'content-type 
                                   (get-property rsp 'content-type))))
                      (else
                       '()))))))
        (lambda ()
          (pool-put p)))))
