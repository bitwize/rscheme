,(use rs.sys.threads.manager
      regex
      tables
      syscalls)

(load "boundedline.scm")
(load "smtpd.scm")
(load "auth.scm")


(define (msa-inject* (self <smtp-message>))
  ;; an indirection to 
  (msa-inject self))

(define (msa-inject (self <smtp-message>))
  ;;
  ;;  Check authorization
  ;;
  (print (content self))
  (make-nonce))

(load "outbound.scm")
(load "manager.scm")
(load "blackboard.scm")

(define (g)
  (let ((h (or (getenv "MTA_HOSTNAME")
               (with-module unixm (hostname)))))
    ;;
    (thread-resume (make-thread mailq-run "mail:outgoing"))
    (thread-resume (make-thread dispatcher-run "mail:dispatch"))
    ;;
    (let ((a (make <smtp-server>
                   smtp-auth-required?: #t
                   injector: msa-inject*
                   name: h
                   port: 5587
                   rcpt-domains: (domains (root-object *mail-ps*))
                   smtp-auth-clients: (users (root-object *mail-ps*)))))
      (thread-resume
       (make-thread
        (lambda ()
          (run-smtp-server a))
        "smtpd:587")))
    ;;
    (let ((a (make <smtp-server>
                   smtp-auth-required?: #f
                   injector: msa-inject*
                   name: h
                   port: 2525
                   rcpt-domains: (domains (root-object *mail-ps*))
                   smtp-auth-clients: #f)))
      (thread-resume
       (make-thread
        (lambda ()
          (run-smtp-server a))
        "smtpd:25")))))
  

(define (try-again)
  (let ((r (root-object *mail-ps*)))
    (dequeue-push-back! (pending-incoming r)
                        (vector-ref (dequeue-pop-front! 
                                     (pending-exceptions r))
                                    2))
    (commit *mail-ps*)
    (values)))

    
