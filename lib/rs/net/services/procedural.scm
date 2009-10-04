(define-class <procedural-system-service> (<system-service>)
  (startup-procedure type: <function>)
  (shutdown-procedure type: <function>)
  (reinit-procedure type: <function>)
  (status init-value: "unknown")        ; can be a string, a list, or a thunk
  (manager-thread init-value: #f))

(define-method service-status ((self <procedural-system-service>))
  (let ((s (status self)))
    (cond
     ((string? s)
      s)
     ((pair? s)
      (case (car s)
        ((terminated) "Terminated")
        (else "<~s>" (car s))))
     (else
      (s)))))

(define-method service-start ((self <procedural-system-service>) #rest opts)
  (set-status! self "initializing")
  (set-manager-thread! self
                       (make-thread
                        (lambda ()
                          (handler-case
                           (call-with-service
                            self
                            (lambda ()
                              (set-status! self "starting")
                              (apply (startup-procedure self) opts)))
                           ((<condition> condition: c)
                            (set-status! self (list 'terminated c)))))
                        (name self)))
  (thread-resume (manager-thread self)))

(define-method service-shutdown ((self <procedural-system-service>) #rest opts)
  (let ((f (shutdown-procedure self)))
    (set-status! self "shutting down")
    (if (null? opts)
        (call-with-service self f)
        (call-with-service
         self
         (lambda ()
           (apply f opts))))
    (set-status! self "shut down")))

(define-method service-reinit ((self <procedural-system-service>) #rest opts)
  (let ((f (reinit-procedure self)))
    (set-status! self "reinitializing")
    (if (null? opts)
        (call-with-service self f)
        (call-with-service
         self
         (lambda ()
           (apply f opts))))
    (set-status! self "running")))
