(define-class <container-system-service> (<system-service>)
  ;; does nothing except run it's children
  (status init-value: "stopped"))

(define-method service-start ((self <container-system-service>) #rest options)
  (set-status! self "starting...")
  (for-each
   (lambda (child)
     (apply service-start child options))
   (children self))
  (set-status! self "running"))

(define-method service-shutdown ((self <container-system-service>) #rest options)
  (set-status! self "stopping...")
  (for-each
   (lambda (child)
     (apply service-shutdown child options))
   (children self))
  (set-status! self "stopped"))

(define-method service-reinit ((self <container-system-service>) #rest options)
  (set-status! self "reinitializing...")
  (for-each
   (lambda (child)
     (apply service-reinit child options))
   (children self))
  (set-status! self "running"))

(define-method service-status ((self <container-system-service>))
  (status self))


;;;

(define (make-root-service)
  (make <container-system-service>
        name: ""
        active-instances: (make-dequeue)
        status: "stopped"))


