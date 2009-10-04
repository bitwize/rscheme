(define-message-table ibis 704)

(define-class <service-error> (<error-message>))

#|
(define-method display-object ((self <service-error>) port)
  (format port "SRV-~03d " (msg-code self))
  (apply format port (default-msg-text self) (msg-args self))
  (newline port))

(define (service-error code text . args)
  (let ((e (make <service-error>
                 msg-code: code
                 msg-args: args
		default-msg-text: text)))
    (note (+ 4000 code) "~a" (apply format #f text args))
    (error e)))
|#

(define-macro (service-error code text . args)
  `(service-error* (alloc-message error ,(+ 4000 code) ,text)
                   (vector ,@args)))

(define (service-error* message args)
  (let ((e (make <service-error>
                 message: message
                 arguments: args)))
    (display e)
    (error e)))

