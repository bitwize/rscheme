
(define-constant *message-prefix* #f)

#|;;; Nobody uses this feature...
(define (with-message-prefix plc thunk)
  (fluid-let ((*message-prefix* plc))
    (thunk)))
|#

;(&module (export with-message-prefix))

;;;

(define-class <messaged-error> (<condition>)            ; (sm ...)
  (message-prefix init-value: #f)
  (message type: <error-message>)
  (arguments type: <vector> init-value: '#()))

(define-class <messaged-fatal> (<error>)                ; (em ...)
  (message-prefix init-value: #f)
  (message type: <fatal-message>)
  (arguments type: <vector> init-value: '#()))

(define-method display-object ((self <messaged-error>) port)
  (display-message port (message self) (arguments self) (message-prefix self)))

(define-method display-object ((self <messaged-fatal>) port)
  (display-message port (message self) (arguments self) (message-prefix self)))


(define (foo t args)
  (cond
   ((eq? (car args) 'at:)
    (bind ((a b (foo t (cddr args))))
      (values a b (list (cadr args)))))
   ((eq? (car args) 'type:)
    (bind ((a b c (foo t (cddr args))))
      (values (cons (cadr args) (cdr a)) b c)))
   (else
    (if (fixnum? (car args))
        (if (fixnum? (cadr args))
            (values (list t (car args) (cadr args) (caddr args)) (cdddr args) '())
            (values (list t (car args) (cadr args)) (cddr args) '()))
	(values (list t (car args)) (cdr args) '())))))

(define (signal-message msg argv . mp)
  (let ((m (make <messaged-error>
                 message-prefix: (if (null? mp) *message-prefix* (car mp))
                 message: msg
                 arguments: argv)))
    (display-message (current-message-dest) msg argv #f)
    (signal m)))

(define (error-message msg argv . mp)
  (let ((m (make <messaged-fatal>
                 message-prefix: (if (null? mp) *message-prefix* (car mp))
                 message: msg
                 arguments: argv)))
    (display-message (current-message-dest) msg argv #f)
    (error m)))

;;; signal a message

(define-macro (sm . args)
  (bind ((msg args xtra (foo 'error args)))
    `(',signal-message (alloc-message ,args ,@msg)
		       (',vector ,@args) ,@xtra)))

(define-macro (em . args)
  (bind ((msg args xtra (foo 'fatal args)))
    `(',error-message (alloc-message ,args ,@msg)
		      (',vector ,@args) ,@xtra)))
   

;(&module (export sm em))
