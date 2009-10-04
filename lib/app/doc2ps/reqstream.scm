;;;

#|
(define-method write-object ((self <closure>) port)
  (format port "#[<closure> env ~a]" 
          (machine-bits->string (environment self))))
|#

(define-class <request-stream> (<object>)
  (current-request init-value: #f)
  (suspension init-value: #f)
  (been-read? init-value: #f))

;;;

(define (make-request-stream proc)
  ;;; `proc' is a procedure of one argument, which is the <request-stream>.
  ;;; Said procedure will be called once a request is made on the
  ;;; request stream (which is also returned to the caller of
  ;;; make-request-stream)
  ;;;
  (let ((self (make <request-stream>)))
    (call-with-current-continuation
     (lambda (return)
       (call-with-current-continuation
        (lambda (cc)
          (set-suspension! self cc)
          (return self)))
       (proc self)
       (let ((s (suspension self)))
         (set-suspension! self #f)
         (s))))))
    
;;;
;;;  This method is used by the consumer
;;;

(define-method request ((self <request-stream>) #rest args)
  (set-current-request! self args)
  (set-been-read?! self #f)
  (let ((resumer (suspension self)))
    (if resumer
        (call-with-current-continuation
         (lambda (cc)
           (set-suspension! self cc)
           (resumer)))
        (values))))

;;;
;;;  These methods are used by the producer
;;;

(define-method request-return ((self <request-stream>) #rest args)
  ;; we should have read the request by the time we return...
  (assert (been-read? self))
  (let ((s (suspension self)))
    (call-with-current-continuation
     (lambda (cc)
       (set-current-request! self #f)
       (set-suspension! self cc)
       (apply s args)))))

(define-method request-read ((self <request-stream>))
  (assert (current-request self))
  (let ((r (current-request self)))
    (set-been-read?! self #t)
    (list->values r)))

(define-method request-peek ((self <request-stream>))
  (list->values (current-request self)))

;;;

(define (adder-proc s)
  (let loop ()
    (bind ((a b (request-read s)))
      (request-return s (+ a b))
      (loop))))

