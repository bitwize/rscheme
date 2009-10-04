
(define *num-threads-alloced* 0)
(define *thread-table* #f)

;;;
;;;  the thread table indexes thread numbers to weak-thread-ptrs
;;;

(define-class <weak-thread-ptr> (<object>) :weak1
  thread)

;;;

(define-method to-string ((self <thread>))
  (thread-name self))

(define-method thread-name ((self <thread>))
  (string-append (internal-thread-name self) 
		 "-" 
		 (number->string (thread-number self))))

(define-method finalize ((self <thread>))
  ;(format #t " [~a] died\n" (thread-name self))
  (table-remove! *thread-table* (thread-number self)))

(define (make-thread (thunk <function>) . opts)
  (if (null? opts)
      (make-thread* thunk "thread" (current-thread-group))
      (if (null? (cdr opts))
	  (make-thread* thunk (car opts) (current-thread-group))
	  (make-thread* thunk (car opts) (cadr opts)))))

(define (make-initial-thread-vars basis)
  (let ((v (clone basis)))
    (update-thread-state! v
                          (*handler-chain*
                           (list
                            (make <handler-context>
                                  condition-class: <condition>
                                  handler-proc: thread-backstop-handler))))
    v))


(define (make-thread* (thunk <function>)
		      (name <string>)
		      (group <thread-group>))
  (bind ((tv (make-initial-thread-vars (get-thread-state-reg)))
         ((t <thread>) (make <thread>
			     thread-stack: (%make <partial-continuation>
						  thunk
						  thread-entry
						  (code-pointer thread-entry)
						  #f) ;; saved continuation reg
			     thread-vars: tv
			     thread-dynamic-state: (get-dynamic-state-reg)
			     thread-initial-dynamic-state: (get-dynamic-state-reg)
			     thread-time: (seconds->interval 0)
			     internal-thread-name: name
			     thread-number: -1
			     thread-group: group))
	 (tn (critical-section (end)
	       (let ((n *num-threads-alloced*))
		 (set! *num-threads-alloced* (add1 n))
		 (end n))))
	 (weak-ref (%make <weak-thread-ptr> t)))
       (set-thread-number! t tn)
       (table-insert! *thread-table* tn weak-ref)
       (set-threads! group (cons weak-ref (threads group)))
       (register-for-finalization t)
       t))
