(define-class <application> (<object>)
    structure-version
    application-version
    (name type: <string>)
    (creation-time type: <time>)
    (user-table type: <string-table>)
    (group-table type: <string-table>)
    (file-system-table type: <string-table>)
    (change-request-table type: <hash-integer-table>)
    (property-table type: <string-table>)
    (num-change-requests type: <fixnum> init-value: 0)
    (domain-table type: <symbol-table>)
    world-group
    (config-info type: <list> init-value: '())
    (state init-value: 'initializing)
    (service-state init-value: 'unavailable)
    (state-data init-value: #f)
    (num-nodes-alloced type: <fixnum> init-value: 0)
    (mail-queue type: <list> init-value: '())
    (postpone-queue type: <list> init-value: '())
    (properties type: <list> init-value: '()))

#|
(define (has-property? obj prop)
  (and (assq prop (properties obj)) #t))

(define (get-property obj prop . opt)
  (let ((e (assq prop (properties obj))))
    (cond
     (e
      (cdr e))
     ((null? opt)
      (error "~s: no property `~s'" obj prop))
     ((procedure? (car opt))
      ((car opt)))
     (else
      (car opt)))))

(define (set-property! obj prop val)
  (let ((e (assq prop (properties obj))))
    (if e
	(let ((o (cdr e)))
	  (set-cdr! e val)
	  o)
	(begin
	  (set-properties! obj (cons (cons prop val) (properties obj)))
	  (values)))))

|#
