(define-method push-prop ((props <list>) key value)
  (cons (cons key value) props))

(define-method lookup-prop ((props <list>) key)
  (let ((p (assq key props)))
    (if p
        (values (cdr p) #t (lambda (v) (set-cdr! p v) (values)))
        (values))))

(define-method remove-prop ((props <list>) rcvr key)
  (let ((p (assq key props)))
    (if p
	(set-properties! rcvr (delq! p props)))))

;;
