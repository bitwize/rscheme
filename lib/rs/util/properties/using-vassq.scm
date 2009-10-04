
(define-method push-prop ((props <vector>) key value)
  (vector-append (vector key value) props))

(define-method lookup-prop ((props <vector>) key)
  (let ((k (vassq key props)))
    (if k
        (values (vector-ref props k) 
		#t 
		(lambda (v) 
		  (vector-set! props k v)
		  (values)))
        (values))))

(define-method remove-prop ((props <vector>) rcvr key)
  (let ((k (vassq key props)))
    (if k
	(set-properties! rcvr (vector-append (subvector props 0 (- k 1))
					     (subvector props (+ k 1)))))))
