
(define (make-bindings)
  (make-dequeue))

(define (add-binding! b k v)
  (dequeue-push-back! b k)
  (dequeue-push-back! b v)
  (values))

(define (binding-lookup b k)
  ((binding-lookuper b) k))

(define (binding-lookuper b)
  (let* (((v <vector>) (dequeue-state b)))
    (lambda (k)
      (let ((x (vassq k v)))
	(if x
	    (vector-ref v x)
	    (values))))))

(define (bindings->alist b)
  (let ((v (dequeue-state b)))
    (let loop ((r '())
	       (i (vector-length v)))
      (if (> i 0)
	  (loop (cons (cons (vector-ref v (- i 2))
			    (vector-ref v (- i 1)))
		      r)
		(- i 2))
	  r))))
