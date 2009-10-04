
(define (write-hnvec (self <homogeneous-numeric-vector>) port write-elem)
  (let (((c <<homogeneous-numeric-vector>>) (object-class self)))
    (format port "#~a(" (homogeneous-type-tag c))
    (let loop ((i (initial-state self))
	       (follow #f))
      (if i
	  (begin
	    (if follow
		(output-port-write-char port #\space))
	    (write-elem (current-element self i) port)
	    (loop (next-state self i)))
	  (output-port-write-char port #\))))))

(define-method write-object ((self <homogeneous-numeric-vector>) port)
  (write-hnvec self port write-object))

(define-method display-object ((self <homogeneous-numeric-vector>) port)
  (write-hnvec self port display-object))
