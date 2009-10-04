
(define-class <structure-descriptor> (<object>)
  (structure-type type: <<class>>)
  (instance-size type: <fixnum>))

(define (make-packing-list descr #optional (endian default: 'native))
  (let ((pt-list (map get-packing-type descr)))
    (make-gvec*
     <structure-descriptor>
     <byte-vector>
     (cons (compute-buffer-length pt-list)
	   (map (lambda (pt)
		  (let ((pid (packing-type-id pt)))
		    ;; if it was specified as `native', promote 
		    ;; to as specified via `endianess'
		    (if (eq? (quotient (modulo pid 6) 2) 0)
			(+ pid (case endian
				 ((native) 0)
				 ((big-endian) 2)
				 ((little-endian) 6)
				 (else
				  (em 402 "make-packing-list: unknown endian `~s';\n   use one of (native big-endian little-endian)" endian))))
			;; otherwise, leave it as-is
			pid)))
		pt-list)))))


