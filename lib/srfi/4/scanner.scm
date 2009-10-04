
;;;  coming in, the `str' includes the leading sharp (#)

(define (scan-hnvec (p <input-port>) (str <string>))
  (let ((h (table-lookup *hnvec-tag-table* (substring str 1))))
    (if (and h
	     ;; note that peek char could ==> EOF
	     (eq? (input-port-peek-char p) #\())
	(bind ((line (input-port-line-number p))
	       (lst x (input-port-read p))
	       (l ((homogeneous-type-constructor h) lst)))
	      (make-token '<literal> l line))
	#f)))

(with-module iolib (add-sharp-handler! scan-hnvec))

