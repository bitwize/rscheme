
;;;
;;;  actually parse html data
;;;  (this is done using the Earley parser from my dissertation)
;;;

(define (parse-html* html-tokens)
  (parse-using-grammar html-tokens *grammar*))
#|
  (let* (((start <production>) (car (lookup *grammar* 'start)))
	 (t (make <tuple>
		  item: (vector-ref (items start) 0)
		  parent-state: #f
		  envt: *grammar*)))
    (let loop ((pstate (make-parser-state (list t) html-tokens 0)))
      ;(print pstate)
      (if (null? (input-stream pstate))
	  (car (parse-tree pstate))
	  (begin
	    ;(format #t "consuming: ~s\n" (car (input-stream pstate)))
	    (loop (advance-input pstate)))))))
|#

(define-method parse-html ((self <list>))
  (parse-html* self))

(define-method parse-html ((self <string>))
  (parse-html* (scan-html self)))


;;;
;;;  transform HTML by running a procedure over all
;;;  the elements (#PCDATA as in strings, and markup
;;;  as lists).
;;;

(define (transform-html proc html)
  (let loop ((html html)
	     (accum '()))
    (if (null? html)
	(reverse accum)
	(let* ((r (if (string? (car html))
		      (proc (car html))
		      (proc (car html) 
			    (lambda ()
			      (transform-html proc (cddar html))))))
	       ;; for convenience, allow a string S as an alias for (S)
	       (r (if (string? r) (list r) r)))
	  (loop (cdr html)
		(append (reverse r) accum))))))
