
(define (thread-group-members (g <thread-group>))
  (critical-section (end)
    (let loop ((res '())
	       (lst (threads g))
	       (prev #f))
      (if (null? lst)
	  (end res)
	  (let ((t (thread (car lst))))
	    (if t
		(loop (cons t res)
		      (cdr lst)
		      lst)
		(if prev
		    (begin
		      (set-cdr! prev (cdr lst))
		      (loop res (cdr lst) prev))
		    (begin
		      (set-threads! g (cdr lst))
		      (loop res (cdr lst) #f)))))))))
		    
	      