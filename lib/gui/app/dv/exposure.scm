
(define (my-exposure-handler display
			     #rest ignore
			     #key window
			          x
				  y
				  width
				  height
				  count)
  (if (eq? count 0)
      (let ((p (window-exposure-thunk window)))
	(if p
	    (begin
	      (p)
	      (display-force-output display))))))
