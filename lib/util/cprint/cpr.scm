
(define (cprint item . opt-tabs)
  (let ((tabs (if (null? opt-tabs) #f (car opt-tabs)))
	(newtabs #f))
    (cond
     ((string? item)
      (display item))
     ((char? item)
      (if (char=? item #\newline)
	  (begin
	    (newline)
	    (if tabs
		(spaces tabs)))
	  (display item)))
     ((symbol? item)
      (display (symbol->string item)))
     ((pair? item)
      (if (symbol? (car item))
	  (begin
	    (cprint (car item))
	    (if (null? (cdr item))
		(display "()")
		(begin
		  (display "( ")
		  (cprint (cadr item))
		  (for-each (lambda (arg)
			      (display ", ")
			      (cprint arg))
			    (cddr item))
		  (display " )"))))
	  (for-each
	   (lambda (z)
	     (if (and (char? z)
		      (char=? z #\tab))
		 (set! newtabs (+ (or tabs 0) 2))
		 (cprint z (or newtabs tabs))))
	   item)))
     (else
      (display item)))))

(define (spaces n)
  (display (make-string n #\space)))
