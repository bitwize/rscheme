;;; format some markup

(define-method meta-port ((self <html-output-port>))
  (underlying-output-port self))

(define-method meta-port ((self <output-port>))
  self)

(define-method pop-tag ((self <output-port>))
  (values))

(define-method pop-tag ((self <html-output-port>))
  (if (pair? (tag-stack self))
      (let ((t (car (tag-stack self))))
	(set-tag-stack! self (cdr (tag-stack self)))
	t)
      (values)))

(define-method push-tag ((self <html-output-port>) tag)
  (set-tag-stack! self (cons tag (tag-stack self)))
  (values))

(define-method push-tag ((self <output-port>) tag)
  (values))

(define (/-formatter info)
  (values 
   0
   (if (braced-modifier info)
       (lambda (port)
	 (let ((t (braced-modifier info)))
	   (write-string (meta-port port)
			 (string-append "<" t ">"))
	      (let ((i (string-search t #\space)))
		(if i
		    (push-tag port (substring t 0 i))
		    (push-tag port t)))))
       (lambda (port)
	 (let ((t (pop-tag port)))
	   (if t
	       (write-string (meta-port port) (string-append "</" t ">"))
	       (write-string (meta-port port) "</>")))))))

(if (assq #\/ *global-formatters*)
    (begin
      (set-cdr! (assq #\/ *global-formatters*) /-formatter)
      (values))
    (append! *global-formatters* (list (cons #\/ /-formatter))))
