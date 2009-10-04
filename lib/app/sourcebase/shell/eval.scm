(define *expr* '())


(define (vsh-var-set! (name <string>) value)
  (table-insert! (variables *vsh-state*) name value))

(define (vsh-var-ref (name <string>))
    (table-lookup (variables *vsh-state*) name))

(define (define-executable (name <string>) proc)
    (vsh-var-set! name proc))

(define (vsh-eval expr)
  (fluid-let ((*expr* (cons expr *expr*)))
    (cond
      ((pair? expr)
        (let ((h (vsh-var-ref (car expr))))
	 (cond
	   ((special? h)
	       (apply h (cdr expr)))
	   ((procedure? h)
	       (apply h (map vsh-eval (cdr expr))))
	   ((eq? h #f)
	     (error "undefined form: ~a" (car expr)))
	   (else
	     (error "not an executable form: ~a\nis: ~s" (car expr) h)))))
      ((number? expr)
         expr)
      ((string? expr)
         expr)
      (else
         (error "invalid expression form: ~s" expr)))))

(define (vsh-load path)
   (call-with-input-file
     path
     (lambda (port)
	(let loop ()
	    (let ((arglist (vsh-read port)))
		(if (eof-object? arglist)
		    (values)
		    (if (null? arglist)
			(loop)
			(begin
			    (vsh-eval arglist)
			    (loop)))))))))


(define (special? proc)
  (eq? proc scheme-escape))
  
(define (scheme-escape expr)
    (eval (if (string? expr)
              (read (open-input-string expr))
	      expr)
  	  *self*))

