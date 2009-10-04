,(use repl)

(define-macro (break-point)
  `(do-break-point ,(make-introspector (current-environment))))

(define (make-introspector env)
  ;; Builds an introspection procedure (at compile time) to 
  ;; provide complete r/w access to all variables in scope
  (let loop ((vars '())
	     (e env))
    (cond
     ((instance? e <top-level-contour>)
      `(lambda (op #optional var val)
	 (case op
	   ((top)
	    ',e)
	   ((list)
	    ',vars)
	   ((get)
	    (case var
	      ,@(map (lambda (v)
		       `((,v) ,v))
		     vars)))
	   ((set)
	    (case var
	      ,@(map (lambda (v)
		       `((,v) (set! ,v val)))
		     vars))))))
     ((instance? e <lexical-contour>)
      (loop (append vars (map car (name->bindings e)))
	    (lexical-enclosing e)))
     (else
      (loop vars (lexical-enclosing e))))))
    

(define (do-break-point inspector)
  (with-command-procs
   (list (list 'set
	       (break-point-set inspector)
	       '(",(set var expr)"
		 "set variable value"))
	 (list 'list 
	       (break-point-list inspector)
	       '(",list" "list variables"))
	 (list 'cont
	       break-point-continue 
	       '(",(cont expr ...)" "continue with exprs")))
   (lambda ()
     (cmd-loop *self* "bp[~d]:"))))

(define (break-point-set inspector)
  (lambda (envt args)
    (inspector 'set
	       (car args)
	       (eval-in-envt (cadr args) envt))))

(define (break-point-list inspector)
  (lambda (envt args)
    (for-each
     (lambda (v)
       (format #t "   ~s = ~#*@40s\n" v (inspector 'get v)))
     (inspector 'list))))

(define (break-point-continue envt args)
  (if args
      (apply (cmd-loop-exit *cmd-loop*)
	     (map (lambda (expr)
		    (eval-in-envt expr envt))
		  args))
      ((cmd-loop-exit *cmd-loop*))))

;;;;;;


(define (t y)
  (let ((x 3))
    (format #t "x = ~s\n" x)
    (bind ((#rest r (break-point)))
      (format #t "bp => ~s\n" r)
      (format #t "now x = ~s\n" x))))
