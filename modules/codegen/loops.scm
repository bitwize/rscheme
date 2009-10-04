#|------------------------------------------------------------*-Scheme-*--|
 | File:    modules/codegen/loops.scm
 |
 |          Copyright (C)1997 Donovan Kolbly <d.kolbly@rscheme.org>
 |          as part of the RScheme project, licensed for free use.
 |          See <http://www.rscheme.org/> for the latest information.
 |
 | File version:     1.5
 | File mod date:    1997-11-29 23:10:33
 | System build:     v0.7.3.4-b7u, 2007-05-30
 | Owned by module:  codegen
 |
 `------------------------------------------------------------------------|#

(define (make-runtime-bind*2 N mode)

    (define (make-shift-regs)
	(let loop ((num (if (caddr mode)
			    (add1 (cadr mode))
			    (cadr mode)))
		    (src N)
		    (dst (cadddr mode))
		    (result '()))
	    (if (eq? num 0)
		result
		(loop (sub1 num)
			(add1 src)
			(add1 dst)
			(cons `(set! (reg ,dst #f) (<obj> ref (reg ,src #f)))
			    result)))))

    (define (make-heap-bind)
	(let loop ((num (if (caddr mode)
			    (add1 (cadr mode))
			    (cadr mode)))
		    (src N)
		    (result '()))
	    (if (eq? num 0)
		(list (cons 'bind (reverse result)))
		(loop (sub1 num)
			(add1 src)
			(cons `(<obj> ref (reg ,src #f))
			    result)))))

    (if (eq? (car mode) 'reg-bind)
	(make-shift-regs)
	(make-heap-bind)))

;; gen-aml-bind*0 is used for loops that bind no vars
; -- untested as of 94.05.07

(define (gen-aml-bind*0 self ct-envt in-regs mode)
    (let ((body (gen-aml (body self) ct-envt in-regs mode)))
	(if (eq? mode 'value)
	    (let ((save-reg `(reg ,(length in-regs) #f)))
	      (make-se (append (se-stmt body)
			       `((set! ,save-reg ,(obj-expr (se-expr body)))))
		       `(<obj> ref ,save-reg)))
	    body)))

(define (gen-aml-bind*2 self ct-envt in-regs mode)
    (let* ((reg-alloc? (not (or (does-save? (body self))
    				(does-lambda? (body self)))))
    	   (dbl-bind? (and reg-alloc?
	   		   (pair? mode)
			   (eq? (car mode) 'reg-bind)))
	   (bindings (bindings (envt self)))
	   (body (gen-aml (body self)
			    (if reg-alloc?
			    	ct-envt
				(cons bindings ct-envt))
			    (if reg-alloc?
			    	(append (reverse bindings) in-regs)
				in-regs)
			    mode))
	   (bind-mode   (if reg-alloc?
			    (list 'reg-bind
				(num-args self)
				(rest? self)
				(if dbl-bind?
				    (+ (cadddr mode) (length in-regs))
				    (length in-regs)))
			    (list 'bind (num-args self) (rest? self))))
	   (binder  (make-runtime-bind*2 (length in-regs) bind-mode)))
	(if (eq? mode 'value)
	    (let ((save-reg `(reg ,(length in-regs) #f)))
		(make-se (append binder
				 (se-stmt body)
				 `((set! ,save-reg ,(obj-expr (se-expr body))))
				 (if reg-alloc? '() '((unbind))))
			 `(<obj> ref ,save-reg)))
	    (if (or (eq? mode 'tail) reg-alloc?)
		(append binder body)
		(append binder body '((unbind)))))))

(define (gen-aml-loop self ct-envt in-regs mode)
    (let ((new-label (alloc-label))
    	  (n (num-args (inits self))))
	(set-first-arg-reg! self (length in-regs))
	(set-ct-envt! self ct-envt)
	(set-label! (loop-var self) new-label)
	(if (> n 0)
	    (let ((args-code (gen-aml (inits self)
					ct-envt
					in-regs
					`(reg-bind ,n #f ,(length in-regs)))))
		(set-inits! self #f)
		(let ((b (gen-aml-bind*2 self ct-envt in-regs mode)))
		  (if (eq? mode 'value)
		      (make-se (append args-code
				       `((jump ,(+ n (length in-regs))
					       ,new-label)
					 (label ,new-label))
				       (se-stmt b))
			       (se-expr b))
		      (append args-code
			      `((jump ,(+ n (length in-regs)) ,new-label)
				(label ,new-label))
			      b))))
	    (let ((b (gen-aml-bind*0 self ct-envt in-regs mode)))
	      (set-inits! self #f)
	      (if (eq? mode 'value)
		  (make-se (append `((jump ,n ,new-label)
				     (label ,new-label))
				   (se-stmt b))
			   (se-expr b))
		  (append `((jump ,n ,new-label)
			    (label ,new-label))
			  b))))))

;;
;;  generate the actual BJUMP aml
;;
;;  +----------+
;;  |  REG 0   |
;;  +----------+
;;  |   ...    |
;;  +----------+
;;  |  REG a   |
;;  +----------+ <---- m = first arg reg
;;  |  REG b   |   ^   (REG0 ... REGa are reg bdgs from outside the loop)
;;  +----------+   |
;;  |   ....   |   |n
;;  +----------+   |
;;  |  REG c   |   v
;;  +----------+ <--- m + n = after loop args are bound
;;  |  REG d   |      (REGb .. REGc are the loop arguments themselves)
;;  +----------+
;;  |   ....   |
;;  +----------+
;;  |  REG e   |
;;  +----------+ <--- (length in-regs)
;;  |   ...    |       includes bindings (REGd ... REGe) introduced 
;;  +----------+       in this code path for this loop iteration -- ie,
;;  |          |       between the (let loop ...) and the (loop) call
;;  +----------+
;;

(define (gen-aml-jump self cte in-regs mode)
    (let* ((m (first-arg-reg (loop-icode (loop-var self))))
	   (n (num-args (args self)))
	   (args-code (gen-aml (args self)
			        cte
			        in-regs
			        `(reg-bind ,n #f ,(length in-regs)))))
	(append args-code
		(pop-envts cte
			   (ct-envt (loop-icode (loop-var self))))
		(shift-regs n (length in-regs) m)
		`((bjump ,(+ n m)
			 ,(label (loop-var self)))))))
