
;;; transform a form, doing constant folding

(define (transform-using-patterns input (pattern-set <patterns>))
  (transform* (reduce transform-using-patterns
                      input
                      (pre-patterns pattern-set))
	      pattern-set))

;;; transform a form without benefit of constant folding

(define (transform* input pattern-set)
  ;(format #t "transform: ~#*@60s\n" input)
  (cond
   ((pair? input)
    (let* ((args (transform-list (cdr input) pattern-set))
	   (head (transform-using-patterns (car input) pattern-set))
	   (rplc (replace-matches (cons-dif input head args) pattern-set)))
      (if (eq? rplc input)
	  input
	  (transform-using-patterns rplc pattern-set))))
   ((symbol? input)
    (let ((a (assq input (compile-time-constants pattern-set))))
      (if a
	  (transform-using-patterns (cdr a) pattern-set)
	  input)))
   (else
    (let ((a (assq input (compile-time-constants pattern-set))))
      (if a
	  (transform-using-patterns (cdr a) pattern-set)
	  input)))))

(define (transform-list input pattern-set)
  (cond
   ((null? input)
    '())
   ((pair? input)
    (cons-dif input
	      (transform-using-patterns (car input) pattern-set)
	      (transform-list (cdr input) pattern-set)))
   (else
    (error "transform-list: improper: ~s" input))))

;;;


(define (replace-matches input pattern-set)
  (if (symbol? (car input))
      ;; only know how to do replacements when the head is a symbol
      (replace-matches* input pattern-set)
      input))

(define (replace-matches* input pattern-set)
  (let loop ((rules (or (table-lookup (rule-by-head pattern-set) (car input))
			'()))
	     (input input))
    (if (null? rules)
	input
	(let* ((r (car rules))
	       (bdgs (match-rule r input)))
	  (if (and bdgs
		   (or (eq? (condition r) #t) 
		       ((condition r) bdgs)))
	      (begin
		(add-gensym-vars (translation (car rules)) bdgs)
		(loop (cdr rules) (substitute (translation (car rules)) bdgs)))
	      (loop (cdr rules) input))))))

(define genpsym
  (let ((n 0))
    (lambda ()
      (set! n (+ n 1))
      (symbol-append "t_" n))))

(define (add-gensym-vars template b)
  (vector-for-each
   (lambda (var)
     (add-binding! b var (genpsym)))
   (collect-gensym-vars template)))

(define (substitute template b)
  (sublis (bindings->alist b) template))

;;;

