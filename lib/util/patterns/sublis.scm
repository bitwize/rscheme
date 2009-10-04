(define (cons-dif (old <pair>) new-car new-cdr)
  (if (and (eq? new-car (car old))
	   (eq? new-cdr (cdr old)))
      (begin
	;(format #t "no change: ~s\n" old)
	old)
      (begin
	;(format #t "changed: ~s vs ~s\n" old (cons new-car new-cdr))
	(cons new-car new-cdr))))

;;; portable Scheme implementation of Common Lisp `sublis'
;;; (modified to support scheme-macros-like `...', which acts
;;; as a splicing/constructor operator)

(define (sublis bdgs tem)
  (cond
   ((symbol? tem)
    (let ((b (assq tem bdgs)))
      (if b
	  (if (vector? (cdr b))
	      ;; expand (foo . ?x) => (foo ?x[0] ... ?x[n-1])
	      (vector->list (cdr b))
	      (cdr b))
	  tem)))
   ;; `...'
   ((and (pair? tem)
	 (pair? (cdr tem))
	 (eq? (cadr tem) '...))
    (let ((h (subddd bdgs (car tem)))
	  (t (sublis bdgs (cddr tem))))
      (if (null? t)
	  h
	  (append h t))))
   ;
   ((pair? tem)
    (cons-dif tem (sublis bdgs (car tem)) (sublis bdgs (cdr tem))))
   (else
    tem)))

(define (subddd bdgs tem)
  (let ((n (num-iters tem bdgs)))
    (let loop ((r '())
	       (i 0))
      (if (= i n)
	  (reverse r)
	  (loop (cons (sublis (map (lambda (b)
				     (if (vector? (cdr b))
					 (cons (car b) (vector-ref (cdr b) i))
					 b))
				   bdgs)
			      tem)
		      r)
		(+ i 1))))))

;;; compute the number of iterations for a ...-ed template form

(define (num-iters tem bdgs)
  (let loop ((n #f)
	     (keys (vector->list (collect-pattern-vars tem))))
    (if (null? keys)
	;; hack:  if none are lists, assume there's one expansion
	;; (I'd rather signal an error here)
	(or n 1)
	(let ((b (assq (car keys) bdgs)))
	  (if (and b (vector? (cdr b)))
	      (if n
		  ;; similar hack... take the min instead of error
		  (loop (min n (vector-length (cdr b))) (cdr keys))
		  (loop (vector-length (cdr b)) (cdr keys)))
	      (loop n (cdr keys)))))))
	    
