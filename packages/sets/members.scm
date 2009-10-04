
(define (member?-tester c test)
  (cond
   ((and (pair? c)
	 (eq? test eq?))
    (lambda (item) 
      (and (memq item c) #t)))
   ((and (pair? c)
	 (eq? test equal?))
    (lambda (item)
      (and (member item c) #t)))
   ((null? c)
    (lambda (item)
      #f))
   ((instance? c <table>)
    (lambda (item)
      (table-key-present? c item)))
   (else
    (lambda (item)
      (let loop ((s (initial-state c)))
	(if s
	    (if (test item (current-element c s))
		#t
		(loop (next-state c s)))
	    #f))))))

;;;

(define (member? item (c <collection>) . opt)
  ((member?-tester c (opt-test-fn opt)) item))

(define (opt-test-fn opt)
  (if (and (pair? opt)
	   (pair? (cdr opt))
	   (eq? (car opt) 'test:))
      (cadr opt)
      eq?))
