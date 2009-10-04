
(define (common-prefix lst)
  (let ((best (car lst)))
    (let loop ((l (cdr lst)))
      (if (null? l)
	  best
          (if (and (<= (string-length best)
                       (string-length (car l)))
                   (string=? best (substring (car l) 0 (string-length best))))
              ;; best still works
              (loop (cdr l))
              (begin
                (set! best (substring best 0 (same-up-to-index best (car l))))
                (loop (cdr l))))))))

(define (same-up-to-index a b)
  (let loop ((i 0))
    (if (and (< i (string-length a))
             (< i (string-length b)))
	(if (char=? (string-ref a i) (string-ref b i))
	    (loop (+ i 1))
	    i)
	i)))

(define (collect-completions (state <readline-state>) (prefix <string>))
  (if (string=? prefix "")
      '()
      (map symbol->string
	   (select
	    (lambda ((cand <symbol>))
	      (let (((candidate <string>) (symbol->string cand)))
		(and (>= (string-length candidate) (string-length prefix))
		     (string=? (substring candidate 0 (string-length prefix)) 
			       prefix))))
	    (completions state)))))

(define (collect-prefix q (state <readline-state>))
  (let loop ((i (dequeue-count q))
	     (r '()))
    (if (> i 0)
	(let ((ch (dequeue-ref q (- i 1))))
	  (if (string-search (word-break state) ch)
	      (list->string r)
	      (loop (- i 1) (cons ch r))))
	(list->string r))))
