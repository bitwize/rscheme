
;;  recognize globable strings

(define (glob? str)
  (and (string-search str #\*)
       #t))

;;
;;  take a glob string (e.g, "*.c") and return
;;  a predicate function, <string> => <boolean>

(define (glob->predicate pat)
  (let ((p (reg-expr->proc
	    (list 'entire
		  (cons 'seq
			(map (lambda (ch)
			       (if (eq? ch #\*)
				   '(+ any)
				   ch))
			     (string->list pat)))))))
    (lambda (item)
      (bind ((s e (p item)))
	(if s
	    #t
	    #f)))))

;;
;; take two conformal glob strings (e.g., "*.scm" and "*.s")
;; and return a conversion function,
;;    <string> => (union <string> #f)
;;

(define (globs->converter src-pat dst-pat)
  (bind ((p v (reg-expr->proc
	       (list 'entire
		     (cons 'seq
			   (map (lambda (ch)
				  (if (eq? ch #\*)
				      '(save (+ any))
				      ch))
				(string->list src-pat))))))
	 (q (string-split dst-pat #\*)))
    (if (eq? (length q) (length v))
	(lambda (item)
	  (bind ((s e #rest hits (p item)))
	    (if s
		(apply string-append
		       (car q)
		       (apply append (map list hits (cdr q))))
		#f)))
	(error "glob-converter: ~s and ~s are not conformal"
	       src-pat dst-pat))))
