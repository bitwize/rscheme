

;;;  support arguments of the form:
;;;       --key v1 [v2 ...]
;;;       --key=v1[,v2...]
;;;       -k
;;;       -kv
;;;       -k v

(define *options*
  '((blech flag "blech" "b" blech: #t)
    (create flag "create" "c" command: create)
    (open flag "open" "o" command: open)
    (file arg "file" "f" file: <string>)
    (1 constraint (xor open create))
    (2 constraint (if create file))))

(define (interp defs args)
  (let ((rest '())
	(keys '())
	(shorts (map (lambda (a)
		       (cons (cadddr a) a))
		     (select (lambda (a)
			       (memq (cadr a) '(flag arg)))
			     defs)))
	(longs (map (lambda (a)
		      (cons (caddr a) a))
		    (select (lambda (a)
			      (memq (cadr a) '(flag arg)))
			    defs))))
    (let loop ((a args))
      (if (null? a)
	  (values (reverse keys)
		  (reverse rest))
	  (let ((s (car a)))
	    (cond
	     ((and (> (string-length s) 2)
		   (string=? (substring s 0 2) "--"))
	      (let ((l (assoc (substring s 2) longs)))
	     ((string=? s "--")
	      ...)
	     ((and (> (string-length s) 1)
		   (char=? (string-ref s 0) #\-))
	      ...)
	     (else
	      (if *posix?*
		  (values (reverse keys) a)
		  (begin
		    (set! rest (cons s rest))
		    (loop (cdr a)))))))))))

(define *posix?* #t)

