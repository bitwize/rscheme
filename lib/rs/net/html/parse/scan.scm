(define (string-downcase str)
  (list->string (map char-downcase (string->list str))))

;;;
;;;  a crude HTML token scanner
;;;

(define tag-pat
  (with-module
      regex
    (reg-expr->proc '(seq #\<
			  (* space)
			  (save (? #\/))
			  (* space)
			  (save (+ (or alpha digit #\: #\. #\_)))))))

(define space-pat
  (with-module
      regex
    (reg-expr->proc '(* space))))

(define (skip-space str i)
  (bind ((s e (space-pat str i)))
    e))

(define attr-pat
  (with-module
      regex
    (reg-expr->proc '(seq (save (+ (or alpha digit #\: #\. #\_)))
			  (* space)
			  (save (? (seq #\=)))))))

(define attr-val-term
  (with-module
      regex
    (reg-expr->proc '(save (or #\> space)))))

(define (parse-tag elem str i sw)
  (if (string=? sw "/")
      (parse-etag elem str i sw)
      (parse-stag elem str i sw)))

(define (parse-etag elem str i sw)
  (values (list 'etag elem)
	  (+ 1 (string-search str #\> i))))

(define (parse-stag-attr str i)
  (bind ((s e key eq (attr-pat str i)))
    (if (not s)
	(em 310 "attr-pat: failed at: ~#*40s" (substring str i)))
    (let ((key (string->symbol key)))
      (if (string=? eq "=")
	  (let ((i (skip-space str e)))
	    (if (char=? (string-ref str i) #\")
		(let ((j (string-search str #\" (+ i 1))))
		  (values
		   (list key (list 'quote (substring str (+ i 1) j)))
		   (+ j 1)))
		(bind ((s e j (attr-val-term str i)))
		  (values (list key (substring str i s))
			  s))))
	  (values (list key) e)))))

(define (parse-stag elem str i sw)
  (let loop ((i (skip-space str i))
	     (attrs '()))
    (cond
     ((char=? (string-ref str i) #\>)
      (values (cons* 'stag elem (reverse! attrs)) (+ i 1)))
     ((and (char=? (string-ref str i) #\/)
           (char=? (string-ref str (skip-space str (+ i 1))) #\>))
      (values (cons* 'setag elem (reverse! attrs)) 
              (+ (skip-space str (+ i 1)) 1)))
     (else
      (bind ((attr i (parse-stag-attr str i)))
        (loop (skip-space str i) (cons attr attrs)))))))

(define (scan-html str)
  (let loop ((i 0)
	     (r '()))
    (bind ((s e sw elem (tag-pat str i)))
      ;(dm "from ~d: at ~s: ~s" i s elem)
      (if s
	  (let cloop ((i i)
		      (r r))
	    (if (> s i)
		(cloop s (cons (substring str i s) r))
		(bind ((tag i (parse-tag (string->symbol
					  (string-downcase elem))
					 str e sw)))
		  (loop i (cons tag r)))))
	  (let cloop ((i i)
		      (r r))
	    (if (< i (string-length str))
		(cloop (string-length str)
		       (cons (substring str i) r))
		(reverse r)))))))
