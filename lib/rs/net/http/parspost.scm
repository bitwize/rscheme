
(%early-once-only
(define post-field (reg-expr->proc '(prefix
				     (seq
				      (save (+ (not #\=)))
				      #\=
				      (save (* (not #\&)))))))
)

(define (parse-post-content (content <string>))
  (let loop ((i 0)
	     (r '()))
    (if (< i (string-length content))
	(bind ((s e f v (post-field content i)))
	  (if s
	      (loop (+ e 1)
		    (cons (cons (string->symbol f) (unescape-str! v)) r))
	      (reverse! r)))
	(reverse! r))))
