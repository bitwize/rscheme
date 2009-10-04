(define (replace+ str)
  (let ((x (string-search str #\+)))
    (if x
        (string-join " " (string-split str #\+))
        str)))

(%early-once-only
(define escape-pat (reg-expr->proc 
		    '(seq #\% (save (seq hex-digit hex-digit)))))
)

(define (http-url-decode (str <string>))
  (let ((str (replace+ str)))
    (let loop ((i 0)
	       (r '()))
      (bind ((s e xx (escape-pat str i)))
        (if s
	    (loop e
		  (cons* (string
			  (integer->char
			   (string->number xx 16)))
			 (substring str i s)
			 r))
	    (if (null? r)
		str
		(apply string-append 
		       (reverse (cons (substring str i) r)))))))))

#|
(%early-once-only
(define need-to-escape-pat (reg-expr->proc
			    '(not (or alpha
				      digit
				      #\.
				      #\-
				      #\_))))
)
|#
