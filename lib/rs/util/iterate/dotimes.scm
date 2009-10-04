(define-macro (do-times (var cnt) . body)
  (let ((loopvar (gensym))
	(cntvar (gensym)))
    `(let ((,cntvar ,cnt))
       (let ,loopvar (((,var <fixnum>) 0))
	    (if (eq? ,var ,cntvar)
		(values)
		(begin
		  (begin ,@body)
		  (,loopvar (add1 ,var))))))))

