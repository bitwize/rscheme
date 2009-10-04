(define $group-subcommand
  (let-syntax ((cmd (syntax-form (name proc)
		      (cons (mquote name)
			    (lambda args
			      (apply proc args))))))
    (list (cmd create handle-group-create-request))))

(define (handle-group-request args req inp out (u <user>))
  (bind ((proc sub-args (parse-subcommand req
					  $group-subcommand
					  'group)))
    (proc (map string->group args) req inp out u)))

(define (handle-group-create-request grps req inp out u)
  (make-group
   (get-exactly-one req 'name)
   (map string->group (cdr (assq 'parent req)))
   (let ((l (assq 'owner req)))
     (if l
	 (string->user (cadr l))
	 u))))
