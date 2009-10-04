
(define (do-undefine id envt) 
  (let ((tbl (table (the-top-level envt))))
    (table-remove! tbl id)))

(define (do-redefine new-id old-id envt)
  (let* ((tbl (table (the-top-level envt)))
	 (v (table-remove! tbl old-id)))
    (if v
	(table-insert! tbl new-id v))))

(define-macro (undefine name)
  (do-undefine name $envt)
  '(values))

(define-macro (redefine new-name old-name)
  (do-redefine new-name old-name $envt)
  '(values))
