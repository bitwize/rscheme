
(define (eval-query (self <open-index>) query)
  (map
   (lambda (id)
     (table-lookup (id-index (doc-index self)) id))
   (set->list
    (eval-query* query 
                 (doc-index self)
                 (get-index-section (doc-index self) 'body)))))

(define-method eval-query* ((self <pair>)
			    (index <document-index>) 
			    (in <keyword-index>))
  (case (car self)
    ((or)
     (reduce union '() (map (rcurry eval-query* index in) (cdr self))))
    ((and)
     (let ((sub (map (rcurry eval-query* index in) (cdr self))))
       (reduce intersection (car sub) (cdr sub))))
    ((in)
     (eval-query* (caddr self)
		  index
		  (get-index-section index (cadr self))))))

(define-method eval-query* ((self <string>)
			    (index <document-index>) 
			    (in <keyword-index>))
  (let ((t (table-lookup (constituents in) self)))
    (if t
        (if (and (pair? t) (not (pair? (cdr t))))
            (list (car t) (cdr t))
            t)
        '())))

(define-method set->list ((self <boolean>))
  '())

(define-method set->list ((self <fixnum>))
  (cons self '()))

(define-method set->list ((self <empty-list>))
  '())

(define-method set->list ((self <pair>))
  (cons* (car self) (cdr self) '()))
