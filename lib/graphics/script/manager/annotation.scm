
(define *annotation-hooks* (make-symbol-table))

(define (set-annotation-hook! key proc)
  (table-insert! *annotation-hooks* key proc))

(define (ahook src head . args)
  (cond
   ((table-lookup *annotation-hooks* head)
    => (lambda (f)
         (apply f src head args)))))

(define-syntax annot*
  (syntax-form (head p)
    (ahook (*FUNCTION* form) 
           (mquote head)
           (reify! p))))

(define-syntax annotation
  (syntax-form () (begin))
  (syntax-form ((head . args) . more)
    (annot* head . args)
    (annotation . more)))
