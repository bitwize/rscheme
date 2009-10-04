
(define (set-property! self key value)
  (bind ((props (properties self))
         (v f? s! (lookup-prop props key)))
    (if s!
        (s! value)
        (begin
          (set-properties! self (push-prop props key value))
          (values)))))

(define (remove-property! self key)
  (remove-prop (properties self) self key)
  (values))

(define (call-with-properties-set target properties thunk)
  (let* ((set (keyword-list->symbol-assoc-list properties))
         (saved '()))
    (dynamic-wind
        (lambda ()
          (set! saved (map
                       (lambda (pp)
                         (let* ((p (car pp))
                                (r (if (has-property? target p)
                                       (let ((saved-value (get-property target p)))
                                         (lambda ()
                                           (set-property! target p saved-value)))
                                       (lambda ()
                                         (remove-property! target p)))))
                           (set-property! target p (cdr pp))
                           r))
                       set)))
        thunk
        (lambda ()
          (for-each (lambda (r) (r)) saved)))))

          
    
