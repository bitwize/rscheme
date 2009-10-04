(define (resolve-module-reference (target-name <symbol>) (space <list>))
  (let ((b (assq target-name space)))
    (if b
        (cdr b)
        (get-module target-name))))
