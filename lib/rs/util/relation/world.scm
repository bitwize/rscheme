
(define-class <universe> (<object>)
  (properties type: <vector> init-value: '#())
  (extents type: <symbol-table> init-function: make-symbol-table)
  (indices type: <symbol-table> init-function: make-symbol-table))

;;;

(define-thread-var *universe* (make <universe>))

;;;

(define (make-universe)
  (make <universe>))

(define (with-universe (u <universe>) thunk)
  (thread-let ((*universe* u))
    (thunk)))

(define (current-universe)
  *universe*)
