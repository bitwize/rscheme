
;; note that a transaction is not a first-class DB object

(define-class <transaction> (<object>)
  (start-time type: <time> init-function: time))

;;

(define-thread-var *txn*)

(define (current-transaction)
  *txn*)

(define (do-transaction thunk)
  (thread-let ((*txn* (make <transaction>)))
    (thunk)))
