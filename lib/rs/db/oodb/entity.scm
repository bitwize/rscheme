
(define-class <entity> (<db-object>)
  ;; note that we have to be in a transaction to create an entity
  (created-by init-function: current-transaction))

(define-macro (pmake class . opts)
  `(make ,class %alloc-area: (entity-allocation-area ,class) ,@opts))

(define (entity-allocation-area entity-class)
  (table-lookup (entity-area-map (current-world-view))
		entity-class))

