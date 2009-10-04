(define *config-database* (make-symbol-table))

(define (config-enable! name)
  (table-insert! *config-database* name #t)
  (add-globally-implemented! (symbol-append "config." name))
  (values))

(define (config-disable! name)
  (table-remove! *config-database* name)
  (remove-globally-implemented! (symbol-append "config." name))
  (values))
  
(define (config-enabled? name)
  (table-key-present? *config-database* name))

(define (config-set! name value)
  (table-insert! *config-database* name value)
  (add-globally-implemented! (symbol-append "config." name))
  (values))

(define (config-value name #rest r)
  (or (table-lookup *config-database* name)
      (if (pair? r)
          (if (pair? (cdr r))
              (error "config-value: too many arguments")
              (car r))
          (error "`~s' not configured" name))))
