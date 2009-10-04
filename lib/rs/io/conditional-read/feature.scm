(define-message-table rs.io.conditional-read 432)

(define-thread-var *features* '())

(define (current-features)
  *features*)

(define (add-feature! f)
  (set! *features* (cons f *features*))
  (values))

(define (remove-feature! f)
  (set! *features* (delq f *features*))
  (values))

(define (call-with-features features thunk)
  (thread-let ((*features* features))
    (thunk)))

(define-method eval-feature ((self <symbol>))
  (and (memq self *features*) #t))

(define-method eval-feature ((self <pair>))
  (case (car self)
    ((and)
     (every? eval-feature (cdr self)))
    ((or)
     (any? eval-feature (cdr self)))
    ((not)
     (not (eval-feature (cadr self))))
    (else
     (em 712 "invalid feature expr: ~s" self))))
