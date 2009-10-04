
(define-class <x-cursor> (<x-object>))

(define (cursor? item)
  (instance? item <x-cursor>))

(define (cursor-id (self <x-cursor>))
  (x-id self))
