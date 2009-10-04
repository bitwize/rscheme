
(define-class <x-object> (<object>)
  (x-id type: <fixnum> :sealed)
  (x-display type: <x-display> :sealed))

(define-class <x-font> (<x-object>)
  (font-name init-value: #f)
  (font-info init-value: #f))

(define-syntax (font? thing)
  (instance? thing <x-font>))
