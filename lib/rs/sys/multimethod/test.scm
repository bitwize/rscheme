
;;;

(define-mm-generic-function bin+)

(define-method bin+ ((a <fixnum>) (b <fixnum>))
  (fixnum+ a b))

(define-method bin+ ((a <double-float>) (b <double-float>))
  (float+ a b))

(define-method bin+ ((a <string>) (b <string>))
  (string-append a b))

(define-method bin+ ((a <object>) (b <string>))
  (format #f "~a~a" a b))

(define-method bin+ ((a <string>) (b <object>))
  (format #f "~a~a" a b))
