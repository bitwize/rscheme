(define-class <line-port> (<input-port>)
  (remaining-lines init-value: '()))

(define (lines->port lines)
  (make <line-port>
        remaining-lines: lines))

(define-method input-port-read-line ((self <line-port>))
  (let ((l (remaining-lines self)))
    (if (pair? l)
        (begin
          (set-remaining-lines! self (cdr l))
          (car l))
        (eof-object))))
