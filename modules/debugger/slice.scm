(define-class <slice> (<object>)
  slice-start
  slice-end)

(define (slice? obj)
  (instance? obj <slice>))

(define-method self-evaluating? ((self <slice>)) #t)

(define-method write-object ((self <slice>) port)
  (format port "~d:~d" (slice-start self) (slice-end self)))

(%early-once-only
 (add-alternate-number-parser!
  (let ((re (reg-expr->proc '(entire (seq (save (+ digit))
                                          #\:
                                          (save (+ digit)))))))
    (lambda (str)
      (bind ((s e l r (re str)))
        (if s
            (make <slice>
                  slice-start: (string->number l)
                  slice-end: (string->number r))
            #f))))))
