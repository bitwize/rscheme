(define-method chop ((self <real>))
  (if (< (abs self) 1e-6)
      0
      self))

(define-method chop ((self <point>))
  (bind ((a b (point->values self))
         (a0 (chop a))
         (b0 (chop b)))
    (if (and (eq? a a0)
             (eq? b b0))
        self
        (make-point a0 b0))))

(define-method chop ((self <size>))
  (bind ((a b (size->values self))
         (a0 (chop a))
         (b0 (chop b)))
    (if (and (eq? a a0)
             (eq? b b0))
        self
        (make-size a0 b0))))
