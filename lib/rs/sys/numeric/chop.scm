(define-method chop ((self <integer>))
  self)

(define-method chop ((self <real>))
  (if (< (abs self) 1e-6)
      0
      self))

;;;  Should this use more of a relative threshold approach?
;;;  i.e., so that (chop 10000000000+0.000001i) = 10000000000

(define-method chop ((self <complex>))
  (bind ((a (real-part self))
         (b (imag-part self))
         (a0 (chop a))
         (b0 (chop b)))
    (if (and (eq? a a0)
             (eq? b b0))
        self
        (if (zero? b)
            a0
            (make-rectangular a0 b0)))))

