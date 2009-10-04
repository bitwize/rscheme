
(define (color=? c1 c2)
  (bind ((r1 g1 b1 (color-rgb-components c1))
         (r2 g2 b2 (color-rgb-components c2)))
    (and (= r1 r2) (= g1 g2) (= b1 b2))))

(define (color->hash c)
  (bind ((r g b (color-rgb-components c)))
    (bitwise-xor
     (integer->hash (+ b 963))
     (integer->hash (bitwise-xor (integer->hash (+ r 101)) g)))))

(define-method hash-code ((self <color>))
  (color->hash self))

(define (make-color-table)
  (make-table color=? color->hash))

