;;;@title Mathematics Test Suite #1
;;;@tag math-1

(test-begin "mathlib : R5RS Math Compliance")

(define small 1000000)
(define medium 10000000000) 
(define big 1000000000000000000000000000000000000000)


(test-begin "floor, ceiling, truncate, round")

;;; straight out of R5RS

(test-equal (floor -4.3)        -5.0)
(test-equal (ceiling -4.3)      -4.0)
(test-equal (truncate -4.3)     -4.0)
(test-equal (round -4.3)        -4.0)

(test-equal (floor 3.5)         3.0)
(test-equal (ceiling 3.5)       4.0)
(test-equal (truncate 3.5)      3.0)
(test-equal (round 3.5)         4.0)

(test-assert (inexact? (floor 3.5)))
(test-assert (inexact? (ceiling 3.5)))
(test-assert (inexact? (truncate 3.5)))
(test-assert (inexact? (round 3.5)))

(test-equal (round 7/2)         4)
(test-assert (exact? (round 7/2)))

;;; extensions for medium (longint) and big (bignum) numbers

(test-equal (truncate (/ big 3)) 333333333333333333333333333333333333333)
(test-equal (truncate (/ big -3)) -333333333333333333333333333333333333333)

(test-equal (truncate (/ big 3)) 333333333333333333333333333333333333333)
(test-equal (truncate (/ big -3)) -333333333333333333333333333333333333333)


(test-end "floor, ceiling, truncate, round")

(test-end "mathlib : R5RS Math Compliance")
