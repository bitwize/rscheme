
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  rational numbers
;;;

(define-class <num-den> (<rational>)
  (rational-numerator type: <number>)
  (rational-denominator type: <number>))

(define-method number-as-string ((self <num-den>) radix)
  (string-append (number-as-string (rational-numerator self) radix)
		 "/"
		 (number-as-string (rational-denominator self) radix)))

(define (make-rational n d)
  (make <num-den>
	rational-numerator: n
	rational-denominator: d))
