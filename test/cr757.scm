
(define-method bob ((self <string>) #key (foo default: 3))
  (+ (* 10 (string-length self)) foo))

(check 23 (bob "ab"))
(check 34 (bob "abc" foo: 4))
(check 35 (bob "abc" foo: 5))

;;;

(define-method bobc ((self <string>) #key (foo type: <fixnum> default: 3))
  (+ (* 10 (string-length self)) foo))

(check 23 (bobc "ab"))
(check 34 (bobc "abc" foo: 4))
(check 35 (bobc "abc" foo: 5))
(expect-to-fail (bobc "abc" foo: "hi"))

;;;

(define-method bobo ((self <string>) #key (foo 3))
  (+ (* 10 (string-length self)) foo))

(check 23 (bobo "ab"))
(check 34 (bobo "abc" foo: 4))
(check 35 (bobo "abc" foo: 5))

;;;

(define-method bobr ((self <string>) #key foo)
  (+ (* 10 (string-length self)) foo))

(expect-to-fail (bobr "ab"))
(check 23 (bobr "ab" foo: 3))
(check 34 (bobr "abc" foo: 4))

;;;

(define-method bobrr ((self <string>) #rest r)
  (apply + (* 10 (string-length self)) r))

(check 130 (bobrr "abc" 100))

;;;

(define-method bobrrl ((self <string>) #rest (r <list>))
  (apply + (* 10 (string-length self)) r))

(check 121 (bobrrl "ab" 101))

;;;
;;; should generate a warning...

(format (current-error-port) "------[ expect a warning here: ]------\n")
(define-method bob (self #rest (r <list>))
  (+ 100 (length r)))
(format (current-error-port) "--------------------------------------\n")

(check 23 (bob "ab"))
(check 103 (bob 'bob 1 2 3))
