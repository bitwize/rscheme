
(define-constant (assign-hash-code)
  (let* (((w <world>) (current-world))
	 (n (next-hash-code w)))
    (set-next-hash-code! w (+ n 1))
    (hash-code n)))

(define-class <db-object> (<object>)
  ;; note that you have to be in a world to create an object
  (hash-code type: <fixnum> init-value: assign-hash-code))

				   