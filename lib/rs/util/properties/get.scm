
(define (get-prop rcvr key have-default? dflt)
  (bind ((v f? s! (lookup-prop (properties rcvr) key)))
    (if f?
	v
	(if have-default?
	    (dflt)
	    (get-default-property rcvr key)))))
	    
;;
(define-syntax get-property
  (syntax-form (item key)
    (get-prop item key #f #f))
  (syntax-form (item key val)
    (get-prop item key #t (lambda () val))))

(define-method has-property? ((self <object>) key)
  (bind ((v f? s? (lookup-prop (properties self) key)))
    f?))
