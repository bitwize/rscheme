;;
;; default method signals an error
;;
(define-method get-default-property ((self <object>) key)
  (signal (make <property-not-defined>
		missing-property: key
		in-object: self)))
    
