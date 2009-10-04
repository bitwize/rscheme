
(define-class <rect-field> (<group>)
  (rect-value init-value: '#uninit)
  (fields init-value: '#uninit)

(define $rect-field-label-width 20)
(define $rect-field-field-width 20)

(define-method setup-view ((self <rect-field>))
  (set-rect-value! self (make-rect 0 0 0 0))
  (set-fields! self
	       (vector (make <text-field>
			     parent: self
			     frame: (make-rect $rect-field-label-width 0 
					       $rect-field-field-width 22)
			     text-contents: "0")
		       (make <text-field>
			     parent: self
			     frame: (make-rect (+ (* 2 $rect-field-label-width)
						  $rect-field-field-width)
					       0
					       $rect-field-field-width 22)
			     text-contents: "0")
		       (make <text-field>
			     parent: self
			     frame: (make-rect $rect-field-label-width 0 
					       $rect-field-field-width 52)
			     text-contents: "0")
		       (make <text-field>
			     parent: self
			     frame: (make-rect (+ (* 2 $rect-field-label-width)
						  $rect-field-field-width)
					       0
					       $rect-field-field-width 52)
			     text-contents: "0")))
  (make <text-field>
	parent: self
	frame: (
