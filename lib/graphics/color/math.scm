
(define (color* (color <color>) (s <real>))
  (make-color red: (* s (color-red color))
              green: (* s (color-green color))
              blue: (* s (color-blue color))))

(define-method color+ ((a <color>) (b <color>))
  (make-color red: (+ (color-red a) (color-red b))
              green: (+ (color-green a) (color-green b))
              blue: (+ (color-blue a) (color-blue b))))

;;

(define-method color+ ((a <pixel>) (b <color>))
  (if (= (alpha-component a) 0)
      b
      (let* ((aa (pixel-alpha a))
	     (inv (- 1.0 aa)))
	(make-pixel red: (+ (* aa (color-red a)) (* inv (color-red b)))
		    green: (+ (* aa (color-green a)) (* inv (color-green b)))
		    blue: (+ (* aa (color-blue a)) (* inv (color-blue b)))
		    alpha: (* aa (pixel-alpha b))))))
