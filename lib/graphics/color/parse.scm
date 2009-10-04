
(define parse-rgbi (unformat->proc "rgbi:~d/~d/~d"))
(define parse-sharp2-color (reg-expr->proc
                            '(entire
                              (seq
                               #\#
                               (save (seq hex-digit hex-digit))
                               (save (seq hex-digit hex-digit))
                               (save (seq hex-digit hex-digit))))))
(define parse-sharp1-color (reg-expr->proc
                            '(entire
                              (seq
                               #\#
                               (save hex-digit)
                               (save hex-digit)
                               (save hex-digit)))))

(%early-once-only

(define *named-colors* (make-string-ci-table))
(table-insert! *named-colors* "black" $black)
(table-insert! *named-colors* "white" $white)
(table-insert! *named-colors* "gray50" (make-color gray: 0.5))

)


(define (named-color->color str)
  (table-lookup *named-colors* str))

(define (rgbi-string->color str)
  (bind ((r g b (parse-rgbi str)))
    (if r
	(make-color red: r green: g blue: b)
	#f)))

(define (sharp-color->color str)
  (case (string-length str)
    ((4)
     (bind ((s e r g b (parse-sharp1-color str)))
       (if s
           (mkcolor (* #x1111 (string->number r 16))
                    (* #x1111 (string->number g 16))
                    (* #x1111 (string->number b 16)))
           #f)))
    ((7)
     (bind ((s e r g b (parse-sharp2-color str)))
       (if s
           (mkcolor (* #x101 (string->number r 16))
                    (* #x101 (string->number g 16))
                    (* #x101 (string->number b 16)))
           #f)))
    (else
     #f)))
     

(define (string->color (str <string>))
  (or (rgbi-string->color str)
      (sharp-color->color str)
      (named-color->color str)))

(define-method to-string ((self <color>))
  ;; `rgbi:r/g/b' is a recent X color spec standard
  (format #f "rgbi:~d/~d/~d"
	  (color-red self)
	  (color-green self)
	  (color-blue self)))

(define-method to-string ((self <pixel>))
  (format #f "rgbai:~d/~d/~d/~d"
	  (color-red self)
	  (color-green self)
	  (color-blue self)
	  (pixel-alpha self)))
