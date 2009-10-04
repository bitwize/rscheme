
(define-class <virtual-image-rep> (<image-rep>)
  get-pixel-proc)

(define-method get-pixel ((self <virtual-image-rep>) x y)
  (let ((c ((get-pixel-proc self) x y)))
    ;(format #t " (~d,~d) => ~s\n" x y c)
    c))

(define (composite-image (a <pixel-source>) (b <pixel-source>))
  (let ((i (make-graphic-image width: (image-width a)
			       height: (image-height a))))
    (make <virtual-image-rep>
	  rep-of: i
	  get-pixel-proc: (lambda (x y)
			    (color+ (get-pixel a x y)
				    (get-pixel b x y))))
    i))

;;;

(define (make-constant-image pixel #key width height)
  (let ((i (make-graphic-image width: width
			       height: height)))
    (make <virtual-image-rep>
      rep-of: i
      get-pixel-proc: (lambda (x y)
			pixel))
    i))
