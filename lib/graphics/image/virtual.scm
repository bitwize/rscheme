
(define-class <virtual-image-rep> (<image-rep>) :abstract)

;;;

(define-class <generic-virtual-rep> (<virtual-image-rep>)
  (get-pixel-proc type: <function> :sealed))

(define-method get-pixel ((self <generic-virtual-rep>) x y)
  ((get-pixel-proc self) x y))

(define (make-virtual-image (w <fixnum>) (h <fixnum>) proc)
  (let ((i (make-graphic-image width: w
			       height: h)))
    (make <generic-virtual-rep>
	  rep-of: i
	  get-pixel-proc: proc)
    i))


(define (make-sub-image (a <pixel-source>) (r <rect>))
  (make-virtual-image (size-width r)
		      (size-height r)
		      (let (((x0 <fixnum>) (origin-x r))
			    ((y0 <fixnum>) (origin-y r)))
			(lambda (i j)
			  (get-pixel a (+ i x0) (+ j y0))))))

(define (make-composite-image (a <pixel-source>) (b <pixel-source>))
  (make-virtual-image (image-width a)
		      (image-height a)
		      (lambda (x y)
			(color+ (get-pixel a x y)
				(get-pixel b x y)))))

(define (make-constant-image pixel #key width height)
  (make-virtual-image width
		      height
		      (lambda (x y)
			pixel)))

(define (make-filtered-image func (a <pixel-source>))
  (make-virtual-image (image-width a)
		      (image-height a)
		      (lambda (x y)
			(func (get-pixel a x y)))))
