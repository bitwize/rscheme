;;;

(define-class <png-image-rep> (<image-rep>)
  png)

(define-method image-rep-quality ((self <png-image-rep>))
  (* 10 (get-property (png self) 'bit-depth)))

(define-method make-image-rep ((self <png-image>) (in <graphic-image>))
  (make <png-image-rep>
        rep-of: in
        png: self))

(define-method copy-to-memory-image-rep ((self <png-image-rep>) 
					 (mem <memory-image-rep>))
  (let ((data (data mem))
	(w (image-width (rep-of self)))
	(k 0))
    (for-each-pixel
     (png self)
     (lambda (y x c)
       (bind ((r g b a (pixel-rgba c)))
	 (bvec-set! data k (logical-shift-right r 8))
	 (bvec-set! data (+ k 1) (logical-shift-right g 8))
	 (bvec-set! data (+ k 2) (logical-shift-right b 8))
	 (bvec-set! data (+ k 3) (logical-shift-right a 8))
	 (set! k (+ k 4)))))))
