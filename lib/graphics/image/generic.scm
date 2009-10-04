(define-macro (do-times (var cnt) . body)
  (let ((loopvar (gensym))
	(cntvar (gensym)))
    `(let ((,cntvar ,cnt))
       (let ,loopvar (((,var <fixnum>) 0))
	    (if (eq? ,var ,cntvar)
		(values)
		(begin
		  (begin ,@body)
		  (,loopvar (add1 ,var))))))))

;;;
;;;  An internal representation of images
;;;

(define-class <pixel-source> (<object>)
  (properties type: <vector> init-value: '#()))


(define-generic-function image-width)
(define-generic-function image-height)
(define-generic-function get-pixel)
(define-generic-function for-each-pixel)

;;;

;;;

(define-class <graphic-image> (<pixel-source>)
  (image-width type: <fixnum>)
  (image-height type: <fixnum>)
  (image-reps type: <vector> init-value: '#())
  (mem-image init-value: #f))

(define-class <image-rep> (<pixel-source>) :abstract
  (rep-of type: <graphic-image>))

(define-method image-width ((self <image-rep>))
  (image-width (rep-of self)))

(define-method image-height ((self <image-rep>))
  (image-height (rep-of self)))


;;;

(define-generic-function image-rep-quality)
(define-generic-function make-image-rep)
(define-generic-function copy-to-memory-image-rep)

(define-method image-rep-quality ((self <image-rep>))
  0)

;;;
;;; memory images are 8 bits per sample, 4 samples per pixel (R G B A)
;;;

(define-class <rgba-matrix> (<object>) :bvec)

(define-class <memory-image-rep> (<image-rep>)
  (data type: <rgba-matrix>))

;;;;

(define-method initialize ((self <image-rep>))
  (let (((of <graphic-image>) (rep-of self)))
    (if (instance? self <memory-image-rep>)
	(set-mem-image! of self))
    (set-image-reps! of (vector-append (image-reps of) (vector self)))
    self))

(define (make-graphic-image #key
			    (from default: #f)
			    (width default: (image-width from))
			    (height default: (image-height from)))
  (let ((i (make <graphic-image>
                 image-width: width
                 image-height: height)))
    (if from
	(make-image-rep from i))
    i))

(define-method memory-image-rep ((self <graphic-image>))
  (or (mem-image self)
      ;; note that the <memory-image-rep> initializer caches the
      ;; rep in the `mem-image' slot
      (let ((r (make <memory-image-rep>
		 rep-of: self
		 data: (bvec-alloc <rgba-matrix> (* (image-width self)
						    (image-height self)
						    4)))))
	(do-times (i (* (image-width self) (image-height self) 4))
	  (bvec-set! (data r) i 0)
	  (values))
	(let ((best-qual -1)
	      (best #f))
	  (vector-for-each
	   (lambda (r)
	     (let ((q (image-rep-quality r)))
	       (if (> q best-qual)
		   (begin
		     (set! best-qual q)
		     (set! best r)))))
	   (image-reps self))
	  ;;
	  (if best
	      (copy-to-memory-image-rep best r))
	  r))))

;;;

(define-method for-each-pixel ((self <graphic-image>) proc)
  (for-each-pixel (memory-image-rep self) proc))

(define-method get-pixel ((self <graphic-image>) x y)
  (get-pixel (memory-image-rep self) x y))

(define-method set-pixel! ((self <graphic-image>) x y p)
  (set-pixel! (memory-image-rep self) x y p))

;;;

(define-method for-each-pixel ((self <memory-image-rep>) proc)
  (let (((w <fixnum>) (image-width self))
        ((h <fixnum>) (image-height self))
	((buf <rgba-matrix>) (data self)))
    (let yloop (((y <fixnum>) 0)
                ((i <fixnum>) 0))
      (if (eq? y h)
          (values)
          (let xloop (((x <fixnum>) 0)
                      ((i <fixnum>) i))
            (if (eq? x w)
                (yloop (add1 y) i)
                (begin
                  (proc x y (rgba8->pixel (bvec-ref buf i)
                                          (bvec-ref buf (fixnum+ i 1))
                                          (bvec-ref buf (fixnum+ i 2))
                                          (bvec-ref buf (fixnum+ i 3))))
                  (xloop (add1 x) (fixnum+ i 4)))))))))
      

(define-method get-pixel ((self <memory-image-rep>) (x <fixnum>) (y <fixnum>))
  (let ((k (* 4 (+ x (* y (image-width self)))))
	((buf <rgba-matrix>) (data self)))
    (make <pixel>
      red-component: (* 257 (bvec-ref buf k))
      green-component: (* 257 (bvec-ref buf (+ k 1)))
      blue-component: (* 257 (bvec-ref buf (+ k 2)))
      alpha-component: (* 257 (bvec-ref buf (+ k 3))))))

(define-method set-pixel! ((self <memory-image-rep>) x y (c <color>))
  (let ((r (quotient (red-component c) 256))
	(g (quotient (green-component c) 256))
	(b (quotient (blue-component c) 256))
	(a (quotient (alpha-component c) 256))
	(k (* 4 (+ x (* y (image-width self)))))
	((buf <rgba-matrix>) (data self)))
    (bvec-set! buf k r)
    (bvec-set! buf (+ k 1) g)
    (bvec-set! buf (+ k 2) b)
    (bvec-set! buf (+ k 3) a)
    (values)))


;;; returns the image rep of the given class that satisfies the
;;; given predicate, or #f if none is present.
;;;
;;; This is used by specialized image providers like `x-image-rep'
;;; to check for a cached rep first

(define (get-image-rep (self <graphic-image>) (class <<class>>) predicate)
  (let (((v <vector>) (image-reps self)))
    (let loop ((i 0))
      (if (< i (vector-length v))
	  (if (and (instance? (vector-ref v i) class)
		   (predicate (vector-ref v i)))
	      (vector-ref v i)
	      (loop (add1 i)))
	  #f))))

;;;
;;;  generic implementation relies on `get-pixel' on the source rep
;;;

(define-method copy-to-memory-image-rep ((self <image-rep>)
					 (mem <memory-image-rep>))
  (let ((data (data mem))
	(w (image-width (rep-of self)))
	(k 0))
    (do-times (y (image-height mem))
      (do-times (x w)
	(bind ((r g b a (pixel-rgba (get-pixel self x y))))
	  (bvec-set! data k (logical-shift-right r 8))
	  (bvec-set! data (+ k 1) (logical-shift-right g 8))
	  (bvec-set! data (+ k 2) (logical-shift-right b 8))
	  (bvec-set! data (+ k 3) (logical-shift-right a 8))
	  (set! k (+ k 4)))))))
