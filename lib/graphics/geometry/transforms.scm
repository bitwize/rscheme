;;;
;;;  Here are some axioms of transformation that
;;;  are useful:
;;;
;;;  (1) Distribution
;;;
;;;       (transform (transform P a0) a1)
;;;       ==
;;;       (transform P (concatenate-transform a1 a0))
;;;


(define-class <transform> (<geometric>) :abstract)

(define-class <affine-transform> (<transform>)
  (matrix type: <vector>))

(define-method exact->inexact ((self <affine-transform>))
  (let (((s <vector>) (matrix self)))
    (make <affine-transform>
          matrix: (vector (exact->inexact (vector-ref s 0))
                          (exact->inexact (vector-ref s 1))
                          (exact->inexact (vector-ref s 2))
                          (exact->inexact (vector-ref s 3))
                          (exact->inexact (vector-ref s 4))
                          (exact->inexact (vector-ref s 5))))))

(define (make-affine-transform)
  (make <affine-transform>
	matrix: '#(1 0 0 1 0 0)))

(define-method write-object ((self <affine-transform>) port)
  (format port "#[<affine-transform> ~d ~d ~d ~d ~d ~d]"
          (vector-ref (matrix self) 0)
          (vector-ref (matrix self) 1)
          (vector-ref (matrix self) 2)
          (vector-ref (matrix self) 3)
          (vector-ref (matrix self) 4)
          (vector-ref (matrix self) 5)))

(define-constant $identity-transform (make-affine-transform))

(define (vector->affine-transform (v <vector>))
  (assert (= (vector-length v) 6))
  (make <affine-transform>
        matrix: v))

(define (transform-matrix t)
  (cond
   ((vector? t)
    (vector->values t))
   ((instance? t <affine-transform>)
    (vector->values (matrix t)))
   ((list? t)
    (list->values t))
   (else
    (error "expected a <vector>, <list>, or <transform>: ~s" t))))

;;;
;;;  Produce a transformation `t' such that t(p) = t2(t1(p))
;;;

(define (concatenate-transform t1 t2)
  (bind ((a1 b1 c1 d1 tx1 ty1 (transform-matrix t2))
	 (a2 b2 c2 d2 tx2 ty2 (transform-matrix t1)))
    (make <affine-transform>
	  matrix: (vector
		   (+ (* a1 a2) (* b1 c2))
		   (+ (* a1 b2) (* b1 d2))
		   (+ (* a2 c1) (* c2 d1))
		   (+ (* b2 c1) (* d1 d2))
		   (+ (* a2 tx1) tx2 (* c2 ty1))
		   (+ (* b2 tx1) ty2 (* d2 ty1))))))
	
(define-method transform1 ((self <affine-transform>) x y)
  (let (((v <vector>) (matrix self)))
    (values (+ (* (vector-ref v 0) x)
	       (* (vector-ref v 2) y)
	       (vector-ref v 4))
	    (+ (* (vector-ref v 1) x)
	       (* (vector-ref v 3) y)
	       (vector-ref v 5)))))

(define-method dtransform1 ((self <affine-transform>) dx dy)
  (let (((v <vector>) (matrix self)))
    (values (+ (* (vector-ref v 0) dx)
	       (* (vector-ref v 2) dy))
	    (+ (* (vector-ref v 1) dx)
	       (* (vector-ref v 3) dy)))))

(define-method transform ((self <point>) xf)
  (bind ((x1 y1 (transform1 xf (x self) (y self))))
    (make-point x1 y1)))

(define-method transform ((self <line>) xf)
  (make <line>
        from: (transform (from self) xf)
        to: (transform (to self) xf)))

(define-method transform ((self <size>) xf)
  (bind ((dx1 dy1 (dtransform1 xf (dx self) (dy self))))
    (make-size dx1 dy1)))

(define-method transform ((self <rect>) ctm)
  (let (((p1 <point>) (transform (lower-left self) ctm))
	((p2 <point>) (transform (lower-right self) ctm))
	((p3 <point>) (transform (upper-right self) ctm))
	((p4 <point>) (transform (upper-left self) ctm)))
    (bbox-rect (min (x p1) (x p2) (x p3) (x p4))
	       (min (y p1) (y p2) (y p3) (y p4))
	       (max (x p1) (x p2) (x p3) (x p4))
	       (max (y p1) (y p2) (y p3) (y p4)))))

;;;

(define-method translate ((self <affine-transform>) (delta <point>))
  (concatenate-transform 
   self
   (make <affine-transform>
	 matrix: (vector 1 0 0 1 (x delta) (y delta)))))

(define-method scale ((self <affine-transform>) scale)
  (let ((sx (if (real? scale) scale (x scale)))
	(sy (if (real? scale) scale (y scale))))
    (concatenate-transform
     self
     (make <affine-transform>
	   matrix: (vector sx 0 0 sy 0 0)))))

(define-method rotate ((self <size>) (angle <real>))
  (transform self (rotate $identity-transform angle)))

;;(define-constant $Pi (atan 0 -1))

(define (compute-rotation-kernel angle)
  (cond
   ((= angle 0) (values 1 0))
   ((= angle 90) (values 0 1))
   ((= angle 180) (values -1 0))
   ((= angle 270) (values 0 -1))
   (else
    (let ((angle (* $Pi (/ angle 180))))
      (values (cos angle) (sin angle))))))

;;; arrange it so that (1,0) in the transformed coordinates
;;; are where `to' was.  That is, combine a rotation and scaling
;;; operation.
;;;
;;; Hence,
;;;   (transform (make-point 1 0) (rotate-and-scale I P)) ==> P
;;;
;;;   where I=$identity-transform

(define (rotate-and-scale (self <affine-transform>) (to <size>))
  (let ((c (dx to))
        (s (dy to)))
    (concatenate-transform 
     self
     (make <affine-transform>
           matrix: (vector c s (- s) c 0 0)))))

  #|
  (scale (rotate self (* (/ 180 $Pi) (atan (dy to) (dx to))))
         (sqrt (inner-product to to)))
  |#

(define-method rotate ((self <affine-transform>) (theta <real>))
  ;; some exact values
  (bind ((c s (compute-rotation-kernel theta)))
    (concatenate-transform 
     self
     (make <affine-transform>
       matrix: (vector c s (- s) c 0 0)))))

(define-method invert-transform ((self <affine-transform>))
  (bind ((a b c d tx ty (list->values (vector->list (matrix self))))
	 (z (/ (- (* a d) (* b c)))))
    (make <affine-transform>
	  matrix: (vector (* z d)     (* z (- b))
			  (* z (- c)) (* z a)
			  (* z (- (* c ty) (* d tx)))  
			  (* z (- (* b tx) (* a ty)))))))

(define (inverse-transform item tm)
  (transform item (invert-transform tm)))

;;;  This should use more of a relative threshold approach.
;;;  Would that involve the determinant somehow?

(define-method chop ((self <affine-transform>))
  (let (((v <vector>) (matrix self)))
    (make <affine-transform>
          matrix: (vector (chop (vector-ref v 0))
                          (chop (vector-ref v 1))
                          (chop (vector-ref v 2))
                          (chop (vector-ref v 3))
                          (chop (vector-ref v 4))
                          (chop (vector-ref v 5))))))

;;;

;;;
;;;
;;;  Compute a transformation such that the rect `old' is mapped
;;;  to the rect `new', that is:
;;;
;;;     (transform A (translate-and-scale A B)) == B
;;;
;;;  The variant form, translate-and-scale-uniform, does the same
;;;  translation but scales the X and Y axes uniformly so that the
;;;  `old' rect, when translated, fits *within* the `new' rect.

(define (translate-and-scale (old <rect>) (new <rect>))
  (let ((sw (/ (size-width new) (size-width old)))
        (sh (/ (size-height new) (size-height old))))
    (vector->affine-transform
     (vector sw 0
             0 sh
             (- (origin-x new) (* sw (origin-x old)))
             (- (origin-y new) (* sh (origin-y old)))))))

(define (translate-and-scale-uniform (old <rect>) (new <rect>))
  (let ((s (min (/ (size-width new) (size-width old))
                (/ (size-height new) (size-height old)))))
    (vector->affine-transform
     (vector s 0
             0 s
             (- (origin-x new) (* s (origin-x old)))
             (- (origin-y new) (* s (origin-y old)))))))


