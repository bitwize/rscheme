(define-class <ellipse> (<curve>)
  (origin type: <point>)
  (zero-degrees type: <size>)
  (aspect type: <real>))

(define-method translate ((self <ellipse>) delta)
  (make <ellipse>
        origin: (make-point (+ (x delta) (x (origin self)))
                            (+ (y delta) (y (origin self))))
        zero-degrees: (zero-degrees self)
        aspect: (aspect self)))

(define *rotations*
  (list (make-affine-transform)
        (rotate (make-affine-transform) 90)
        (rotate (make-affine-transform) 180)
        (rotate (make-affine-transform) 270)))

;;; Reflect about the Y axis

(define (y-reflect (p <point>))
  (make-point (- (x p)) (y p)))

(define (ellipse->bezier-segments (self <ellipse>))
  (let* ((segs (unit-circle-beziers))
         (fixup (scale
                 (rotate-and-scale 
                  (translate $identity-transform (origin self))
                  (zero-degrees self))
                 (make-point 1 (aspect self)))))
    (map (lambda (s)
           (transform s fixup))
         (unit-circle-beziers))))
  
(define (make-ellipse #key 
                      (origin default: $zero-point)
                      (angle default: 0)
                      (major-axis default: 1)
                      (aspect default: #f)
                      (minor-axis default: #f)
		      (frame default: #f))
  (if frame
      (make <ellipse>
	    origin: (center frame)
	    zero-degrees: (make-size (/ (size-width frame) 2) 0)
            aspect: (/ (size-height frame) (size-width frame)))
      (let ((rot (rotate (make-affine-transform) angle)))
        (make <ellipse>
              origin: origin
              zero-degrees: (transform (make-size major-axis 0) rot)
              aspect: (if minor-axis
                          (/ minor-axis major-axis)
                          (or aspect 1))))))

(define-method point-on ((self <ellipse>) t)
  (let* ((a (zero-degrees self))
         (b (make-size (* (aspect self) (- (dy a)))
                       (* (aspect self) (dx a)))))
    (point+ (origin self)
            (size+ (size* a (cos (* 2 $Pi t)))
                   (size* b (sin (* 2 $Pi t)))))))

(define-method tangent-on ((self <ellipse>) t)
  (let* ((a (zero-degrees self))
         (b (make-size (* (aspect self) (- (dy a)))
                       (* (aspect self) (dx a)))))
    (size+ (size* a (* -2 $Pi (sin (* 2 $Pi t))))
           (size* b (* 2 $Pi (cos (* 2 $Pi t)))))))

;;; return the outward facing normal vector (unnormalized)

(define-method normal-on ((self <ellipse>) t)
  (let* ((a (zero-degrees self))
         (b (make-size (* (aspect self) (- (dy a)))
                       (* (aspect self) (dx a))))
         (t (size+ (size* a (* -2 $Pi (sin (* 2 $Pi t))))
                   (size* b (* 2 $Pi (cos (* 2 $Pi t)))))))
    (make-size (dy t) (- (dx t)))))

;;;  Return the parameter value for which the ellipse 
;;;  has a horizonal tangent and is at the top of the curve


(define-method ellipse-top-at ((self <ellipse>))
  ;; The derivation for this function involves some non-trivial trigonometry,
  ;; basically starting from the angle of the normal at angle `theta' to an
  ;; unrotated ellipse with aspect ratio `s', which turns out to be:
  ;;    (atan (* s (sin theta)) (cos theta))
  ;;
  ;; From there, it's a matter of adding back in the rotation angle,
  ;; which is:
  ;;    (atan (dy z) (dx z))
  ;;
  ;; and observing that we want to find the value of `theta' for which
  ;; the normal angle is equal to 90 degrees minus the rotation angle.
  ;; The rest is trig, but takes advantage of the fact that:
  ;;    (tan (- 90 t)) == (cot t) == (/ (tan t))
  ;; and, of course, the fact that
  ;;    (tan (atan y x)) == (/ y x)
  ;;
  (let* ((z (zero-degrees self))
         (a (aspect self))
         (t (/ (atan (* a (dx z)) (dy z)) (* 2 $Pi))))
    ;; normalize to the range 0...1
    (if (< t 0)
        (+ t 1)
        t)))

(define (ellipse-tangent-at (self <ellipse>) (tangent <size>))
  (let* ((z (rotate (zero-degrees self)
                    (* (/ 180 $Pi)
                       (atan (dy tangent) (- (dx tangent))))))
         (a (aspect self))
         (ang (atan (* a (dx z)) (dy z)))
         (t (/ ang (* 2 $Pi))))
    ;; normalize to the range 0...1
    (if (< t 0)
        (+ t 1)
        t)))

(define-method subdivide ((self <ellipse>) spec)
  ;;
  (define (compass x)
    (cond
     ((= x 0) 'e)
     ((= x 1/4) 'n)
     ((= x 1/2) 'w)
     ((= x 3/4) 's)
     (else x)))
  ;;
  (ellipse-subdivide self (compass (car spec)) (compass (cadr spec))))

#|
(define (ecurve (self <ellipse>) t0 t1)
  (bind ((x0 y0 x1 y1   (ellipse-handle-at self t0))
         (x3 y3 x2- y2- (ellipse-handle-at self t1)))
    (curv x0 y0 x1 y1 (- x2-) (- y2-) x3 y3)))
|#

(define (ellipse-subdivide (self <ellipse>) t0 t1)
  (let loop ((t t0)
             (r '()))
    (if (eq? t t1)
        (reverse! r)
        (bind ((seg next-t (ellipse-subdiv-segment self t t1)))
          (print seg)
          (format #t "next => ~s\n" next-t)
          (loop next-t (cons seg r))))))

;;; chomp off at exactly one segment

(define (ellipse-next-handle t0 t1)
  (if (number? t0)
      (cond
       ((< t0 1/4) (if (and (number? t1) (< t1 1/4)) t1 'n))
       ((< t0 1/2) (if (and (number? t1) (< t1 1/2)) t1 'w))
       ((< t0 3/4) (if (and (number? t1) (< t1 3/4)) t1 's))
       ((< t0 1) 
        (if (number? t1)
            (if (<= t1 1) 
                t1
                (error "ellipse-sudivide: parameter t1 ~s out of range"
                       t1))
            1))
       (else
        (error "ellipse-sudivide: parameter t0 ~s out of range"
               t0)))
      (if (number? t1)
          (cond
           ((eq? t0 'e) (if (< t1 1/4) t1 'n))
           ((eq? t0 'n) (if (< t1 1/2) t1 'w))
           ((eq? t0 'w) (if (< t1 3/4) t1 's))
           ((eq? t0 's) 
            (if (<= t1 1) 
                t1
                (error "ellipse-sudivide: parameter t1 ~s out of range"
                       t1)))
           (else
            (error "ellipse-subdivide: parameter t0 ~s is invalid" t0)))
          (case t0
            ((e) 'n)
            ((n) 'w)
            ((w) 's)
            ((s) 1)
            (else (error "ellipse-subdivide: parameter t0 ~s is invalid" 
                         t0))))))
      
(define (ellipse-subdiv-segment self t0 t1)
  (let* ((n (ellipse-next-handle t0 t1))
         (q (cons t0 n)))
    (format #t "ellipse-subdivide-segment: ~s .. ~s\n" t0 n)
    (values
     (cond
      ((member q '((e . n) (0 . n)))
       (car (ellipse->bezier-segments self)))
      ((equal? q '(n . w))
       (cadr (ellipse->bezier-segments self)))
      ((equal? q '(w . s))
       (caddr (ellipse->bezier-segments self)))
      ((member q '((s . e) (s . 1)))
       (cadddr (ellipse->bezier-segments self)))
      (else
       (ellipse-subdiv1 self
                        (case t0
                          ((e) 0)
                          ((n) 1/4)
                          ((w) 1/2)
                          ((s) 3/4)
                          (else t0))
                        (case n
                          ((e) 0)
                          ((n) 1/4)
                          ((w) 1/2)
                          ((s) 3/4)
                          (else n)))))
     n)))

(define (ellipse-subdiv1 self t0 t1)
  (let ((e0 (point-on self t0))
        (e1 (point-on self t1))
        (em (point-on self (/ (+ t0 t1) 2))))
    (fit-curve e0 em e1 (make <bezier-curve>
                              start-point: e0
                              first-handle: (point+ e0 (tangent-on self t0))
                              second-handle: (point+ e1 (size*
                                                         (tangent-on self t1)
                                                         -1))
                              end-point: e1))))
         

;;;  Determine the point on an ellipse such that the normal
;;;  is parallel to the given value

#|
(define (ellipse-parameter-for-normal-angle (self <ellipse>) norm)
  ;; XX only works for angle=0
  (let ((t (atan (/ (/ (dy norm) (dx norm)) s))))
    (cond
      ((< a (/ $Pi 2))
       (values (seg 0) 
|#

#|
   a = { ax, ay }
   b = { bx, by }
   n = { nx, ny }

   Solve[ (-a Sin[t] + b Cos[t]) . n == 0, t ]

   Unprotect[ Sin, Cos ]
   Cos[ArcTan[x_,y_]] := x / Sqrt[x^2+y^2]
   Sin[ArcTan[x_,y_]] := y / Sqrt[x^2+y^2]
 
(* Basic bezier setup *)

p0 = { x0, y0 }
p1 = { x1, y1 }
p2 = { x2, y2 }
p3 = { x3, y3 }

q0 = { X0, Y0 }
q1 = { X1, Y1 }
q2 = { X2, Y2 }
q3 = { X3, Y3 }

a = p3 - 3 p2 + 3 p1 - p0
b = 3 p0 - 6 p1 + 3 p2
c = 3 p1 - 3 p0

bez[t_] := a t^3 + b t^2 + c t + p0

src[t_] := q0 (1-t)^3 + 3 t (1-t)^2 q1 + 3 t^2 (1-t) q2 + t^3 q3

(* Starting position *)

startPosn = bez[0] == { E0X, E0Y }

(* Starting angle *)

startAngle = ((D[ bez[t], t ] /. t->0) == (alpha D[ src[t], t ] /. t->0))

(* Midpoint position *)

midPosn = (bez[1/2] == {EMX, EMY })

(* Ending position *)

endPosn = (bez[1] == {E1X, E1Y })

(* Ending angle *)

endAngle = ((D[ bez[t], t ] /. t->1) == (beta D[ src[t], t ] /. t->1))

soln = Solve[ { Simplify[startPosn], 
                Simplify[endPosn], 
                Simplify[midPosn], 
                Simplify[startAngle], 
                Simplify[endAngle] },
              { x0, y0, x1, y1, x2, y2, x3, y3, alpha, beta } ]

Simplify [ (x1-x0)/.soln ]
Simplify[ 

soln /. { X0 -> 0, Y0 -> 0, 
          X1 -> 5, Y1 -> 50, 
          X2 -> 85/2, Y2 -> 175/2, 
          X3 -> 145/2, Y3 -> 95, 
          E0X -> (-5), E0Y -> 2,
          EMX -> (215/8-5), EMY -> (1015/16+2),
          E1X -> (145/2-5), E1Y -> (95+2) }

|#

         
