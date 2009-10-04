
(define (vexcept (v <vector>) i)
  (cond
   ((= i 0)
    (subvector v 1))
   ((= i (sub1 (vector-length v)))
    (subvector v 0 (sub1 (vector-length v))))
   (else
    (vector-append
     (subvector v 0 i)
     (subvector v (+ i 1))))))
   
;; eliminate row[i] and column[j]
;; where the matrix is #(row row ....)

(define (matrix-elim (matrix <vector>) i j)
  (vector-map (lambda (row)
                (vexcept row j))
              (vexcept matrix i)))
  
(define (cofactor (matrix <vector>) i j)
  (let ((d (determinant (matrix-elim matrix i j))))
    (if (odd? (+ i j))
        (- d)
        d)))

(define (determinant (matrix <vector>))
  (define (a i j)
    (vector-ref (vector-ref matrix (- i 1)) (- j 1)))
  ;;
  (let ((n (vector-length matrix)))
    (case n
      ((1)
       (a 1 1))
      ((2)
       (- (* (a 1 1) (a 2 2)) (* (a 2 1) (a 1 2))))
      ((3)
       (+ (* (a 1 1) (a 2 2) (a 3 3))
          (* (a 1 3) (a 2 1) (a 3 2))
          (* (a 1 2) (a 2 3) (a 3 1))
          (- (+ (* (a 1 3) (a 2 2) (a 3 1))
                (* (a 1 1) (a 2 3) (a 3 2))
                (* (a 1 2) (a 2 1) (a 3 3))))))
      (else
       (let ((top-row (vector-ref matrix 0)))
         (reduce (lambda (accum j)
                   (+ accum
                      (* (vector-ref top-row j)
                         (cofactor matrix 0 j))))
                 0
                 (range (vector-length top-row))))))))

(define (p^2 (p <point>))
  (let ((x (x p))
        (y (y p)))
    (+ (* x x) (* y y))))

(define-class <circle> (<geometric>)
  (center type: <point>)
  (radius type: <real>))

;;; For circles, the parameter is the angle in radians

(define-method point-on ((self <circle>) t)
  (make-point (+ (x (center self)) (* (cos t) (radius self)))
              (+ (y (center self)) (* (sin t) (radius self)))))

(define-method intersection-parameter ((c <circle>) p)
  ;; standard double-dispatch trick
  (circle-intersection p c))


;;; The intersection between a point and a circle returns the angle
;;; from the center of the circle, regardless of how far off the given
;;; point is from the circle itself

(define-method circle-intersection ((p <point>) (c <circle>))
  (atan (- (y p) (y (center c)))
        (- (x p) (x (center c)))))

;;; On the other hand, the intersection between a circle and a line
;;; returns zero, one, or two hits according to whether or not
;;; (a) the line misses the circle entirely, (b) the line is tangent
;;; to the circle, or (c) the line is a secant
;;;
;;; c.f. http://mathworld.wolfram.com/Circle-LineIntersection.html

(define-method circle-intersection ((l <line>) (c <circle>))
  (let* ((x0 (x (center c)))
         (y0 (y (center c)))
         (x1 (- (x (from l)) x0))
         (y1 (- (y (from l)) y0))
         (x2 (- (x (to l)) x0))
         (y2 (- (y (to l)) y0))
         ;;
         (dx (- x2 x1))
         (dy (- y2 y1))
         (dr^2 (+ (* dx dx) (* dy dy)))
         (det (- (* x1 y2) (* x2 y1)))
         (r (radius c))
         (disc (- (* r r dr^2) (* det det))))
    (cond
     ((< disc 0)
      '())
     ((= disc 0)
      (let ((p (make-point (+ x0 (/ (* det dy) dr^2))
                           (+ y0 (- (/ (* det dx) dr^2))))))
        (list (list (circle-intersection p c) p))))
     (else
      (let* ((sx (* (if (< dy 0) -1 1) dx (sqrt disc)))
             (sy (* (abs dy) (sqrt disc)))
             (p1 (make-point (+ x0 (/ (+ (* det dy) sx) dr^2))
                             (+ y0 (- (/ (+ (* det dx) sy) dr^2)))))
             (p2 (make-point (+ x0 (/ (- (* det dy) sx) dr^2))
                             (+ y0 (- (/ (- (* det dx) sy) dr^2))))))
        (list (list (circle-intersection p1 c) p1)
              (list (circle-intersection p2 c) p2)))))))
    

(define-method distance ((c <circle>) (p <point>))
  (distance (center c) p))

(define (make-circle #key
                     (point1 default: #f)
                     (point2 default: #f)
                     (point3 default: #f)
                     (center default: #f)
                     (radius default: #f))
  (cond
   ((and point1 point2 point3
         (not center) (not radius))
    (three-points->circle point1 point2 point3))
   ((and center point1 
         (not point2) (not point3) (not radius))
    (make <circle>
          center: center
          radius: (distance center point1)))
   ((and center radius
         (not point1) (not point2) (not point3))
    (make <circle>
          center: center
          radius: radius))
   (else
    (error "Cannot compute figure out circle from arguments"))))

;;; http://mathworld.wolfram.com/Circle.html

(define (three-points->circle (p1 <point>) (p2 <point>) (p3 <point>))
  (let* ((a (determinant (vector (vector (x p1) (y p1) 1)
                                 (vector (x p2) (y p2) 1)
                                 (vector (x p3) (y p3) 1))))
         (r1 (p^2 p1))
         (r2 (p^2 p2))
         (r3 (p^2 p3))
         (d (- (determinant (vector (vector r1 (y p1) 1)
                                    (vector r2 (y p2) 1)
                                    (vector r3 (y p3) 1)))))
         (e (determinant (vector (vector r1 (x p1) 1)
                                 (vector r2 (x p2) 1)
                                 (vector r3 (x p3) 1))))
         (f (- (determinant (vector (vector r1 (x p1) (y p1))
                                    (vector r2 (x p2) (y p2))
                                    (vector r3 (x p3) (y p3)))))))
    ;;
    (make <circle>
          center: (make-point (- (/ d (* 2 a)))
                              (- (/ e (* 2 a))))
          radius: (sqrt (- (/ (+ (* d d) (* e e)) (* 4 a a))
                           (/ f a))))))

#|
(define (polar c r a)
  (make-point (+ (x c) (* r (cos (* a (/ $Pi 180)))))
              (+ (y c) (* r (sin (* a (/ $Pi 180)))))))
|#
