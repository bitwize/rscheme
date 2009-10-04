;;;
;;;  Procedures to do more gaudy things...
;;;

(define (roundrectstroke dev (frame <rect>) r)
  (bind ((x y w h (rect->values frame))
         (-r (- r)))
    ;;
    (moveto dev (make-point (+ x w) (+ y r)))
    (arc dev (make-point (+ x w -r) (+ y h -r)) r 0 90)
    (arc dev (make-point (+ x r) (+ y h -r)) r 90 180)
    (arc dev (make-point (+ x r) (+ y r)) r 180 270)
    (arc dev (make-point (+ x w -r) (+ y r)) r 270 360)
    (stroke dev)))

;;;
;;;  `point-list' is a list of <point>s
;;;

(define (arrowstroke dev point-list #key 
                     (radius default: #f)
                     (setback default: #f))
  (with-gstate-saved
   dev
   (lambda ()
     (bind ((rev (reverse point-list))
            (lastpt arrow (compute-arrow-path (cadr rev) 
                                              (car rev)
                                              setback: setback))
            (steps (reverse (cons lastpt (cdr rev)))))
       ;;
       (if radius
           (radiuslinestroke dev steps radius)
           (begin
             (moveto dev (car steps))
             (for-each (lambda (p)
                         (lineto dev p))
                       (cdr steps))
             (stroke dev)))
       ;;
       (areafill dev arrow)))))


(define (radiuslinestroke dev pts radius)
  (radiuspath dev pts radius)
  (stroke dev))

(define (rot90 (d <size>))
  ;; rotates `d' counter-clockwise by 90 degrees
  (make-size (- (dy d)) (dx d)))

(define (radius-path-joins (p0 <point>) (p1 <point>) (p2 <point>) radius)
  ;;
  (let* ((da (point- p1 p0))
         (db (point- p2 p1))
         (na (normalize (rot90 da)))
         (nb (normalize (rot90 db)))
         ;;
         (na+r (size* na radius))
         (na-r (size* na (- radius)))
         (nb+r (size* nb radius))
         (nb-r (size* nb (- radius)))
         ;;
         (p0-left (point+ p0 na+r))
         (p0-right (point+ p0 na-r))
         (p1a-left (point+ p1 na+r))
         (p1a-right (point+ p1 na-r))
         ;;
         (p1b-left (point+ p1 nb+r))
         (p1b-right (point+ p1 nb-r)))
    ;;
    (bind ((pl hl tl (clip-to-segment p0-left p1a-left p1b-left nb+r))
           (pr hr tr (clip-to-segment p0-right p1a-right p1b-right nb-r))
           (center (if (eq? hl 'pe) pl pr)))
      ;;
      (define (angle-to p)
        (* 180 (/ (atan (- (y p) (y center))
                        (- (x p) (x center)))
                  $Pi)))
      ;;
      (values center
              (if (eq? hl 'pe) '+ '-)
              (angle-to (point+ center (if (eq? hl 'pe) na-r na+r)))
              (angle-to (point+ center (if (eq? hl 'pe) nb-r nb+r)))))))

;;; XXX doesn't work for closed paths (yet)
(define (radiuspath dev pts radius)
  (let* (((p <vector>) (list->vector pts))
         (n (- (vector-length p) 2)))
    (moveto dev (vector-ref p 0))
    (let loop ((i 0))
      (if (< i n)
          (bind ((ctr dir a1 a2 (radius-path-joins (vector-ref p i)
                                                   (vector-ref p (+ i 1))
                                                   (vector-ref p (+ i 2))
                                                   radius)))
            (if (eq? dir '+)
                (arc dev ctr radius a1 a2)
                (arcn dev ctr radius a1 a2))
            (loop (+ i 1)))
          (lineto dev (vector-ref p (+ i 1)))))))

;;;
;;;  A helper function for various arrow shaders...
;;;

;;;
;;;  By default, no setback is applied.
;;;
;;;  If `setback' is supplied, then the arrow tip
;;;  is set back by the given distance
;;;
;;;  If `setback-line' and `setback-linewidth' are supplied,
;;;  then the arrow tip is set back by the amount necessary
;;;  to have the tip be on the edge of the stroked line
;;;
;;;  Returns two values:
;;;
;;;    [0] a point inside the fillable arrow, 
;;;        which is where the from->to line should end
;;;        (i.e., instead of at `to')
;;;
;;;    [1] a path, represented as a <path-bounded-area> object
;;;        from graphics.geometry

(define (compute-arrow-path fromp top #key 
                            (scale default: 1)
                            (style default: 'default)
                            (setback default: #f)
                            (setback-line default: #f)
                            (setback-linewidth default: 1))
  (let* ((d (normalize (point- top fromp)))
         (n (make-size (- (dy d)) (dx d)))
         (tip (cond
               (setback-line
                ;; doesn't take into account the line's orientation
                (let ((c (abs (inner-product d (normal-on setback-line)))))
                  (if (< c 0.0001)
                      ;; forget the setback --
                      ;; the line and the setback-line are essentially parallel
                      top
                      (point+ top (size* d (- (/ setback-linewidth 2 c)))))))
               (setback
                (point+ top (size* d (- setback))))
               (else
                top))))
    (compute-default-arrow tip d n scale)))

(define (compute-default-arrow tip d n scale)
  (let* ((arrow-width (* scale 2))
         (arrow-length (* arrow-width 3))
         (ap2 (point+ tip (size* d (- arrow-length))))
         (ap3 (point+ ap2 (size* n arrow-width)))
         (ap4 (point+ ap2 (size* n (- arrow-width)))))
    (values (point+ tip (size* d (- arrow-length)))
            (simple-polygon-area tip ap3 ap4))))

;;;
;;;
;;;   Draw a long brace like this:
;;;
;;;         ____________^___________
;;;        /                        \
;;;
;;;   If supplied, executed the given proc in a graphic context
;;;   that has been translated and rotated so that (0,0) is the
;;;   tip of the brace

(define (draw-long-brace (dev <graphics-device>)
                         #key
                         (base type: <line>)
                         (stepin type: <real> default: 2)
                         (radius type: <real> default: 3)
                         (ontip default: #f))
  (with-gstate-saved
   dev
   (lambda ()
     ;; set up the coordinate system so that (0,0) is at the midpoint,
     ;; (-w,0) is at the start, and (w,0) is at the end
     (let ((w (lay-flat dev (point-on base 1/2) (to base)))
           (r radius))
       (moveto dev (make-point (- w) (- (+ stepin r))))
       (lineto dev (make-point (- w) (- r)))
       (arcn dev (make-point (- r w) (- r)) r 180 90)
       (lineto dev (make-point (- r) 0))
       (arc dev (make-point (- r) r) r 270 360)
       (arc dev (make-point r r) r 180 270)
       (lineto dev (make-point (- w r) 0))
       (arcn dev (make-point (- w r) (- r)) r 90 0)
       (lineto dev (make-point w (- (+ stepin r))))
       (stroke dev)
       (if ontip
           (ontip))))))

;; adjust the coordinate system so that `from' is at 0,0
;; and `to' is at w,0 where w = |to - from|.  i.e., do a rotation
;; and a translation, but not a scale.  Returns w

(define (lay-flat device (from <point>) (to <point>))
  (translate device from)
  (let* ((x1 (- (x to) (x from)))
         (y1 (- (y to) (y from)))
         (w (sqrt (+ (* x1 x1) (* y1 y1))))
         (c (/ x1 w))
         (s (/ y1 w)))
    (concat device (vector->affine-transform (vector c s (- s) c 0 0)))
    w))

;;;

