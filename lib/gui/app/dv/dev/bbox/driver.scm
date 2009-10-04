(define-class <bbox-device> (<virtual-device>)
  (graphic-bounding-box init-value: #f))

(define-method merge-bbox ((self <bbox-device>) new)
  (let ((n (merge-bbox (graphic-bounding-box self) new)))
    (set-graphic-bounding-box! self n)
    n))

(define-method merge-bbox ((self <boolean>) new)
  new)

(define-method merge-bbox ((self <rect>) new)
  (union-rect self new))

(define (with-bbox-device proc)
  (let ((b (make <bbox-device>)))
    (proc b)
    (graphic-bounding-box b)))

(define-method stroke-subpath ((self <bbox-device>) (subpath <vector>))
  (let ((minx (x (vector-ref subpath 0)))
        (miny (y (vector-ref subpath 0)))
        (maxx (x (vector-ref subpath 0)))
        (maxy (x (vector-ref subpath 0))))
    (for-each
     (lambda (pt)
       (set! minx (min (x pt) minx))
       (set! miny (min (y pt) miny))
       (set! maxx (max (x pt) maxx))
       (set! maxy (max (y pt) maxy)))
     (vector->list subpath))
    ;;
    (merge-bbox self (make-rect minx 
                                miny
                                (- maxx minx)
                                (- maxy miny)))))

(define-method show ((self <bbox-device>) str)
  (bind ((x y (current-point self))
         (f (font (gstate self)))
         (sw (string-width f str)))
    (merge-bbox self (make-rect x y sw (font-size f)))))
