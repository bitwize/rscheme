,(use graphics.fontmgr)

(define *heap-picture-size* (make-size 150 80))
(define *heap-picture-gap* 10)

;(define *label-font* (get-text-font "Times" "Italic" 8))
(define *label-font* (get-text-font "Helvetica" "Bold" 9))

(define *draw-obj-radius* 7)

(define (heap-frame i)
  (make-rect 0
             (* i (+ *heap-picture-gap* (height *heap-picture-size*)))
             (width *heap-picture-size*)
             (height *heap-picture-size*)))

(define *heap-grid* (make-size 10 5))

(define (per-x-unit) (/ (width *heap-picture-size*) (width *heap-grid*)))
(define (per-y-unit) (/ (height *heap-picture-size*) (height *heap-grid*)))

(define (obj heap at-x at-y)
  (let ((x (* at-x (per-x-unit)))
        (y (- (limit-y (heap-frame heap)) (* at-y (per-y-unit)))))
    ;;
    (define (draw fc)
      (gsaved
       (translate (make-point x y))
       (moveto *draw-obj-radius* 0)
       (arc $zero-point *draw-obj-radius* 0 360)
       (gsaved
        (setcolor fc)
        (fill))
       (stroke)))
    ;;
    (lambda (action #rest r)
      (case action
        ((draw) (draw (device-color 'white)))
        ((draw-black) (draw (device-color 'black)))
        ((draw-gray) (draw (device-color '(gray 0.667))))
        ((at) (make-point x y))
        ((edge) (bind ((radius-ratio angle (list->values r)))
                  (make-point 
                   (+ x (* (cos angle) radius-ratio *draw-obj-radius*))
                   (+ y (* (sin angle) radius-ratio *draw-obj-radius*)))))))))

(define *edge-inside-ratio* 0.75)

(define (edge p q #optional (t default: 0) (td default: 0))
  (let* ((p-at (p 'at))
         (q-at (q 'at))
         (rel (point- q-at p-at))
         (relang (atan (dy rel) (dx rel)))
         (t (* t (/ $Pi 180)))
         (pp (p 'edge *edge-inside-ratio* (+ relang t)))
         (qp (q 'edge 1 (+ $Pi relang (- t))))
         (cp (point+ p-at (size* rel 0.5)))
         (norm (normalize (make-size (- (dy rel)) (dx rel)))))
    ;;
    (setlinecap 'round)
    (if (not (= td 0))
        (arrowstroke (list pp 
                           (point+ cp (size* norm td))
                           qp) 
                     setback: 0.5
                     radius: (* 0.5 (distance p-at q-at)))
        (arrowstroke (list pp qp) setback: 0.5))
    (values pp qp)))

(define *root-box-height* 7)
(define *root-box-width* 7)
(define *heap-label-font* (get-text-font "Times" "Roman" 10))

(define (show-heap iy root-corner label)
  (let ((f (heap-frame iy)))
    (rectstroke f)
    (let* ((root-box-y (case root-corner
                         ((ul) (- (limit-y f) *root-box-height*))
                         ((ll) (origin-y f))))
           (root-box (make-rect (origin-x f)
                                root-box-y
                                *root-box-width*
                                *root-box-height*)))
      (rectstroke root-box)
      (gsaved
       (setfont *heap-label-font*)
       (rshow (- (limit-x f) 2)
              (case root-corner
                ((ul) (- (limit-y f) (font-size *heap-label-font*)))
                ((ll) (+ (origin-y f) 2)))
              label))
      (let ((c (make-point (center-x root-box) (center-y root-box))))
        (lambda (op #rest r)
          (case op
            ((at) c)
            ((edge) c)
            (else (error "Root box can't handle: ~s" (cons op r)))))))))

(define (draw-grid (user <rect>) (grid <size>))
  (rectstroke user)
  (for-each (lambda (ix)
              (let ((x0 (+ (origin-x user) (* (+ ix 1) (per-x-unit)))))
                (moveto x0 (origin-y user))
                (lineto x0 (limit-y user))))
            (range (- (width grid) 1)))
  (for-each (lambda (iy)
              (let ((y0 (+ (origin-y user) (* (+ iy 1) (per-y-unit)))))
                (moveto (origin-x user) y0)
                (lineto (limit-x user) y0)))
            (range (- (height grid) 1)))
  (stroke))
  

(define (background-grid)
  (gsaved
   (setlinewidth 0.1)
   (setcolor (device-color '(rgb 0 0 1)))
   (draw-grid (heap-frame 0) *heap-grid*)
   (draw-grid (heap-frame 1) *heap-grid*)))
