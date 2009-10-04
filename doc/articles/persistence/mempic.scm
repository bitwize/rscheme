,(use graphics.fontmgr)

(define *draw-page-width* 40)
(define *draw-page-height* 30)

(define *draw-obj-radius* 5)
(define *node-label-font* (get-text-font "Helvetica" "Bold" 8))


(define (page-frame i)
  (make-rect (* *draw-page-width* i)
             0
             *draw-page-width*
             *draw-page-height*))

(define (obj page at-x at-y)
  (let ((x (+ at-x (* *draw-page-width* page)))
        (y at-y))
    (define (draw)
      (gsaved
       (translate (make-point x y))
       (moveto *draw-obj-radius* 0)
       (arc $zero-point *draw-obj-radius* 0 360)
       (gsaved
        (setcolor (device-color 'white))
        (fill))
       (stroke)))
    ;;
    (lambda (action #rest r)
      (case action
        ((draw) (draw))
        ((at) (make-point x y))
        ((label) (cshow x (- y 2.5) (car r)))
        ((edge) (bind ((radius-ratio angle (list->values r)))
                  (make-point 
                   (+ x (* (cos angle) radius-ratio *draw-obj-radius*))
                   (+ y (* (sin angle) radius-ratio *draw-obj-radius*)))))))))

(define (pagegroup-label i n #optional thunk)
  (let ((w *draw-page-width*)
        (h *draw-page-height*))
    (draw-long-brace base: (make-line (* (+ i n) w) -15 
                                      (* i w) -15)
                     ontip: (lambda ()
                              (rotate 180)
                              (translate (make-point 0 -3))
                              (thunk)))))

(define (free-page i)
  (gsaved
   (setdash '#(2 2) 0)
   (rectstroke (page-frame i))))
;;
(define (loaded-page i)
  (gsaved
   (rectstroke (page-frame i))))
;;
(define (reserved-page i)
  (let ((f (page-frame i)))
    (gsaved
     (setcolor (device-color '(gray 0.667)))
     (rectfill f))
    (rectstroke f)))
    ;;

(define (edge p q t td)
  (let* ((rel (point- (q 'at) (p 'at)))
         (relang (atan (dy rel) (dx rel)))
         (t (* t (/ $Pi 180)))
         (pp (p 'edge 0.5 (+ relang t)))
         (qp (q 'edge 1 (+ $Pi relang (- t))))
         (cp (point+ (p 'at) (size* rel 0.5)))
         (norm (normalize (make-size (- (dy rel)) (dx rel)))))
    ;;
    (if (not (= td 0))
        (arrowstroke (list pp 
                           (point+ cp (size* norm td))
                           qp) 
                     setback: 0.5
                     radius: (* 0.5 (distance (p 'at) (q 'at))))
        (arrowstroke (list pp qp) setback: 0.5))))
