(define *middle-size* 10)

;;;   A rectangle with part of the vertical piece dashed

(define (rectstroke-middash-v r)
  (let ((y0 (origin-y r))
        (y1 (- (center-y r) (/ *middle-size* 2)))
        (y2 (+ (center-y r) (/ *middle-size* 2)))
        (y3 (limit-y r))
        (x0 (origin-x r))
        (x1 (limit-x r)))
    ;;
    (moveto x0 y2)
    (lineto x0 y3)
    (lineto x1 y3)
    (lineto x1 y2)
    ;;
    (moveto x1 y1)
    (lineto x1 y0)
    (lineto x0 y0)
    (lineto x0 y1)
    (stroke)
    (with-gstate-saved
     (lambda ()
       (setdash '#(0 2) 0)
       (setlinecap 'round)
       (moveto x0 y1)
       (lineto x0 y2)
       (moveto x1 y2)
       (lineto x1 y1)
       (stroke)))))

;;;   A rectangle with part of the horizontal piece dashed

(define (rectstroke-middash-h r)
  (with-gstate-saved
   (lambda ()
     (translate (lower-right r))
     (rotate 90)
     (rectstroke-middash-v (make-rect 0 0 (size-height r) (size-width r))))))
