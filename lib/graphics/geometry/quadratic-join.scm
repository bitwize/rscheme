

(define (quadratic-join (line <line>) (bend <real>))
  (let* ((u (point- (to line) (from line)))
         (len (parameter->distance-along line 1))
         (n (normal-on line))
         (p (lambda (t)
              (point+ (point-on line (/ (+ t 1) 2))
                      (size* n (* bend (- 1 (* t t)))))))
         (dp (lambda (t)
               (size+ (size* u 1/2)
                      (size* n (* -2 bend t))))))
    ;;
    (with-gstate-saved
     *current-device*
     (lambda ()
       (setlinewidth *current-device* 0.25)
       (show-vector (p -1) (dp -1))
       (show-vector (p 0) (dp 0))))
    (list
     (make-bezier start-point: (p -1)
                  start-tangent: (dp -1)
                  mid-point: (p -1/2)
                  end-tangent: (dp 0)
                  end-point: (p 0))
     (make-bezier start-point: (p 0)
                  start-tangent: (dp 0)
                  mid-point: (p 1/2)
                  end-tangent: (dp 1)
                  end-point: (p 1)))))
              
             
(define (tq)
  (minipage
   "Quadratic Join #1"
   (make-rect 36 36 200 200)
   (lambda ()
     (let ((j (quadratic-join (make-line 10 30 130 120) 20)))
       (show-curve (car j))
       (show-curve (cadr j))
       )))
  (minipage
   "Quadratic Join #2"
   (make-rect (+ 210 36) 36 200 200)
   (lambda ()
     (let ((j (quadratic-join (make-line 10 30 150 70) 50)))
       (show-curve (car j))
       (show-curve (cadr j))
       )))
  (minipage
   "Quadratic Join #3"
   (make-rect 36 (+ 250 36) 200 200)
   (lambda ()
     (let ((j (quadratic-join (make-line 50 10 180 160) 60)))
       (show-curve (car j))
       (show-curve (cadr j))
       ))))
