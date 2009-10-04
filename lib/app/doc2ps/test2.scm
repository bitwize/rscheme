(define (tgs)
  (reset *dev*)
  (test2 *dev*)
  (flush-output-port *dev*))

(define (tpr)
  (let ((p (open-ps-device "/tmp/out.ps")))
    (test2 p)
    (close-graphics-device p)))



#|      
(define-class <hlist> (<object>)
  placement
  height
  width)
|#

(define (render-marginal-tic-marks dev h)
  (with-gstate-saved
   dev
   (lambda ()
     (setcolor dev (device-color dev '(gray 0.5)))
     (setlinewidth dev 0.5)
     (moveto dev (make-point 5 0))
     (lineto dev (make-point 0 0))
     (lineto dev (make-point 0 h))
     (lineto dev (make-point 5 h))
     (stroke dev))))

(define-method render-outline ((self <vskip>) dev)
  (render-marginal-tic-marks dev (height self)))

(define-method render-outline ((self <vbox>) dev)
  (render-marginal-tic-marks dev (height self)))

(define-method render-outline ((self <hlist>) dev)
  (with-gstate-saved
   dev
   (lambda ()
     (setcolor dev (device-color dev '(gray 0.5)))
     (setlinewidth dev 0.5)
     (rectstroke dev (make-rect (origin-x (placement self))
                                (- (origin-y (placement self)) (height self))
                                (width self)
                                (height self))))))
                                
(define (test2 dev)
  (let ((frames (chapter-start-page)))
    (for-each (lambda (f)
                (render-outline f dev))
              frames)
    ;;
    (let* ((pf0 (make <placement-subframe>
                      page: 0
                      frame: (list-ref frames 0)
                      column: 0))
           (pf1 (make <placement-subframe>
                      page: 0
                      frame: (list-ref frames 1)
                      column: 'primary-sidebar))
           (pf2 (make <placement-subframe>
                      page: 0
                      frame: (list-ref frames 1)
                      column: 0))
           (pf3 (make <placement-subframe>
                      page: 0
                      frame: (list-ref frames 1)
                      column: 1))
           (placer (lambda (sf y)
                     (make <placement>
                           subframe: sf
                           y: (* y 15))))
           (hlist (lambda (p)
                    (make <hlist>
                          placement: p
                          height: 14
                          width: 50)))
           (h (list (hlist (placer pf0 0))
                    (hlist (placer pf0 1))
                    (hlist (placer pf0 2))
                    (hlist (placer pf0 3))
                    (hlist (placer pf1 0))
                    (hlist (placer pf2 0))
                    (hlist (placer pf2 1))
                    (hlist (placer pf2 2)))))
      (for-each
       (lambda (h)
         (render-outline h dev))
       h))))

#|
(define (hlist-processor page-engine)
  ;;  maintain a "pending" list of <placement>'s, which
  ;;  are all the placements since the last committed
  ;;  <placement>.  A transition to a new <placement-group>
  ;;  always commits the preceding <placement>'s, so that
  ;;  the pending list always contains <placement>'s from the
  ;;  same <placement-subframe>
  ;;
  ;;  to support balancing columns, we maintain an even larger
  ;;  list of "slushy" placements which are committed unless
  ;;  the run ends without filling all the columns in the frame;
  ;;  if balanced columns is turned on, then we go back over
  ;;  the slushy placements and adjust them so all the columns
  ;;  balance
  ...)
|#
