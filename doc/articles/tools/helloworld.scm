;;;
;;;  a "Hello World" illustration
;;;

(load "../persistence/graphic-styles.scm")     ; bring in common stylesheet
(load "cloud.scm")
(load "util.scm")

(define (meta)
  '((draw
     (info
      (project "xynthesis")
      (name "helloworld"))
     ;;
     (history
      (sketch
       (date "2006-06-08")
       (author "DMK"))
      (illustration
       (date "2006-06-08")
       (author "DMK")
       (revision "1"))
      (illustration
       (date "2006-06-13")
       (author "DMK")
       (revision "2"))
     ;;
     (design-size 
      small)))))

(define (draw)
  (style-apply 'medium-stroke)
  (moveto 10 15)
  (lineto 70 23)
  (stroke)
  (style-apply 'serif-italic-font)
  (moveto 70 20)
  (rshow 70 23 "Hello, world")
  ;;
  ;;  Demonstrate the cloud utility
  ;;
  (let ((f (make-rect 10 10 80 35)))
    (gsaved
     (setcolor (device-color '(rgb 1 0 0)))
     (setlinewidth 0.2)
     (rectstroke f))
    (cloudpath f)
    (stroke))
  ;;
  ;;  Demonstrate multi-segment radius-cornered arrow paths
  ;;
  (rectstroke (make-rect 70 50 40 10))
  (arrowstroke
   (list (make-point 10 10)
         (make-point 90 10)
         (make-point 90 50))
   radius: 4
   setback: 0.5)

  (bind ((lastpt arrow (compute-arrow-path (make-point 30 30)
                                           (make-point 32 26))))
    (areafill arrow))
                      
)




