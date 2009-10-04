
(define (chap)
  (let ((dev *dev*))
    (reset dev)
    (with-gstate-saved
     dev
     (lambda ()
       (setfont dev (get-text-font "Helvetica" "Bold" 9))
       (translate dev (make-point 150 50))
       (scale dev 5 5)
       (with-gstate-saved
        dev
        (lambda ()
          (rotate dev 90)
          (moveto dev (make-point -3 2))
          (scale dev 0.9 1)
          (show dev "Chapter")))
       ;;
       (let ((f (get-text-font "Palatino" "Bold" 36))
             (cn "18"))
         (setcolor dev (device-color dev 'black))
         (rectfill dev (make-rect 1 -3 (+ 5 (string-width f cn)) 30))
         (setcolor dev (device-color dev 'white))
         ;;
         (setfont dev f)
         (moveto dev (make-point 3 0))
         (show dev cn))))
    (flush-output-port dev)))
