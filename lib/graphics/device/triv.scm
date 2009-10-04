,(use graphics.device
      graphics.geometry)

(define (t1-pdf)
  (let ((dev (open-pdf-device "/tmp/test1.pdf")))
    (startpage dev)
    ;;
    (moveto dev (make-point 10 10))
    (lineto dev (make-point 30 20))
    (stroke dev)
    ;;
    (endpage dev)
    (close-graphics-device dev)))
