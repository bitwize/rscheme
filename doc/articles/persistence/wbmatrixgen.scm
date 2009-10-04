(load "matrixpic.scm")

(define (draw)
  (translate (make-point 60 60))
  (scale 1.3 1.3)
  (define (pc lg lc rg rc)
    (make <graphic-cell-content>
          bbox: (make-rect 0 0 20 20)
          render-proc: (lambda (frame)
                         (gsaved
                          (translate (origin frame))
                          (translate (make-point 2.5 0))
                          (mini-heap-pic lg (if (= lc 0) 'white 'gray)
                                         rg (if (= rc 0) 'white 'gray))))))
  ;;
  (render-matrix
   (wb-table
    (vector (vector (pc 0 0 0 0) (pc 0 0 0 1) (pc 0 0 1 0) (pc 0 0 1 1))
            (vector (pc 0 1 0 0) (pc 0 1 0 1) (pc 0 1 1 0) (pc 0 1 1 1))
            (vector (pc 1 0 0 0) (pc 1 0 0 1) (pc 1 0 1 0) (pc 1 0 1 1))
            (vector (pc 1 1 0 0) (pc 1 1 0 1) (pc 1 1 1 0) (pc 1 1 1 1))))))
