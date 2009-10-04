(load "matrixpic.scm")
                                   
(define (twb)
  (let ((- $blank)
        (g1 (text-cell "G1" *content-font* padding: 3))
        (g2 (text-cell "G2" *content-font* padding: 3))
        (p (text-cell "P" *content-font* padding: 3)))
    (wb-table
     (vector (vector -  -    -   -)
             (vector -  -    g2  -)
             ;;
             (vector p  p    g1  -)
             (vector p  p    g1  -)))))

(define *body-font* (get-text-font "Times" "Roman" 9))

(define (draw)
  (translate (make-point 60 60))
  (scale 1.3 1.3)
  (let ((d (render-matrix (twb))))
    ;;
    (gsaved
     (translate (make-point (+ 10 (width d)) 0))
     (scale (/ 1.3) (/ 1.3))
     (moveto 0 0)
     (setfont *content-font*)
     (show "P=")
     (setfont *body-font*)
     (show "WB_PERSISTENT"))
    ;;
    (gsaved
     (translate (make-point (+ 10 (width d)) 10))
     (scale (/ 1.3) (/ 1.3))
     (moveto 0 0)
     (setfont *content-font*)
     (show "G1,G2=")
     (setfont *body-font*)
     (show "WB_GENERATION"))
    ;;
    (values)))

  #|
  (bind ((lst (twb))
         (size cw rh (layout-table #f lst)))

    (let ((bs (compute-borders lst 
                              col-widths: cw
                              row-heights: rh)))
      (chase-all-borders bs))

    ;;
    (for-each
     (lambda ((c <table-cell>))
       (print c)
       (if (has-property? c 'content)
           (cell-content-render (get-property c 'content) (assigned-frame c))))
     lst)
    ;;
    (format #t "cw : ~s\n" (vector-map exact->inexact cw))
    (format #t "rh : ~s\n" (vector-map exact->inexact rh)))
  |#

