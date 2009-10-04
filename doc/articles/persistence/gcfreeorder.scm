(load "mempic.scm")

(set! *draw-page-width* 40)
(set! *draw-page-height* 30)
(set! *draw-obj-radius* 5)

(define (draw)
  (let ((one-page (make-size 40 30)))
    ;;
        ;;
    (loaded-page 0)
    (loaded-page 1)
    ;;
    (bind ((a (obj 0 10 20))
           (b (obj 0 25 10))
           (c (obj 1 20 15))
           )
      ;;
      (a 'draw)
      (b 'draw)
      (c 'draw)
      (setfont *node-label-font*)
      (a 'label "a")
      (b 'label "b")
      (c 'label "c")
      (edge b c 45 10)
      ;;
      (setfont (get-text-font "Helvetica" "Regular" 8))
      (for-each (lambda (i)
                  (moveto (point+ (origin (page-frame i))
                                  (make-size 0 -8)))
                  (show (~ "~03d" (+ 201 i))))
                (range 2))
      ;;
      (setfont (get-text-font "Times" "Italic" 8))
      ;;
      (values))))
