(load "mempic.scm")

(define (draw)
  (let ((one-page (make-size 40 30)))
    ;;
        ;;
    (loaded-page 0)
    (loaded-page 1)
    (reserved-page 2)
    (reserved-page 3)
    (free-page 4)
    (free-page 5)
    (free-page 6)
    ;;
    (bind ((a (obj 0 10 20))
           (b (obj 0 25 10))
           (c (obj 1 20 15))
           (d (obj 2 25 20))
           (e (obj 3 20 15)))
      ;;
      (a 'draw)
      (b 'draw)
      (c 'draw)
      (d 'draw)
      (e 'draw)
      (edge a b 0 0)
      (edge b c 45 10)
      (edge c e -30 -10)
      (edge a d 15 8)
      ;;
      (setfont (get-text-font "Helvetica" "Regular" 8))
      (for-each (lambda (i)
                  (moveto (point+ (origin (page-frame i))
                                  (make-size 0 -8)))
                  (show (~ "~03d" (+ 201 i))))
                (range 7))
      ;;
      (setfont (get-text-font "Times" "Italic" 8))
      (pagegroup-label 0 2
                       (lambda ()
                         (cshow 0 -8 "Loaded")))
      (pagegroup-label 2 2
                       (lambda ()
                         (cshow 0 -8 "Reserved")))
      (pagegroup-label 4 3
                       (lambda ()
                         (cshow 0 -8 "Free")))
      ;;
      (values))))


