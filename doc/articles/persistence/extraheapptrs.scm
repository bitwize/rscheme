(load "heappic.scm")
(load "cloud.scm")

(set! *heap-grid* (make-size 5 3))
(set! *draw-obj-radius* 13)
(set! *edge-inside-ratio* 0.3)

(define *node-name-font* (get-text-font "Helvetica" "Bold" 10))

(define *annotation-font* (get-text-font "Times" "Italic" 8))

(define (draw)
  (translate (make-point 100 100))
  ;(background-grid)
  ;;
  (show-heap 0 'll "Persistent")
  (show-heap 1 'ul "Transient")
  ;;    
  (define (label n name)
    (gsaved
     (moveto (+ 2 (x (n 'at)) *draw-obj-radius*)
             (- (y (n 'at)) 2))
     (show name)))
  
  ;;
  (setfont *node-name-font*)
  (let ((a (obj 0 2 1))
        (b (obj 1 3 2)))
    ;;
    (a 'draw)
    (b 'draw)
    (label a "A")
    (label b "B")
    ;;
    (bind ((p q (edge a b))
           (slot (inset-rect (make-rect (x p) (y p) 0 0) -4 -2)))
           ;;
      (rectstroke slot)
      ;;
      (arrowstroke (list (make-point (+ -40 20) (+ 60 10))
                         (upper-left slot))
                   setback: 0.5)))
  (setfont *annotation-font*)
  (cshow -35 85 "extraHeapPointers")
  (drawcloud (make-rect -40 60 30 20))
  ;;
  
  )

