,(use gui.x)
,(use graphics.image
      graphics.color)
,(use graphics.png)
,(use gui.util.x)                       ; x-composite

(define x (open-x-display))

(define s (car (display-roots x)))

(define w (create-window parent: (screen-root s)
                         x: 0
                         y: 0
                         width: 100
                         height: 100
                         background: (screen-white-pixel s)))

(map-window w)
(display-force-output x)

(define g (create-gcontext drawable: w
                           background: (screen-white-pixel s)
                           foreground: (screen-black-pixel s)))
                           

(define paintg
  (let* ((cmap (screen-default-colormap s))
         (white (screen-white-pixel s))
         (red1 (alloc-color cmap (string->color "rgbi:1/0.3/0.3")))
         (green1 (alloc-color cmap (string->color "rgbi:0.3/1/0.3")))
         (blue1 (alloc-color cmap (string->color "rgbi:0.9/0.9/1")))
         (darkblue (alloc-color cmap (string->color "rgbi:0/0/0.5")))
         (g (create-gcontext drawable: w
                             background: (screen-white-pixel s)
                             foreground: (screen-black-pixel s)))
         (gdash (create-gcontext drawable: w
                                 background: (screen-white-pixel s)
                                 foreground: blue1
                                 dash-offset: 0
                                 dashes: 1
                                 line-style: 'on-off-dash))
         (width (drawable-width w))
         (height (drawable-height w))
         (parity 0))
    (lambda ((v <vector>))
      (let* ((x0 (vector-ref v 0))
             (x1 (vector-ref v (- (vector-length v) 2)))
             (delta-width (- x1 x0)))
        (copy-area w g 0 0 width height
                   w (- delta-width) 0)
        (clear-area w x: x0 y: 0 width: delta-width height: height)
        ;(draw-line w gdash x0 0 x0 100)
        (set-gcontext-clip-mask! gdash (list x0 0 delta-width height))
        ;;  Draw the background grid
        (set! parity (bitwise-xor parity (bitwise-and delta-width 1)))
        (draw-line w gdash parity 10 100 10)
        (draw-line w gdash parity 20 100 20)
        (draw-line w gdash parity 30 100 30)
        ;;  Draw the graph content
        (set-gcontext-foreground! g darkblue)
        (draw-lines w g
                    (vector-append (vector x1 height x0 height) v)
                    fill?: #t)
        ;;  Draw the foreground grid
        (draw-line w gdash (bitwise-xor parity 1) 10 100 10)
        (draw-line w gdash (bitwise-xor parity 1) 20 100 20)
        (draw-line w gdash (bitwise-xor parity 1) 30 100 30)
        ;;  Draw the value trace (overlay line)
        (set-gcontext-foreground! g red1)
        (draw-lines w g v)
        ;;  Flush output
        (display-force-output x)))))


#|
(define img (make-graphic-image from: (read-png-image "test2.png")))
(x-composite img w g 0 0)
|#

(define *t* (list->vector
             (apply append
                    (map (lambda (i)
                           (list (* i 5)
                                 (inexact->exact (round (+ 50 (* 40 (sin (/ i 2))))))))
                         (range 21)))))
