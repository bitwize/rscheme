
(define *rstep-client* #f)

(define (rstep-client)
  (or *rstep-client*
      (let ((c (current-client)))
        (with-module
            gui.rstep.x
          (set! *rstep-client* (open-client-on-x 
                                (display-name c)
                                (on-display c)
                                (on-screen c))))
        *rstep-client*)))

(define (make-stroke-style-inspector)
  (with-module
      gui.rstep.x
    (let* ((w (make-window frame: (make-rect 0 0 300 200)
                           title: "Stroke Style"
                           client: (rstep-client)))
           (ok (make-button frame: (make-rect 230 170 50 21)
                            parent: (content-view w)
                            title: "OK"))
           (cancel (make-button frame: (make-rect (- 230 60) 170 50 21)
                                parent: (content-view w)
                                title: "Cancel"))
           (cw (make-color-well frame: (make-rect 10 70 30 30)
                                parent: (content-view w))))
      w)))

