;;;
;;;  draw the page frame
;;;

(define (user->device-box (box <rect>) ctm o)
  (bind ((x y w h (rect->values box)))
    (values
     (device-point (point+ (transform (make-point x y) ctm) o))
     (device-point (point+ (transform (make-point (+ x w) y) ctm) o))
     (device-point (point+ (transform (make-point x (+ y h)) ctm) o))
     (device-point (point+ (transform (make-point (+ x w) (+ y h)) ctm) o)))))

(define (draw-page-frame (in <open-view>))
  (bind ((pg (page-size (view-page (underlying-object in))))
         (mar (page-margins (view-page (underlying-object in))))
         (ctm (view-ctm (underlying-object in)))
         (o (size* (point->size (view-origin (underlying-object in))) -1))
         (ll lr ul ur (user->device-box (make-rect2 $zero-point pg) ctm o)))
      ;;
      ;;  draw the page frame shadow
      ;;
      (set-gcontext-foreground! (grid-gcontext in) (page-frame-color in))
      (set-gcontext-fill-style! (grid-gcontext in) 'stippled)
      (set-gcontext-line-width! (grid-gcontext in) 3)
      (draw-lines (content-window in)
                  (grid-gcontext in)
                  (list (+ (x ll) 3) (+ (y ll) 2)
                        (+ (x lr) 2) (+ (y lr) 2)
                        (+ (x ur) 2) (+ (y ur) 3)))
    (set-gcontext-line-width! (grid-gcontext in) 1)
    ;; 
    ;;  draw the page margins
    ;;
    (bind ((ll lr ul ur (user->device-box mar ctm o)))
      (draw-lines (content-window in)
                  (grid-gcontext in)
                  (list (x ll) (y ll)
                        (x lr) (y lr)
                        (x ur) (y ur)
                        (x ul) (y ul)
                        (x ll) (y ll))))
    ;;
    ;;  draw the page box itself
    ;;
    (set-gcontext-fill-style! (grid-gcontext in) 'solid)
    (draw-lines (content-window in)
                (grid-gcontext in)
                (list (x ll) (y ll)
                      (x lr) (y lr)
                      (x ur) (y ur)
                      (x ul) (y ul)
                      (x ll) (y ll)))
    ;;
    (values)))
