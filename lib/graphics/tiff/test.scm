(define *tiff* #f) ; tiff
(define *img* #f)  ; ximage

(define (t)
  (let ((img (open-tiff-image "about.tiff")))
    ;(print (properties (vector-ref (tiff-subimages img) 0)))
    (set! *tiff* img)
    img))

#|
(define (t2)
  (for-each-row 
   i 1 
   (lambda (y r) 
     (format #t "~3d: ~#*@60s\n" y r))))
|#

(define dpy (open-display "wind"))

(define scrn (car (display-roots dpy)))

(define win (create-window parent: (screen-root scrn)
			   x: 10
			   y: 20
			   background: #xFFFFFF
			   width: 300
			   height: 200))
(map-window win)
(display-force-output dpy)

(define gc (create-gcontext drawable: win))

#|
(define (t2m)
  (make-color-mapped-image (screen-default-colormap scrn) *tiff* 1))

(define (t2t)
  (make-truecolor-image (screen-default-colormap scrn) *tiff* 1))
|#

(define (t2)
  (set! *img* (tiff->x-image *tiff* (screen-default-colormap scrn) 24)))

(define (t3)
  (put-image win gc *img* x: 10 y: 10))
