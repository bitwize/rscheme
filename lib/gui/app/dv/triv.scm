,(use gui.x)
,(use graphics.color)
,(use graphics.image)
,(use graphics.png)
,(use tables)

(load "ximages.scm")

(define $fg "images/select.png")
(define $bg "images/cells.png")

(define *dpy* (open-display (with-module unixm (hostname))))

(define (t)
  (open-in-window
   (make-graphic-image from: (read-png-image $fg))
   *dpy*))
  
(define (ehandler #rest r #key display event-key)
  (format #t "event: ~s\n" event-key)
  #t)

(define (eat)
  (process-event *dpy* handler: ehandler))

(define (open-in-window (self <graphic-image>) dpy)
  (let* ((s (car (display-roots dpy)))
	 (cmap (screen-default-colormap s))
	 (w (create-window parent: (screen-root s)
			   width: (image-width self)
			   height: (image-height self)
			   x: 10
			   y: 10
			   event-mask: '()))
	 (gc (create-gcontext drawable: w)))
    (map-window w)
    (display-finish-output dpy)
    (format #t "computing X image rep...\n")
    (let ((r (x-image-rep self cmap)))
      (composite r w gc 0 0))
    (display-finish-output dpy)
    w))

(define (fgi)
  (make-graphic-image from: (read-png-image $fg)))

(define (bgi)
  (make-graphic-image from: (read-png-image $bg)))

(load "composite.scm")

(define (tcom)
  (composite-image (fgi) (bgi)))
