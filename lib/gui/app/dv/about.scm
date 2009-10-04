,(use graphics.tiff)

;;; nb, the about window has a mini console in it

(define *about-tiff* (load-into-heap (open-tiff-image "about.tiff")))

(define *mini-console* (make-dequeue))
(define *mini-console-windows* '())

(define (draw-miniconsole-tail win gc)
  (let loop ((y 36)
	     (k (dequeue-count *mini-console*)))
    (if (> k 0)
	(begin
	  (draw-glyphs win gc 2 y (dequeue-ref *mini-console* (- k 1)))
	  (if (> y -5)
	      (loop (- y 12) (- k 1)))))))

(define (miniconsole-write-line str)
  (dequeue-push-back! *mini-console* str)
  (for-each (lambda (w)
	      (need-to-clear w (get-property w 'exposure-thunk)))
	    *mini-console-windows*))

(define-interactive (show-about-window client)
  (interactive (client))
  (bind ((w h (tiff-image-size *about-tiff*))
	 (screen (on-screen client))
	 (dpy (on-display client))
	 (win (create-window parent: (screen-root screen)
			     x: 50
			     y: 50
			     width: w
			     height: h
			     background: (screen-white-pixel screen)
			     event-mask: '(exposure focus-change)))
	 (cwr (make-rect 5 (- h 5 38) (- w 10) 38))
	 (mini (create-window parent: win
			      x: (origin-x cwr)
			      y: (origin-y cwr)
			      width: (size-width cwr)
			      height: (size-height cwr)
			      event-mask: '(exposure)
			      border: (screen-black-pixel screen)
			      background: (screen-white-pixel screen)))
	 (gc (create-gcontext drawable: win)))
    ;
    (set-property! win
		   'exposure-thunk
		   (let ((img (tiff->x-image *about-tiff*
					     (screen-default-colormap screen)
					     (screen-root-depth screen)))
			 (bezel-rect (inset-rect cwr -1 -1))
			 (colorv (vector (get-lt-color
					  screen
					  "rgbi:0.93/0.93/0.93")
					 (get-dk-color 
					  screen
					  "rgbi:0.61/0.61/0.61"))))
		     (lambda ()
		       (put-image win gc img x: 0 y: 0)
		       (draw-bezeled win gc bezel-rect
				     colorv '(0 bottom right
					      1 left top)))))
    ;
    (set-property! mini
		   'exposure-thunk
		   (let ((gc (create-gcontext 
			      drawable: mini
			      foreground: (screen-black-pixel screen))))
		     (set-gcontext-font! gc (get-property dpy 'miniconsole-font))
		     (lambda ()
		       (clear-area mini)
		       (draw-miniconsole-tail mini gc))))
    ;
    (map-window mini)
    (map-window win)
    ;
    (set! *mini-console-windows* (cons mini *mini-console-windows*))
    (display-force-output dpy)
    ;
    win))
