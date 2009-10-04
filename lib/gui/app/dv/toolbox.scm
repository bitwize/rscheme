(define *cell0* (make-graphic-image 
		 from: (read-png-image "images/cell.png")))
(define *cell1* (make-graphic-image
		 from: (read-png-image "images/cells.png")))
(define *dim* (make-constant-image
	       (make-pixel red: 0 green: 0 blue: 0 alpha: 0.5)
	       width: (image-width *cell0*)
	       height: (image-height *cell0*)))

(define *tools*
  (map
   (lambda (e)
     (list (car e) 
	   (make-graphic-image 
	    from: (read-png-image
		   (string-append "images/" (cadr e) ".png")))))
   '((select "select")
     (place-text "text")
     (draw-box "box")
     (draw-line "line")
     (pen "pen")
     (zoom "magnify"))))
#|
  (list (list 'select (make-graphic-image 
		       from: (read-png-image "images/select.png")))
	(list 'place-text (make-graphic-image 
			   from: (read-png-image "images/text.png")))
	(list 'draw-box (make-graphic-image 
			 from: (read-png-image "images/box.png")))
	(list 'draw-line (make-graphic-image
			  from: (read-png-image "images/line.png")))
	(list 'draw-box (make-graphic-image
			 from: (read-png-image "images/ellipse.png")))
	(list 'pen (make-graphic-image
		    from: (read-png-image "images/pen.png")))
	(list 'pen (make-graphic-image
		    from: (read-png-image "images/magnify.png")))))
|#

(define-class <toolbox> (<object>)
  for-client
  (tools type: <vector>)
  toolbox-window)

(define-class <tool> (<object>)
  in-toolbox
  tool-name
  (tool-enabled? init-value: #t)
  (tool-selected? init-value: #f)
  tool-button-window
  tool-major)

(define (select-tool! (t <tool>))
  (vector-for-each
   (lambda (ot)
     (let ((sb (eq? t ot)))
       (if (not (eq? (tool-selected? ot) sb))
	   (begin
	     (set-tool-selected?! ot sb)
	     (need-to-redraw (tool-button-window ot))))))
   (tools (in-toolbox t)))
  ;;
  (let ((c (for-client (in-toolbox t))))
    (if (instance? (next-owner c) <open-view>)
	(set-major-mode! (next-owner c) (tool-major t)))))

(define (make-tool-button scrn in ix iy tool box)
  (let* ((w (create-window parent: in
			   x: (- (* (+ 1 (image-width *cell0*)) ix) 1)
			   y: (- (* (+ 1 (image-height *cell0*)) iy) 1)
			   width: (image-width *cell0*)
			   height: (image-height *cell0*)
			   border-width: 1
			   background: (screen-white-pixel scrn)
			   border: (screen-black-pixel scrn)
			   event-mask: '(button-press exposure)))
	 (gc (create-gcontext drawable: w))
	 (img0 (make-composite-image (cadr tool) *cell0*))
	 (img1 (make-composite-image (cadr tool) *cell1*))
	 (img2 (make-composite-image *dim* img0))
	 (t (make <tool>
	      tool-major: (get-major-mode (car tool))
	      tool-name: (car tool)
	      in-toolbox: box
	      tool-button-window: w)))
    (if (memq ':disabled (cddr tool))
	(set-tool-enabled?! t #f))
    ;;
    (set-property!
     w
     'exposure-thunk
     (lambda ()
       (x-composite (if (tool-enabled? t)
			(if (tool-selected? t) img1 img0)
			img2)
		    w gc 0 0)))
    ;;
    (set-property! 
     w
     'button-press
     (lambda (w at state)
       (if (tool-enabled? t)
	   (select-tool! t))))
    (map-window w)
    t))

(define (make-toolbox)
  (let* ((scrn (on-screen (current-client)))
	 (matrix-width 2)
	 (matrix-height (quotient (+ (length *tools*) (- matrix-width 1))
				  matrix-width))
	 (cell-width (+ 1 (image-width *cell0*)))
	 (cell-height (+ 1 (image-height *cell0*)))
	 (win (create-window parent: (screen-root scrn)
			     x: 16
			     y: 16
			     width: (* matrix-width cell-width)
			     height: (* matrix-height cell-height)
			     event-mask: '()))
	 (box (make <toolbox>
		toolbox-window: win
		for-client: (current-client)
		tools: '#()))
	 (tools (map (lambda (i t)
		       (make-tool-button 
			scrn
			win
			(modulo i matrix-width)
			(quotient i matrix-width)
			t
			box))
		     (range (length *tools*))
		     *tools*)))
    (set-tools! box (list->vector tools))
    (map-window win)
    (set-toolbox-menu! (current-client) box)
    box))

(define-interactive (open-toolbox)
  (interactive)
  (make-toolbox))
