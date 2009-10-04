
(define-class <dragged-image> (<object>)
  redraw-thunk)

(define-method draw ((self <dragged-image>) rects)
  ((redraw-thunk self)))

(define (any-view-ok (view <view>))
  #t)

(define (drag-and-drop #key owner
		            pixmap
		            (client default: (current-client))
			    (starting-at type: <point>)
			    (pixmap-offset type: <size> default: $zero-size)
			    (size type: <size>)
			    (valid-targets type: <list>)
			    (test-proc default: any-view-ok)
			    (drop-proc default: #f))
  (dm 108 "drag-and-drop: ~s ~s" starting-at size)
  (bind ((s size)
	 (root (screen-root (on-screen client)))
	 (x0 y0 (translate-coordinates (in-window owner)
				       (x starting-at)
				       (y starting-at)
				       root))
	 (dx (+ (dx pixmap-offset)
		(- x0 (* 2 (x starting-at)) 1))) ; `-1' adjusts for border-width
	 (dy (+ (dy pixmap-offset) 
		(- y0 (* 2 (y starting-at)) 1)))
	 (w (create-window 
	     parent: root
	     x: (+ (x starting-at) dx)
	     y: (+ (y starting-at) dy)
	     width: (width s)
	     height: (height s)
	     override-redirect: #t
	     save-under: #t
	     border-width: 1
	     event-mask: '(exposure)))
	 (gc (create-gcontext
	      foreground: (get-pixel-resource 'window.foreground client)
	      drawable: w))
	 (v (make <dragged-image>
		  redraw-thunk: (if (instance? pixmap <graphic-image>)
				    (lambda ()
				      (x-composite pixmap w gc 0 0))
				    (lambda ()
				      (copy-area pixmap
						 gc 0 0
						 (width s)
						 (height s) w 0 0))))))
    ;;
    (define (mouse-upper pt state root-pt)
      (unmap-window w)
      (if drop-proc
	  (let loop ((vt valid-targets))
	    (if (null? vt)
		(values)
		(bind ((t (car vt))
		       (w (in-window t))
		       (xx yy ss ch (query-pointer w))
		       (v (get-property (or ch w)
					'rstep-view
					#f)))
		  (if (and
		       v
		       (>= xx 0)
		       (< xx (size-width (frame t)))
		       (>= yy 0)
		       (< yy (size-height (frame t)))
		       (test-proc v))
		      (drop-proc
		       v
		       (make-point 
			(- xx (x starting-at))
			(- yy (y starting-at))))
		      (loop (cdr vt))))))
	  (values pt state)))
    ;;
    (define (mouse-mover pt state root-pt)
      (with-state w
		  (set-drawable-x! w (+ dx (x pt)))
		  (set-drawable-y! w (+ dy (y pt)))))
				#|
				(let ((x1 (+ x0 (- (x pt) (x starting-at))))
				      (y1 (+ y0 (- (y pt) (y starting-at)))))
				  (dm "root => ~s,~s" x1 y1))
				|#
    ;;
    (dm 651 "window is (~s, ~s) ==> (~s, ~s)"
	dx dy (+ (x starting-at) dx) (+ (y starting-at) dy))
    (map-window w)
    (set-property! w 'rstep-view v)
    ((redraw-thunk v))
    (track-mouse owner
		 mouse-moved: mouse-mover
		 mouse-up: mouse-upper)))
