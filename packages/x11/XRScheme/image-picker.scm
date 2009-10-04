
;;
;;  an <image-picker> is used to select sub-images out of a
;;  zoomable image
;;

(define-class <image-picker> (<view>)
    (zoom-factor init-value: 5)
    (zoomed-pixmap init-value: #f)
    base-image
    (selection init-value: #f)
    (background-pattern init-value: #f)
    (target init-value: #f))

(define *grid-color* (delay (exact-color 211 184 184)))
(define *background-color* (delay *dark*))
(define *selection-color-1* (exact-color 255 0 0))
(define *selection-color-2* (exact-color 0 255 0))

(define (draw-zoomed-image (ctx <X-drawing-context>)
			   (src <X-image>)
			   (zoom-factor <fixnum>)
			   grid-color)
  (let ((w (width (image-size src)))
	(h (height (image-size src))))
    (do ((y 0 (+ y 1))
    	 (yz 0 (+ yz zoom-factor)))
    	((eq? y h))
      (do ((x 0 (+ x 1))
           (xz 0 (+ xz zoom-factor)))
	  ((eq? x w))
	;; looking at a source pixel
	(set-foreground ctx (image-get-pixel src x y))
	(let ((f (make-rect xz yz zoom-factor zoom-factor)))
	    (fill-rectangle ctx f)
	    (if grid-color
		(begin
		    (set-foreground ctx grid-color)
		    (draw-rectangle ctx f))))))))

(define (get-zoomed-pixmap (self <image-picker>))
  (or (zoomed-pixmap self)
      (let* ((z (zoom-factor self))
	     (src-size (image-size (base-image self)))
	     (px (create-pixmap (x-window (window self))
				(make <size>
				      width: (* z (width src-size))
				      height: (* z (height src-size)))
				*X-default-depth*))
	     (ctx (create-drawing-context px)))
	(draw-zoomed-image ctx (base-image self) z (force *grid-color*))
	(set-zoomed-pixmap! self px)
	px)))

(define-method draw-view ((self <image-picker>) ctx)
    ;;
  (if (background-pattern self)
      (begin
	(set-tiled ctx (background-pattern self))
	(set-fill-style ctx 'FillTiled))
      (set-foreground ctx (force *background-color*)))
  (fill-rectangle ctx (bounds self))
  (set-fill-style ctx 'FillSolid)
  ;;
  ;;
  (let* ((pxm (get-zoomed-pixmap self))
	 (sz (image-size (base-image self))))
    (copy-area pxm (make-rect 0 0 
			      (* (zoom-factor self)
				 (width sz))
			      (* (zoom-factor self)
				 (height sz)))
	       ctx $zero-point))
  ;;
  (if (and (selection self)
	   (show-selection-in? self))
      (let* ((s (selection self))
	     (z (zoom-factor self))
	     (r (make-rect (* (origin-x s) z)
			   (* (origin-y s) z)
			   (* (size-width s) z)
			   (* (size-height s) z))))
	(set-dashes ctx 0 '#(3 3))
	(set-line-attributes ctx 0 'LineDoubleDash 'CapProjecting 'JoinMiter)
	(set-foreground ctx *black*)
	(set-background ctx *white*)
	(draw-rectangle ctx r)
	(set-line-attributes ctx 0 'LineSolid 'CapProjecting 'JoinMiter))))

(define (click->point (self <image-picker>) (pt <point>) window-coord?)
  (let ((org (if window-coord? (origin self) $zero-point))
	(z (zoom-factor self)))
    (make <point>
	  x: (quotient (- (x pt) (x org)) z)
	  y: (quotient (- (y pt) (y org)) z))))

(define (limits->rect (a <point>) (b <point>))
  (let ((origin-x (min (x a) (x b)))
	(origin-y (min (y a) (y b))))
    (make-rect origin-x
	       origin-y
	       (- (max (x a) (x b)) origin-x)
	       (- (max (y a) (y b)) origin-y))))

(define-method mouse-down ((self <image-picker>) (pt <point>) event)
  (call-with-locked-focus
   self
   (lambda (ctx)
     (let ((from (click->point self pt #f))
	   (sel (selection self)))
       (set-dashes ctx 0 '#(2 2))
       (set-line-attributes ctx 0 'LineDoubleDash 'CapProjecting 'JoinMiter)
       (if sel
	   (draw-image-picker-sel ctx (zoom-factor self) sel #t))
       (track-mouse-in-view 
	self 
	event
	(lambda (at)) ;; enter
	(lambda (at)) ;; leave
	(lambda ((to <point>))
	  (let* ((to (click->point self to #t))
		 (r (limits->rect from to)))
	    (format #t "new to: ~s (~s)\n" to r)
	    (draw-image-picker-sel ctx (zoom-factor self) sel #t)
	    (draw-image-picker-sel ctx (zoom-factor self) r #f)
	    (set-value! (target self) r)
	    (flush-graphics (window self))
	    ; (x-sync *X-display*)
	    (set! sel r))))))))

(define (draw-image-picker-sel ctx z rect erase?)
  (if erase?
      (let ((c (force *grid-color*)))
	(set-foreground ctx c)
	(set-background ctx c))
      (begin
	(set-foreground ctx *selection-color-1*)
	(set-background ctx *selection-color-2*)))
  (let ((rect (make-rect (* (origin-x rect) z)
			 (* (origin-y rect) z)
			 (* (size-width rect) z)
			 (* (size-height rect) z))))
    (draw-rectangle ctx rect)
    rect))

(define (show-selection-in? (view <view>))
  #t)

;    (eq? (visibility-status (window view)) 'key)
