;;;
;;;  Scrollers (the scrolly part of a scrollbar)
;;;

(define-class <scroller> (<control>) :abstract
  (thumb type: <rect> init-value: $zero-rect))

(define-class <horz-scroller> (<scroller>))
(define-class <vert-scroller> (<scroller>))

;;;

(define (make-horz-scroller #key parent
			         frame)
  (make-view class: <horz-scroller>
	     frame: frame
	     parent: parent
	     background: 'scroller.mid
	     wants-button-motion: #t
	     thumb: (make-rect 2 2 30 16)))

;;;

(define-method pixels ((self <scroller>))
  (get-color-changers-resource 'scroller.shades (for-client self)))

(define-constant $horz-gripper-points
  '(      3 1   3 -1   3 1
    -9 2  3 1   3 -1   3 1
    -9 2  3 1   3 -1   3 1))

(define-constant $vert-gripper-points
  '(      3 1   3 -1
    -6 3  3 1   3 -1
    -6 3  3 1   3 -1
    -6 3  3 1   3 -1))

(define-constant $scroller-bezels
  '(0 bottom right
    4 left top
    1 bottom right
    1 middle))

(define-method draw-self ((self <scroller>) rects)
  (dm 341 "draw-scroller")
  (bind ((win gc o (lock-focus self))
	 (pix (pixels self)))
   ;draw outer, black frame
   ;(draw-rectangle win gc ...)
   ;draw inner, light frame
   ;(draw-rectangle win gc ...)
   ; background is mid-tone
   (draw-bezeled-rect win gc pix $scroller-bezels (thumb self))))

#|
(define $min-thumb-length 16)

(define (update-thumb-length! (self <scroller>))
  (let* ((w (* (value-scope self)
	       (/ (widget-length self)
		  (- (max-value self) (min-value self)))))
	 (wclipped (min (widget-length self)
			(max $min-thumb-length 
			     ;; (+ 0 k) -- cheap hack to get back a fixnum 
			     (+ 0 (inexact->exact (round w)))))))
    (set-thumb-length! self wclipped)
    w))

(define (update-user-device-ratio! (self <scroller>))
  (set-user-device-ratio! self
			  (/ (- (max-value self) 
				(min-value self)
				(value-scope self))
			     (- (widget-length self) 
				(thumb-length self)))))
;;;

;;; There are three cases to consider
;;;
;;;  (1) thumb length can represent scope ratio
;;;
;;;  (2) scope ratio is too small to represent w/thumb length 
;;;      (ie, scaled thumb length < $min-thumb-length)
;;;
;;;  (3) scope ratio is too large to represent w/thumb length
;;;      (ie, value-scope >= (max - min))
;;;
;;; However, these cases fall out of doing the computatioin
;;; in the right order (ie, first figuring out the thumb length,
;;; then using that to determine the effective travel range)

(define (scroller-thumb-at (self <scroller>))
  (inexact->exact
   (+ 0.5
      (/ (- (position self)
	    (+ (min-value self) (/ (value-scope self) 2)))
	 (user-device-ratio self)))))

(define (update-value-range! (self <scroller>) min max position)
  (set-min-value! self min)
  (set-max-value! self max)
  (update-thumb-length! self)
  (update-user-device-ratio! self)
  (need-to-redraw (x-window self)))


(define (set-scroller-thumb-to! (self <scroller>) t)
  (let ((t (max 0 (min t (- (widget-length self) (thumb-length self))))))
    (set-position! self
		   (+ (* t (user-device-ratio self))
		      (+ (min-value self) (/ (value-scope self) 2))))))

(define (scroller-value-range (self <scroller>))
  (let ((half (/ (value-scope self) 2)))
    (values (- (position self) half)
	    (+ (position self) half))))

(define-method compute-button-press-proc ((self <horz-scroller>))
  (lambda (ignore-win click-at state)
    (start-tracking-scroller self (x click-at) x)))

(define-method compute-button-press-proc ((self <vert-scroller>))
  (lambda (ignore-win click-at state)
    (start-tracking-scroller self (y click-at) y)))

(define (start-tracking-scroller (self <scroller>)
				  starting-pt
				  get-axis)
  (let ((thumb-start-at (scroller-thumb-at self))
	(posn (position self)))
    ;; check to see if we need to do an initial major move
    (if (or (< starting-pt thumb-start-at)
	    (> starting-pt (+ thumb-start-at (thumb-length self))))
	(begin
	  (set-scroller-thumb-to! self (- starting-pt
					   (/ (thumb-length self) 2)))
	  (set! thumb-start-at (scroller-thumb-at self))
	  (set! posn (position self))
	  (need-to-redraw (x-window self))))
    ;;
    (set-property! (x-window self)
		   'button-release
		   (lambda (ignore-win end-posn state)
		     ((action self) self)))
    ;; set up the motion tracker
    (set-property! (x-window self)
		   'button-motion
		   (lambda (ignore-win new-posn state)
		     (let ((delta (- (get-axis new-posn) starting-pt)))
		       (if (meta-state? state)
			   ;; adjust in value steps
			   (set-position! self (+ posn delta))
			   ;; adjust in thumb steps
			   (set-scroller-thumb-to! 
			    self
			    (+ thumb-start-at delta)))
		       (need-to-redraw (x-window self)))))))

(define-method compute-exposure-thunk ((self <horz-scroller>))
  (let ((win (x-window self))
	(gc (x-gcontext self))
	(clrs (get-scroller-colors (drawable-screen (x-window self)))))
    (lambda ()
      (let ((x (scroller-thumb-at self))
	    (w (thumb-length self)))
	(clear-area win)
	(draw-bezeled win gc (make-rect x 0 w 16) clrs $scroller-bezels)
	(let ((x (+ x (quotient w 2) -5)))
	  (set-gcontext-foreground! gc (vector-ref clrs 3))
	  (draw-points win gc (cons* x 4 $horz-gripper-points) #t)
	  (set-gcontext-foreground! gc (vector-ref clrs 0))
	  (draw-points win gc (cons* (+ x 1) 5 $horz-gripper-points) #t))))))

(define-method compute-exposure-thunk ((self <vert-scroller>))
  (let ((win (x-window self))
	(gc (x-gcontext self))
	(clrs (get-scroller-colors (drawable-screen (x-window self)))))
    (lambda ()
      (let ((y (scroller-thumb-at self))
	    (h (thumb-length self)))
	(clear-area win)
	(draw-bezeled win gc (make-rect 0 y 16 h) clrs $scroller-bezels)
	(let ((y (+ y (quotient h 2) -5)))
	  (set-gcontext-foreground! gc (vector-ref clrs 3))
	  (draw-points win gc (cons* 4 y $vert-gripper-points) #t)
	  (set-gcontext-foreground! gc (vector-ref clrs 0))
	  (draw-points win gc (cons* 5 (+ y 1) $vert-gripper-points) #t))))))
  

(define (make-horz-scroller #key ;; X window properties
			          x y width parent
				  ;; value properties
				  position min-value max-value 
				  value-scope)

  (let* ((s (drawable-screen parent))
	 (sbw (create-window parent: parent
			     x: x
			     y: y
			     width: width
			     height: 16
			     border: (screen-black-pixel s)
			     background: (vector-ref 
					  (get-scroller-colors s) 
					  2)
			     border-width: 1
			     event-mask: '(exposure
					   button-press
					   button-motion
					   button-release)))
	 (sb (make <horz-scroller>
		   widget-length: width
		   position: position
		   min-value: min-value
		   max-value: max-value
		   value-scope: value-scope
		   x-window: sbw
		   x-gcontext: (create-gcontext drawable: sbw))))
    (set-property! sbw 'scroller sb)
    (set-property! sbw 'exposure-thunk (compute-exposure-thunk sb))
    (set-property! sbw 'button-press (compute-button-press-proc sb))
    (map-window sbw)
    sb))

(define (make-vert-scroller #key ;; X window properties
			          x y height parent
				  ;; value properties
				  position min-value max-value 
				  value-scope)

  (let* ((s (drawable-screen parent))
	 (sbw (create-window parent: parent
			     x: x
			     y: y
			     width: 16
			     height: height
			     border: (screen-black-pixel s)
			     background: (vector-ref 
					  (get-scroller-colors s) 
					  2)
			     border-width: 1
			     event-mask: '(exposure
					   button-press
					   button-motion
					   button-release)))
	 (sb (make <vert-scroller>
		   widget-length: height
		   position: position
		   min-value: min-value
		   max-value: max-value
		   value-scope: value-scope
		   x-window: sbw
		   x-gcontext: (create-gcontext drawable: sbw))))
    (set-property! sbw 'scroller sb)
    (set-property! sbw 'exposure-thunk (compute-exposure-thunk sb))
    (set-property! sbw 'button-press (compute-button-press-proc sb))
    (map-window sbw)
    sb))
|#