
(define-color-list *scrollbar-colors*
  "black"
  "rgbi:0.3/0.3/0.333"				;"rgbi:0.333/0.333/0.4"
  "rgbi:0.6/0.6/0.667"				;"rgbi:0.667/0.667/0.8"
  "white")

(define (get-scrollbar-colors scrn)
  (pixels (get-bound-colors (screen-default-colormap scrn) 
			    *scrollbar-colors*)))

(define (draw-bezeled win gc (r <rect>) (colors <vector>) pat)
  (let loop ((x (x r))
	     (y (y r))
	     (w (width r))
	     (h (height r))
	     (p pat))
    (if (pair? p)
	(case (car p)
	  ((left)
	   (draw-rectangle win gc x y 1 h #t)
	   (loop (+ x 1) y (- w 1) h (cdr p)))
	  ((right)
	   (draw-rectangle win gc (+ x w -1) y 1 h #t)
	   (loop x y (- w 1) h (cdr p)))
	  ((top)
	   (draw-rectangle win gc x y w 1 #t)
	   (loop x (+ y 1) w (- h 1) (cdr p)))
	  ((bottom)
	   (draw-rectangle win gc x (+ y h -1) w 1 #t)
	   (loop x y w (- h 1) (cdr p)))
	  ((0)
	   (set-gcontext-foreground! gc (vector-ref colors 0))
	   (loop x y w h (cdr p)))
	  ((1)
	   (set-gcontext-foreground! gc (vector-ref colors 1))
	   (loop x y w h (cdr p)))
	  ((2)
	   (set-gcontext-foreground! gc (vector-ref colors 2))
	   (loop x y w h (cdr p)))
	  ((3)
	   (set-gcontext-foreground! gc (vector-ref colors 3))
	   (loop x y w h (cdr p)))
	  ((4)
	   (set-gcontext-foreground! gc (vector-ref colors 4))
	   (loop x y w h (cdr p)))
	  ((middle)
	   (draw-rectangle win gc x y w h #t))))))


;;;------------------------------------------------------------

(define-class <widget> (<object>) :abstract
  (x-window init-value: #f)
  (x-gcontext init-value: #f))

(define-constant $uninit-real (clone 0.0))

;; a widget

(define-constant (no-action sender)
  (values))

(define-class <scrollbar> (<widget>) :abstract
  (action type: <function> init-value: no-action)
  (enabled type: <boolean> init-value: #f)
  ;; in window (device) units...
  (widget-length type: <fixnum> init-value: 17)
  ;; in user units...
  (position type: <real> init-value: $uninit-real) ; center position
  (min-value type: <real>)
  (max-value type: <real>)
  (value-scope type: <real>)
  (line-size type: <real> init-value: 0)  ; reasonable default (1/10 of scope)
  (page-size type: <real> init-value: 0)  ; reasonable default (scope)
  ;; derived quantities
  (thumb-length type: <fixnum> init-value: 0)    ; clipped to min & max
  (user-device-ratio type: <real> init-value: 0))

;;;
(define-method initialize ((self <scrollbar>))
  (if (eq? (position self) $uninit-real)
      (set-position! self (/ (+ (min-value self) (max-value self)) 2)))
  (if (= (line-size self) 0)
      (set-line-size! self (/ (value-scope self) 10)))
  (if (= (page-size self) 0)
      (set-page-size! self (value-scope self)))
  (update-thumb-length! self)
  (update-user-device-ratio! self))

(define $min-thumb-length 16)

(define (update-thumb-length! (self <scrollbar>))
  (let ((range (- (max-value self) (min-value self))))
    (if (< range 1)
        (set-enabled! self #f)
        (let* ((w (* (value-scope self)
                     (/ (widget-length self) range)))
               (wclipped (min (widget-length self)
                              (max $min-thumb-length 
                                   (+ 0 (inexact->exact (round w)))))))
          (set-enabled! self #t)
          (set-thumb-length! self wclipped)))))

(define (update-user-device-ratio! (self <scrollbar>))
  (let ((span (- (widget-length self) 
                 (thumb-length self))))
    (if (< span 1)
        (set-enabled! self #f)
        (begin
          (set-enabled! self #t)
          (set-user-device-ratio! self
                                  (/ (- (max-value self) 
                                        (min-value self)
                                        (value-scope self))
                                     span))))))
;;;

(define-class <horz-scrollbar> (<scrollbar>))
(define-class <vert-scrollbar> (<scrollbar>))

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

(define (scrollbar-thumb-at (self <scrollbar>))
  (let ((x (inexact->exact
	    (+ 0.5
	       (/ (- (position self)
		     (+ (min-value self) (/ (value-scope self) 2)))
		  (user-device-ratio self))))))
    (dm 920 "thumb at => ~s" x)
    x))

;    (max 0 (min x (- (widget-length self) (thumb-length self))))

(define (update-value-range! (self <scrollbar>) min max position)
  (set-min-value! self min)
  (set-max-value! self max)
  (update-thumb-length! self)
  (update-user-device-ratio! self)
  (set-position! self position)
  (need-to-redraw (x-window self)))


(define (set-scrollbar-thumb-to! (self <scrollbar>) t)
  (let* ((t (max 0 (min t (- (widget-length self) (thumb-length self)))))
	 (p (+ (* t (user-device-ratio self))
	       (+ (min-value self) (/ (value-scope self) 2)))))
    (dm 921 "set position to: ~s ==> ~s" t p)
    (set-position! self p)))

(define (scrollbar-value-range (self <scrollbar>))
  (let ((half (/ (value-scope self) 2)))
    (values (- (position self) half)
	    (+ (position self) half))))

(define-method compute-button-press-proc ((self <horz-scrollbar>))
  (lambda (ignore-win click-at state)
    (if (enabled self)
        (start-tracking-scrollbar self (x click-at) x))))

(define-method compute-button-press-proc ((self <vert-scrollbar>))
  (lambda (ignore-win click-at state)
    (if (enabled self)
        (start-tracking-scrollbar self (y click-at) y))))

(define (start-tracking-scrollbar (self <scrollbar>)
				  starting-pt
				  get-axis)
  (let ((thumb-start-at (scrollbar-thumb-at self))
	(posn (position self)))
    ;; check to see if we need to do an initial major move
    (if (or (< starting-pt thumb-start-at)
	    (> starting-pt (+ thumb-start-at (thumb-length self))))
	(begin
	  (set-scrollbar-thumb-to! self (- starting-pt
					   (/ (thumb-length self) 2)))
	  (set! thumb-start-at (scrollbar-thumb-at self))
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
			   (set-scrollbar-thumb-to! 
			    self
			    (+ thumb-start-at delta)))
		       (need-to-redraw (x-window self)))))))

(define-method compute-exposure-thunk ((self <horz-scrollbar>))
  (let ((win (x-window self))
	(gc (x-gcontext self))
	(clrs (get-scrollbar-colors (drawable-screen (x-window self)))))
    (lambda ()
      (clear-area win)
      (if (enabled self)
          (let ((x (scrollbar-thumb-at self))
                (w (thumb-length self)))
            (draw-bezeled win gc (make-rect x 0 w 16) clrs $scrollbar-bezels)
            (let ((x (+ x (quotient w 2) -5)))
              (set-gcontext-foreground! gc (vector-ref clrs 3))
              (draw-points win gc (cons* x 4 $horz-gripper-points) #t)
              (set-gcontext-foreground! gc (vector-ref clrs 0))
              (draw-points win gc (cons* (+ x 1) 5 $horz-gripper-points) #t)))))))

(define-method compute-exposure-thunk ((self <vert-scrollbar>))
  (let ((win (x-window self))
	(gc (x-gcontext self))
	(clrs (get-scrollbar-colors (drawable-screen (x-window self)))))
    (lambda ()
      (clear-area win)
      (if (enabled self)
          (let ((y (scrollbar-thumb-at self))
                (h (thumb-length self)))
            (draw-bezeled win gc (make-rect 0 y 16 h) clrs $scrollbar-bezels)
            (let ((y (+ y (quotient h 2) -5)))
              (set-gcontext-foreground! gc (vector-ref clrs 3))
              (draw-points win gc (cons* 4 y $vert-gripper-points) #t)
              (set-gcontext-foreground! gc (vector-ref clrs 0))
              (draw-points win gc (cons* 5 (+ y 1) $vert-gripper-points) #t)))))))
  
(define-constant $horz-gripper-points
  '(      3 1   3 -1   3 1
    -9 2  3 1   3 -1   3 1
    -9 2  3 1   3 -1   3 1))

(define-constant $vert-gripper-points
  '(      3 1   3 -1
    -6 3  3 1   3 -1
    -6 3  3 1   3 -1
    -6 3  3 1   3 -1))

(define-constant $scrollbar-bezels
  '(0 bottom right
    3 left top
    1 bottom right
    2 middle))

#|
;;----------------------------------------------------------------------

(define (test sb)
  (for-each
   (lambda (x)
     (set-scrollbar-thumb-to! sb x)
     (bind ((f t (scrollbar-value-range sb)))
       (format #t " ~d: (~d - ~d)\n" x f t)))
   (range (+ 1 (- (widget-length sb) (thumb-length sb))))))

(define t1 (make <horz-scrollbar>
		 widget-length: 17
		 position: -65
		 min-value: -70
		 max-value: 80
		 value-scope: 10))

(define t2 (make <horz-scrollbar>
		 widget-length: 25
		 position: -20
		 min-value: -500
		 max-value: 500
		 value-scope: 160))

(define t3 (make <horz-scrollbar>
		 widget-length: 19
		 position: 0
		 min-value: -340  ;; each pixel is 2* the scope
		 max-value: 340
		 value-scope: 20))

;;; test

(define (sbt)
  (let* ((c (current-client))
	 (s (on-screen c))
	 (w (create-window parent: (screen-root s)
			   x: 10
			   y: 10
			   width: 200
			   height: 100
			   background: (screen-white-pixel s)))
	 (sb1 (make-horz-scrollbar parent: w
				   x: 5
				   y: 5
				   width: 190
				   position: 50
				   min-value: 0
				   max-value: 100
				   value-scope: 25))
 	 (sb2 (make-horz-scrollbar parent: w
				   x: 5
				   y: 25
				   width: 190
				   position: 50
				   min-value: 0
				   max-value: 100
				   value-scope: 50))
	 (sb3 (make-horz-scrollbar parent: w
				   x: 5
				   y: 45
				   width: 190
				   position: 0
				   min-value: -500
				   max-value: 500
				   value-scope: 10)))
   (map-window w)
   sb))

|#

(define (make-horz-scrollbar #key ;; X window properties
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
					  (get-scrollbar-colors s) 
					  2)
			     border-width: 1
			     event-mask: '(exposure
					   button-press
					   button-motion
					   button-release)))
	 (sb (make <horz-scrollbar>
		   widget-length: width
		   position: position
		   min-value: min-value
		   max-value: max-value
		   value-scope: value-scope
		   x-window: sbw
		   x-gcontext: (create-gcontext drawable: sbw))))
    (set-property! sbw 'scrollbar sb)
    (set-property! sbw 'exposure-thunk (compute-exposure-thunk sb))
    (set-property! sbw 'button-press (compute-button-press-proc sb))
    (map-window sbw)
    sb))

(define (make-vert-scrollbar #key ;; X window properties
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
					  (get-scrollbar-colors s) 
					  2)
			     border-width: 1
			     event-mask: '(exposure
					   button-press
					   button-motion
					   button-release)))
	 (sb (make <vert-scrollbar>
		   widget-length: height
		   position: position
		   min-value: min-value
		   max-value: max-value
		   value-scope: value-scope
		   x-window: sbw
		   x-gcontext: (create-gcontext drawable: sbw))))
    (set-property! sbw 'scrollbar sb)
    (set-property! sbw 'exposure-thunk (compute-exposure-thunk sb))
    (set-property! sbw 'button-press (compute-button-press-proc sb))
    (map-window sbw)
    sb))
