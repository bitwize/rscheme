
;;; three hilite states
;;;   0 = off & not tracking
;;;   1 = off & tracking
;;;   2 = on & tracking

(define-class <button> (<view>)
  (state type: <boolean> init-value: #f)
  (hilite-state type: <fixnum> init-value: 0)
  (pixels init-value: #f)
  (title type: <string> init-value: "")
  (font #|type: <x-font>|# init-function: application-font)
  (title-x type: <fixnum> init-value: 0)
  (title-y type: <fixnum> init-value: 0))

(define-method initialize ((self <button>))
  (set-pixels! self
	       (get-pixels-resource 'button.shades
				    (for-client (window self))))
  (if (> (string-length (title self)) 0)
      (bind ((w a d l r fa (text-extents (font self) (title self))))
	;; need to center it...
	(set-title-x! self (quotient (- (size-width (frame self)) w) 2))
	(set-title-y! self (- (quotient (+ (size-height (frame self)) fa) 2)
			      1)))))

(define-macro (make-button . args)
  `(make-view class: <button> 
	      wants-enter-and-leave: #t
	      ,@args))

(define-constant $button-bezel
  '(0 bottom right
    3 left top
    1 bottom right))

(define-constant $button-bezel-in
  '(3 bottom right
    1 left top
    3 middle))

(define-method draw-self ((self <button>) rects)
  (dm 340 "draw-button")
  (bind ((win gc o (lock-focus self))
	 (pix (pixels self)))
    (if (< (hilite-state self) 2)
	(begin
	  (draw-bezeled win gc pix $button-bezel (dx o) (dy o)
			(size-width (frame self))
			(size-height (frame self)))
	  (if (> (string-length (title self)) 0)
	      (begin
		(set-gcontext-foreground! gc (vector-ref pix 0))
		(set-gcontext-font! gc (font self))
		(draw-glyphs win gc
			     (+ (dx o) (title-x self))
			     (+ (dy o) (title-y self))
			     (title self)))))
	(begin
	  (draw-bezeled win gc pix $button-bezel-in (dx o) (dy o)
			(size-width (frame self))
			(size-height (frame self)))
	  (if (> (string-length (title self)) 0)
	      (begin
		(set-gcontext-foreground! gc (vector-ref pix 0))
		(set-gcontext-font! gc (font self))
		(draw-glyphs win gc
			     (+ (dx o) (title-x self) 1) ; over and down
			     (+ (dy o) (title-y self) 1)
			     (title self))))))))

;;;

(define-method mouse-enter ((self <button>) (at <point>) (state <fixnum>))
  (if (> (hilite-state self) 0)
      (begin
	(set-hilite-state! self 2)
	(set-state! self #t)
	(update self))))

(define-method mouse-leave ((self <button>) (at <point>) (state <fixnum>))
  (if (> (hilite-state self) 0)
      (begin
	(set-hilite-state! self 1)
	(set-state! self #f)
	(update self))))

(define-method mouse-down ((self <button>) (at <point>) (state <fixnum>))
  (set-hilite-state! self 2)
  (set-state! self #t)
  (update self))

(define-method mouse-up ((self <button>) (at <point>) (state <fixnum>))
  (if (eq? (hilite-state self) 2)
      (dm "button: perform action"))
  (set-hilite-state! self 0)
  (set-state! self #f)
  (update self))

