
(define-class <view> (<object>) :abstract
  (window type: <window>)
  (in-window type: <x-drawable>)
  (parent)                ;; only #f for a window's content-view
  (frame type: <rect> setter: set-frame*!)
  (subviews type: <vector> init-value: '#())
  (view-flags type: <fixnum>)
  (use-gcontext type: <x-gcontext>))

(define $resize-flags-mask #b00000111111)
(define $own-window-flag   #b00001000000)
(define $own-gcontext-flag #b00010000000)

(define-method resize-flags ((self <view>))
  (bitwise-and (view-flags self) #b111111))

(define-method own-window? ((self <view>))
  (eq? (bitwise-and (view-flags self) $own-window-flag) $own-window-flag))

;;;

(define-method for-client ((self <view>))
  (for-client (window self)))

;;;

(define-class <content-view> (<view>))

(define (make-content-view (w <window>))
  (make <content-view>
    window: w
    frame: (make-rect 0 0 (size-width (frame w)) (size-height (frame w)))
    in-window: (in-window w) ;; we also don't create a new X window
    view-flags: #b001001 ;; this is immutable, because we ARE the window
    parent: #f
    use-gcontext: (create-gcontext
		   foreground: (get-pixel-resource 
				'window.foreground
				(for-client w))
		   drawable: (in-window w))))

;;;

(define (make-view #rest all
		   #key (frame type: <rect>)
		   (parent type: <view>)
		   (background default: 'window.background)
		   (class type: <<class>> #| default: <view>|#)
		   (wants-gcontext default: #f)
		   (wants-button-motion default: #f)
		   (resize-flags default: #b100100)
		   (wants-enter-and-leave default: #f))
  (define (remaining-kwds)
    (let loop ((r '())
	       (s all))
      (if (null? s)
	  (reverse! r)
	  (if (memq (car s) '(frame: 
			      class:
			      background:
			      parent: 
			      wants-gcontext:
			      resize-flags:
			      wants-enter-and-leave:
			      wants-button-motion:))
	      (loop r (cddr s))
	      (loop (cons* (cadr s) (car s) r) (cddr s))))))
  ;
  (assert (subclass? class <view>))
  ;
  (let* ((c (for-client (window parent)))
	 (event-mask (append
		      (if wants-enter-and-leave 
			  '(enter-window leave-window) 
			  '())
		      (if wants-button-motion
			  '(button-motion)
			  '())
		      '(exposure
			button-press
			button-release
                        key-press
                        key-release)))
	 (own-window? (not (instance? (window parent) <offscreen-window>)))
	 (w (if own-window?
		(create-window 
		 parent: (in-window parent)
		 x: (+ (origin-x frame) (x (content-origin parent)))
		 y: (+ (origin-y frame) (y (content-origin parent)))
		 width: (size-width frame)
		 height: (size-height frame)
		 event-mask: event-mask
		 background: (get-pixel-resource background c))
		#f))
	 (gc (if wants-gcontext
		 (create-gcontext
		  foreground: (get-pixel-resource 
			       'window.foreground
			       c)
		  drawable: (or w (in-window parent)))
		 (use-gcontext parent)))
	 (v (with-client c ;; initializer/init-fns may access current-client
			 (lambda ()
			   (apply make-instance
				  class
				  in-window: (or w (in-window parent))
				  window: (window parent)
				  parent: parent
				  use-gcontext: gc
				  frame: frame
				  view-flags: (+ resize-flags
						 (if own-window?
						     $own-window-flag
						     0)
						 (if wants-gcontext
						     $own-gcontext-flag
						     0))
				  (remaining-kwds))))))
    (if w
	(set-property! w 'rstep-view v))
    (set-subviews! parent (vector-append (vector v) (subviews parent)))
    (if w
	(map-window w))
    v))

(define (lock-focus (self <view>))
  (if (own-window? self)
      (values (in-window self)
	      (use-gcontext self)
	      $zero-size)
      (if (parent self)
	  (bind ((win gc delta (lock-focus (parent self))))
	    (values
	     win
	     gc
	     (size+ delta (point->size (origin (frame self))))))
	  (values (in-window self)
		  (use-gcontext self)
		  $zero-size))))

		  
	  
;;;

(define-method did-resize ((self <view>) old-size)
  (let ((dw (- (size-width (frame self)) (width old-size)))
	(dh (- (size-height (frame self)) (height old-size))))
    (dm 301 "did-resize ~s: ~s x ~s" self dw dh)
    (vector-for-each
     (lambda (sub)
       (parent-did-resize sub dw dh))
     (subviews self))))

(define-method content-origin ((self <view>))
  $zero-point)

(define-method set-frame! ((self <view>) (new-frame <rect>))
  (let ((old-size (size (frame self))))
    (set-frame*! self new-frame)
    (let (((o <point>) (content-origin (parent self))))
      (set-drawable-frame! (in-window self)
			   (+ (x o) (origin-x new-frame))
			   (+ (y o) (origin-y new-frame))
			   (size-width new-frame)
			   (size-height new-frame))
      (if (not (equal? (size new-frame) old-size))
	  (did-resize self old-size)))))

(define-method parent-did-resize ((self <view>) dw dh)
  (bind ((f (frame self))
	 (new-x new-w (adj-axis (origin-x f)
				(size-width f)
				(resize-flags self)
				dw))
	 (new-y new-h (adj-axis (origin-y f)
				(size-height f) 
				(logical-shift-right (resize-flags self) 3)
				dh))
	 (old-size (size f)))
    (set-frame*! self (make-rect new-x new-y new-w new-h))
    (let (((o <point>) (content-origin (parent self))))
      (set-drawable-frame! (in-window self) 
			   (+ new-x (x o))
			   (+ new-y (y o))
			   new-w
			   new-h))
    (did-resize self old-size)))

(define-method mouse-moved ((self <view>) (at <point>) (state <fixnum>))
  (values))

(define-method mouse-down ((self <view>) (at <point>) (state <fixnum>))
  (values))

(define-method mouse-up ((self <view>) (at <point>) (state <fixnum>))
  (values))

(define-method key-down ((self <view>) (key <fixnum>) (state <fixnum>))
  (values))

(define-method key-up ((self <view>) (key <fixnum>) (state <fixnum>))
  (values))

(define-method mouse-enter ((self <view>) (at <point>) (state <fixnum>))
  (values))

(define-method mouse-leave ((self <view>) (at <point>) (state <fixnum>))
  (values))

(define-method draw-self ((self <view>) rects)
  (values))

(define-method use-colormap ((self <view>))
  (use-colormap (for-client self)))

(define-method draw ((self <view>) rects)
  (if (own-window? self)
      (begin
	(clear-area (in-window self))
	(draw-self self rects))
      (bind ((win gc sh (lock-focus self)))
	(dm "draw: ~s ~s" self sh)
	(set-gcontext-foreground! 
	 gc
	 (get-pixel-resource 'window.background (for-client self)))
	(draw-rectangle win gc 
			(dx sh) 
			(dy sh) 
			(size-width (frame self))
			(size-height (frame self))
			#t)
	(draw-self self rects)
	(vector-for-each
	 (lambda (sub)
	   (draw sub rects))
	 (subviews self)))))

(define (adj-axis x w flags dw)
  (case (bitwise-and flags #b111)
    ((#b000 #b100)
     (values x w)) ; no change
    ((#b001)
     (values (+ x dw) w))
    ((#b010)
     (values x (+ w dw)))
    ((#b011)
     (let ((dw1 (quotient dw 2))
	   (dw2 (quotient (+ dw 1) 2)))
       (values (+ x dw1) (+ w dw2))))
    ((#b110)
     (let ((dw2 (quotient (+ dw 1) 2)))
       (values x (+ w dw2))))
    ((#b111)
     (let ((dw1 (quotient dw 3))
	   (dw2 (quotient (+ dw 1) 3)))
       (values (+ x dw1) (+ w dw2))))))

;;;

(define (update (self <view>))
  (let* ((c (for-client (window self)))
	 (t (pending-updates c)))
    ;; nb, there is a miss window here -- we could extract the table just
    ;; before it is overwritten by the main event loop
    (table-insert! t self #t)
    ;; if we are not running in the event loop thread, give
    ;; the event loop a kick
    (if (and (not (eq? (current-thread) 
		       (event-loop-thread c)))
	     (eq? (table-size t) 1))
	(queue-event (on-display c) 'kick))))

;;;
