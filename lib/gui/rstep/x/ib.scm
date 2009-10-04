
;;;
;;;  An RStep Interface Builder
;;;  --------------------------
;;;

(define-class <interface-builder> (<object>)
  (interfaces init-value: '()))

(define-class <interface> (<object>)
  (target-windows init-value: '()))

(define (current-ib)
  (get-property (current-client) 'interface-builder))

(define-class <meta-view> (<view>)
  instantiate-proc
  (prototype-pixmap init-value: #f))

(define-class <view-prototype> (<meta-view>)
  title)

;;; these occur in target windows

(define-class <instance-top> (<view>)
  (selection type: <vector> init-value: '#()))

(define-class <view-instance> (<meta-view>))

;;;

(define (get-prototype-pixmap (self <meta-view>))
  (or (prototype-pixmap self)
      (let* ((s (size (frame self)))
	     (w (make-offscreen-window size: s)))
	((instantiate-proc self) (content-view w)
	                         (make-rect 0 0 (width s) (height s)))
	(draw (content-view w) '())
	(set-prototype-pixmap! self (in-window w))
	(in-window w))))

(define-method draw-self ((self <meta-view>) rects)
  (bind ((win gc o (lock-focus self))
	 (px (get-prototype-pixmap self))
	 (w (size-width (frame self)))
	 (h (size-height (frame self))))
    (copy-area px gc 0 0 w h win (dx o) (dy o))))

(define $handle '(1 top left 0 bottom right 1 middle))

(define-method draw-self ((self <instance-top>) rects)
  (bind ((win gc o (lock-focus self))
	 (pix (get-pixels-resource 'button.shades (for-client self)))
	 (dx dy (size->values o)))
   (dm "instance-top sel: ~s" (selection self))
   (vector-for-each
    (lambda ((s <view>))
      (bind ((x y w h (rect->values (frame s))))
	(dm " handles at ~s" (frame s))
        (draw-bezeled win gc pix $handle (+ x dx -2) (+ y dy -2) 5 5)
        (draw-bezeled win gc pix $handle (+ x w dx -2) (+ y dy -2) 5 5)
        (draw-bezeled win gc pix $handle (+ x dx -2) (+ y h dy -2) 5 5)
        (draw-bezeled win gc pix $handle (+ x w dx -2) (+ y h dy -2) 5 5)))
    (selection self))))

(define-method renew-selection ((self <view>) new)
  (renew-selection (parent self) new))

(define-method renew-selection ((self <instance-top>) new)
  (set-selection! self (vector new))
  (update self))

(define-method container ((self <view>))
  (container (parent self)))

(define-method container ((self <instance-top>))
  self)

(define-method container ((self <box>))
  self)

(define-method mouse-down ((self <view-instance>) at state)
  (bind ((initial-frame (frame self))
	 (x0 y0 (translate-coordinates 
		 (in-window self) (x at) (y at)
		 (screen-root (on-screen (for-client self))))))
    ;;
    (define (adjer win-at state (at <point>))
      (bind ((x1 y1 (point->values at)))
	(set-frame! self (make-rect (+ (origin-x initial-frame)
				       (- x1 x0))
				    (+ (origin-y initial-frame)
				       (- y1 y0))
				    (size-width initial-frame)
				    (size-height initial-frame)))))
    ;;
    (track-mouse self
		 mouse-moved: adjer
		 mouse-up: adjer)
    (renew-selection self self)))

;;;

(define-method mouse-down ((self <view-prototype>) at state)
  ;;
  (define (drop-proc in-view at)
    (dm "drop in: ~s at: ~s" in-view at)
    (let* ((c (container in-view))
	   (v (make-view class: <view-instance>
			 prototype-pixmap: (prototype-pixmap self)
			 instantiate-proc: (instantiate-proc self)
			 parent: c
			 wants-button-motion: #t
			 frame: (make-rect (x at) (y at)
					   (size-width (frame self))
					   (size-height (frame self))))))
      (renew-selection c v)
      v))
  ;;
  (let ((v (drag-and-drop owner: self
			  pixmap: (prototype-pixmap self)
			  client: (for-client self)
			  size: (size (frame self))
			  starting-at: at
			  valid-targets: (apply 
					  append
					  (map target-windows
					       (interfaces (current-ib))))
			  drop-proc: drop-proc)))
    (print v)
    (values)))

;;;----------------------------------------------------------------------

(define (make-kit-window)
  (let ((w (make-window title: "Components"
			frame: (make-rect 100 50 200 200))))
    (make-view class: <view-prototype>
	       title: "<color-picker>"
	       wants-button-motion: #t
	       instantiate-proc: (lambda (parent frame)
				   (make-color-picker frame: frame
						      parent: parent))
	       frame: (make-rect 10 10 80 100)
	       parent: (content-view w))
    (make-view class: <view-prototype>
	       title: "<button>"
	       wants-button-motion: #t
	       instantiate-proc: (lambda (parent frame)
				   (make-button frame: frame
						parent: parent
						title: "Button"))
	       frame: (make-rect 10 120 50 21)
	       parent: (content-view w))))

(define (new-interface-window (self <interface>))
  (let ((w (make-window
	    frame: (make-rect 200 50 300 300)
	    title: "Untitled Interface")))
    (make-view class: <instance-top>
	       frame: (make-rect 0 0 300 300)
	       parent: (content-view w)
	       resize-flags: #b100100)
    (set-target-windows! self (cons w (target-windows self)))
    (set-property! w 'interface-owner self)
    w))

(define (open-new-interface)
  (let ((i (make <interface>))
	(ib (current-ib)))
    (set-interfaces! ib (cons i (interfaces ib)))
    (new-interface-window i)
    i))

(define (ib)
  (set-property! (current-client)
		 'interface-builder
		 (make <interface-builder>))
  (make-kit-window)
  (open-new-interface))
