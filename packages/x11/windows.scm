
(define-class <window> (<responder>)
  frame
  window-title
  (icon-title init-value: '#uninit)
  (shared-context init-value: '#uninit)
  (content-view init-keyword: #f)
  (first-responder init-value: '#uninit))

(define-class <buffered-window> (<window>)
  (actual-window-gc init-value: '#uninit))

(define-method x-window ((self <window>))
  (x-window (shared-context self)))

(define-method x-window ((self <buffered-window>))
  (x-window (actual-window-gc self)))

(define (init-buffered-window (self <buffered-window>))
  (let* ((actual-win-gc (shared-context self))
	 (actual-x-win (x-window actual-win-gc))
	 (pixm (create-pixmap actual-x-win
			      (rect-size (frame self))
			      (x-default-depth *X-display*) ;;XXX window-depth
			      ))
	 (pixm-gc (create-drawing-context pixm)))
    (set-shared-context! self pixm-gc)
    (set-actual-window-gc! self actual-win-gc)))

(define-syntax kwd 
  (syntax-form (k lst)
    (let ((m (memq (mquote k) lst)))
      (if m
	  (cadr m)
	  (error "required keyword `~s' not specified" (mquote k)))))
  (syntax-form (k lst dflt)
    (let ((m (memq (mquote k) lst)))
      (if m
	  (cadr m)
	  dflt))))

(define-method initialize ((self <window>) #rest init-kwds)
  (let* ((xw (create-simple-window 
	      *X-display*
	      (x-display-default-root-window *X-display*)
	      (frame self)
	      0
	      *std-window-background*
	      *std-window-foreground*))
	 (xwgc (create-drawing-context xw)))
    (set-first-responder! self self)
    (let ((b (make-rect 0 0 
			(size-width (frame self))
			(size-height (frame self)))))
      (set-content-view! self
			 (make <group>
			       window: self
			       parent: #f
			       frame: b
			       clip-rect: b
			       origin: $zero-point
			       bounds: b)))
    (set-shared-context! self xwgc)
    (if (instance? self <buffered-window>)
	(init-buffered-window self))
    (if (eq? (icon-title self) '#uninit)
	(set-icon-title! self (window-title self)))
    (set-local-object! xw self)
    (select-input xw (X-event-mask ButtonPress KeyPress Exposure))
    (set-std-properties xw
			(window-title self)
			(icon-title self)
			(frame self)
			#t #t)
    self))

(define (order-front (w <window>))
  (if (instance? w <buffered-window>)
      (display-window w))
  (map-raised (x-window w)))

(define (set-window-event-mask! (w <window>) mask)
  (select-input (x-window w) mask))

(define (display-window (self <window>))
  (let ((ctx (shared-context self)))
    (set-background ctx *std-window-background*)
    (set-foreground ctx *std-window-background*)
    (fill-rectangle ctx (bounds (content-view self)))
    (set-foreground ctx *std-window-foreground*)
    (display-view (content-view self))))

(define-method Expose ((self <window>) (event <X-event>))
  (display-window self))

(define-method Expose ((self <buffered-window>) (event <X-event>))
  (let (((area <rect>) (frame self)))
    (copy-area (x-window (shared-context self))
	       (make-rect 0 0
			  (size-width area)
			  (size-height area))
	       (actual-window-gc self)
	       $zero-point)))

(define (flush-graphics (self <window>))
  (if (instance? self <buffered-window>)
      (let (((area <rect>) (frame self)))
	(copy-area (x-window (shared-context self))
		   (make-rect 0 0
			      (size-width area)
			      (size-height area))
		   (actual-window-gc self)
		   $zero-point))))
  
