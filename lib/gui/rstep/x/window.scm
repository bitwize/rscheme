
(define-class <window> (<object>)
  (properties type: <vector> init-value: '#())
  (in-window type: <x-drawable>)
  (for-client type: <client>)
  (title type: <string>)
  (content-view)
  (frame type: <rect>))

(define-method initialize ((self <window>))
  (let (((c <client>) (for-client self)))
    (set-all-windows! c (cons self (all-windows c)))))

(define-class <offscreen-window> (<window>))

(define (make-offscreen-window #key (size type: <size>)
			            (client default: (current-client)))
  (dm 103 "make-offscreen-window: ~s" size)
  (let* ((w (create-pixmap
	     width: (width size)
	     height: (height size)
	     depth: (screen-root-depth (on-screen client))
	     drawable: (screen-root (on-screen client))))
	 (o (make <offscreen-window>
	      for-client: client
	      title: ""
	      frame: (make-rect 0 0 (width size) (height size))
	      in-window: w
	      content-view: #f)))
    (set-content-view! o (make-content-view o))
    (set-property! w 'rstep-view (content-view o))
    o))

(define (make-window #key (frame type: <rect>)
		          (title default: "Untitled")
			  (client default: (current-client)))
  (dm 103 "make-window: ~s ~s" frame title)
  (let* ((w (create-window 
	     parent: (screen-root (on-screen client))
	     x: (origin-x frame)
	     y: (origin-y frame)
	     width: (size-width frame)
	     height: (size-height frame)
	     event-mask: '(structure-notify
			   focus-change)
	     background: (get-pixel-resource 'window.background client)))
	 (o (make <window>
	      for-client: client
	      title: title
	      frame: frame
	      in-window: w
	      content-view: #f)))
    (set-content-view! o (make-content-view o))
    (change-property w "WM_NAME" title "STRING" 8)
    (set-property! w 'rstep-window o)
    (set-property! w 'rstep-view (content-view o))
    (map-window w)
    o))

;;;

(define-method did-become-focus ((self <window>))
  (dm 302 "focus in: ~s" self))

(define-method did-resign-focus ((self <window>))
  (dm 303 "focus out: ~s" self))

(define-method did-resize ((self <window>) old-size)
  (dm 301 "did-resize from ~s to ~s" old-size (size (frame self)))
  (set-frame*! (content-view self)
	       (make-rect 0 0 (width (frame self)) (height (frame self))))
  (did-resize (content-view self) old-size))

(define-method do-configure-notify ((self <window>) new-frame above)
  (let ((old-frame (frame self)))
    (set-frame! self new-frame)
    ;;
    (if (not (equal? (size old-frame)
		     (size new-frame)))
	(did-resize self (size old-frame))
	(dm 302 "did-configure from ~s to ~s" old-frame (frame self)))))

;;;

(define-method equal? ((a <rect>) b)
  (and (instance? b <rect>)
       (bind ((x1 y1 w1 h1 (rect->values a))
	      (x2 y2 w2 h2 (rect->values b)))
	 (and (= x1 x2)
	      (= y1 y2)
	      (= w1 w2)
	      (= h1 h2)))))

(define-method equal? ((a <size>) b)
  (and (instance? b <size>)
       (bind ((w1 h1 (size->values a))
	      (w2 h2 (size->values b)))
	 (and (= w1 w2)
	      (= h1 h2)))))
