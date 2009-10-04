
(define-class <view> (<responder>)
    ;;
    ;; window is the <window> object that we belong to
    ;;
    (window init-value: #f)
    ;;
    ;; an optional drawing-context
    ;;
    (private-context init-value: #f)
    ;;
    ;; automatic behavior, if any, when parent resizes
    ;;  bits 0,1,2:  left, width, right
    ;;  bits 3,4,5:  top, height, bottom
    ;; 0=no automatic behavior (equivalent to 001001)
    ;;
    (auto-resize-mode init-value: 0)
    ;;
    ;; parent is the <view> which contains us,
    ;; #f ONLY for the content-view of a <window>
    ;;
    parent
    ;;
    ;;---------------- view geometry --------------
    ;;
    ;; the area occupied by the view, in our parent coordinates
    ;;
    frame
    ;;
    ;; the window-frame, clipped to our parent's clip-rect
    ;;
    (clip-rect init-value: '#uninit)
    ;;
    ;; the location of this view's (0,0) in window coordinates
    ;;
    (origin init-value: '#uninit)
    ;;
    ;; the area occupied by this view, in it's own coordinate
    ;; system
    ;;
    (bounds init-value: '#uninit))

#|
    view geometry:

	bounds.size = frame.size
    
	window-frame.origin = frame.origin + parent.origin
	window-frame.size = frame.size
	
    	origin = window-frame.origin - bounds.origin
	
	clip-rect = intersect( window-frame, parent.clip-rect )
	
	
|#

(define-generic-function setup-view)
(define-generic-function mouse-down)
(define-generic-function draw-view)
(define-generic-function display-view)

(define-class <group> (<view>)
  (children init-value: '()))

(define-method children ((self <view>))
    '())

;; these methods mean you don't have to check for
;; a #f receiver beforing sending value messages

(define-method set-value! ((self <boolean>) new-value)
  #f)

(define-method value ((self <boolean>))
  #f)

