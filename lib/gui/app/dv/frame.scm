
;;; a frame is an application (top-level) window
;;;
;;; every top-level X window that we create has a frame

(define-class <frame> (<object>)
  (frame-title type: <string>)
  x-window)

(define-class <view-frame> (<frame>)
  open-view)

(define-class <menu-frame> (<frame>))

(define-class <about-frame> (<frame>))

(define-class <inspector-frame> (<frame>))

(define-class <minibuffer-frame> (<frame>))
