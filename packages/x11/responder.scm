
(define-class <responder> (<object>)
  (next-responder init-value: #f))


;;
;; this gets called when a view gets clicked on to see
;; if it wants to become first responder
;;
;; by default, responders do not become first responders...
;;

(define-generic-function accepts-first-responder?)

(define-method accepts-first-responder? ((self <responder>))
  #f)

;;
;; this gets sent to the current first responder when someone
;; else wants to become the first responder.  A responder
;; can veto the transfer of power by returning #f
;;

(define-generic-function resign-first-responder)

(define-method resign-first-responder ((self <responder>))
  #t)

;;
;; this gets sent when a responder becomes a window's first
;; responder
;;
;; by default, do nothing

(define-generic-function become-first-responder)

(define-method become-first-responder ((self <responder>))
  (values))
