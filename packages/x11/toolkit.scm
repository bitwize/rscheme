
;; the toolkit restricts operation to a single display
;; the glue layer should not have that restriction

(define *X-display* #f)

(define *X-root-window* #f)

(define (X-init)
  (if (not *X-display*)
      (real-X-init))
  (values))

(define (real-X-init)
  (let ((d (open-display (getenv "DISPLAY"))))
    (if (not d)
	(error "X-init: could not open display `~a'\n" 
	       (getenv "DISPLAY")))
    (set! *X-display* d)
    (set! *X-root-window* (make <X-window>
				x-display-ptr: (x-display-ptr *X-display*)
				x-display: *X-display*
				xid: (x-display-default-root-window 
				      *X-display*)
				local-object: #f))
    (values)))


