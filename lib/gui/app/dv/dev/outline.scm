
(define-class <outline-device> (<virtual-device>)
  (all-subpaths* init-function: make-dequeue*))

(define-method all-subpaths ((self <outline-device>))
  (dequeue-state (all-subpaths* self)))

(define-method stroke-subpath ((self <outline-device>) (subpath <vector>))
  (dequeue-push-back! (all-subpaths* self) subpath)
  (values))

(define (with-outline-device proc)
  (proc (make <outline-device>)))

