(define-class <graphics-device> (<object>) :abstract
  (properties type: <vector> init-value: '#()))

(define (make-dequeue*)
  (make-dequeue))
  
(define (dequeue-copy deq)
  (with-module primops
    (let ((q (clone deq)))
      (gvec-set! q 0 (clone (gvec-ref q 0)))
      q)))


;(define-constant $Pi (with-module mathlib $Pi))
(define $Deg-to-rad (/ $Pi 180))


(define-method rectstroke ((self <graphics-device>) rect)
  (with-gstate-saved
   self
   (lambda ()
     (moveto self (lower-left rect))
     (lineto self (lower-right rect))
     (lineto self (upper-right rect))
     (lineto self (upper-left rect))
     (closepath self)
     (stroke self))))

(define-method rectfill ((self <graphics-device>) rect)
  (with-gstate-saved
   self
   (lambda ()
     (moveto self (lower-left rect))
     (lineto self (lower-right rect))
     (lineto self (upper-right rect))
     (lineto self (upper-left rect))
     (closepath self)
     (fill self))))

