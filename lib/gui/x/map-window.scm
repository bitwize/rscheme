
(define (map-window (self <x-window>))
  (internal-send
   (x-display self)
   (make-buffer u1: 8
		u1: 0
		u2: 2
		u4: (x-id self))))
