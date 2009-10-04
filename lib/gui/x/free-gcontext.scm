
(define (free-gcontext (gc <x-gcontext>))
  (internal-send
   (x-display gc)
   (make-buffer u1: 60 ;; FreeGC
		u1: 0
		u2: 2
		u4: (x-id gc))))
