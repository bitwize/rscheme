
(define (copy-gcontext (source <x-gcontext>) (destination <x-gcontext>))
  (assert (eq? (x-display source) (x-display destination)))
  (internal-send
   (x-display source)
   (make-buffer u1: 57 ;; CopyGC
		u1: 0
		u2: 4
		u4: (x-id source)
		u4: (x-id destination)
		u4: #x7FFFFF)))
