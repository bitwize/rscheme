
(define (set-input-focus (self <x-display>) focus revert-to #optional time)
  (internal-send
   self
   (make-buffer u1: 42 ; SetInputFocus
		u1: (case revert-to
		      ((none) 0)
		      ((pointer-root) 1)
		      ((parent) 2)
		      (else (em 504 "set-input-focus: invalid revert-to `~a'"
				revert-to)))
		u2: 3
		u4: (case focus
		      ((none) 0)
		      ((pointer-root) 1)
		      (else (x-id focus)))
		u4: (or time 0))))
	      

(define (input-focus (self <x-display>))
  (with-unpacked (common-reply
		  (internal-rpc self
				(make-buffer u1: 43
					     u1: 0
					     u2: 1)))
   (u1: -
    u1: revert-to
    u2: -
    u4: -
    u4: focus-window)
   (values (case focus-window
	     ((0) 'none)
	     ((1) 'pointer-root)
	     ;; need to translate XID into corresponding <x-window>
	     (else focus-window))
	   (case revert-to
	     ((0) 'none)
	     ((1) 'pointer-root)
	     ((2) 'parent)))))
