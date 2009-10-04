
(define (create-cursor #key (source type: <x-pixmap>)
		            (mask default: #f)
		            x y
			    (foreground type: <color>)
			    (background type: <color>))
  (bind ((id (alloc-x-id (x-display source)))
         (R1 G1 B1 (color-rgb-components foreground))
         (R0 G0 B0 (color-rgb-components background)))
    (internal-send
     (x-display source)
     (make-buffer u1: 93  ; CreateCursor
		  u1: 0
		  u2: 8
		  u4: id
		  u4: (x-id source)
		  u4: (if mask (x-id mask) 0)
		  u2: R1
		  u2: G1
		  u2: B1
		  u2: R0
		  u2: G0
		  u2: B0
		  u2: x
		  u2: y))
    (make <x-cursor>
	  x-display: (x-display source)
	  x-id: id)))

	  
