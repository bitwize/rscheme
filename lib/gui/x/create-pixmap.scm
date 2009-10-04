
(define (create-pixmap #key (width type: <fixnum>)
		            (height type: <fixnum>)
			    (depth type: <fixnum>)
		            (drawable type: <x-drawable>))
  (let ((id (alloc-x-id (x-display drawable))))
    (internal-send
     (x-display drawable)
     (make-buffer u1: 53 ;; CreatePixmap
		  u1: depth
		  u2: 4
		  u4: id
		  u4: (x-id drawable)
		  u2: width
		  u2: height))
    (let ((px (make <x-pixmap>
		    x-display: (x-display drawable)
		    x-id: id
		    drawable-root: (drawable-root drawable))))
      (table-insert! (xid-table (x-display drawable)) id px)
      px)))
