
(define (query-colors (colormap <x-colormap>) pixels 
		      #key (result-type default: <list>))
  (let* ((n (sequence-length pixels))
	 (r (internal-rpc
	     (x-display colormap)
	     (vector (make-buffer u1: 91 ; QueryColors
				  u1: 0
				  u2: (+ 2 n)
				  u4: (x-id colormap))
		     (vector-map (lambda (c)
				   (make-buffer u4: c))
				 (sequence->vector pixels))))))
    (with-unpacked (common-reply r)
		   (u1: -
		    u1: -
		    u2: -
		    u4: -
		    u2: n)
      (let ((v (make-vector n))
	    ((b <string>) (remainder-reply r)))
	(for-each
	 (lambda (i)
	   (bind ((r g b (unpack* b (* i 8) u2: u2: u2:)))
	     ;; should uniquify for $black and $white
	     (vector-set! v i (make <color>
				    red-component: r
				    green-component: g
				    blue-component: b))))
	 (range n))
	(sequence-as v result-type)))))
