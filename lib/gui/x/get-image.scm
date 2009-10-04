
;;; how do we determine the bpp of the returned data?

(define (get-image (drawable <x-drawable>)
		   #key x
			y
			width
			height
			(format default: 'z-format)
			(result-type default: (case format
						((z-format) 'image-z)
						((xy-format) 'image-xy)))
			(plane-mask default: 0))
  (let* ((fmt (case format
		((xy-format) 1) ; XYPixmap
		((z-format) 2)  ; ZPixmap
		(else
		 (em 834 "unknown format `~s'\n   use one of (xy-format z-format)" 
		     format))))
	 (r (internal-rpc
	     (x-display drawable)
	     (make-buffer u1: 73   ; GetImage
			  u1: fmt
			  u2: 5
			  u4: (x-id drawable)
			  s2: x
			  s2: y
			  u2: width
			  u2: height
			  u4: plane-mask))))
    (with-unpacked
     (common-reply r)
     (u1: -     ; 1=reply
      u1: depth
      u2: -     ; sequence number
      u4: len
      u4: vis-id)
     (let ((data (remainder-reply r)))
       (case result-type
	 ((image-z)
	  (make <z-image>
	    image-red-mask: #f
	    image-green-mask: #f
	    image-blue-mask: #f
	    image-width: width
	    image-height: height
	    image-depth: depth
	    image-plist: '()
	    image-x-hot: #f
	    image-y-hot: #f
	    image-z-bits-per-pixel: 8
	    image-z-pixarray: data))
	 (else
	  (em 835 "result-type `~s' not supported" result-type)))))))	    
