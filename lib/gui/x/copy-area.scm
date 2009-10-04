
(define (copy-area (source <x-drawable>)
		   (gc <x-gcontext>)
		   (source-x <integer>)
		   (source-y <integer>)
		   (width <integer>)
		   (height <integer>)
		   (dest <x-drawable>)
		   (dest-x <integer>)
		   (dest-y <integer>))
  (internal-send
   (x-display dest)
   (make-buffer u1: 62 ;; CopyArea
		u1: 0
		u2: 7
		u4: (x-id source)
		u4: (x-id dest)
		u4: (x-id gc)
		s2: source-x
		s2: source-y
		s2: dest-x
		s2: dest-y
		u2: width
		u2: height)))
