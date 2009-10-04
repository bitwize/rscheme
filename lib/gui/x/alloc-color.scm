
;;; as an extension to CLX, in addition to color names, we
;;; accept standard color specifications in the forms that
;;; `string->color' understands

(define-method alloc-color ((self <x-colormap>) color)
  (if (string? color)
      (let ((c (string->color color)))
	(if c
	    (alloc-specific-color self c)
	    (alloc-named-color self color)))
      (alloc-specific-color self color)))

(define (alloc-named-color (self <x-colormap>) (color <string>))
  (bind ((p pad (pad4 (string-length color)))
	 (r (common-reply
	     (internal-rpc
	      (x-display self)
	      (vector
	       (make-buffer u1: 85 ;; AllocNamedColor
			    u1: 0
			    u2: (+ 3 (quotient (+ (string-length color) p) 4))
			    u4: (x-id self)
			    u2: (string-length color)
			    u2: 0)
	       color
	       pad)))))
    (with-unpacked
     r
     (u1: -
      u1: -
      u2: -
      u4: -
      u4: pixel
      u2: exact-red
      u2: exact-green
      u2: exact-blue
      u2: visual-red
      u2: visual-green
      u2: visual-blue)
     (values pixel
	     (make <rgb-color>
		   red-component: visual-red
		   green-component: visual-green
		   blue-component: visual-blue)
	     (make <rgb-color>
		   red-component: exact-red
		   green-component: exact-green
		   blue-component: exact-blue)))))


(define (alloc-specific-color (self <x-colormap>) (color <color>))
  (bind ((R G B (color-rgb-components color))
         (r (common-reply
             (internal-rpc
              (x-display self)
              (make-buffer u1: 84 ;; AllocColor
                           u1: 0
                           u2: 4
                           u4: (x-id self)
                           u2: R
                           u2: G
                           u2: B
                           u2: 0)))))
    (with-unpacked
     r
     (u1: -
      u1: -
      u2: -
      u4: -
      u2: visual-red
      u2: visual-green
      u2: visual-blue
      u2: -
      u4: pixel)
     (values pixel
	     (make <rgb-color>
		   red-component: visual-red
		   green-component: visual-green
		   blue-component: visual-blue)
	     color))))

;;;

(define (alloc-color-cells (self <x-colormap>) (colors <fixnum>)
			   #key (contiguous? default: #f)
			        (result-type default: <list>)
				(planes default: 0))
  (let ((r (internal-rpc
	    (x-display self)
	    (make-buffer u1: 86 ;; AllocColorCells
			 u1: (if contiguous? 1 0)
			 u2: 3
			 u4: (x-id self)
			 u2: colors
			 u2: planes))))
    (with-unpacked
     (common-reply r)
     (u1: -
      u1: -
      u2: -
      u4: reply-len
      u2: n
      u2: -
      u4: red-mask
      u4: green-mask
      u4: blue-mask)
     (let* ((src (open-input-string (remainder-reply r)))
	    (pixels (unpack-list-of 
		     n
		     (lambda ()
		       (with-unpacked-from-input-string
			src (u4: pix) 
			pix)))))
       (values
	(if (eq? result-type <vector>)
	    pixels
	    (vector->list pixels))
	'())))))

;; color string not implemented yet... need lookup-color for that

(define (store-color (self <x-colormap>)
		     (pixel <fixnum>)
		     (color <color>))
  (bind ((R G B (color-rgb-components color)))
    (internal-send
     (x-display self)
     (make-buffer u1: 89 ; StoreColors
                  u1: 0
                  u2: (+ 2 (* 3 1))
                  u4: (x-id self)
                  u4: pixel
                  u2: R
                  u2: G
                  u2: B
                  u1: #b111
                  u1: 0))))

(define (free-colors (self <x-colormap>) (pixels <list>))
  (let* ((v (list->vector pixels))
	 (n (vector-length v)))
    (internal-send
     (x-display self)
     (vector
      (make-buffer u1: 87 ; FreeColors
		   u1: 0
		   u2: (+ 3 n)
		   u4: (x-id self)
		   u4: 0)
      (vector-map u32->string v)))))

