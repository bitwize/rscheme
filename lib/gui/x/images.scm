
(define-class <image> (<object>) :abstract
  (properties type: <vector> init-value: '#())
  (image-name type: <string> init-value: "")
  ;
  image-red-mask 
  image-green-mask
  image-blue-mask
  ;
  image-width
  image-height
  image-depth
  ;
  image-plist
  image-x-hot
  image-y-hot)

(define-class <bit-image> (<image>)
  image-bitarray
  (image-bytes-per-scanline type: <fixnum>))

(define-class <xy-image> (<image>)
  image-xy-bitmap-list)

(define-class <z-image> (<image>)
  image-z-bits-per-pixel
  image-z-pixarray)

(define-method image-data-size ((self <z-image>))
  (quotient (* (image-z-bits-per-pixel self)
	       (vector-length (image-z-pixarray self))
	       (vector-length (vector-ref (image-z-pixarray self) 0)))))

(define-method image-format ((self <z-image>))
  2)

(define-method image-format ((self <xy-image>))
  1)

(define-method image-format ((self <bit-image>))
  0)

(define (create-image #key (bit-lsb-first? default: #f)
		           bits-per-pixel
			   (bytes-per-line default: #f)
			   (byte-lsb-first? default: #f)
			   data
			   depth
			   (format default: 'z-pixmap)
			   (plist default: '())
			   (red-mask default: #xFF0000)
			   (green-mask default: #x00FF00)
			   (blue-mask default: #x0000FF)
			   height
			   width
			   (x-hot default: #f)
			   (y-hot default: #f))
  (case format
    ((z-pixmap)
     (if (not (vector? data))
         (em 711 "create-image: expected vector array of pixels for z-pixmap, not a ~s"
             (object-class data)))
     ;; it's a z-image
     (make <z-image>
	   image-z-bits-per-pixel: bits-per-pixel
	   image-z-pixarray: data
	   image-x-hot: x-hot
	   image-y-hot: y-hot
	   image-width: width
	   image-height: height
	   image-depth: depth
	   image-plist: plist
	   image-red-mask: red-mask
	   image-green-mask: green-mask
	   image-blue-mask: blue-mask))
    ((xy-pixmap)
      ;; it's an xy-image
      (error "create-image: xy-image not supported"))
    ((bitmap)
      ;; it's a bitmap
     (if (not (instance? data <byte-vector>))
         (em 710 "create-image: expected <byte-vector> for bitmap pixel data, not a ~s" 
             (object-class data)))
     (if (not (= depth 1))
         (em 712 "create-image: expected depth of 1 for bitmap, not ~s" depth))
     (make <bit-image>
           image-bitarray: data
           image-red-mask: red-mask
           image-green-mask: green-mask
           image-blue-mask: blue-mask
           image-width: width
           image-height: height
           image-depth: 1
           image-plist: plist
           image-x-hot: x-hot
           image-y-hot: y-hot
           image-bytes-per-scanline: bytes-per-line))
    (else
     (error "create-image: unknown format `~s'" format))))
	    
(define (put-image (drawable <x-drawable>)
		   (gcontext <x-gcontext>)
		   (image <image>)
		   #key (src-x default: 0)
		        (src-y default: 0)
			x
			y
			(width default: (image-width image))
			(height default: (image-height image))
			(bitmap? default: #f))
  (bind ((dat n (image-data image
			    (image-pixmap-format image (x-display drawable))
			    src-x src-y width height
			    (display-image-lsb-first? (x-display drawable))))
	 (pad-n pad-str (pad4 n)))
    ;(print dat)
    (internal-send
     (x-display drawable)
     (vector (make-buffer u1: 72 ; PutImage
			  u1: (image-format image)
			  u2: (+ 6 (quotient (+ n pad-n) 4))
			  u4: (x-id drawable)
			  u4: (x-id gcontext)
			  u2: width
			  u2: height
			  s2: x ;; dst-x
			  s2: y ;; dst-y
			  u1: 0 ;; left-pad (??)
			  u1: (image-depth image)
			  u2: 0)
	     dat
	     pad-str))))

;;;

(define-method image-bits-per-pixel ((self <z-image>))
  (image-z-bits-per-pixel self))

(define-method image-bits-per-pixel ((self <xy-image>))
  (length (image-xy-bitmap-list self)))

(define-method image-bits-per-pixel ((self <bit-image>))
  1)

(define (image-pixmap-format (self <image>) (dpy <x-display>))
  (let ((d (image-depth self))
	(bpp (image-bits-per-pixel self)))
    (let loop ((lst (display-pixmap-formats dpy)))
      (if (null? lst)
	  (em 503 "no compatible pixmap format for ~s (depth ~d, ~d bpp)" 
	      self
	      d
	      bpp)
	  (if (and (= (depth (car lst)) (image-depth self))
		   (= (bits-per-pixel (car lst)) bpp))
	      (car lst)
	      (loop (cdr lst)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Automatic code generation for manipulating pixmaps etc
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; supports numbits in (1 2 4)

(define (gen-bit-munger bits-per-pixel num-pixels mode)
  (fix-expr
   (gen-bit-munger* bits-per-pixel 
		    num-pixels
		    (case mode
		      ((lsb-first)
		       (lambda (k)
			 (* k bits-per-pixel)))
		      ((msb-first)
		       (lambda (k)
			 (- *word-size* (* (+ k 1) bits-per-pixel))))))))


(define (gen-bit-munger* bits-per-pixel num-pixels compute-shift)
  (let ((mask (- (logical-shift-left 1 bits-per-pixel) 1)))
    (let loop ((k 0)
	       (height (case bits-per-pixel
			 ((8) 0)
			 ((4) 1)
			 ((2) 2)
			 ((1) 3))))
      (if (< k num-pixels)
	  (if (= height 0)
	      `(logical-shift-left 
		(bitwise-and 
		 (vector-ref row (fixnum+ s ,k))
		 ,mask)
		,(compute-shift k))
	      `(fixnum+
		,(loop k (- height 1))
		,(loop (+ k (logical-shift-left 1 (- height 1))) (- height 1))))
	  0))))

(define *word-size* 8)

(define (fix-expr e)
  (if (pair? e)
      (case (car e)
	((fixnum+)
	 (case (caddr e)
	   ((0) (fix-expr (cadr e)))
	   ((1) (list 'add1 (fix-expr (cadr e))))
	   (else (map fix-expr e))))
	((logical-shift-left)
	 (if (eq? (caddr e) 0)
	     (fix-expr (cadr e))
	     (map fix-expr e)))
	(else
	 (map fix-expr e)))
      e))

(define (gen-pixarray-row-copier bits-per-pixel mode)
  (let ((pixels-per-word (quotient *word-size* bits-per-pixel)))
    `(let loop (((s <fixnum>) x)
		((d <fixnum>) ix)
		((left <fixnum>) w))
       (if (fixnum<? left ,pixels-per-word)
	   (copy-rmdr dest d row s)
	   (begin
	     (bvec-set! dest d ,(gen-bit-munger bits-per-pixel 
						pixels-per-word
						mode))
	     (loop (fixnum+ s ,pixels-per-word) 
		   (add1 d) 
		   (fixnum- left ,pixels-per-word)))))))

(define (gen-pixarray-rmdr-row-copier bits-per-pixel mode n)
  `(begin
     (bvec-set! dest d ,(gen-bit-munger bits-per-pixel n mode))
     (values)))

(define (gen-pixarray-copier-vector mode bpp pixels-per-word)
  `(vector
    (lambda ',(symbol-append "copy-whole-" bpp) 
             ((dest <byte-vector>)
	      (ix <fixnum>)
	      (row <vector>)
	      (x <fixnum>)
	      (w <fixnum>)
	      copy-rmdr)
      ,(gen-pixarray-row-copier bpp mode))
    ,@(map (lambda (n)
	     (if (= n 0)
		 `(lambda 'nop ((dest <byte-vector>)
				(d <fixnum>)
				(row <vector>)
				(s <fixnum>))
			  (values))
		 `(lambda ',(symbol-append "copy-" n "-of-" pixels-per-word)
		    ((dest <byte-vector>)
		     (d <fixnum>)
		     (row <vector>)
		     (s <fixnum>))
		    ,(gen-pixarray-rmdr-row-copier bpp mode n))))
	   (range pixels-per-word))))

(define-macro (define-pixarray-row-copiers name mode)
  `(define ,name 
     (vector #f
	     ,(gen-pixarray-copier-vector mode 1 8)
	     ,(gen-pixarray-copier-vector mode 2 4)
	     #f
	     ,(gen-pixarray-copier-vector mode 4 2))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   image data and data copiers
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (cpy-pxa-row-24-bit-l (dest <byte-vector>)
			      (ix <fixnum>)
			      (row <vector>)
			      (x <fixnum>)
			      (w <fixnum>)
			      cpy-rmdr-bits)
  (let ((lim (fixnum+ x w)))
    (let loop (((s <fixnum>) x)
	       ((d <fixnum>) ix))
      (if (eq? s lim)
	  (values)
	  (let (((v <fixnum>) (vector-ref row s)))
	    (bvec-set! dest d (bitwise-and v #xFF))
	    (let (((d <fixnum>) (add1 d)))
	      (bvec-set! dest d (bitwise-and (logical-shift-right v 8) #xFF))
	      (let (((d <fixnum>) (add1 d)))
		(bvec-set! dest d (bitwise-and (logical-shift-right v 16) #xFF))
		(loop (add1 s) (add1 d)))))))))

(define (cpy-pxa-row-32-bit-l (dest <byte-vector>)
			      (ix <fixnum>)
			      (row <vector>)
			      (x <fixnum>)
			      (w <fixnum>)
			      cpy-rmdr-bits)
  (let ((lim (fixnum+ x w)))
    (let loop (((s <fixnum>) x)
	       ((d <fixnum>) ix))
      (if (eq? s lim)
	  (values)
	  (let (((v <fixnum>) (vector-ref row s)))
	    (bvec-set! dest d (bitwise-and v #xFF))
	    (let (((d <fixnum>) (add1 d)))
	      (bvec-set! dest d (bitwise-and (logical-shift-right v 8) #xFF))
	      (let (((d <fixnum>) (add1 d)))
		(bvec-set! dest d (bitwise-and (logical-shift-right v 16) #xFF))
		(let (((d <fixnum>) (add1 d)))
		  (bvec-set! dest d 0)
		  (loop (add1 s) (add1 d))))))))))

(define (cpy-pxa-row-32-bit-m (dest <byte-vector>)
			      (ix <fixnum>)
			      (row <vector>)
			      (x <fixnum>)
			      (w <fixnum>)
			      cpy-rmdr-bits)
  (let ((lim (fixnum+ x w)))
    (let loop (((s <fixnum>) x)
	       ((d <fixnum>) ix))
      (if (eq? s lim)
	  (values)
	  (let (((v <fixnum>) (vector-ref row s)))
	    (bvec-set! dest d 0)
	    (let (((d <fixnum>) (add1 d)))
	      (bvec-set! dest d (bitwise-and (logical-shift-right v 16)  #xFF))
	      (let (((d <fixnum>) (add1 d)))
		(bvec-set! dest d (bitwise-and (logical-shift-right v 8) #xFF))
		(let (((d <fixnum>) (add1 d)))
		  (bvec-set! dest d (bitwise-and v #xFF))
		  (loop (add1 s) (add1 d))))))))))

(define (cpy-pxa-row-8-bit (dest <byte-vector>)
			   (ix <fixnum>)
			   (row <vector>)
			   (x <fixnum>)
			   (w <fixnum>)
			   cpy-rmdr-bits)
  (let ((lim (fixnum+ x w)))
    (let loop (((s <fixnum>) x)
	       ((d <fixnum>) ix))
      (if (eq? s lim)
	  (values)
	  (begin
	    (bvec-set! dest d (vector-ref row s))
	    (loop (add1 s) (add1 d)))))))

;;;

(define-pixarray-row-copiers *bitwise-lsb-copiers* lsb-first)
(define-pixarray-row-copiers *bitwise-msb-copiers* msb-first)

;;; the key in these vectors is `bits-per-pixel/8'

(define *bytewise-lsb-copiers* (vector #f
				       cpy-pxa-row-8-bit
				       #f ; 16 bit not impl
				       cpy-pxa-row-24-bit-l
				       cpy-pxa-row-32-bit-l))

(define *bytewise-msb-copiers* (vector #f
				       cpy-pxa-row-8-bit
				       #f ; 16 bit not impl
				       #f ; cpy-pxa-row-24-bit-m not impl
				       cpy-pxa-row-32-bit-m)) ; 32 bit not impl


(define (get-data-copiers (fmt <x-pixmap-format>) w lsb-first?)
  (let (((bpp <fixnum>) (bits-per-pixel fmt)))
    (if (< bpp 8)
	(let (((x <vector>) (vector-ref (if lsb-first? 
					    *bitwise-lsb-copiers*
					    *bitwise-msb-copiers*)
					bpp)))
	  (values (vector-ref x 0)
		  (vector-ref x (add1 (modulo w bpp)))))
	(values (vector-ref (if lsb-first?
				*bytewise-lsb-copiers*
				*bytewise-msb-copiers*)
			    (quotient bpp 8)) #f))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Image data access function
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-method image-data ((self <z-image>)
			   (fmt <x-pixmap-format>)
			   src-x src-y w h
			   lsb-first?)
  (bind ((bytes-per-row (* (quotient (+ (* (bits-per-pixel fmt) w) 
					(scanline-pad fmt)
					-1)
				     (scanline-pad fmt))
			   (quotient (scanline-pad fmt) 8)))
	 (src-data (image-z-pixarray self))
	 (arry (bvec-alloc <byte-vector> (* bytes-per-row h)))
	 (cpy-whole-bytes cpy-rmdr-bits (get-data-copiers fmt w lsb-first?)))
    (if (not cpy-whole-bytes)
	(em 804 "image-data: copier not implemented (depth ~d, ~d bpp, pad ~d, ~s ~s)" 
	    (depth fmt)
	    (bits-per-pixel fmt)
	    (scanline-pad fmt)
	    w 
	    lsb-first?))
    (let y-loop ((y 0))
      (if (< y h)
	  (begin
	    (apply* arry (* bytes-per-row y)
		    (vector-ref src-data (+ y src-y))
		    src-x w cpy-rmdr-bits '() cpy-whole-bytes)
	    (y-loop (+ y 1)))
	  (values arry (bvec-length arry))))))

;;;

(define-method image-data ((self <bit-image>)
                           (fmt <x-pixmap-format>)
                           src-x src-y w h
                           lsb-first?)
  (bind ((bytes-per-row (* (quotient (+ (* (bits-per-pixel fmt) w) 
					(scanline-pad fmt)
					-1)
				     (scanline-pad fmt))
			   (quotient (scanline-pad fmt) 8)))
	 (src-data (image-bitarray self))
	 (arry (bvec-alloc <byte-vector> (* bytes-per-row h))))
    (let loop ((y 0))
      (if (< y h)
          (let ((src-offset (+ (* (image-bytes-per-scanline self) (+ src-y y))
                               (quotient src-x 8)))
                (dst-offset (* y bytes-per-row)))
            (if (= (modulo src-x 8) 0)
                (bvec-copy arry dst-offset
                           src-data src-offset
                           (image-bytes-per-scanline self))
                (em 981 "unsupported <bit-image> copy, x ~d mod 8 != 0" src-x))
            (loop (+ y 1)))
          (values arry (bvec-length arry))))))
