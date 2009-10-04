
(define (write-png-image port (from <pixel-source>))
  (let ((image-data (open-output-string)))
    ;
    (for-each-pixel
     from
     (lambda (x y c)
       (if (eq? x 0)
	   (write-char #\nul image-data)) ;; scanline filter -- plain copy
       (bind ((r g b a (pixel-rgba c)))
	 (write-string image-data
		       (pack-string
			u8: (logical-shift-right r 8)
			u8: (logical-shift-right g 8)
			u8: (logical-shift-right b 8)
			u8: (logical-shift-right a 8))))))
    ;
    (write-png-image*
     (make <png-image>
       chunks: (vector
		(make <chunk>
		  type: "IHDR"
		  data: (pack-string
			 u32/b: (image-width from)
			 u32/b: (image-height from)
			 u8/b: 8 ; bits
			 u8/b: 6 ; color type (RGBA)
			 u8/b: 0 ; compress meth
			 u8/b: 0 ; filter meth
			 u8/b: 0); interlace meth
		  crc: #f)
		(make <chunk>
		  type: "IDAT"
		  data: (compress (close-output-port image-data) 
				  level: 5)
		  crc: #f)
		(make <chunk>
		  type: "IEND"
		  data: ""
		  crc: #f)))
     port)))

(define-method size ((self <byte-vector>))
  (bvec-length self))

(define-method write-bvec ((self <output-port>) elem
			   #optional (offset default: 0)
			             (length default: (size elem)))
  ;; yuck... buffer copy city
  (let ((t (bvec-alloc <string> (+ length 1))))
    (bvec-copy t 0 elem offset length)
    (write-string self t)))

(define (write-png-chunk (ch <chunk>) port)
  (if (not (crc ch))
      (set-crc! ch (compute-crc-string (type ch) (data ch))))
  (write-bvec port (pack u32/b: (size (data ch))))
  (write-bvec port (type ch))
  (write-bvec port (data ch))
  (write-bvec port (crc ch)))

(define (write-png-image* png port)
  (write-string port (list->string (map integer->char $png-signature)))
  (vector-for-each
   (lambda (ch)
     (write-png-chunk ch port))
   (chunks png))
  png)