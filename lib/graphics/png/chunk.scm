(define-class <chunk> (<object>)
  type
  data
  crc)

(define-method write-object ((self <chunk>) port)
  (format port "#[<chunk> ~a len ~d]" (type self) (string-length (data self))))

(define (read-u8 b (ix <fixnum>))
  (bvec-ref b ix))

(define $u16/b-packing (make-packing-list '(u16/b:)))
(define $u32/b-packing (make-packing-list '(u32/b:)))

(define (read-u16 (b <string>) (ix <fixnum>))
  (unpack-using $u16/b-packing b ix))

(define (read-u32 (b <string>) (ix <fixnum>))
  (unpack-using $u32/b-packing b ix))

(define $png-signature '(137 80 78 71 13 10 26 10))

(define (read-signature inp)
  (let ((sig (read-string inp 8)))
    (assert (equal? (map char->integer (string->list sig)) $png-signature))
    (values)))

(define (read-chunk inp)
  (let* ((pre (read-string inp 8))
	 (len (read-u32 pre 0))
	 (type (substring pre 4))
	 (data (read-string inp len))
	 (crc (read-string inp 4)))
    (make <chunk>
	  type: type
	  data: data
	  crc: crc)))

;;;

(define-class <png-image> (<pixel-source>)
  (chunks type: <vector>))

;;; these methods are to plug into the <pixel-source> protocol

(define-method image-width ((self <png-image>))
  (get-property self 'width))

(define-method image-height ((self <png-image>))
  (get-property self 'height))

;;; ...
;;; though we don't implement `get-pixel' because it doesn't
;;; map well onto png-images (if necessary, we can make a
;;; generic get-pixel on <pixel-source> that creates a memory
;;; image rep using for-each-pixel and caches that)

(define-method read-png-image ((port <input-port>))
  (let ((q (make-dequeue)))
    (read-signature port)
    (let loop ()
      (let ((ch (read-chunk port)))
	(dequeue-push-back! q ch)
	(if (string=? (type ch) "IEND")
	    (let ((img (make <png-image>
			     chunks: (dequeue-state q))))
	      (parse-IHDR img)
	      img)
	    (loop))))))

(define-method read-png-image ((file <string>))
  (call-with-input-file file read-png-image))

;;;

(define (parse-IHDR png)
  (with-unpack
   (data (vector-ref (chunks png) 0))
   (u32/b: w
    u32/b: h
    u8/b: bits
    u8/b: color-type
    u8/b: compress-meth
    u8/b: filter-meth
    u8/b: interlace-meth)
   (set-property! png 'width w)
   (set-property! png 'height h)
   (set-property! png 'bit-depth bits)
   (set-property! png 'color-type color-type)
   (set-property! png 'compression-method compress-meth)
   (set-property! png 'filter-method filter-meth)
   (set-property! png 'interlace-method interlace-meth)
   (values w h)))
