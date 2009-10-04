
(define-class <pixel-array> (<object>)
  image-width
  image-height
  image-depth
  image-bits-per-pixel
  image-scanline-pad
  image-bytes-per-scanline
  pixel-data)

(define-class <pixel-data> (<object>) :bvec)
(define-class <u8-pixel-data> (<pixel-data>))
(define-class <u24-pixel-data> (<pixel-data>))
(define-class <u32-pixel-data> (<pixel-data>))

(define (make-pixel-array #key width height depth bpp pad)
  (let ((bpscan (quotient (+ (* width bpp) pad -1) 8)))
    (dm "bytes per scanline: ~d" bpscan)
    (make <pixel-array>
	  image-width: width
	  image-height: height
	  image-depth: depth
	  image-bpp: bpp
	  image-scanline-pad: pad
	  image-bytes-per-scanline: bpscan
	  pixel-data: (bvec-alloc
		       (case bpp
			 ((8) <u8-pixel-data>)
			 ((24) <u24-pixel-data>)
			 ((32) <u32-pixel-data>)
			 (else <pixel-data>))
		       (* height bpscan)))))
