
(define (extract-data png)
  (let ((o (open-output-string)))
    (vector-for-each
     (lambda (ch)
       (if (string=? (type ch) "IDAT")
	   (write-string o (data ch))))
     (chunks png))
     (uncompress (close-output-port o))))


;;; returns => samples/pixel bits/pixel indexed?  
;;;
;;; --- from the PNG documentation ---
;;; Bit depth is a single-byte integer giving the number of bits per
;;; sample or per palette index (not per pixel). Valid values are 1, 2, 4,
;;; 8, and 16, although not all values are allowed for all color types.
;;; 
;;; Color type is a single-byte integer that describes the interpretation
;;; of the image data. Color type codes represent sums of the following
;;; values: 1 (palette used), 2 (color used), and 4 (alpha channel used).
;;; Valid values are 0, 2, 3, 4, and 6.
;;; 
;;; Bit depth restrictions for each color type are imposed to simplify
;;; implementations and to prohibit combinations that do not compress
;;; well. Decoders must support all valid combinations of bit depth and
;;; color type. The allowed combinations are:
;;; 
;;;    Color    Allowed    Interpretation
;;;    Type    Bit Depths
;;;    
;;;    0       1,2,4,8,16  Each pixel is a grayscale sample.
;;;    
;;;    2       8,16        Each pixel is an R,G,B triple.
;;;    
;;;    3       1,2,4,8     Each pixel is a palette index;
;;;                        a PLTE chunk must appear.
;;;    
;;;    4       8,16        Each pixel is a grayscale sample,
;;;                        followed by an alpha sample.
;;;    
;;;    6       8,16        Each pixel is an R,G,B triple,
;;;                        followed by an alpha sample.
;;; 
;;; The sample depth is the same as the bit depth except in the case of
;;; color type 3, in which the sample depth is always 8 bits.

(define (bit-metrics png)
  (let ((ctype (get-property png 'color-type))
	(bits (get-property png 'bit-depth)))
    (if (eq? ctype 3)
	(values 1 bits #t)              ; palette index
	(let ((spp (case ctype
		     ((0) 1)            ; gray-scale
		     ((4) 2)            ; gray-scale + alpha
		     ((2) 3)            ; rgb
		     ((6) 4)            ; rgb + alpha
		     (else
		      (error "png: invalid color type ~s" ctype)))))
	  (values spp (* bits spp) #f)))))

(define (for-each-scanline png proc)
  (bind ((h (get-property png 'height))
	 (w (get-property png 'width))
	 (spp bits/pix (bit-metrics png))
	 (bytes/pix (max 1 (quotient bits/pix 8)))
	 (scanline-bytes (quotient (+ (* bits/pix w) 7) 8))
	 (d (extract-data png))
         (getpix #f))
    ;;
    (assert (= (get-property png 'filter-method) 0))
    ;;
    (format #t "bit-metrics ~s spp, ~s bits/pix, ~s bytes/scanline\n" 
            spp bits/pix scanline-bytes)
    (set! getpix
          (case (+ (* 10 bits/pix) spp)
            ((11) ; 1
             (lambda (scan x)
               (if (eq? (bitwise-and (logical-shift-right
                                      (read-u8 scan (quotient x 8))
                                      (- 7 (modulo x 8)))
                                     #b1)
                        0)
                   $white
                   $black)))
            ((81) ; 8
             (lambda (scan x)
               (let ((c (* 257 (read-u8 scan x))))
                 (make <pixel>
                       red-component: c
                       green-component: c
                       blue-component: c
                       alpha-component: #xFFFF))))
            ((243) ; 888
             (lambda (scan x)
               (make <pixel>
                     red-component: (* 257 (read-u8 scan x))
                     green-component: (* 257 (read-u8 scan (+ x 1)))
                     blue-component: (* 257 (read-u8 scan (+ x 2)))
                     alpha-component: #xFFFF)))
            ((324) ; 8888
             (lambda (scan x)
               (make <pixel>
                     red-component: (* 257 (read-u8 scan x))
                     green-component: (* 257 (read-u8 scan (+ x 1)))
                     blue-component: (* 257 (read-u8 scan (+ x 2)))
                     alpha-component: (* 257 (read-u8 scan (+ x 3))))))
            (else
             (error "unhandled spp = ~d, bytes/pix = ~d" spp bytes/pix))))
    ;;
    (let loop ((i 0)
	       (k 0)
	       (prev #f))
      (if (< i h)
          ;; the first byte of each scanline chunk is the filter type.
	  (let* ((filt-type (bvec-ref d k))
		 (line (unfilter filt-type
                                 d
                                 (+ k 1)
                                 scanline-bytes
                                 bytes/pix
                                 prev)))
            ;(print line)
	    ;(format #t "[~d] " filt-type)
	    (proc i bytes/pix line getpix)
	    (loop (+ i 1)
		  (+ k scanline-bytes 1)
		  line))))))

;;; calls the given `proc' for each pixel in scanline (y-major) order
;;; the proc is called with three arguments:
;;;   `x'  x coordinate (fastest-varying)
;;;   `y'  y coordinate
;;;   `c'  <pixel> value

(define-method for-each-pixel ((png <png-image>) proc)
  (let ((w (get-property png 'width))
	(Bps (quotient (* 4 (get-property png 'bit-depth)) 8)))
    (format #t "width = ~s\n" w)
    (format #t "bit-depth = ~s Bps = ~s\n" 
            (get-property png 'bit-depth)
            Bps)
    (for-each-scanline
     png
     (lambda (y Bps scanline getpix)
       (let loop ((i 0)
		  (k 0))
	 (if (< i w)
	     (begin
	       (proc i y (getpix scanline k))
	       (loop (+ i 1) (+ k Bps)))))))))

