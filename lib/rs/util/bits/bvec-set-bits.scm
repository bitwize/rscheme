

;;; `bit-offset' and `bit-width' are interpreted in PowerPC notation,
;;; which is that the bits are numbered in lexical order, ie, if you
;;; write down the bytes in base 2, then numbered in that order.
;;;
;;; (ie, the MSB of byte 0 is bit 0, the LSB of byte 0 is bit 7,
;;;  the MSB is byte 1 is bit 8)
;;; (to get into byte major/bit minor order, just XOR the bit # with 7)

(define (bvec-set-bits! dest
			(bit-offset <fixnum>)
			(bit-width <fixnum>)
			(int-value <fixnum>))
  (let* ((first-byte-boundary (quotient (+ bit-offset 7) 8))
	 (last-byte-boundary (quotient (+ bit-offset bit-width) 8))
	 (pre-w (- (* first-byte-boundary 8) bit-offset))
	 (post-w (- (+ bit-offset bit-width) (* last-byte-boundary 8)))
	 (mid-w (- last-byte-boundary first-byte-boundary)))
    ;;
    (define (value-extract shift mask)
      (bitwise-and (logical-shift-right int-value shift) mask))
    ;;
    ;(dm "fbb ~d, lbb ~d\n" first-byte-boundary last-byte-boundary)
    ;(dm "pre-w ~d, mid-w ~d*8, post-w ~d\n" pre-w mid-w post-w)
    ;;
    (if (< mid-w 0)
	;;------------------------------------------------------------
	;; mid-byte-bits only
	(let* ((mbb (bitwise-and (right-mask pre-w) (left-mask post-w))))
#|
	  (dm "mid[~d]: ~a"
	      last-byte-boundary
	      (format #f "mid[~~d]: ~~0~db\n" bit-width)
	      int-value)
	  (dm "mbb: ~08b\n" mbb)
	  (dm "old: ~08b\nnew: ~08b\n===> ~08b\n"
		  (bvec-ref dest last-byte-boundary)
		  (bitwise-and mbb (logical-shift-left int-value (- 8 post-w)))
		  (bitwise-or
		   (bitwise-and mbb (logical-shift-left int-value (- 8 post-w)))
		   (bitwise-and (bitwise-xor mbb #xFF)
				(bvec-ref dest last-byte-boundary))))
|#
	  (bvec-set! dest
		     last-byte-boundary
		     (bitwise-or
		      (bitwise-and mbb (logical-shift-left int-value (- 8 post-w)))
		      (bitwise-and (bitwise-xor mbb #xFF)
				   (bvec-ref dest last-byte-boundary))))
	  (values))
	;;------------------------------------------------------------
	;; no mid-byte bits
	(begin
	  ;;
	  ;; do the part before the first whole byte (if any)
	  ;;
	  (if (> pre-w 0)
	      (let ((v (value-extract (- bit-width pre-w) (right-mask pre-w))))
		#|(dm (format #f "pre[~~d]: ~~0~db\n" pre-w)
			(sub1 first-byte-boundary)
			v)|#
		(bvec-set! dest 
			   (sub1 first-byte-boundary)
			   (bitwise-or
			    v
			    (bitwise-and (left-mask pre-w)
					 (bvec-ref dest first-byte-boundary))))))
	  ;;
	  ;; do the whole bytes in the middle (if any)
	  ;;
	  (let loop ((cnt mid-w)
		     (sh (- bit-width pre-w 8))
		     (i first-byte-boundary))
	    (if (> cnt 0)
		(let ((v (value-extract sh #xFF)))
		  ;(dm "fill[~d]: ~08b\n" i v)
		  (bvec-set! dest i v)
		  (loop (- cnt 1) (- sh 8) (+ i 1)))))
	  ;;
	  ;; do the part after the last whole byte (if any)
	  ;;
	  (if (and (> post-w 0) (>= mid-w 0))
	      (let ((v (value-extract 0 (right-mask post-w))))
		#|(dm (format #f "post[~~d]: ~~0~db\n" post-w)
			last-byte-boundary
			v)|#
		(bvec-set! dest
			   last-byte-boundary
			   (bitwise-or
			    v
			    (bitwise-and (right-mask post-w)
					 (bvec-ref dest last-byte-boundary))))))
	  (values)))))

(define (right-mask k)
  (vector-ref '#(#b00000000
		 #b00000001
		 #b00000011
		 #b00000111
		 #b00001111
		 #b00011111
		 #b00111111
		 #b01111111
		 #b11111111)
	      k))

(define (left-mask k)
  (vector-ref '#(#b00000000
		 #b10000000
		 #b11000000
		 #b11100000
		 #b11110000
		 #b11111000
		 #b11111100
		 #b11111110
		 #b11111111)
	      k))
#|
(define (t)
  (let ((a (bvec-alloc <byte-vector> 16)))
    (for-each (lambda (i) (bvec-set! a i 0) (values)) (range 16))
    (bvec-set-bits! a 13 13 #b1010101010101)
    (bvec-set-bits! a 39 18 #b110011110011110011)
    (bvec-set-bits! a 41 4 #b1100)
    (print a)))

|#