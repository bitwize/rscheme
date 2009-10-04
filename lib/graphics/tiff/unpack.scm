
(define (num-strips img k)
  (vector-length (get-tag-array img k 'strip-offsets)))

(define (strip-data-getter img k)
  (let (((si <tiff-subimage>) (vector-ref (tiff-subimages img) k)))
    (if (get-property si '*strips* #f)
	; if the property *strips* is defined, then the image content
	; has already been loaded into the heap (as by `load-into-heap')
	(let (((strips <vector>) (get-property si '*strips*)))
	  (lambda (strip)
	    (vector-ref strips strip)))
	; otherwise, we're expected to read it off the disk on the fly
	(let (((strip-bytes <vector>) (get-tag-array img k
						     'strip-byte-counts))
	      ((strip-offsets <vector>) (get-tag-array img k 
						       'strip-offsets)))
	  (lambda (strip)
	    (read-bytes (tiff-fd img)
			(vector-ref strip-bytes strip)
			(vector-ref strip-offsets strip)))))))

(define (for-each-row (img <tiff-image>) k proc)
  (let* ((get-strip (strip-data-getter img k))
	 (num-strips (num-strips img k))
	 (image-length (get-tag-scalar img k 'image-length))
	 (rows-per-strip (get-tag-scalar img k 'rows-per-strip))
	 (bits-per-pixel (apply + (vector->list
				   (get-tag-array img k 'bits-per-sample))))
	 (bytes-per-row (quotient (* bits-per-pixel
				     (get-tag-scalar img k 'image-width))
				  8))
	 (row-buffer (make-string bytes-per-row))
	 (uncompressor (case (get-tag-scalar img k 'compression)
			 ((0 1) (null-uncompressor row-buffer))
			 ((#x8005) (packbits-uncompressor row-buffer))
			 (else
			  (error "unsupported compression scheme: #x~x"
				 (get-tag-scalar img k 'compression))))))
    (let loop ((strip 0)
	       (row 0))
      (if (< strip num-strips)
	  (let ((strip-data (get-strip strip))
		(row-limit (min image-length (+ row rows-per-strip))))
            ;(format #t "strip ~d ~@#*60s\n" strip (substring strip-data 100))
	    (let row-loop ((row row)
			   (data-offset 0)
			   (n 0))
	      (if (< row row-limit)
		  (let ((x2 (uncompressor strip-data data-offset)))
                    ;(format #t "  row ~d (at ~d .. ~d)\n" row data-offset x2)
		    (proc row row-buffer)
		    (row-loop (+ row 1) x2 (+ n 1)))
		  (loop (+ strip 1) row))))
	  (values)))))

(define (null-uncompressor (buf <string>))
  (lambda ((data <string>) offset)
    (bvec-copy buf 0 data offset (string-length buf))
    (+ offset (string-length buf))))

(define (packbits-uncompressor (buf <string>))
  (lambda ((data <string>) offset)
    (let loop ((s offset)
	       (d 0))
      ;; Loop until we get the number of unpacked bytes we are expecting
      (if (< d (string-length buf))
	  ;; Read the next source byte into `n'
	  (let ((n (bvec-read-signed-8 data s)))
	    (cond
	     ;; If n is between 0 and 127, incl..
	     ((>= n 0)
	      ;; Copy the next n+1 bytes literally
	      (let ((m (+ n 1)))
		(bvec-copy buf d data (+ s 1) m)
		(loop (+ s 1 m) (+ d m))))
	     ;; else if n is between -127 and -1, incl.,
	     ((>= n -127)
	      ;; Copy the next byte -n+1 times
	      (let ((nxt (bvec-ref data (+ s 1)))
		    (m (- 1 n)))
		(let copy-loop ((i 0)
				(d d))
		  (if (< i m)
		      (begin
			(bvec-set! buf d nxt)
			(copy-loop (+ i 1) (+ d 1)))
		      (loop (+ s 2) d)))))
	     ;; else, noop
	     (else (loop (+ s 1) d))))
	  s))))

;;;

(define-method get-pixel-row ((self <tiff-image>) k y)
  (let* ((row-data (get-row self k y))
         (bits-per-sample (get-tag-array self k 'bits-per-sample))
         ((w <fixnum>) (get-tag-scalar self k 'image-width))
         (row (make-vector w)))
    (cond
     ((equal? bits-per-sample '#(4 4 4 4))
      (let loop (((i <fixnum>) 0)
                 ((j <fixnum>) 0))
        (if (eq? i w)
            row
            (let ((hi (bvec-ref row-data j))
                  (lo (bvec-ref row-data (add1 j))))
              (vector-set! row
                           i
                           (rgba8->pixel 
                            (fixnum* 17 (logical-shift-right hi 4))
                            (fixnum* 17 (bitwise-and hi #x0F))
                            (fixnum* 17 (logical-shift-right lo 4))
                            (fixnum* 17 (bitwise-and lo #x0F))))
              (loop (add1 i) (fixnum+ j 2))))))
     (else
      'unknown-sample-format))))

(define-method get-pixel ((self <tiff-image>) k x y)
  (let ((row (get-row self k y))
        (bits-per-sample (get-tag-array self k 'bits-per-sample)))
    (cond
     ((equal? bits-per-sample '#(4 4 4 4))
      (let* ((ix (* x 2))
             (hi (bvec-ref row ix))
             (lo (bvec-ref row (+ ix 1))))
        (rgba8->pixel (* 17 (logical-shift-right hi 4))
                      (* 17 (bitwise-and hi #x0F))
                      (* 17 (logical-shift-right lo 4))
                      (* 17 (bitwise-and lo #x0F)))))
     (else
      'unknown-sample-format))))

(define-method get-row ((self <tiff-image>) k y)
  (let* ((get-strip (strip-data-getter self k))
	 (num-strips (num-strips self k))
	 (image-length (get-tag-scalar self k 'image-length))
	 (rows-per-strip (get-tag-scalar self k 'rows-per-strip))
	 (bits-per-pixel (apply + (vector->list
				   (get-tag-array self k 'bits-per-sample))))
	 (bytes-per-row (quotient (* bits-per-pixel
				     (get-tag-scalar self k 'image-width))
				  8))
	 (row-buffer (make-string bytes-per-row))
         (uncompressor (case (get-tag-scalar self k 'compression)
			 ((0 1) (null-uncompressor row-buffer))
			 ((#x8005) (packbits-uncompressor row-buffer))
			 (else
			  (error "unsupported compression scheme: #x~x"
				 (get-tag-scalar self k 'compression)))))
         (strip-index (quotient y rows-per-strip))
         (strip-data (get-strip strip-index)))
    (let loop ((row (* strip-index rows-per-strip))
               (data-offset 0))
      (let ((x2 (uncompressor strip-data data-offset)))
        (if (= row y)
            row-buffer
            (loop (+ row 1) x2))))))

;;

(define-method get-pixel ((self <tiff-subimage>) x y)
  (get-pixel (owner self) 
             (subimage-number self)
             x y))

(define-method for-each-pixel ((self <tiff-subimage>) proc)
  (for-each-pixel* (owner self) 
                   (subimage-number self)
                   proc))


(define-method for-each-pixel ((self <tiff-image>) proc)
  (for-each-pixel* self 0 proc))

(define (for-each-pixel* (self <tiff-image>) k proc)
  (let ((h (get-tag-scalar self k 'image-length))
        ((w <fixnum>) (get-tag-scalar self k 'image-width)))
    ;;
    (define (xiter y (vec <vector>))
      (let loop (((x <fixnum>) 0))
        (if (eq? x w)
            (values)
            (begin
              (proc x y (vector-ref vec x))
              (loop (add1 x))))))
    ;;
    (let loop ((y 0))
      (if (< y h)
          (begin
            (xiter y (get-pixel-row self k y))
            (loop (+ y 1)))))))
