
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-class <tiff-image> (<object>)
  (tiff-file type: <string>)
  (tiff-subimages type: <vector>)
  (tiff-fd type: <fixnum>)
  (reader-procs type: <vector>))

;;;

(define-class <tiff-subimage> (<object>)
  (owner type: <tiff-image>)
  (properties type: <vector>)
  (subimage-number type: <fixnum> init-value: 0))

(define (number-of-subimages (self <tiff-image>))
  (vector-length (tiff-subimages self)))

(define (get-tiff-subimage (self <tiff-image>) (index <fixnum>))
  (vector-ref (tiff-subimages self) index))

;;;

(define-method image-width ((self <tiff-subimage>))
  (get-tag-scalar (owner self) (subimage-number self) 'image-width))

(define-method image-height ((self <tiff-subimage>))
  (get-tag-scalar (owner self) (subimage-number self) 'image-length))

(define (tiff-image-size (self <tiff-image>) #optional (subimage default: 0))
  (values (get-tag-scalar self subimage 'image-width)
	  (get-tag-scalar self subimage 'image-length)))

;;;

(define (read-bytes fd (len <fixnum>) #optional offset)
  ;; use `bvec-alloc' instead of `make-string' because
  ;; we don't want to bother initializing the byte array
  (let ((s (bvec-alloc <string> (add1 len))))
    (bvec-set! s len 0)
    (if offset
	(fd-lseek fd offset 0))
    (let ((n (fd-read fd s 0 len)))
      (if (not (eq? len n))
	  (error "read-bytes: read only ~s instead of ~s" n len))
      s)))

;;;

(define (tiff-read-short-ii (b <string>) (ix <fixnum>))
  (fixnum+ 
   (bvec-ref b ix)
   (logical-shift-left (bvec-ref b (add1 ix)) 8)))

(define (tiff-read-long-ii (b <string>) (ix <fixnum>))
  (+ (bvec-ref b ix)
     (logical-shift-left (bvec-ref b (add1 ix)) 8)
     (logical-shift-left (bvec-ref b (fixnum+ ix 2)) 16)
     (logical-shift-left (bvec-ref b (fixnum+ ix 3)) 24)))

(define (tiff-read-rat-ii (b <string>) (ix <fixnum>))
  (make-rational (tiff-read-long-ii b ix)
		 (tiff-read-long-ii b (+ ix 4))))

;;;

(define (tiff-read-short-mm (b <string>) (ix <fixnum>))
  (fixnum+ 
   (bvec-ref b (add1 ix))
   (logical-shift-left (bvec-ref b ix) 8)))

(define (tiff-read-long-mm (b <string>) (ix <fixnum>))
  (+ (bvec-ref b (fixnum+ ix 3))
     (logical-shift-left (bvec-ref b (fixnum+ ix 2)) 8)
     (logical-shift-left (bvec-ref b (add1 ix)) 16)
     (logical-shift-left (bvec-ref b ix) 24)))

(define (tiff-read-rat-mm (b <string>) (ix <fixnum>))
  (make-rational (tiff-read-long-mm b ix)
		 (tiff-read-long-mm b (+ ix 4))))

;;;

(define (tiff-read-byte* bvec offset)
  (bvec-ref bvec offset))

(define *big-endian-readers*
  (vector tiff-read-short-mm
	  tiff-read-long-mm
	  tiff-read-byte*
	  tiff-read-rat-mm))

(define *little-endian-readers*
  (vector tiff-read-short-ii
	  tiff-read-long-ii
	  tiff-read-byte*
	  tiff-read-rat-ii))

(define (tiff-read-short bvec (offset <fixnum>) reader-proc-vec)
  ((vector-ref reader-proc-vec 0) bvec offset))
	  
(define (tiff-read-long bvec (offset <fixnum>) reader-proc-vec)
  ((vector-ref reader-proc-vec 1) bvec offset))
	  
(define (read-tiff-headers file fd (img <tiff-image>))
  (let (((hdr <string>) (read-bytes fd 8)))
    (let* ((e (case (bvec-read-unsigned-16 hdr 0)
		((#x4949) *little-endian-readers*)
		((#x4D4D) *big-endian-readers*)))
	   (magic (tiff-read-short hdr 2 e)))
      (if (eq? magic 42)
	  (let ((ifds (read-tiff-ifds img (tiff-read-long hdr 4 e) e)))
	    (let loop ((i 0)
		       (l ifds))
	      (if (null? l)
		  (values (list->vector ifds) e)
		  (begin
		    (set-subimage-number! (car l) i)
		    (loop (+ i 1) (cdr l))))))
	  (begin
	    (fd-close fd)
	    (error (make <not-a-tif-file>
			 file: file)))))))

(define (read-tiff-ifds (img <tiff-image>) offset rdrs)
  (if (eq? offset 0)
      '()
      (bind ((ifd nxt (read-tiff-ifd img offset rdrs)))
	(cons ifd (read-tiff-ifds img nxt rdrs)))))

(define (read-tiff-ifd (self <tiff-image>) offset rdrs)
  (let* ((file (tiff-file self))
         (fd (tiff-fd self))
         (n (tiff-read-short (read-bytes fd 2 offset) 0 rdrs))
	 (ifd (read-bytes fd (+ (* n 12) 4)))
	 (tags (make-dequeue)))
    (let loop ((i 0))
      (if (< i n)
	  (bind ((k v (read-tag ifd (* i 12) rdrs)))
	    (if k
		(begin
		  (dequeue-push-back! tags k)
		  (dequeue-push-back! tags v)))
	    (loop (+ i 1)))
	  (values (make <tiff-subimage>
                        owner: self
			properties: (dequeue-state tags))
		  (tiff-read-long ifd (* n 12) rdrs))))))

;;;

(define (read-tag ifd offset rdrs)
  (let* ((tag (tag->name (tiff-read-short ifd offset rdrs)))
	 (type (type-id->type (tiff-read-short ifd (+ 2 offset) rdrs)))
	 (count (tiff-read-long ifd (+ 4 offset) rdrs)))
    (if type
	(if (<= (* (element-size type) count) 4)
	    (values tag (cons type (extract-array ifd
						  (+ 8 offset) 
						  type
						  count
						  rdrs)))
	    (values tag (list type 
			      count
			      (tiff-read-long ifd (+ 8 offset) rdrs))))
	(values))))

(define (extract-array (buf <string>)
		       (offset <fixnum>) 
		       (type <tiff-type>) 
		       (count <fixnum>) 
		       rdrs)
  (let ((a (make-vector count))
	(rdr (vector-ref rdrs (reader-index type))))
    (for-each 
     (lambda (i)
       (vector-set! a i (rdr buf (+ offset (* i (element-size type))))))
     (range count))
    a))

(define (get-tag-scalar (self <tiff-image>) (k <fixnum>) tag)
  (vector-ref (get-tag-array self k tag) 0))

(define (get-tag-array (self <tiff-image>) (k <fixnum>) tag)
  (let ((info (get-property (vector-ref (tiff-subimages self) k) tag #f)))
    (if (pair? info)
	(ensure-tiff-data self info)
	(error (make <no-such-tag>
		     tiff-file: (tiff-file self)
		     tiff-subimage: k
		     tiff-tag: tag)))))

;;;

(define (ensure-tiff-data (img <tiff-image>) (info <pair>))
  (if (vector? (cdr info))
      (cdr info)
      (let* ((type (car info))
	     (count (cadr info))
	     (offset (caddr info))
	     (buf (read-bytes (tiff-fd img)
			      (* (element-size type) count)
			      offset)))
	(set-cdr! info (extract-array buf 0 type count (reader-procs img)))
	(cdr info))))

(define (load-into-heap (img <tiff-image>))
  (vector-for-each
   (lambda ((si <tiff-subimage>))
     (for-each
      (lambda (i)
	(ensure-tiff-data img (vector-ref (properties si) (+ (* i 2) 1))))
      (range (quotient (vector-length (properties si)) 2)))
     ;
     (let ((k (subimage-number si)))
       (set-property! si 
		      '*strips*
		      (list->vector
		       (map (strip-data-getter img k)
			    (range (num-strips img k)))))))
   (tiff-subimages img))
  (fd-close (tiff-fd img))
  (set-tiff-fd! img -1)
  (set-reader-procs! img '#())
  img)

;;;

(define (open-tiff-image file)
  (bind ((fd (or (fd-open (with-module paths (relative-file file))
		          (make-fd-open-mode 'read) 0)
	         (error "~a: cannot open" file)))
         (img (make <tiff-image>
                    tiff-file: file
                    tiff-fd: fd
                    reader-procs: '#()
                    tiff-subimages: '#()))
	 (ifds rdrs (read-tiff-headers file fd img)))
    (set-tiff-subimages! img ifds)
    (set-reader-procs! img rdrs)
    img))

;;;
