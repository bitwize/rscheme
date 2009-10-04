
(define-class <pnm-image> (<object>)
  (image-width type: <fixnum>)
  (image-height type: <fixnum>))

(define-class <pnm-mem-image> (<pnm-image>)
  (filedes type: <fixnum>)
  (offset type: <fixnum>)
  (data type: <string>)
  (dirty? type: <boolean> init-value: #f))

(define-class <p6-image> (<pnm-mem-image>))
(define-class <p5-image> (<pnm-mem-image>))

(define-class <pgm-header-in-port> (<buffered-input-port>)
  (byte-offset init-value: 0)
  underlying-input-port)

(define (make-pgm-header-port fd)
  (make <pgm-header-in-port>
        underlying-input-port: (make <fd-input-port>
                                     file-descriptor: fd)))

(define-method more-input-ready? ((self <pgm-header-in-port>))
  #t)

(define-method provide-more-input ((self <pgm-header-in-port>))
  (let ((l (read-line (underlying-input-port self))))
    (set-byte-offset! self (+ (byte-offset self) 1 (string-length l)))
    (if (and (> (string-length l) 0)
	     (char=? (string-ref l 0) #\#))
	(provide-more-input self)
	(string-append l "\n"))))

(define (read-pgm-header fd)
  (let* ((i (make-pgm-header-port fd))
         (v (read i)))
    (if (not (memq v '(P5 P6)))
        (error "Unsupported PNM format: ~s" v))
    (let* ((w (read i))
	   (h (read i))
	   (max (read i)))
      (values v w h max (byte-offset i)))))

(define (open-input-pnm-file (file <string>))
  (bind ((fd (fd-open file (make-fd-open-mode 'read) 0))
	 (type w h maxv at (read-pgm-header fd))
         (n (case type
              ((P5) (* w h))
              ((P6) (* 3 w h))))
	 (data (bvec-alloc <string> (+ 1 n))))
    (fd-lseek fd at 0)
    (let ((m (fd-read fd data 0 n)))
      (format #t "~a: ~d x ~d ... ~s (~d bytes)\n" file w h m n))
    (case type
      ((P5) (make <p5-image>
                  filedes: fd
                  image-width: w
                  image-height: h
                  offset: at
                  data: data))
      ((P6) (make <p6-image>
                  filedes: fd
                  image-width: w
                  image-height: h
                  offset: at
                  data: data)))))

(define open-input-pgm-file open-input-pnm-file) ; backwards compat; old name


(define-method get-pixel ((self <p5-image>) y x)
  (bvec-ref (data self) (+ x (* y (image-width self)))))

(define-method get-pixel ((self <p6-image>) y x)
  (let (((k <fixnum>) (* 3 (+ x (* y (image-width self)))))
        ((d <string>) (data self)))
    (rgba8->pixel (bvec-ref d k)
                  (bvec-ref d (fixnum+ k 1))
                  (bvec-ref d (fixnum+ k 2))
                  #xFF)))

;;;

(define (open-output-pgm-file (file <string>) #key 
			      width
			      height
			      (maxval default: 255))
  ;;
  (let ((h (format #f "P5\n~d ~d\n~d\n" width height maxval))
	(fd (fd-open file (make-fd-open-mode 'write 'create) #o666)))
    (fd-write fd h 0 (string-length h))
    (make <p5-image>
          filedes: fd
          image-width: width
          image-height: height
          offset: (string-length h)
          dirty?: #t
          data: (bvec-alloc <string> (+ 1 (* width height))))))

(define-method close-image ((self <p5-image>))
  (let ((fd (filedes self)))
    (if (dirty? self)
	(begin
	  (fd-lseek fd (offset self) 0)
	  (let ((n (fd-write fd (data self) 0 (string-length (data self)))))
	    (assert (eq? n (string-length (data self)))))))
    (set-filedes! self -1)
    (fd-close fd)
    (values)))

(define-method set-pixel! ((self <p5-image>) y x v)
  (bvec-set! (data self) (+ x (* y (image-width self))) v)
  (values))
