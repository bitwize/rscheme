
(define (set-gcontext-foreground! (self <x-gcontext>) (color <fixnum>))
  (internal-send
   (x-display self)
   (make-buffer u1: 56 ;; ChangeGC
		u1: 0
		u2: 4
		u4: (x-id self)
		u4: #x00004
		u4: color)))

(define (set-gcontext-tile! (self <x-gcontext>) (tile <x-pixmap>))
  (internal-send
   (x-display self)
   (make-buffer u1: 56 ;; ChangeGC
		u1: 0
		u2: 4
		u4: (x-id self)
		u4: #x00400
		u4: (x-id tile))))

(define (set-gcontext-stipple! (self <x-gcontext>) (stipple <x-pixmap>))
  (internal-send
   (x-display self)
   (make-buffer u1: 56 ;; ChangeGC
		u1: 0
		u2: 4
		u4: (x-id self)
		u4: #x00800
		u4: (x-id stipple))))

(define (set-gcontext-fill-style! (self <x-gcontext>) fill-style)
  (internal-send
   (x-display self)
   (make-buffer u1: 56 ;; ChangeGC
		u1: 0
		u2: 4
		u4: (x-id self)
		u4: #x00100
		u4: (vmemq fill-style $fill-styles))))

;;;

(define (set-gcontext-font! (self <x-gcontext>) (font <x-font>))
  (internal-send
   (x-display self)
   (make-buffer u1: 56 ;; ChangeGC
		u1: 0
		u2: 4
		u4: (x-id self)
		u4: #x04000
		u4: (x-id font))))

(define (set-gcontext-line-width! (self <x-gcontext>) (width <fixnum>))
  (internal-send
   (x-display self)
   (make-buffer u1: 56 ;; ChangeGC
		u1: 0
		u2: 4
		u4: (x-id self)
		u4: #x10
		u4: width)))

(define-macro (define-gcontext-setter (name (var class)) tag expr)
  `(define (,name (self <x-gcontext>) (,var ,class))
     (internal-send
      (x-display self)
      (make-buffer u1: 56 ;; ChangeGC
                   u1: 0
                   u2: 4
                   u4: (x-id self)
                   u4: ,tag
                   u4: ,expr))))

(define-gcontext-setter (set-gcontext-clip-x! (x <fixnum>))
  #x20000 x)

(define-gcontext-setter (set-gcontext-clip-y! (y <fixnum>)) 
  #x40000 y)

(define-gcontext-setter (set-gcontext-clip-mask!/pm (pm <object>)) ; <x-pixmap> | #f
  #x80000 (if pm (x-id pm) 0))

(define (set-gcontext-clip-mask! (self <x-gcontext>) what #optional ordering)
  (if (pair? what)
      ;; must be a rect-seq; use SetClipRectangles
      (let ((clip-x-origin 0)           ; where should these come from?
            (clip-y-origin 0)
            (n (length what)))
        ;;
        (if (not (zero? (modulo n 4)))
            (error "set-gcontext-clip-mask!: Not a list of rectanges: ~s" what))
        ;;
        (internal-send
         (x-display self)
         (vector
          (make-buffer u1: 59                           ; SetClipRectangles
                       u1: (case ordering 
                             ((#f unsorted) 0)
                             ((y-sorted) 1)
                             ((yx-sorted) 2)
                             ((yx-banded) 3)
                             (else (error "set-gcontext-clip-mask!: Unknown ordering: ~s" 
                                          ordering)))
                       u2: (+ 3 (quotient n 2))
                       u4: (x-id self)
                       u2: clip-x-origin
                       u2: clip-y-origin)
          (points->bvec what))))
      ;; must be a pixmap or #f; use ChangeGC
      (set-gcontext-clip-mask!/pm self what)))

(define-gcontext-setter (set-gcontext-dashes!/n (dashes <fixnum>))
  #x00200000 dashes)

(define (set-gcontext-dashes! (self <x-gcontext>) spec #optional dash-offset)
  (cond
   ((fixnum? spec)
    (set-gcontext-dashes!/n self spec))
   ((or (pair? spec)
        (vector? spec))
    (bind ((n (if (pair? spec)
                  (length spec)
                  (vector-length spec)))
           (pad-n pad-str (pad4 n))
           ((dash-offset <fixnum>) (or dash-offset 0)))
      ;;
      (internal-send
       (x-display self)
       (vector
        (make-buffer u1: 58               ; SetDashes
                     u1: 0
                     u2: (+ 3 (quotient (+ pad-n n) 4))
                     u4: (x-id self)
                     u2: dash-offset
                     u2: n)
        (octets->bvec spec)
        pad-str))))
   ;;
   (else
    (error "Unknown dashes specification: ~s" spec))))

(define-method octets->bvec ((octets <vector>))
  (let* ((n (vector-length octets))
	 ((pbuf <byte-vector>) (bvec-alloc <byte-vector> n)))
    (let loop ((i 0))
      (if (< i n)
	  (begin
	    (xbo-write-u1 pbuf i (fixnumify (vector-ref octets i)))
	    (loop (+ i 1)))
	  pbuf))))

(define-method octets->bvec ((octets <pair>))
  (let* ((n (length octets))
	 ((pbuf <byte-vector>) (bvec-alloc <byte-vector> n)))
    (let loop ((i 0)
               (l octets))
      (if (< i n)
	  (begin
	    (xbo-write-u1 pbuf i (fixnumify (car l)))
	    (loop (+ i 1) (cdr l)))
	  pbuf))))

(define-gcontext-setter (set-gcontext-line-style! (line-style <symbol>))
  #x00000020
  (vmemq line-style $line-styles))
