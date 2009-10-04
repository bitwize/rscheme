(define-class <x11-device> (<graphics-device>)
  (x-display :sealed)
  (x-window :sealed type: <x-window>)
  (x-gc :sealed type: <x-gcontext>)
  (current-path init-value: #f)
  (subpath-starts init-value: '())
  (current-font init-value: #f)
  (ctm type: <transform>)
  (local-ctm type: <transform>)
  (owner init-value: #f))

(define (with-x11-device (w <x-window>) proc)
  (let ((d (open-x11-device w)))
    (let ((r (proc d)))
      (close-device d)
      r)))

(define-method close-device ((self <x11-device>))
  (free-gcontext (x-gc self))
  self)

(define (open-x11-device (w <x-window>))
  (bind ((scrn (drawable-screen w))
	 (gc (create-gcontext drawable: w
			      foreground: (screen-black-pixel scrn)
			      background: (screen-white-pixel scrn))))
    (make <x11-device>
	  x-window: w
	  x-display: (drawable-display w)
	  ctm: (make-affine-transform)
          local-ctm: (make-affine-transform)
	  x-gc: gc)))

(define (scale-512 n)
  (bitwise-and (max 0 (min 511 (inexact->exact (* n 511)))) 511))

(define (get-x11-device-color (dev <x11-device>) r g b)
  (let ((key (+ (logical-shift-left (scale-512 r) 18)
		(logical-shift-left (scale-512 g) 9)
                (scale-512 b)))
	(tbl (get-property (x-window dev) 'device-color-map #f)))
    (if (not tbl)
	(begin
	  (set! tbl (make-fixnum-table))
	  (set-property! (x-window dev) 'device-color-map tbl)))
    (or (table-lookup tbl key)
	(let ((pix (get-dk-color (drawable-screen (x-window dev))
				 (make-color red: r
					     green: g
					     blue: b))))
          (dm "X11 color ~s => ~s" (list r g b) pix)
	  (table-insert! tbl key pix)
	  pix))))


(define-method device-color ((dev <x11-device>) c)
  (let ((s (drawable-screen (x-window dev))))
    (case c
      ((black) (screen-black-pixel s))
      ((white) (screen-white-pixel s))
      (else
       (case (car c)
	 ((rgb) (get-x11-device-color dev (cadr c) (caddr c) (cadddr c)))
	 ((gray) (get-x11-device-color dev (cadr c) (cadr c) (cadr c))))))))
#|
       (get-dk-color
	s
	(case (car c)
	  ((gray)
	   (make-color red: (cadr c)
		       green: (car c)
		       blue: (cadr c)))
	  ((rgb)
	   (make-color red: (cadr c)
		       green: (caddr c)
		       blue: (cadddr c)))))))))
  |#
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   X11
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (device-point (pt <point>))
  (make-point (inexact->exact (if (integer? (x pt))
                                  (x pt)
                                  (+ (x pt) 0.0)))
	      (inexact->exact (if (integer? (y pt))
                                  (y pt)
                                  (+ (y pt) 0.0)))))

(define-method transform ((self <x11-device>) (p0 <point>))
  (let* ((pt (transform p0 (ctm self)))
	 (p2 (device-point pt)))
    ;;
    ;;  the geometry table maps device-space geometry
    ;;  to user-space locations.  This avoids any roundoff
    ;;  errors associated with converting to device space
    ;;  and back again
    ;;
    (if (owner self)
        (table-insert! (current-geometry (owner self)) 
                       p2 
                       (transform p0 (local-ctm self))))
    (values p2 pt)))

(define-method setdash ((self <x11-device>) (segments <vector>)
                                            (offset <real>))
  (set-gcontext-dashes! (x-gc self) 
                        (vector-map x11-len->pixels segments)
                        (x11-len->pixels offset)))

(define (x11-len->pixels (self <x11-device>) (len <real>))
  (let* ((p (transform (make-size len 0) (ctm self)))
         (dx (dx p))
         (dy (dy p)))
    (inexact->exact (sqrt (+ (* dx dx) (* dy dy))))))


(define-method setcolor ((self <x11-device>) (pixel-value <fixnum>))
  ;(dm "setcolor ~x" pixel-value)
  (set-gcontext-foreground! (x-gc self) pixel-value))

(define-method setlinewidth ((self <x11-device>) width)
  (set-gcontext-line-width! (x-gc self) 
                            (x11-len->pixels self width)))
  
(define-method setfont ((self <x11-device>) (font <text-font>))
  (set-current-font! self font)
  (let ((w (x11-len->pixels self (font-size font))))
    ;(dm "font size ~d ==> SCALE ==> ~d" (font-size font) w)
    (let ((f (if (= w (font-size font))
		 font
		 (make <text-font>
                       font-name: (font-name font)
                       font-member: (font-member font)
                       font-size: (string->number (number->string w))))))
      (set-gcontext-font! (x-gc self) (get-x-font f (x-display self))))))

(define (get-x-font font dpy)
  (or (get-property dpy font #f)
      (fill-x-font-cache font dpy)))

(define (fill-x-font-cache font dpy)
  (let* ((fn (get-x-font-name font))
	 (f (open-font dpy fn)))
    (text-extents f "0")
    (set-property! dpy font f)
    f))

  
(define-method current-point ((self <x11-device>))
  (let* ((p (current-path self))
	 (k (dequeue-count (or p (error "current-point: no current path")))))
    (values (dequeue-ref p (- k 2))
	    (dequeue-ref p (- k 1)))))

(define-method newpath ((self <x11-device>))
  (set-current-path! self (make-dequeue))
  (set-subpath-starts! self '()))

(define-method clearpath ((self <x11-device>))
  (set-current-path! self #f)
  (set-subpath-starts! self '()))

(define-method moveto ((self <x11-device>) (pt <point>))
  (if (not (current-path self))
      (newpath self))
  (set-subpath-starts! self (cons (dequeue-count (or (current-path self)
                                                     "moveto: no path"))
                                  (subpath-starts self)))
  (let (((pt <point>) (device-point (transform self pt))))
    (dequeue-push-back! (current-path self) (x pt))
    (dequeue-push-back! (current-path self) (y pt))
    (values)))

(define-method lineto ((self <x11-device>) (pt <point>))
  (let (((pt <point>) (device-point (transform self pt)))
        (p (or (current-path self)
               (error "lineto: no current path"))))
    (dequeue-push-back! p (x pt))
    (dequeue-push-back! p (y pt))
    (values)))

(define-method arc ((self <x11-device>) (center <point>)
                                        (radius <real>) 
                                        (start-angle <real>)
                                        (end-angle <real>))
  (arcx self center radius start-angle end-angle 0.15 <))

(define-method arcn ((self <x11-device>) (center <point>)
                                        (radius <real>) 
                                        (start-angle <real>)
                                        (end-angle <real>))
  (arcx self center radius start-angle end-angle -0.15 >))

(define (arcx (self <x11-device>) (center <point>)
              (radius <real>) 
              (start-angle <real>)
              (end-angle <real>)
              epsilon
              keep-going?)
  ;;
  (let ((cp (or (current-path self)
                (error "curveto: no current path"))))
    (define (to-point-on-arc t)
      (let ((p (transform self (make-point 
                                (+ (x center) (* radius (cos t)))
                                (+ (y center) (* radius (sin t)))))))
        (dequeue-push-back! cp (inexact->exact (round (x p))))
        (dequeue-push-back! cp (inexact->exact (round (y p))))
        (values)))
    ;;
    (let ((end-angle (* $Deg-to-rad end-angle)))
      (let loop ((theta (* $Deg-to-rad start-angle)))
        (to-point-on-arc theta)
        (let ((next (+ theta epsilon)))
          (if (keep-going? next end-angle)
              (loop next)
              (to-point-on-arc end-angle)))))))

(define-method curveto ((self <x11-device>) (h1 <point>)
					    (h2 <point>) 
					    (pt <point>))
  (let (((h1 <point>) (transform self h1))
	((h2 <point>) (transform self h2))
	((pt <point>) (device-point (transform self pt))))
    (bind ((cx cy (current-point self))
           (cp (or (current-path self)
                   (error "curveto: no current path"))))
      (let ((c (curv cx cy (x h1) (y h1) (x h2) (y h2) (x pt) (y pt))))
	(for-each
	 (lambda (t)
	   (let ((p (point-on c t)))
	     (dequeue-push-back! cp (inexact->exact (round (x p))))
	     (dequeue-push-back! cp (inexact->exact (round (y p))))
	     (values)))
	 '(0.1 0.2 0.3 0.4 0.5 0.6 0.7 0.8 0.9))
	(dequeue-push-back! cp (x pt))
	(dequeue-push-back! cp (y pt))
	(values)))))

(define-method rectstroke ((self <x11-device>) (r <rect>))
  (moveto self (lower-left r))
  (lineto self (lower-right r))
  (lineto self (upper-right r))
  (lineto self (upper-left r))
  (lineto self (lower-left r))
  (stroke self))


(define-method stroke ((self <x11-device>))
  (let ((cp (or (current-path self)
                (error "stroke: no current path"))))
    (draw-lines (x-window self) 
                (x-gc self) 
                (dequeue-state cp))
    (clearpath self)))

(define-method fill ((self <x11-device>))
  (let ((cp (or (current-path self)
                (error "stroke: no current path"))))
    ;(dm "fill ~s" (dequeue-state cp))
    (draw-lines (x-window self) 
                (x-gc self) 
                (dequeue-state cp)
                fill?: #t)
    (clearpath self)))

(define-method show ((self <x11-device>) (str <string>))
  (if (> (string-length str) 0)
      (bind ((devx devy (current-point self))
	     (dxs (xshow-x-list (font-afm (current-font self)) 
				(font-size (current-font self))
				str)))
	    (let loop ((cursor (make-point devx devy))
		       (dxs dxs)
		       (i 0))
              (let ((d (device-point cursor)))
                (draw-glyphs (x-window self) 
                             (x-gc self)
                             (x d)
                             (y d)
                             (string (string-ref str i)))
                (if (< (+ i 1) (string-length str))
                    (let ((dp (transform (make-size (car dxs) 0) (ctm self))))
                      ;(dm 141 "for ~s, dp = ~s" (string-ref str i) dp)
                      (loop (point+ cursor dp)
                            (cdr dxs) 
                            (+ i 1)))))))))

(define-method closepath ((self <x11-device>))
  ;; what should this do..?
  (let ((q (current-path self))
        (k (car (subpath-starts self))))
    (dequeue-push-back! q (dequeue-ref q k))
    (dequeue-push-back! q (dequeue-ref q (+ k 1))))
  ;
  (values))

;;;

(define-method with-gstate-saved ((self <x11-device>) thunk)
  (let ((saved-ctm (ctm self))
        (saved-local-ctm (local-ctm self))
	(gc (create-gcontext drawable: (x-window self)))
        (saved-current-path (current-path self))
        (saved-subpath-starts (subpath-starts self)))
    (copy-gcontext (x-gc self) gc)
    (if (and saved-current-path
             (> (dequeue-count saved-current-path) 0))
        (set-current-path! self (dequeue-copy saved-current-path)))
    (thunk)
    (copy-gcontext gc (x-gc self))
    (free-gcontext gc)
    (set-ctm! self saved-ctm)
    (set-local-ctm! self saved-local-ctm)
    (set-current-path! self saved-current-path)
    (set-subpath-starts! self saved-subpath-starts)
    (values)))

(define-method with-ctm-saved ((self <x11-device>) thunk)
  (let ((saved-ctm (ctm self))
        (saved-local-ctm (local-ctm self)))
    (thunk)
    (set-ctm! self saved-ctm)
    (set-local-ctm! self saved-local-ctm)
    (values)))

(define-method show-handle ((dev <x11-device>) at)
  (bind ((pt pte (transform dev at)))
    (draw-rectangle (x-window dev) (x-gc dev) 
		    (- (x pt) 1) (- (y pt) 1) 3 3
		    #t)
    pte))

;;;
;;;  these alter `local' coordinates, too
;;;

(define-method translate ((self <x11-device>) (delta <point>))
  (set-ctm! self (translate (ctm self) delta))
  (set-local-ctm! self (translate (local-ctm self) delta)))

(define-method concat ((self <x11-device>) tm)
  (set-ctm! self (concatenate-transform (ctm self) tm))
  (set-local-ctm! self (concatenate-transform (local-ctm self) tm)))

;;;
;;;  these do not update `local' coordinates
;;;

(define-method translate* ((self <x11-device>) (delta <point>))
  (set-ctm! self (translate (ctm self) delta)))

(define-method concat* ((self <x11-device>) tm)
  (set-ctm! self (concatenate-transform (ctm self) tm)))

