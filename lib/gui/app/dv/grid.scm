
;;; ...nb this is broken!

(define (compute-grid-ctm (v <view>) (g-origin <point>) (g-spacing <real>))
  (invert-transform
   (scale
    (translate
     (make-affine-transform)
     (point+ (transform g-origin (view-ctm v))
             (size* (point->size (view-origin v)) -1)))
    g-spacing)))

(define (get-grid-ctm (ov <open-view>))
  (or (grid-ctm ov)
      (let* ((v (underlying-object ov))
             (c (compute-grid-ctm 
                 v 
                 (or (get-inheritable-property v 'grid-origin) $zero-point)
                 (or (get-inheritable-property v 'grid-spacing) 9))))
        (set-grid-ctm! ov c)
        c)))

#|
(define (spacing-ctms spacing)
  (values (scale (make-affine-transform) spacing)
	  (scale (make-affine-transform) (/ spacing))))
|#

;;; the input point is in device (view) coordinates 
;;; (i.e., device sheet translated by view-origin)

(define (snap-to-grid (in <open-view>) (pt0 <point>))
  (let ((u-spacing (get-inheritable-property (underlying-object in) 
					     'grid-spacing)))
    (if u-spacing
	(bind ((ctm (get-grid-ctm in))
	       (pt (transform pt0 ctm))
               (xp (inverse-transform (make-point (round (x pt)) 
                                                  (round (y pt)))
                                      ctm)))
          (dm "snap to grid: ~s => ~s => ~s" pt0 pt xp)
          xp)
	pt0)))

;;; point given and returned in user coords

(define-method round ((self <point>))
  (make-point (round (x self))
              (round (y self))))

(define (grid->user-transform (g-origin <point>) (g-spacing <real>))
  (scale
   (translate
    (make-affine-transform)
    (make-point (x g-origin)
                (y g-origin)))
   g-spacing))

(define (snap-to-grid/u (in <open-view>) (pt <point>))
  (let ((u-spacing (get-inheritable-property (underlying-object in) 
					     'grid-spacing)))
    (if u-spacing
	(let ((gm (grid->user-transform
                   (or (get-inheritable-property (underlying-object in)
                                                 'grid-origin)
                       $zero-point)
                   u-spacing)))
          (transform (round (inverse-transform pt gm)) gm))
	#f)))

(define (draw-grid-points (in <open-view>))
  (let ((u-spacing (get-inheritable-property (underlying-object in) 
					     'grid-spacing)))
    (if (and u-spacing (visible-grid in))
	(draw-grid-points* in u-spacing))))

(define (draw-grid-points* (in <open-view>) u-spacing)
  (set-grid-ctm! in #f)  ; clear grid-ctm cache
  ;; compute the user-space bbox
  (bind (((v <view>) (underlying-object in))
	 (ictm (invert-transform (view-ctm v)))
	 (ctm (invert-transform (translate ictm (view-origin v))))
         ((vf <rect>) (make-rect (x (view-origin v))
                                 (y (view-origin v))
                                 (width (view-frame v))
                                 (height (view-frame v))))
	 ((bb <rect>) (transform vf ictm))
	 (min-x min-y num-x num-y spacing (spacing-to-draw bb u-spacing))
         (x0 y0 (values 0 0))
         (x1 y1 (point->values (device-point (make-point (limit-x vf)
                                                         (limit-y vf))))))
    (dm "grid bbox (user space): ~s (~d,~d - ~d x ~d array - ~d spacing)" 
        bb 
        min-x min-y
        num-x num-y
        spacing)
    ;;
    (set-gcontext-foreground! (grid-gcontext in) (grid-color in))
    ;;
    (let x-loop ((ix 0)
                 (gx min-x))
      (if (<= ix num-x)
          (let ((p (device-point (transform (make-point gx 0) ctm))))
            (draw-line (content-window in)
                       (grid-gcontext in)
                       (x p)
                       y0
                       (x p)
                       y1)
            (x-loop (+ ix 1) (+ gx spacing)))))
    ;;
    (let y-loop ((iy 0)
                 (gy min-y))
      (if (<= iy num-y)
          (let ((p (device-point (transform (make-point 0 gy) ctm))))
            (draw-line (content-window in)
                       (grid-gcontext in)
                       x0
                       (y p)
                       x1
                       (y p))
            (y-loop (+ iy 1) (+ gy spacing)))))))
#|
    (let y-loop ((iy 0)
		 (gy min-y))
      (if (< iy num-y)
	  (let x-loop ((ix 0) 
		       (gx min-x))
	    (if (< ix num-x)
		(let ((p (device-point (transform (make-point gx gy) ctm))))
		  (set! lst (cons* (x p) (y p) lst))
		  (x-loop (+ ix 1) (+ gx spacing)))
		(y-loop (+ iy 1) (+ gy spacing))))
	  (draw-points (content-window in) 
		       (grid-gcontext in)
		       lst)))
|#

(define (spacing-to-draw (bb <rect>) user-grid-spacing)
  (let loop ((grid-spacing-mult '(1 2 5 10 25 50 100)))
    (let* ((grid-spacing (* user-grid-spacing (car grid-spacing-mult)))
           (min-x (* grid-spacing (floor (/ (origin-x bb) grid-spacing))))
           (x-pts (inexact->exact
                   (ceiling (/ (- (limit-x bb) min-x) grid-spacing))))
           (min-y (* grid-spacing (floor (/ (origin-y bb) grid-spacing))))
           (y-pts (inexact->exact
                   (ceiling (/ (- (limit-y bb) min-y) grid-spacing)))))
      (if (or (> x-pts 100) (> y-pts 100))
          (loop (if (pair? (cdr grid-spacing-mult))
                    (cdr grid-spacing-mult)
                    (list (* 2 (car grid-spacing-mult)))))
          (values min-x min-y x-pts y-pts grid-spacing)))))

#|
(define (t)
  (for-each
   draw-grid-points
   (open-views (current-client))))
|#

(define-interactive (set-grid open-view size)
  (interactive (open-view) (minibuffer <number:length> "Grid size: "))
  (set-property! (underlying-object open-view)
                 'grid-spacing 
                 (quantity/ size 1pt))
  (set-visible-grid! open-view #t)
  (need-to-clear-and-redraw (content-window open-view))
  (values))

(define-interactive (clear-grid view)
  (interactive (open-view))
  (set-visible-grid! view #f)
  (set-property! (underlying-object view) 'grid-spacing #f))

(define-interactive (hide-grid view)
  (interactive (open-view))
  (if (visible-grid view)
      (begin
        (set-visible-grid! view #f)
        (need-to-redraw (content-window view))))
  (values))
  
(define-interactive (show-grid view)
  (interactive (open-view))
  (if (not (visible-grid view))
      (begin
        (set-visible-grid! view #t)
        (need-to-redraw (content-window view))))
  (values))
