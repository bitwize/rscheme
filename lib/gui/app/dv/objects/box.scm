;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   <box-graphic>
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  persistent object model
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-class <box-graphic> (<leaf-object>)
  (width type: <real>)
  (height type: <real>))

(define-method status-line-when-sel ((self <box-graphic>))
  (format #f "Box ~d" (id self)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   drawing
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-method pick-list* ((self <box-graphic>) pt ctm)
  (pick-on-path self
		pt
		ctm
		(list (make-point 0 0)
		      (make-point (width self) 0)
		      (make-point (width self) (height self))
		      (make-point 0 (height self))
		      (make-point 0 0))))


(define-method paint-artwork* ((self <box-graphic>) dev)
  (moveto dev (make-point 0 0))
  (lineto dev (make-point (width self) 0))
  (lineto dev (make-point (width self) (height self)))
  (lineto dev (make-point 0 (height self)))
  (lineto dev (make-point 0 0))
  (closepath dev)
  (stroke dev))

(define-method paint-object* ((self <box-graphic>) dev)
  (with-gstate-saved
   dev
   (lambda ()
     (define (geom)
       (moveto dev (make-point 0 0))
       (lineto dev (make-point (width self) 0))
       (lineto dev (make-point (width self) (height self)))
       (lineto dev (make-point 0 (height self)))
       (lineto dev (make-point 0 0))
       (closepath dev))
     ;;
     (let ((s (get-property self 'stroke-style #f))
           (f (get-property self 'fill-style #f)))
       (geom)
       (dm "box: stroke-style ~s fill-style ~s" s f)
       (cond
        ((and s f)
         (with-gstate-saved 
          dev
          (lambda ()
            (setstyle dev f)
            (fill dev)))
         (setstyle dev s)
         (stroke dev))
        (s
         (setstyle dev s)
         (stroke dev))
        (f
         (setstyle dev f)
         (fill dev)))))))

;;;
;;; handles are as follows:
;;;
;;;         near-x            far-x
;;;            3                2
;;;      far-y *----------------*
;;;            |                |
;;;            |       * 4      |
;;;            |                |
;;;     near-y *----------------*
;;;            0                1
;;;

(define-method accum-handles ((self <box-graphic>) accum)
  (accum self (make-point 0 0) 0)
  (accum self (make-point (width self) 0) 1)
  (accum self (make-point (width self) (height self)) 2)
  (accum self (make-point 0 (height self)) 3))


;;;
;;; The procedure this thing produces operates in initial-object coords
;;;

(define (box-adjuster (self <box-graphic>) handle-id o->d initial-thunk)
  (case handle-id
    ((0)
     (let ((w (width self))
           (h (height self)))
       (lambda ((p <point>))
         (set-width! self (- w (x p)))
         (set-height! self (- h (y p)))
         (chop (point->size p)))))
    ((1)
     (let ((h (height self)))
       (lambda ((p <point>))
         (set-width! self (x p))
         (set-height! self (- h (y p)))
         (chop (make-size 0 (y p))))))
    ((2)
     (lambda ((p <point>))
       (set-width! self (x p))
       (set-height! self (y p))
       $zero-size))
    ((3)
     (let ((w (width self)))
       (lambda ((p <point>))
         (set-width! self (- w (x p)))
         (set-height! self (y p))
         (chop (make-size (x p) 0)))))
    ((4)
     (let ((p0 (initial-thunk)))
       (lambda ((p <point>))
         (chop (point- p p0)))))))

;;;
;;;  Apply a transformation to a bunch of points
;;;

(define (map-object-points shift ctm (oseq <vector>))
  (let ((ctm2 (concatenate-transform 
               ctm
               (translate $identity-transform 
                          (size->point shift)))))
    (vector-map (lambda (p)
                  (transform p ctm2))
                oseq)))

(define (box-device-corners (self <box-graphic>) ctm (ds <size>))
  (let* ((w (width self))
         (h (height self)))
    (map-object-points
     ds
     ctm
     (vector $zero-point
             (make-point w 0)
             (make-point w h)
             (make-point 0 h)))))

    
(define-method apply-style-to-graphic ((self <box-graphic>) 
                                       (style <stroke-style>))
  (set-property! self 'stroke-style style))

(define-method apply-style-to-graphic ((self <box-graphic>) 
                                       (style <fill-style>))
  (set-property! self 'fill-style style))

;;;
;;;  Take a sequence of points and explode it into individual coords
;;;  and crunching them into device (i.e., fixnums) as the X apis prefer
;;;

(define-method explode-coords ((seq <list>))
  (apply append (map (lambda ((p <point>))
                       (let (((dp <point>) (device-point p)))
                         (list (x dp) (y dp))))
                     seq)))

(define-method explode-coords ((seq <vector>))
  (let ((c (make-vector (* 2 (vector-length seq))))
        (n (vector-length seq)))
    (let loop ((i 0)
               (j 0))
      (if (< i n)
          (let (((dp <point>) (device-point (vector-ref seq i))))
            (vector-set! c j (x dp))
            (vector-set! c (+ j 1) (y dp))
            (loop (+ i 1) (+ j 2)))
          c))))

(define (box-corners->point-list (v <vector>))
  (let (((a <point>) (vector-ref v 0))
        ((b <point>) (vector-ref v 1))
        ((c <point>) (vector-ref v 2))
        ((d <point>) (vector-ref v 3)))
    (explode-coords (list a b c d a))))

(define-method start-active-drag-handle ((self <box-graphic>)
					 (in-view <open-view>)
					 handle-id
					 (initial-pt <point>))
  ;; during a drag, we use the object coordinates that were
  ;; in place at the beginning of the drag.  This avoids accumulation
  ;; of error during the many small changes of a drag.
  ;; `o->d' converts from (initial) object coords to device coords
  (let* ((o->u (compute-view-ctm-for self in-view))
         (o->d (concatenate-transform
                (invert-transform (window-ictm in-view))
                o->u))
	 (adjuster (box-adjuster self
                                 handle-id 
                                 o->d
                                 (lambda ()
                                   (inverse-transform 
                                    (refine-point in-view initial-pt)
                                    o->u))))
         (o-shift $zero-size)
	 (win (content-window in-view))
	 (gc (transient-gc win))
	 (last #f))
    ;
    (define (update-self new-pt)
      (set! o-shift (adjuster (inverse-transform 
                               (refine-point in-view new-pt)
                               o->u))))
    ;
    (vector
     ;; mouse-motion handler
     (lambda ((new-pt <point>) flags)
       (update-self new-pt)
       (let* ((bc (box-device-corners self o->d o-shift))
              (pl (box-corners->point-list bc)))
         (dm "box-device move ~s => ~s" o-shift bc)
	 (if last
	     (draw-lines win gc last))
	 (set! last pl)
	 (draw-lines win gc last)
	 (flush-client)))
     ;; mouse-up handler
     (lambda ((new-pt <point>) flags)
       (if last
	   (draw-lines win gc last))
       (mark-as-dirty (in-document in-view))
       (update-self new-pt)
       (set-origin! self (point+ (origin self) o-shift))
       (clear-all-areas (in-document in-view))))))

(define-method start-active-drag ((self <box-graphic>)
				  (in-view <open-view>)
				  (initial-pt <point>)) ;; window coords
  (start-active-drag-handle self in-view 4 initial-pt))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   box-drawing tool
;;;
;;;   (nb, this is really a graphic command functionality)
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (drawbox-button-press (in-view <open-view>)
			      (dp0 <point>) ;; window device coords
			      modifier-state)
  (let* (;; `ctm' converts window-device coords to user coords
	 (ctm (window-ictm in-view))
	 (fwd-ctm (invert-transform ctm)) ;; and back again
	 (win (content-window in-view))
	 (up0 (refine-point in-view dp0)) ;; snap to geometry (window d.c.)
         (dp0 (transform up0 fwd-ctm))
	 (gc (transient-gc win))
	 (last #f))
    (dm "BOX      up0 = ~s  (dp0 = ~s)" up0 dp0)
    (set-active-drag-proc!
     in-view
     (vector
      (lambda ((at <point>) flags)
	; we have to convert to user coordinates to draw the box
	; because our view might be rotated.  For just straight lines,
	; this is not a problem, because they are invariant under
	; affine xforms.
	(let* ((up2 (refine-point in-view at))
               (dp2 (transform up2 fwd-ctm))
	       (dp1 (transform (make-point (x up2) (y up0)) fwd-ctm))
	       (dp3 (transform (make-point (x up0) (y up2)) fwd-ctm)))
          ;(dm "BOX DRAG up2 = ~s  (dp2 = ~s) ~a" up2 dp2 at)
	  (if last
	      (draw-lines win gc last))
	  (set! last (map inexact->exact
			  (list (x dp0) (y dp0)
				(x dp1) (y dp1)
				(x dp2) (y dp2)
				(x dp3) (y dp3)
				(x dp0) (y dp0))))
	  (draw-lines win gc last)
	  (flush-client)))
      (lambda ((at <point>) flags)
	(let* ((up2 (refine-point in-view at))
               (dp2 (transform up2 fwd-ctm))
	       (par (page-contents
		     (view-page
		      (underlying-object in-view))))
	       (box (make <box-graphic>
			  in-document: (in-document par)
			  parent-object: par
			  origin: (make-point (min (x up0) (x up2))
					      (min (y up0) (y up2)))
			  graphic-bounding-box: (make-rect 0 0 0 0)
			  width: (abs (- (x up2) (x up0)))
			  height: (abs (- (y up2) (y up0))))))
	  (if last
	      (draw-lines win gc last))
          ;
          (let ((s (get-current-style 'stroke)))
            (if s
                (set-property! box 'stroke-style s)))
          (let ((s (get-current-style 'fill)))
            (if s
                (set-property! box 'fill-style s)))
          ;
	  (clear-all-areas (in-document in-view))
	  (do-select in-view box 0)))))))

(add-major-mode!
 (make <major-mode>
       name: 'draw-box
       button-press-proc: drawbox-button-press))

(define-interactive (draw-box-mode view)
  (interactive (owner))
  (set-major-mode! view (get-major-mode 'draw-box)))

(graphic-set-key #\M-2 draw-box-mode)

;;;

(define-method externalize ((self <box-graphic>))
  `(box origin-x: ,(x (origin self))
	origin-y: ,(y (origin self))
	width: ,(width self)
	height: ,(height self)))

(define (paste-box-from-extern extern group offset)
  (apply (lambda (#key (origin-x default: 0)
		       (origin-y default: 0)
		       width
		       height
		       (stroke-color default: #f)
		       (fill-color default: #f)
		       (stroke-width default: #f))
	   (let* ((sw (or stroke-width 1))
		  (b (make <box-graphic>
			  in-document: (in-document group)
			  parent-object: group
			  origin: (point+ (make-point origin-x origin-y)
					  offset)
			  graphic-bounding-box: (inset-rect
						  (make-rect 0 0 width height)
						  (- sw)
						  (- sw))
						 ;(dx offset)
						 ;(dy offset)
			  width: width
			  height: height)))
	     (if (or stroke-width
                     stroke-color)
                 (let ((style (override-style 'default-line)))
                   (if stroke-color
                       (set-style-attribute! 
                        style
                        color: (spec->color-style stroke-color)))
                   (if stroke-width
                       (set-style-attribute!
                        style
                        line-width: stroke-width))
                   (set-property! b 'stroke-style style)))
             ;;
	     (if fill-color (set-property! b 'fill-color fill-color))
	     b))
	 (cdr extern)))

;;;


(define-interactive (apply-rotation in-view objects angle)
  (interactive (owner) (selection) (minibuffer <number> "Angle: "))
  (let* ((centers (map center-of-gravity objects))
         (n (length objects))
         (center-x (/ (reduce + 0 (map x centers)) n))
         (center-y (/ (reduce + 0 (map y centers)) n))
         (T (translate
             (rotate
              (translate $identity-transform (make-point center-x center-y))
              angle)
             (make-point (- center-x) (- center-y)))))
    (apply-transform in-view objects T)))

(define (apply-transform in-view objects T)
  (for-each 
   (lambda (item)
     (let ((t (get-property item 'transform $identity-transform)))
       (set-property! item
                      'transform 
                      (chop (concatenate-transform t T)))))
   objects)
  (clear-all-areas (in-document in-view))
  (set-need-to-recompute-handles! in-view #t))

(define-method center-of-gravity ((self <graphic-object>))
  (origin self))

;;;

(define-interactive (rot30 in-view objects)
  (interactive (owner) (selection))
  (apply-rotation in-view objects 30))

(graphic-set-key '(#\M-r) rot30)
