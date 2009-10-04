
(define-interactive (scale-view open-view pct #optional keep)
  (interactive (owner) (minibuffer <number> "Scale Percent: "))
  (let* (((view <view>) (underlying-object open-view))
	 (xf (scale $identity-transform (/ pct 100))))
    (if keep
	(do-view-concat open-view xf keep)
	(do-view-concat open-view xf))))

(define (do-view-concat (open-view <open-view>) ctm #optional keep)
  (let ((v (underlying-object open-view)))
    (if keep
	(concat-view-ctm! v ctm keep)
	(concat-view-ctm! v ctm))
    (set-need-to-recompute-handles! open-view #t)
    (let ((vx (view-extent v))
	  (vo (view-origin v))
	  (vf (view-frame v)))
      (update-value-range! (bottom-scrollbar open-view)
			   0
			   (width vx)
			   (+ (x vo) (/ (size-width vf) 2)))
      (update-value-range! (right-scrollbar open-view)
			   0
			   (height vx)
			   (+ (y vo) (/ (size-height vf) 2))))
    (clear-area (content-window open-view))
    (need-to-redraw (content-window open-view))))

(define (do-pan (open-view <open-view>) (delta <size>))
  (let ((v (underlying-object open-view)))
    (set-view-origin! v (point+ (view-origin v) delta))
    (set-need-to-recompute-handles! open-view #t)
    (dm "pan [~a], new view origin [~a]" delta (view-origin v))
    ;;
    (let* ((vx (view-extent v))
	   (vo (view-origin v))
	   (vf (view-frame v)))
      (if (not (zero? (dx delta)))
	  (begin
	    (set-position! (bottom-scrollbar open-view)
			   (+ (x vo) (/ (size-width vf) 2)))
	    (need-to-redraw (x-window (bottom-scrollbar open-view)))))
      (if (not (zero? (dy delta)))
	  (begin
	    (set-position! (right-scrollbar open-view)
			   (+ (y vo) (/ (size-height vf) 2)))
	    (need-to-redraw (x-window (right-scrollbar open-view)))))
      (clear-area (content-window open-view))
      (need-to-redraw (content-window open-view)))))

(define-interactive (zoom-in view #optional keep)
  (interactive (owner))
  (if keep
      (scale-view view 125 keep)
      (scale-view view 125)))

(define-interactive (zoom-out view #optional keep)
  (interactive (owner))
  (if keep
      (scale-view view 80 keep)
      (scale-view view 80)))

(graphic-set-key #\M-[ zoom-in)
(graphic-set-key #\M-] zoom-out)

;;;

(define (translate-view open-view xratio yratio)
  (let* (((v <view>) (underlying-object open-view))
	 ((f <rect>) (view-frame v))
	 (delta (make-size (* (width f) xratio)
			   (* (height f) yratio))))
    (do-pan open-view delta)))

(define-interactive (translate-up view)
  (interactive (owner))
  (translate-view view 0 -0.2))

(define-interactive (translate-down view)
  (interactive (owner))
  (translate-view view 0 0.2))

(define-interactive (translate-right view)
  (interactive (owner))
  (translate-view view 0.2 0))

(define-interactive (translate-left view)
  (interactive (owner))
  (translate-view view -0.2 0))

(graphic-set-key 'M-up translate-up)
(graphic-set-key 'M-down translate-down)
(graphic-set-key 'M-left translate-left)
(graphic-set-key 'M-right translate-right)

;;;
;;;  center the page within the window frame 
;;;  with only a small margin
;;;
;;;      ********************************
;;;	 *				*
;;;      *  +------------------------+  *
;;;      *  |                        |  *
;;;      *  |                        |  *
;;;      *  |                        |  *
;;;      *  |                        |  *
;;;      *  |                        |  *
;;;      *  |                        |  *
;;;      *  |                        |  *
;;;      *  |                        |  *
;;;      *  |                        |  *
;;;      *  |                        |  *
;;;      *  |                        |  *
;;;      *  |                        |  *
;;;      *  |                        |<---- page
;;;      *  |                        |  *
;;;      *  +------------------------+  *
;;;      *				*<--- window
;;;    	 ********************************
;;;

(define (reconfig-to-fit-window (view <view>))
  (let* ((pg (view-page view))
	 (window-size (size (view-frame view)))
	 (y-scale (/ (height window-size) (height (page-size pg))))
	 (x-scale (/ (width window-size) (width (page-size pg))))
	 (use-scale (* 0.9 (min x-scale y-scale)))
	 (xlate (size* (size- window-size (size* (page-size pg) use-scale))
		       0.5))
	 (ctm (make-affine-transform))
	 (ctm (scale (translate ctm (size->point xlate)) use-scale))
	 (ctm (translate (scale ctm (make-point 1 -1))
			 (make-point 0 (- (height (page-size pg)))))))
    ;(print ctm)
    (set-view-ctm! view ctm)))

(define-interactive (fit-in-window view)
  (interactive (owner))
  (reconfig-to-fit-window (underlying-object view))
  ;(did-reposition view) ;; what's this supposed to do?
  (values))

(define-interactive (actual-size open-view)
  (interactive (owner))
  (let* ((view (underlying-object open-view))
	 (window-size (size (view-frame view)))
	 (x-center (size->point (size* window-size 0.5)))
	 (center (transform x-center (invert-transform (view-ctm view)))))
    ;
    (format #t "old center (X) => ~s\n" x-center)
    (format #t "old center (u) => ~s\n" center)
    ;
    (let* ((ctm (translate
		 (scale (make-affine-transform)
			(make-point 1 -1))
		 (make-point 0 (- (height (page-size (view-page view)))))))
	   (c2 (transform center ctm))
	   (xl (point- c2 x-center)))
      (format #t "center in (X): ~s\n" c2)
      (format #t "off by: ~s\n" xl)
      (set-view-ctm! view (translate ctm (size->point xl)))
      (set-need-to-recompute-handles! open-view #t)
      (clear-area (content-window open-view) exposures?: #t)
      (values))))

;;;
;;;  Interactive procedure to create a new view which is a copy
;;;  of the current (or given) view.  Note that in addition to a
;;;  window, this creates an underlying <view> object in the 
;;;  core object model.
;;;

(define-interactive (new-view the-open-view)
  (interactive (owner))
  (let ((v (make-clone (underlying-object the-open-view))))
    (open-view-window (in-document the-open-view) v)))

