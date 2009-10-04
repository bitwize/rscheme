
(define-class <color-cell> (<object>)
  color
  (image init-value: #f)
  (use-pixel init-value: #f))

(define-method draw-cell ((self <color-cell>) win gc rect)
  (draw-color-in-well self win gc rect))

;;;

(define-class <color-swatch> (<control>)
  color
  (pixels init-value: #f)
  (image init-value: #f)
  (dedicated-pixel init-value: #f)
  (use-pixel init-value: #f))

(define-method live? ((self <color-swatch>))
  #f)

(define-class <color-well> (<color-swatch>)
  (live? init-value: #f))

(define-method initialize ((self <color-swatch>))
  (set-pixels! self
	       (get-pixels-resource 'button.shades
				    (for-client (window self)))))

(define (make-color-swatch #key frame parent (color default: $white))
  (make-view class: <color-swatch>
	     parent: parent
	     frame: frame
	     wants-button-motion: #t
	     color: color))

(define (make-color-well #key frame parent (color default: $white))
  (make-view class: <color-well>
	     parent: parent
	     frame: frame
	     wants-button-motion: #t
	     color: color))

(define-method activate ((self <color-well>) 
			 #optional (exclusive? default: #t))
  (if (not (live? self))
      (add-active-color-well! (for-client self) self exclusive?)))

(define-method deactivate ((self <color-well>))
  (if (live? self)
      (remove-active-color-well! (for-client self) self)))

(define-method resign-live ((self <color-well>))
  (if (live? self)
      (begin
	(set-live?! self #f)
	(if (dedicated-pixel self)
	    (begin
	      (free-colors (use-colormap self)
			   (list (dedicated-pixel self)))
	      (set-dedicated-pixel! self #f)
	      (set-use-pixel! self #f)))
	(update self))))

(define-method become-live ((self <color-well>))
  (if (not (live? self))
      (begin
	(set-live?! self #t)
	;; try to allocate a dedicated color cell
	(handler-case
	 (let ((c (alloc-color-cells (use-colormap self) 1)))
	   (set-dedicated-pixel! self (vector-ref c 0))
	   (set-use-pixel! self (vector-ref c 0))
	   (dm "become-live: dedicated pixel ~s" c))
	 ((<x-error>)
	  (dm "become-live: couldn't use dedicated color cell")))
	(update self))))

(define-constant $color-well
  '(0 bottom right
    3 left top
    1 bottom right
    2 middle))

(define-constant $live-color-well
  '(0 bottom right
    3 left top
    1 bottom right
    3 middle))

(define-constant $inner-well
  '(3 bottom right
    1 top left))

(define-method set-value! ((self <color-swatch>) new-color)
  (set-color! self new-color)
  (set-image! self #f)
  (if (dedicated-pixel self)
      (store-color (use-colormap self)
		   (dedicated-pixel self)
		   new-color)
      (begin
	(if (eq? (colormap-visual-class (use-colormap self)) 
		 'true-color)
	    (set-use-pixel! self
			    (alloc-color (use-colormap self) new-color)))
	(update self))))

(define (draw-color-in-well self win gc rect)
  (let ((pix (use-pixel self)))
    (if pix
	(begin
	  (set-gcontext-foreground! gc pix)
	  (draw-rectangle win gc 
			  (origin-x rect)
			  (origin-y rect)
			  (size-width rect)
			  (size-height rect)
			  #t))
	(x-composite (or (image self)
			 (make-well-image self rect))
		     win
		     gc
		     (origin-x rect)
		     (origin-y rect)))))

(define-method draw-self ((self <color-swatch>) rects)
  (bind ((win gc o (lock-focus self))
	 (pix (pixels self))
	 (f (make-rect (dx o) (dy o)
		       (size-width (frame self))
		       (size-height (frame self)))))
    (draw-bezeled-rect win gc pix $inner-well f)
    (draw-color-in-well self win gc (inset-rect f 2 2))))

(define-method draw-self ((self <color-well>) rects)
  (bind ((win gc o (lock-focus self))
	 (pix (pixels self))
	 (f (make-rect (dx o) (dy o)
		       (size-width (frame self))
		       (size-height (frame self)))))
    (draw-bezeled-rect win gc pix 
		       (if (live? self)
			   $live-color-well
			   $color-well)
		       f)
    (draw-bezeled-rect win gc pix
		       $inner-well
		       (inset-rect f 5 5))
    (draw-color-in-well self win gc (inset-rect f 6 6))))

(define (make-well-image (self <color-swatch>) rect)
  (let ((im (make-constant-image 
	     (color self)
	     width: (size-width rect)
	     height: (size-height rect))))
    (set-image! self im)
    im))

(define-method mouse-down ((self <color-swatch>) (pt <point>) state)
  (let* ((im (make-constant-image (color self) width: 10 height: 10))
	 (client (for-client self))
	 (pxm (image->pixmap im
			     (screen-root (on-screen client))
			     (use-colormap client))))
    (define (drop-proc (in <color-well>) at)
      (dm "drop in: ~s at: ~s" in at)
      (set-value! in (color self)))
    (drag-and-drop owner: self
		   pixmap: im
		   size: (make-size 10 10)
		   starting-at: pt
		   test-proc: (lambda (v)
				(instance? v <color-well>))
		   pixmap-offset: (make-size (- (x pt) 5) (- (y pt) 5))
		   valid-targets: (all-windows client)
		   drop-proc: drop-proc)))

(define-method mouse-down ((self <color-well>) (pt <point>) state)
  (let* ((f (make-rect 0 0 
		       (size-width (frame self))
		       (size-height (frame self))))
	 (inner (inset-rect f 7 7)))
    (if (point-in-rect? inner pt)
	(next-method)
	(if (live? self)
	    (deactivate self)
	    (activate self)))))
