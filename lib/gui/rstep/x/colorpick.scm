
(define-class <color-picker> (<view>)
  (color setter: set-color*!)
  sample-swatch
  (current-picker-panel init-value: #f))

(define-method initialize ((self <color-picker>))
  (next-method)
  (set-property! (for-client self) 'color-picker self))

(define-method set-color! ((self <color-picker>) color)
  (set-color*! self color)
  (if (sample-swatch self)
      (set-value! (sample-swatch self) color))
  (adjust-sliders self))

(define-method adjust-sliders ((self <color-picker>))
  (if (current-picker-panel self)
      (adjust-sliders (current-picker-panel self))))

(define-method set-value! ((self <color-picker>) color)
  (set-color! self color)
  (if (sample-swatch self)
      (set-value! (sample-swatch self) color))
  (for-each
   (lambda (w)
     (set-value! w color))
   (active-color-wells (for-client self))))

(define-class <rgb-colorpick> (<view>)
  red-slider
  green-slider
  blue-slider
  (pixels init-value: #f))

(define-method initialize ((self <rgb-colorpick>))
  (set-pixels! self
	       (get-pixels-resource 'button.shades
				    (for-client (window self)))))

(define-class <colorslide> (<view>)
  (scale-value init-value: 0)
  scale-proc
  (scale-image init-value: #f))

(define-method color ((self <rgb-colorpick>))
  (color (parent self)))

(define-method color ((self <colorslide>))
  (color (parent self)))

(define-method adjust-sliders ((self <rgb-colorpick>))
  (bind ((r g b (color-rgb (color self))))
    (dm "adjusting sliders to: ~s ~s ~s" r g b)
    (for-each (lambda (sl com)
		(set-scale-image! sl #f)
		(set-value! sl com))
	      (list (red-slider self)
		    (green-slider self)
		    (blue-slider self))
	      (list r g b))))

(define (r-scaler r g b f)
  (make-color red: f green: g blue: b))

(define (g-scaler r g b f)
  (make-color red: r green: f blue: b))

(define (b-scaler r g b f)
  (make-color red: r green: g blue: f))

(define (make-color-picker #key frame parent (color default: $black))
  (let* ((g (make-view class: <color-picker>
		       parent: parent
		       frame: frame
		       color: color
		       sample-swatch: #f))
	 (p (make-view class: <rgb-colorpick>
		       frame: (make-rect 0 44
					 (size-width frame)
					 (size-height frame))
		       parent: g
		       red-slider: #f
		       green-slider: #f
		       blue-slider: #f)))
    (set-current-picker-panel! g p)
    (set-sample-swatch! g (make-color-swatch 
			   frame: (make-rect 25 0
					     (- (size-width frame) 25)
					     20)
			   parent: g))
    (set-red-slider! p (make-view class: <colorslide>
				  wants-button-motion: #t
				  frame: (make-rect 0 0
						    (size-width frame)
						    16)
				  parent: p
				  scale-proc: r-scaler))
    (set-green-slider! p (make-view class: <colorslide>
				    wants-button-motion: #t
				    frame: (make-rect 0 20
						      (size-width frame)
						      16)
				    parent: p
				    scale-proc: g-scaler))
    (set-blue-slider! p (make-view class: <colorslide>
				   wants-button-motion: #t
				   frame: (make-rect 0 40
						     (size-width frame)
						     16)
				   parent: p
				   scale-proc: b-scaler))
    p))

(define (colorslide-background (self <colorslide>))
  (bind ((w (size-width (frame self)))
	 (h (size-height (frame self)))
	 (r (color-red (color self)))
	 (g (color-green (color self)))
	 (b (color-blue (color self)))
	 (P (scale-proc self)))
   (color-scale (- w 4) (- h 4)
		(lambda (f)
		  (P r g b f)))))

(define (color-scale w h gen)
  (bind ((im (make-graphic-image width: w
				 height: h))
	 (mem (memory-image-rep im)))
    ;(dm "colorslide-background: ~s" im)
   (do-times (x w)
     (let ((c (gen (/ x (- w 1)))))
       (do-times (y h)
	 (set-pixel! mem x y c))))
   im))

(define (pick-handle (self <colorslide>))
  (let* ((maxw (- (size-width (frame self)) 10))
	 (at (min (inexact->exact (round (* (scale-value self) maxw)))
		  maxw)))
    (make-rect (+ at 2) 2 6 (- (size-height (frame self)) 4))))

(define-method draw-self ((self <colorslide>) rects)
  (bind ((win gc o (lock-focus self)))
    ;(dm "~s at ~s" self o)
    (if (not (scale-image self))
	(set-scale-image! self (colorslide-background self)))
    (let ((pix (pixels (parent self)))
	  (h (pick-handle self)))
      (draw-bezeled win gc pix $sunken-bezel
		    (dx o) (dy o)
		    (size-width (frame self))
		    (size-height (frame self)))
      (x-composite (x-pixmap-image-rep (scale-image self)
				       win
				       (use-colormap self))
		   win gc (+ (dx o) 2) (+ (dy o) 2))
      (draw-bezeled win gc pix $filled-button
		    (+ (dx o) (origin-x h))
		    (+ (dy o) (origin-y h))
		    (size-width h)
		    (size-height h)))))

(define-constant $filled-button
  '(0 bottom right
    3 left top
    1 bottom right
    2 middle))

;;;

(define-method set-value! ((self <colorslide>) (to <real>))
  (set-scale-value! self to)
  (update self))

(define (colorslide-adj (self <colorslide>) v)
  (let* ((v (min 1 (max 0 v)))
	 ((c <color>) (color self))
	 (r (color-red c))
	 (g (color-green c))
	 (b (color-blue c)))
    (dm "colorslide: mouse moved to ~s" v)
    (set-value! (parent (parent self)) ((scale-proc self) r g b v))
    (set-value! self v)))

#|
(define-method mouse-moved ((self <colorslide>) (to <point>) state)
  (colorslide-adj self to))
|#

(define-method mouse-down ((self <colorslide>) (from <point>) state)
  (let* ((h (pick-handle self))
	 (r (/ (exact->inexact (- (size-width (frame self)) 8))))
	 (adjer (if (point-in-rect? h from)
		    (lambda ((at <point>) state root-at)
		      (colorslide-adj
		       self
		       (* r (+ (origin-x h) 
			       (- (x at) (x from))))))
		    (lambda ((at <point>) state root-at)
		      (colorslide-adj
		       self
		       (* r (- (x at) 3)))))))
    ;;
    (if (not (point-in-rect? h from))
	(adjer from state from))
    ;;
    (track-mouse self
		 mouse-moved: adjer
		 mouse-up: adjer)
    ;;
    (let ((p (parent self)))
      (for-each
       (lambda ((sl <colorslide>))
	 (format #t "~s / ~s\n" sl self)
	 (if (not (eq? sl self))
	     (begin
	       (set-scale-image! sl #f)
	       (update sl))))
       (list (red-slider p)
	     (green-slider p)
	     (blue-slider p))))))
