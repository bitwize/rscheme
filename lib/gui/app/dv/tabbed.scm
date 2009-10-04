
(define-class <tab-control> (<object>)
  (frame type: <rect>)
  (tab-cells type: <vector>)
  selected-tab
  in-window
  using-colormap
  using-font)

(define-class <tab-cell> (<object>)
  (parent type: <tab-control>)
  (tab-width init-value: #f) ; not counting tab xover graphics
  (label-x init-value: #f)
  (tab-label type: <string>))


(define-color-list *tab-colors*
  "black"
  "rgbi:0.667/0.667/0.667"
  "white"
  "rgbi:0.5/0.5/0.5")

;;;

(define $tab-graphic-width 15)
(define $tab-graphic-height 19)

(define *tab-graphics*
  (vector-map
   (lambda (e)
     (make-graphic-image 
      from: (read-png-image
	     (string-append "images/" e ".png"))))
   '#("tab0" "tab1" "tab2" "tab3"
      "tab4" "tab5" "tab6" "tab7")))

;;;

(define-method mouse-down ((self <tab-cell>) at state)
  (set-selected-tab! (parent self) self)
  (need-to-redraw (in-window (parent self))))

(define-method mouse-down ((self <tab-control>) at state)
  (let ((vec (tab-cells self)))
    (let loop ((cx (origin-x (frame self)))
	       (i 0))
      (if (< i (vector-length vec))
	  (let ((x2 (+ cx $tab-graphic-width (tab-width (vector-ref vec i)))))
	    (if (< (x at) x2)
		(mouse-down (vector-ref vec i) at state)
		(loop x2 (+ i 1))))))))

(define-method draw-object ((self <tab-control>) win gc)
  (let* ((n (vector-length (tab-cells self)))
	 ((f <rect>) (frame self))
	 (bottom (size-height (frame self)))
	 (pixels (pixels (get-bound-colors (using-colormap self) 
					   *tab-colors*)))
	 (black (vector-ref pixels 0))
	 (non-sel-color (vector-ref pixels 1))
	 (sel-color (vector-ref pixels 2))
	 (white sel-color))
    ;;
    (define (sel? c)
      (eq? c (selected-tab self)))
    ;;
    (let loop ((i 0)
	       (x (origin-x f))
	       (did-left-graphic? #f))
      (if (< i n)
	  (let* ((c (vector-ref (tab-cells self) i))
		 (left-graphic-k (+ (if (sel? c) 0 4)
				    (if (eq? i 0) 0 2)))
		 (right-graphic-k (+ (if (sel? c) 0 4)
				     (if (< (+ i 1) n) 3 1))))
	    (if (not (tab-width c))
		;; fix up the cell's metrics
		(let ((sw (text-extents (using-font self) (tab-label c))))
		  (set-tab-width! c (- (+ 1 (with-module
					     usual-inlines
					     (bitwise-or
					      (+ $tab-graphic-width sw)
					      3)))
				       $tab-graphic-width))
		  (set-label-x! c $tab-graphic-width)))
	    #|(dm "tab: ~s at ~d for ~d: ~d ~d" c x (tab-width c)
		left-graphic-k right-graphic-k)|#
	    (if (or (sel? c)
		    (not did-left-graphic?))
		(x-composite (vector-ref *tab-graphics* left-graphic-k)
			     win
			     gc
			     x
			     0))
	    (if (sel? c)
		(set-gcontext-foreground! gc sel-color)
		(set-gcontext-foreground! gc non-sel-color))
	    (draw-rectangle win gc 
			    (+ x $tab-graphic-width)
			    0
			    (tab-width c)
			    $tab-graphic-height
			    #t)
	    ;; white line along top of tab
	    (set-gcontext-foreground! gc white)
	    (draw-line win gc
		       (+ x $tab-graphic-width)
		       1
		       (+ x $tab-graphic-width (tab-width c))
		       1)
	    ;; white line along bottom of tab
	    (draw-line win gc
		       (+ x $tab-graphic-width)
		       (- $tab-graphic-height 1)
		       (+ x $tab-graphic-width (tab-width c))
		       (- $tab-graphic-height 1))
	    ;; top black line
	    (set-gcontext-foreground! gc black)
	    (draw-line win gc
		       (+ x $tab-graphic-width)
		       0
		       (+ x $tab-graphic-width (tab-width c))
		       0)
	    ;; black line along bottom of tab
	    (if (not (sel? c))
		(draw-line win gc
			   (+ x $tab-graphic-width)
			   (- $tab-graphic-height 2)
			   (+ x $tab-graphic-width (tab-width c))
			   (- $tab-graphic-height 2)))
	    (draw-glyphs win gc
			 (+ x (label-x c))
			 (- bottom 5)
			 (tab-label c))
	    (if (sel? c)
		;; note that this is drawing into our successor's space
		(x-composite (vector-ref *tab-graphics* right-graphic-k)
			     win
			     gc
			     (+ $tab-graphic-width x (tab-width c))
			     0))
	    (loop (+ i 1)
		  (+ x (tab-width c) $tab-graphic-width)
		  (sel? c)))
      (begin
	(if (not did-left-graphic?)
	    (x-composite (vector-ref *tab-graphics* 5)
			 win
			 gc
			 x
			 0))
	(draw-line win gc 
		   x
		   (- $tab-graphic-height 1)
		   (- (size-width f) 1)
		   (- $tab-graphic-height 1)))))))

;;;

(define (make-tab-control win (frame <rect>) labels)
  (let* ((scrn (drawable-screen win))
	 (cmap (window-colormap win))
	 (w (create-window parent: win
			   x: (origin-x frame)
			   y: (origin-y frame)
			   width: (size-width frame)
			   height: (size-height frame)
			   background: (vector-ref
					(pixels (get-bound-colors
						 cmap
						 *tab-colors*))
					3)
			   event-mask: '(exposure
					 button-press
					 button-release)))
	  (c (make <tab-control>
		 frame: frame
		 tab-cells: '#()
		 selected-tab: #f
		 in-window: w
		 using-font: (get-property (drawable-display win)
					   'menu-font)
		 using-colormap: cmap))
	  (gc (create-gcontext drawable: win
                               foreground: (screen-black-pixel scrn)
                               background: (screen-white-pixel scrn))))
    (set-gcontext-font! gc (using-font c))
    (set-property! w
		   'exposure-thunk
		   (lambda ()
		     (clear-area w)
		     (draw-object c w gc)))
    (set-property! w 
		   'button-press
		   (lambda (win at state)
		     (mouse-down c at state)))
    (set-tab-cells! c (list->vector
		       (map (lambda (l)
			      (make <tab-cell>
				    parent: c
				    tab-label: l))
			    labels)))
    (map-window w)
    c))
