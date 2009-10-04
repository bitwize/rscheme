
(define-class <progress-bar> (<view>)
  (font init-function: application-font)
  (amount-completed type: <real> init-value: 0)
  (pixels init-value: #f)
  (bar-width type: <fixnum> init-value: 0))

(define-method initialize ((self <progress-bar>))
  (set-pixels! self
	       (get-pixels-resource 'button.shades
				    (for-client (window self)))))

(define-macro (make-progress-bar . args)
  `(make-view class: <progress-bar> 
	      ,@args))

(define $sunken-bezel
  '(3 bottom right
    1 left top))

(define (compute-bar-width (self <progress-bar>))
  (max 0
       (min (inexact->exact (round (* (amount-completed self)
				      (size-width (frame self)))))
	    (- (size-width (frame self)) 4))))

(define-method draw-self ((self <progress-bar>) rects)
  (dm 340 "draw-progress-bar")
  (let ((win (in-window self))
	(gc (use-gcontext self))
	(pix (pixels self))
	(w (compute-bar-width self)))
    (set-bar-width! self w)
    (draw-bezeled win gc pix $sunken-bezel 0 0
		  (size-width (frame self))
		  (size-height (frame self)))
    (if (> w 4)
	(draw-bezeled win gc pix $button-bezel 2 2
		      w
		      (- (size-height (frame self)) 4)))
    (if (> w 36)
	(begin
	  (set-gcontext-foreground! gc (vector-ref pix 0))
	  (set-gcontext-font! gc (font self))
	  (draw-glyphs win gc
		       8
		       (- (size-height (frame self)) 6)
		       (string-append
			(number->string 
			 (inexact->exact 
			  (floor
			   (* 100 (amount-completed self)))))
			"%"))))))

(define-method set-value! ((self <progress-bar>) value)
  (let ((old-pct (round (* (amount-completed self) 100)))
	(new-pct (round (* value 100))))
    (set-amount-completed! self (min 1 (max value 0)))
    (if (or (not (eq? (compute-bar-width self) (bar-width self)))
	    (not (= old-pct new-pct)))
	(update self))))
