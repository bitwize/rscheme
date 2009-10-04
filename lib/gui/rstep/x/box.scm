
(define-constant $chisel-bezel
  '(3 bottom right
    1 top left
    1 bottom right
    3 top left))

(define-class <box> (<view>)
  (title type: <string> init-value: "")
  (font #|type: <x-font>|# init-function: application-font)
  (bezel-pattern init-value: $chisel-bezel)
  (bezel-inset init-value: 3)
  (pixels init-value: #f))

(define-method initialize ((self <box>))
  (set-pixels! self
	       (get-pixels-resource 'button.shades
				    (for-client (window self)))))

(define-macro (make-box . args)
  `(make-view class: <box>
	      ,@args))

#|
	frame.x
 frame.y +--------------------------------------+  ---\	 (outer)
         |                                      |      > bezel-inset
         |  ************* L A B E L **********  |  ---X
         |  *                                *  |      > (inner)
         |  *  +--------------------------+  *  |  ---/	 bezel-inset
         |  *  |inner coord system        |  *  |
         |  *  |                          |  *  |
         |  *  |                          |  *  |
         |  *  |                          |  *  |
         |  *  |                          |  *  |
         |  *  |                          |  *  |
         |  *  |                          |  *  |
         |  *  +--------------------------+  *  |  ---\	 (inner)
         |  *                                *  |      > bezel-inset
         |  **********************************  |  ---/
         |                                      |
         +--------------------------------------+
|#

(define-method content-origin ((self <box>))
  (let ((i (bezel-inset self)))
    ; the `+2' is the bezel thickness
    ; the `*2' is the inner and outer bezel insets
    (let ((d (+ 2 (* i 2))))
      (make-point d d))))

(define-method draw-self ((self <box>) rects)
  (let ((i (bezel-inset self))
	(w (size-width (frame self)))
	(h (size-height (frame self))))
    (draw-bezeled (in-window self)
		  (use-gcontext self)
		  (pixels self)
		  (bezel-pattern self)
		  i
		  i
		  (- w (* i 2))
		  (- h (* i 2)))))



