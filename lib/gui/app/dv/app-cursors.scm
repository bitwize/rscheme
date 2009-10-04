
(define-macro (define-cursor-shape name . lines)
  (let ((cursor-points '())
	(mask-points '())
	(y -1)
	(hot-x 0)
	(hot-y 0))
    ;
    (for-each
     (lambda ((line <string>))
       (set! y (+ y 1))
       (for-each
	(lambda (x)
	  (case (string-ref line (* x 2))
	    ((#\. #\+))
	    ((#\w #\W) (set! mask-points (cons* x y mask-points)))
	    ((#\b #\B) (set! mask-points (cons* x y mask-points))
		       (set! cursor-points (cons* x y cursor-points))))
	  (case (string-ref line (* x 2))
	    ((#\+ #\W #\B)
	     (set! hot-x x)
	     (set! hot-y y))))
	(range (quotient (string-length line) 2))))
     lines)
    ;
    `(define (,name)
       (values ,(string-length (car lines))
	       ,(length lines)
	       ,hot-x ,hot-y
	       ',cursor-points
	       ',mask-points))))

(define-cursor-shape arrow-cursor
  ". . . . . . . . . . . . . . . ."
  ". W . . . . . . . . . . . . . ."
  ". w w . . . . . . . . . . . . ."
  ". w b w . . . . . . . . . . . ."
  ". w b b w . . . . . . . . . . ."
  ". w b b b w . . . . . . . . . ."
  ". w b b b b w . . . . . . . . ."
  ". w b b b b b w . . . . . . . ."
  ". w b b b w w w w . . . . . . ."
  ". w b w w w . . . . . . . . . ."
  ". w w . w w . . . . . . . . . ."
  ". w . . . w w . . . . . . . . ."
  ". . . . . w w . . . . . . . . ."
  ". . . . . . w w . . . . . . . ."
  ". . . . . . w w . . . . . . . ."
  ". . . . . . . . . . . . . . . .")

(define-cursor-shape plus-cursor
  ". . . . . . . . . . . . . . . ."
  ". . . . . . . . . . . . . . . ."
  ". . . . . . . w . . . . . . . ."
  ". . . . . . . w b . . . . . . ."
  ". . . . . . . w b . . . . . . ."
  ". . . . . . . w b . . . . . . ."
  ". . . . . . . w b . . . . . . ."
  ". . w w w w w W w w w w w . . ."
  ". . . b b b b w b b b b b b . ."
  ". . . . . . . w b . . . . . . ."
  ". . . . . . . w b . . . . . . ."
  ". . . . . . . w b . . . . . . ."
  ". . . . . . . w b . . . . . . ."
  ". . . . . . . . b . . . . . . ."
  ". . . . . . . . . . . . . . . ."
  ". . . . . . . . . . . . . . . .")

(define-cursor-shape plus-dot-cursor
  ". . . . . . . . . . . . . . . ."
  ". . . . . . . w . . . . . . . ."
  ". . . . . . . . . . . . . . . ."
  ". . . . . . . w . . . . . . . ."
  ". . . . . . . w b . . . . . . ."
  ". . . . . . . w b . . . . . . ."
  ". . . . . . . w b . . . . . . ."
  "w . w w w w w W w w w w w . w ."
  ". . . b b b b w b b b b b b . ."
  ". . . . . . . w b . . . . . . ."
  ". . . . . . . w b . . . . . . ."
  ". . . . . . . w b . . . . . . ."
  ". . . . . . . w b . . b b b b b"
  ". . . . . . . . b . . b . . . b"
  ". . . . . . . w . . . b . . . b"
  ". . . . . . . . . . . b b b b b")

(define-cursor-shape pen-cursor
  ". . . . . . . W . . . . . . . ."
  ". . . . . . w b w . . . . . . ."
  ". . . . . . w b w . . . . . . ."
  ". . . . . w b w b w . . . . . ."
  ". . . . . w b w b w . . . . . ."
  ". . . . w b b w b b w . . . . ."
  ". . . . w b b b b b w . . . . ."
  ". . . . w b b b b b w . . . . ."
  ". . . w b b b w b b b w . . . ."
  ". . . w b b w b w b b w . . . ."
  ". . . w b b b w b b b w . . . ."
  ". . . w b b b b b b b w . . . ."
  ". . . . w b b b b b w . . . . ."
  ". . . . . w w w w w . . . . . ."
  ". . . . . w w w w w . . . . . ."
  ". . . . . w w w w w . . . . . .")

(define-cursor-shape adjust-anchor-cursor
  "W . . . . . . . . . . . . . . ."
  ". w w w . . . . . . . . . . . ."
  ". w b b w w w . . . . . . . . ."
  ". w b . b b b w w w . . . . . ."
  ". . w b . . . b b b w w w . . ."
  ". . w b . . . . . . b b b . . ."
  ". . w b . . . . . . . . . . . ."
  ". . . w b . . . . . . . . . . ."
  ". . . w b . . . . . . . . . . ."
  ". . . w b . . . . . . . . . . ."
  ". . . . w b . . . . . . . . . ."
  ". . . . w b . . . . . . . . . ."
  ". . . . w b . . . . . . . . . ."
  ". . . . . . . . . . . . . . . ."
  ". . . . . . . . . . . . . . . ."
  ". . . . . . . . . . . . . . . .")

(define-cursor-shape zoom-cursor
  ". . . . w w w w . . . . . . . ."
  ". . w w b b b b w w . . . . . ."
  ". w b b b b b b b b w . . . . ."
  ". w b b b w w b b b w . . . . ."
  "w b b b b w w b b b b w . . . ."
  "w b b w w W w w w b b w . . . ."
  "w b b w w w w w w b b w . . . ."
  "w b b b b w w b b b b w . . . ."
  ". w b b b w w b b b w . . . . ."
  ". w b b b b b b b b w . . . . ."
  ". . w w b b b b w w w w . . . ."
  ". . . . w w w w . . w w w . . ."
  ". . . . . . . . . . . w w w . ."
  ". . . . . . . . . . . . w w w ."
  ". . . . . . . . . . . . . w w .")

(define-cursor-shape zoomo-cursor
  ". . . . w w w w . . . . . . . ."
  ". . w w b b b b w w . . . . . ."
  ". w b b b b b b b b w . . . . ."
  ". w b b b b b b b b w . . . . ."
  "w b b b b b b b b b b w . . . ."
  "w b b w w W w w w b b w . . . ."
  "w b b w w w w w w b b w . . . ."
  "w b b b b b b b b b b w . . . ."
  ". w b b b b b b b b w . . . . ."
  ". w b b b b b b b b w . . . . ."
  ". . w w b b b b w w w w . . . ."
  ". . . . w w w w . . w w w . . ."
  ". . . . . . . . . . . w w w . ."
  ". . . . . . . . . . . . w w w ."
  ". . . . . . . . . . . . . w w .")

(define-cursor-shape text-cursor
  ". . . . w w . . . w w . . . . ."
  ". . . . . b w . w . b b . . . ."
  ". . . . . . . w . b . . . . . ."
  ". . . . . . . w b . . . . . . ."
  ". . . . . . . w b . . . . . . ."
  ". . . . . . . w b . . . . . . ."
  ". . . . . . . w b . . . . . . ."
  ". . . . . . . w b . . . . . . ."
  ". . . . . . . w b . . . . . . ."
  ". . . . . . . w b . . . . . . ."
  ". . . . . . . w b . . . . . . ."
  ". . . . . . w W w . . . . . . ."
  ". . . . . . . w b . . . . . . ."
  ". . . . . . w . w . . . . . . ."
  ". . . . w w . b . w w . . . . ."
  ". . . . . b b . . . b b . . . .")

(define (make-mask (for <x-window>) w h pts)
  (let* ((m (create-pixmap width: w height: h depth: 1 drawable: for))
         (gc (create-gcontext drawable: m
                              foreground: 0
                              background: 0)))
    (draw-rectangle m gc 0 0 w h #t)
    (set-gcontext-foreground! gc 1)
    (draw-points m gc pts)
    (free-gcontext gc)
    m))

(define (make-cursor (for <x-window>) proc fg-color bg-color)
  (bind ((w h hot-x hot-y fg-points bg-points (proc))
	 (c (create-pixmap width: w height: h depth: 1 drawable: for))
	 (m (create-pixmap width: w height: h depth: 1 drawable: for))
	 (gc (create-gcontext drawable: c
		              foreground: 0
		              background: 0)))
    (draw-rectangle c gc 0 0 w h #t)
    (draw-rectangle m gc 0 0 w h #t)
    (set-gcontext-foreground! gc 1)
    ;
    (draw-points c gc fg-points)
    (draw-points m gc bg-points)
    (free-gcontext gc)
    ;
    (create-cursor source: c
		   mask: m
		   x: hot-x
		   y: hot-y
		   foreground: fg-color
		   background: bg-color)))

;;;

(define (make-mode-cursors win)
  (define (def-cursor name data c1 c2)
    (cons name (make-cursor win data c1 c2)))
  ;;
  (let ((dark-blue (make-color red: 0.3 green: 0.3 blue: 0.5))
         (light-blue (make-color red: 0.667 green: 0.667 blue: 1))
         (v-light-blue (make-color red: 0.8 green: 0.8 blue: 1))
         (black (make-color red: 0 green: 0 blue: 0)))
    ;;
    (list (def-cursor 'select arrow-cursor dark-blue black)
          (def-cursor 'snap   arrow-cursor light-blue black)
          (def-cursor 'zoom   zoom-cursor  v-light-blue black)
          (def-cursor 'zoomo  zoomo-cursor v-light-blue black)
          (def-cursor 'pen    pen-cursor   v-light-blue black)
          (def-cursor 'place-text text-cursor v-light-blue black)
          (def-cursor 'draw-line plus-cursor light-blue black)
          (def-cursor 'draw-box  plus-dot-cursor light-blue black))))

;;;

(define (gray-stipple screen)
  (or (get-property screen 'gray-stipple #f)
      (let ((m (make-mask (screen-root screen) 2 2 '(0 0 1 1))))
        (set-property! screen 'gray-stipple m)
        m)))
