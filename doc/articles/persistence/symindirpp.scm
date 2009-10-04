(load "graphic-styles.scm")

(define (meta)
  '((draw 
     (design-size small))))

(define (draw)
  (style-apply 'medium-stroke)
  (style-apply 'serif-font)
  (define *height* 27)
  (moveto 0 0)
  (lineto 216 0)
  (lineto 216 *height*)
  (lineto 0 *height*)
  (closepath)
  (moveto 45 0)
  (lineto 45 *height*)
  (moveto 90 0)
  (lineto 90 *height*)
  (stroke)
  (moveto 9 9)
  (show "count")
  (moveto 60 9)
  (show "len")
  (moveto 130 9)
  (show "text data")
  (draw-dimen-labels base: (make-line 90 36 216 36)
		     
		     setback: 0
		     title: "len bytes"
		     title-font: (get-text-font 'dimen-font))
  (draw-dimen-labels base: (make-line 45 46 216 46)
		     
		     setback: 0
		     title: "repeat \140count' times"
		     title-font: (get-text-font 'dimen-font)))
