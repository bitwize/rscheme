(load "graphic-styles.scm")
(load "tools/util.scm")

(define (meta)
  '((draw)))

(define (draw)
  (define box1_height 20)
  (define box1_width 160)
  (define box1_y 10)
  (define box2_width 50)
  (define box2_height 30)
  (define box2_y 50)
  (define box2_x 55)
  (define box3_sub 9);
  (define box3_width 30)
  (define box3_x 135)
  (define box3_y 40)
  (define box4_y -30)
  (define box4_x 160)
  (define box4_width (/ box1_width 2))
  (define box4_height (/ box1_height 2))
  ;(format #t "foo\n")
  ;(rectstroke (make-rect 0 0 216 108)) ;bounding box,small
  (rectstroke 
   (make-rect 0 box1_y (/ box1_width 2) box1_height));page number box
  (rectstroke (make-rect (/ box1_width 2) box1_y 
			 (* 2 (/ box1_width 6)) box1_height));slot #box
  (rectstroke (make-rect (- box1_width (/ box1_width 6)) 
			 box1_y (/ box1_width 6) box1_height))
  (style-apply 'serif-font)
  (cshow (/ box1_width 4) (+ box1_y 7) "page number" )
  (cshow (+ (/ box1_width 2) (/ box1_width 6)) (+ box1_y 7) "slot number")
  (cshow (- box1_width (/ box1_width 12)) (+ box1_y 7) "11")
  ;begin box2
  (rectstroke (make-rect box2_x box2_y box2_width box2_height))
  (draw-long-brace base: (make-line (- box2_x 7) box2_y (- box2_x 7) 
				    (+ box2_height box2_y)));
  (arrowstroke
   (list (make-point (- (/ box1_width 4) 15) (+ 2 box1_y (/ box1_height 2)))
	 (make-point (- (/ box1_width 4) 15) (+ (/ box2_height 2) box2_y))
	 (make-point (- box2_x 12) (+ box2_y (/ box2_height 2))))
   radius: 5
   setback: 2)
  (moveto (+ 3 box2_x) (+ box2_y 3))
  (show "indirect=1")
  (moveto (+ 3 box2_x) (+ box2_y (/ box2_height 2)));
  (show "mem_addr")
  (stroke)
  (style-apply 'sans-font)
  (moveto (+ box2_x 3) (+ box2_y box2_height 3))
  (show "VMPR")
  (stroke)
;begin box3
  (rectstroke (make-rect box3_x (+ box3_sub box3_y) box3_width box3_sub))
  (rectstroke (make-rect box3_x (+ (* 2 box3_sub) box3_y) box3_width box3_sub))
  (rectstroke (make-rect box3_x (+ (* 3 box3_sub) box3_y) box3_width box3_sub))
  (rectstroke (make-rect box3_x (+ (* 4 box3_sub) box3_y) box3_width box3_sub))
  (rectstroke (make-rect box3_x (+ (* 5 box3_sub) box3_y) box3_width box3_sub))
  (rectstroke (make-rect box3_x (+ (* 6 box3_sub) box3_y) box3_width box3_sub))
  (rectstroke (make-rect box3_x (+ (* 7 box3_sub) box3_y) box3_width box3_sub))
  (rectstroke (make-rect box3_x (+ (* 8 box3_sub) box3_y) box3_width box3_sub))
  (arrowstroke 
   (list (make-point (+ box2_width box2_x) (+ (/ box2_height 2) box2_y))
	 (make-point 
	  (+ box2_width box2_x (/ box3_width 4)) 
	  (+ (/ box2_height 2) box2_y))
	 (make-point (+ box2_width box2_x (/ box3_width 4)) 
		     (+ (* 9 box3_sub) box3_y))
	 (make-point box3_x (+ (* 9 box3_sub) box3_y)))
   radius: 5
   setback: 0)
  (draw-long-brace base: (make-line (- box3_x 7) 
				    (+ box3_y (* 2 box3_sub)) 
				    (- box3_x 7) 
				    (+ box3_y (* 3 box3_sub))) 
		   radius: 2)
  (arrowstroke
   (list (make-point (* 3 (/ box1_width 4)) (- (+ box1_y (/ box1_height 2)) 5))
	 (make-point (* 3 (/ box1_width 4)) 0)
	 (make-point (+ box1_width 20) 0)
	 (make-point (+ box1_width 20) (+ box1_y box1_height 10))
	 (make-point (- (* 3 (/ box1_width 4)) 8) (+ box1_y box1_height 10))
	 (make-point (- (* 3 (/ box1_width 4)) 8) (+ box3_y (* 2.5 box3_sub)));
	 (make-point (- box3_x 12) (+ box3_y (* 2.5 box3_sub))))
   radius: 5
   setback: 0)
;begin box4
  (rectstroke (make-rect box4_x box4_y box4_width box4_height))
  (arrowstroke 
   (list (make-point (+ box3_width box3_x) (+ box3_y (* 2.5 box3_sub)))
	 (make-point (+ box4_x (/ box4_width 2)) (+ box3_y (* 2.5 box3_sub)))
	 (make-point (+ box4_x (/ box4_width 2)) (+ box4_height box4_y)))
   radius: 5
   setback: 0)
  (cshow (+ (/ box4_width 2) box4_x) 
	 (- box4_y (getfontsize 'sans-font)) 
	 "output object"))
