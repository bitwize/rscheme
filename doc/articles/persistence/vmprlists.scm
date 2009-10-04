(load "graphic-styles.scm")
(load "tools/util.scm")
(load "tools/cloud.scm")
(define-font-style state-font (default-font) family: "Courier" weight: 'bold size: 10)

(define (meta)
  '((draw)))

(define (draw-vmpr texta textb x y size)
  (gsaved
   (translate (make-point x y))
   (let ((w (+ 2 (/ size 2))))
     (rectstroke (make-rect 0 0 w size))
     (rectstroke (make-rect 0 (* 2 (/ size 7)) w (/ size 7)))
     (rectstroke (make-rect 0 (* 3 (/ size 7)) w (/ size 7)))
     (rectstroke (make-rect 0 (* 4 (/ size 7)) w (/ size 7)))
     (style-apply 'helvetica-font)
     (cshow (/ w 2) (+ (* (/ size 7) 5)(/ size 14)) texta)
     (style-apply 'state-font)
     (cshow (/ w 2) (/ size 14) textb))))

(define (draw)
  (define size 70)
  (style-apply 'medium-stroke)
  (draw-vmpr "201" "loaded" 90 50 size)
  (draw-vmpr "202" "dirty" 180 50 size)
  (draw-vmpr "203" "rsrvd" 270 50 size)
  (draw-vmpr "204" "rsrvd" 360 50 size)
  (arrowstroke
   (list (make-point 40 72)
	 (make-point 50 72)
	 (make-point 50 30)
	 (make-point 170 30)
	 (make-point 170 (+ 50 (* 5 (/ size 14))))
	 (make-point 180 (+ 50 (* 5 (/ size 14)))))
   radius: 5
   setback: (/ (getlinewidth 'medium-stroke) 2))
  (style-apply 'serif-font)
  (rshow 39 (- 72 (/ (getfontsize 'serif-font) 4)) "dirty")
  (rshow 39 (+ 72 (getfontsize 'serif-font)) "loaded")
  (rshow 39 (+ 72 (* 2 (getfontsize 'serif-font))) "reserved")
  (arrowstroke
   (list (make-point 39 (+ 72 (* 1.25 (getfontsize 'serif-font))))
	 (make-point 90 (+ 50 (* 7 (/ size 14)))))
   radius: 1000000
   setback: (/ (getlinewidth 'medium-stroke) 2))
  (arrowstroke
   (list (make-point 39 (+ 72 (* 2.25 (getfontsize 'serif-font))))
	 (make-point 90 (+ 72 (* 2.25 (getfontsize 'serif-font)))))
   radius: 1
   setback: (/ (getlinewidth 'medium-stroke) 2))
  (cloudpath (make-rect 85 140 300 72))
  (stroke)
  (style-apply 'helvetica-font)
  (rshow 180 172 "201")
  (moveto 185 182)
  (show "202")
  (moveto 225 172)
  (show "203")
  (moveto 300 182)
  (show "204")
;draw arrow 1
  (bind ((p0 area
  (compute-arrow-path (make-point 110 150) (make-point 90 (+ 50 size)) setback-line: (make-line 85 (+ size 50) 115 (+ size 50)) setback-linewidth: (getlinewidth 'medium-stroke))))
  (areafill area)
  (moveto 165 175)
  (curveto (make-point 110 150) (make-point 110 150) p0)(stroke))
  (bind ((p0 area
  (compute-arrow-path (make-point 285 170) (make-point 361 (+ 50 size)) setback-line: (make-line 170 (+ 50 size) 300 (+ 50 size)) setback-linewidth: (getlinewidth 'medium-stroke))))
  (areafill area)
  (moveto 300 182)
  (curveto (make-point 285 170) (make-point 330 155) p0)(stroke))
  (bind ((p0 area
  (compute-arrow-path (make-point 211 150) (make-point 271 (+ 50 size)) setback-line: (make-line 100 (+ size 50) 400 (+ 50 size)) setback-linewidth: (getlinewidth 'medium-stroke))))
  (areafill area)
  (moveto 225 172)
  (curveto (make-point 210 160) (make-point 211 150) p0)(stroke))
  (bind ((p0 area
  (compute-arrow-path (make-point 190 150) (make-point 180 (+ 50 size)) setback-line: (make-line 100 (+ 50 size) 400 (+ 50 size)) setback-linewidth: (getlinewidth 'medium-stroke))))
  (areafill area)
  (moveto 185 182)
  (curveto (make-point 190 150) (make-point 190 150) p0))
  (stroke)
  (arrowstroke
   (list (make-point 120 (+ 50 (* 9 (/ size 14))))
	 (make-point 180 (+ 50 (* 9 (/ size 14)))))
   radius: 100
   setback: (/ (getlinewidth 'medium-stroke) 2))
  (arrowstroke
   (list (make-point 210 (+ 50 (* 9 (/ size 14))))
	 (make-point 270 (+ 50 (* 9 (/ size 14)))))
   radius: 1000
   setback: (/ (getlinewidth 'medium-stroke) 2))
  (arrowstroke
   (list (make-point 300 (+ 50 (* 9 (/ size 14))))
	 (make-point 360 (+ 50 (* 9 (/ size 14)))))
   radius: 1000
   setback: (/ (getlinewidth 'medium-stroke) 2))
  (arrowstroke
   (list (make-point 120 (+ 50 (* 7 (/ size 14))))
	 (make-point 180 (+ 50 (* 7 (/ size 14)))))
   radius: 100
   setback: (/ (getlinewidth 'medium-stroke) 2))
  (moveto 210 (+ 50 (* 7 (/ size 14))))
  (lineto 240 (+ 50 (* 7 (/ size 14))))
  (let ((location (make-point 240 (+ 50 (* 7 (/ size 14))))))
    (ground location))
  (moveto 210 (+ 50 (* 5 (/ size 14))))
  (lineto 230 (+ 50 (* 5 (/ size 14))))
  (let ((location (make-point 230 (+ 50 (* 5 (/ size 14))))))
    (ground location))
  (moveto 390 (+ 50 (* 9 (/ size 14))))
  (lineto 415 (+ 50 (* 9 (/ size 14))))
  (let ((location (make-point 415 (+ 50 (* 9 (/ size 14))))))
    (ground location)))

(define (ground location)
  (gsaved
   (translate location)
   (lineto 0 -5)
   (moveto -5 -5)
   (lineto 5 -5)
   (moveto -3 -8)
   (lineto 3 -8)
   (moveto -1 -11)
   (lineto 1 -11)
   (stroke)))


