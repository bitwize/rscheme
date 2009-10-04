
;; this is wrong... it does the integral of the square of dlength

(define-method path-length ((self <bezier-curve>))
  (let ((x0 (x (start-point self)))
	(y0 (y (start-point self)))
	(x1 (x (first-handle self)))
	(y1 (y (first-handle self)))
	(x2 (x (second-handle self)))
	(y2 (y (second-handle self)))
	(x3 (x (end-point self)))
	(y3 (y (end-point self))))
    (* (/ 3 5) 
       (+ (* 3 (expt x0 2)) (* -3 x0 x1) (* 2 (expt x1 2)) (* -2 x0 x2)
	  (* x1 x2) (* 2 (expt x2 2)) (* -1 x0 x3) (* -2 x1 x3) (* -3 x2 x3) 
	  (* 3 (expt x3 2)) (* 3 (expt y0 2)) (* -3 y0 y1) (* 2 (expt y1 2))
	  (* -2 y0 y2) (* y1 y2) (* 2 (expt y2 2)) (* -1 y0 y3) (* -2 y1 y3)
	  (* -3 y2 y3) (* 3 (expt y3 2))))))
