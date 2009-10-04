;;;
;;;  generate a related-event map
;;;

(define *orientation* 'horizontal)
(define *jig-dt* 5)    ; in "t" units (see *t-scale*)
(define *jig-du* 36)   ; in points
(define *t-scale* (/ 18 10))

(define *label-font* '(font "Times" "Roman" 12))
(define *mark-font* '(font "Times" "Roman" 12))

;;;

(define (event-jig t0 t1 u)
  (let* ((t-list (map (curry * *t-scale*)
		      (list (- t0 *jig-dt*)
			    t0
			    t1
			    (+ t1 *jig-dt*))))
	 (u-list (list 0
		       (* u *jig-du*)
		       (* u *jig-du*)
		       0))
	 (pts (case *orientation*
		((horizontal) 
		 (map (lambda (t u) `(x: ,t y: ,u)) t-list u-list))
		((vertical)
		 (map (lambda (t u) `(y: ,t x: ,u)) t-list u-list)))))
    `(path
      subpaths: ((subpath points: ,(map (lambda (p)
					  `(path-point ,@p))
					pts))))))

#|
(define-interactive (pjig view jig at)
  (interactive (owner) (minibuffer <string> "Jig? ") (click))
  (clear-current-selection! view)
  (let* ((j (read (open-input-string jig)))
	 (p (event-jig (car j) (cadr j) (caddr j))))
    (format #t "at ~s ==> ~s\n" at p)
    (paste-from-extern
     p
     (page-contents (view-page (underlying-object view)))
     at)
    (clear-all-areas (in-document view))))
|#

(define (flip-if-vertical a b)
  (case *orientation*
    ((horizontal) (values a b))
    ((vertical) (values b a))))

(load "arrow.scm")

(define (event-merge t u)
  (bind ((x0 y0 (flip-if-vertical (* t *t-scale*) 0))
	 (x1 y1 (flip-if-vertical (* t *t-scale*) (* u *jig-du*)))
	 (from (make-point x0 y0))
	 (to (make-point x1 y1))
	 (sgn (if (< u 0) -1 1)))
    (case *orientation*
      ((horizontal) (vline-with-arrow from to sgn))
      ((vertical) (hline-with-arrow from to sgn)))))

(define (event-mark t u label)
  (bind ((x y (flip-if-vertical (* t *t-scale*) (* u *jig-du*)))
	 (tic 2))
    `(group
      (path
       subpaths: ((subpath points: ((path-point x: ,x y: ,(+ y tic))
				    (path-point x: ,(+ x tic) y: ,y)
				    (path-point x: ,x y: ,(- y tic))
				    (path-point x: ,(- x tic) y: ,y))
			   closed?: #t))
       fill-color: black)
      (text origin-x: ,(+ x 2)
	    origin-y: ,(+ y 1.5)
	    string: ,label
	    font: ,*mark-font*))))

(define (eval-event-map emap)
  (cons 
   'group
   (map
    (lambda (eme)
      (case (car eme)
	((baseline)
	 (bind ((x0 y0 (flip-if-vertical (* (cadr eme) *t-scale*) 0))
		(x1 y1 (flip-if-vertical (* (caddr eme) *t-scale*) 0)))
	       `(line start-x: ,x0
		      start-y: ,y0
		      end-x: ,x1
		      end-y: ,y1)))
	((mark)
	 (apply event-mark (cdr eme)))
	((merge)
	 (apply event-merge (cdr eme)))
	((label)
	 (bind ((x y (flip-if-vertical (* (cadr eme) *t-scale*)
				       (* (caddr eme) *jig-du*))))
	   `(text origin-x: ,x
		  origin-y: ,y
		  string: ,(cadddr eme)
		  font: ,*label-font*)))
	((event-jig)
	 (apply event-jig (cdr eme)))))
    emap)))

(define *tmap*
  '(;;
    ;; A trace
    ;;
    (event-jig 15 90 1)
    (label 95 0.5 "store")
    (mark 70 1 "1'")
    ;;
    ;; B trace
    ;;
    (event-jig 15 165 -1)
    (label 170 -0.5 "store")
    (mark 50 -1 "1''")
    (merge 130 -1)
    (label 132 -0.5 "merge")
    (mark 130 -1 "2''")
    ;;
    ;; baseline
    ;;
    (baseline 0 200)
    (mark 0 0 "1")
    (mark 95 0 "2")
    (mark 170 0 "3")))

(define-interactive (tem view)
  (interactive (owner))
  (let ((p (eval-event-map *tmap*)))
    (pp p)
    (paste-from-extern p
		       (page-contents (view-page (underlying-object view)))
		       (make-size 100 100))
    (clear-all-areas (in-document view))))

(define (read-event-map port)
  (bind ((d g (make-new-eps-doc)))
    (paste-from-extern (eval-event-map (read port)) g (make-size 100 100))
    d))

