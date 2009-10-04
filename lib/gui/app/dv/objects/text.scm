,(use graphics.charpath)

;;;    <text-graphic>

(define-class <text-graphic> (<leaf-object>)
  (text-runs type: <vector> init-value: '#())
  (text-width-cache init-value: #f))

(define-class <text-run> (<object>)
  (text-run-string type: <string>)
  (text-run-font type: <text-font>))

;;;

(define-method status-line-when-sel ((self <text-graphic>))
  (format #f "Text ~d" (id self)))

;;;    drawing

(define (text-width (self <text-graphic>))
  (or (text-width-cache self)
      (let ((w 0))
	(vector-for-each
	 (lambda (run)
	   (set! w (+ w (string-width (text-run-font run)
				      (text-run-string run)))))
	 (text-runs self))
	(set-text-width-cache! self w)
	w)))

(define (enumerate-text-runs (self <text-graphic>) dev)
  (let* ((w (text-width self))
	 (pt (case (get-property self 'alignment 'left)
	       ((left) 0)
	       ((right) (- w))
	       ((center) (/ w -2)))))
    (moveto dev (make-point pt 0))
    ;;
    (vector-for-each
     (lambda (run)
       (setfont dev (text-run-font run))
       (show dev (text-run-string run)))
     (text-runs self))
    (values w pt)))

(define (text-alignment self)
  (get-property self 'alignment 'left))

(define-method pick-list* ((self <text-graphic>) pt ctm)
  (let* ((w (text-width self))
	 (p (case (text-alignment self)
	      ((left) (list $zero-point (make-point w 0)))
	      ((center) (list (make-point (/ w -2) 0) (make-point (/ w 2) 0)))
	      ((right) (list (make-point (- w) 0) $zero-point)))))
    (pick-on-path self pt ctm p)))

(define-method paint-artwork* ((self <text-graphic>) dev)
  (bind ((w x (enumerate-text-runs self dev)))
    (moveto dev (make-point x 0))
    (lineto dev (make-point (+ x w) 0))
    (stroke dev)))

(define-method paint-object* ((self <text-graphic>) dev)
  (enumerate-text-runs self dev))

(define-method accum-handles ((self <text-graphic>) accum)
  (accum self (make-point 0 0) 0))

(define-method start-active-drag ((self <text-graphic>) 
				  (in-view <open-view>)
				  (initial-pt <point>))
  (generic-dragger self in-view -1 initial-pt))


(define-method start-active-drag-handle ((self <text-graphic>)
					 (in-view <open-view>)
					 handle-id
					 (initial-pt <point>))
  (generic-dragger self in-view handle-id initial-pt))

(define-method make-adjuster ((self <text-graphic>) handle-id o->d ithunk)
  (let ((p0 (origin self)))
    (mha
     ;; the point `p' is in object coordinates.  Hence, the shift 
     ;; amount is EQUAL to the new position in object coords
     (lambda ((p <point>))
       ;(point- p p0)
       (point->size p))
     (let* ((w (text-width self))
            (vp (case (get-property self 'alignment 'left)
                  ((left) (vector $zero-point (make-point w 0)))
                  ((right) (vector (make-point (- w) 0) $zero-point))
                  ((center) (vector (make-point (/ w -2) 0)
                                    (make-point (/ w 2) 0))))))
       (lambda (shift)
         (let ((pts (explode-coords
                     (vector->list
                      (map-object-points shift
                                         ;; `p0' (the origin) is already 
                                         ;; taken into account because o->d
                                         ;; translates from OBJECT coordinates
                                         ;; to DEVICE coordinates
                                         o->d
                                         vp)))))
           (lambda (win gc)
             (draw-lines win gc pts))))))))

;;;

(define-interactive (place-text-mode view)
  (interactive (owner))
  (set-major-mode! view (get-major-mode 'place-text)))

(graphic-set-key #\a place-text-mode)

(define (place-text-button-press (in-view <open-view>)
				 (at <point>)
				 modifier-state)
  (bg
   (let* ((str (read-from-minibuffer "String: "))
	  (par (page-contents (view-page (underlying-object in-view))))
	  (at (window->user-point in-view at))
	  (txt (make <text-graphic>
		     text-runs: (vector 
				 (make <text-run>
				       text-run-font: (active-font
						       (current-client))
				       text-run-string: str))
		     in-document: (in-document par)
		     parent-object: par
		     origin: at
		     graphic-bounding-box: (make-rect 0 0 0 0))))
     (clear-all-areas (in-document in-view))
     (do-select in-view txt 0))))

(add-major-mode!
 (make <major-mode>
       name: 'place-text
       button-press-proc: place-text-button-press))

;;;

(define-method externalize ((self <text-font>))
  `(font ,(font-name self) ,(font-member self) ,(font-size self)))

(define (paste-font-from-extern extern group offset)
  (apply get-text-font (cdr extern)))

;;;

(define-method externalize ((self <text-graphic>))
  (let ((r (vector-ref (text-runs self) 0)))
    `(text origin-x: ,(x (origin self))
	   origin-y: ,(y (origin self))
	   string: ,(text-run-string r)
	   font: ,(externalize (text-run-font r)))))

(define (paste-text-from-extern extern group offset)
  (apply (lambda (#key (origin-x default: 0)
		       (origin-y default: 0)
		       (alignment default: #f)
		       string font)
	   (let* ((fnt (paste-font-from-extern font group offset))
		  (g (make <text-graphic>
			   in-document: (in-document group)
			   parent-object: group
			   graphic-bounding-box: $zero-rect
			   origin: (point+ (make-point origin-x origin-y)
					   offset)
			   text-runs: (vector
				       (make <text-run>
					     text-run-font: fnt
					     text-run-string: string)))))
	     (if alignment 
		 (if (memq alignment '(left center right))
		     (set-property! g 'alignment alignment)
		     (wm "text: ignored invalid text alignment of '~s'" 
			 alignment)))
	     (recompute-graphic-bounding-box! g)
	     g))
	 (cdr extern)))

(define-method recompute-graphic-bounding-box! ((self <text-graphic>))
  (let* ((w (text-width self))
	 (h (apply max (map (lambda (r)
			      (font-size (text-run-font r)))
			    (vector->list (text-runs self)))))
	 (ox (case (text-alignment self)
	       ((left) 0)
	       ((right) (- w))
	       ((center) (/ w -2))))
	 (r (make-rect ox 0 w h)))
    (set-graphic-bounding-box! self r)
    r))

(define-method explode->list ((self <text-graphic>))
  (unlink-graphic self)
  (let ((lst '())
        (at (origin self)))
    (vector-for-each
     (lambda ((run <text-run>))
       (bind ((xr dx dy (charpath->path-extern-rep
                         (text->path (get-ps-font-name (text-run-font run))
                                     (font-size (text-run-font run))
                                     (text-run-string run)))))
         (paste-from-extern xr 
                            (parent-object self) 
                            (point->size at))
         (set! at (point+ at (make-size dx dy)))))
     (text-runs self))
    (reverse lst)))

(define (charpath->path-extern-rep p)
  (define (mkppl h)             ; make path-point (line)
    `(path-point x: ,(cadr h)
                 y: ,(caddr h)))
  (define (mkppc h)             ; make path-point (curve)
    `(path-point x: ,(list-ref h 5)
                 y: ,(list-ref h 6)
                 in-dx: ,(- (list-ref h 3) (list-ref h 5))
                 in-dy: ,(- (list-ref h 4) (list-ref h 6))))
  (define (fixpp pp h)
    (let* ((out-dx (- (cadr h) (cadr (memq 'x: pp))))
           (out-dy (- (caddr h) (cadr (memq 'y: pp)))))
      (set-cdr! pp (cons* 'out-dx: out-dx
                          'out-dy: out-dy
                          (cdr pp)))
      (values)))
  (let loop ((src p)
             (open '())
             (closed '()))
    (if (null? src)
        (values `(path fill-color: black
                       subpaths: ,(reverse closed))
                (if (pair? open) (cadr (memq 'x: (car open))) #f)
                (if (pair? open) (cadr (memq 'y: (car open))) #f))
        (let ((h (cons (caar src) (reverse (cdar src)))))
          (case (car h)
            ((moveto)
             (assert (null? open))      ; can't handle this yet
             (loop (cdr src)
                   (cons (mkppl h) '())
                   closed))
            ((lineto)
             (loop (cdr src)
                   (cons (mkppl h) open)
                   closed))
            ((curveto)
             (assert (pair? open))      ; nocurrentpoint
             (fixpp (car open) h)
             (loop (cdr src)
                   (cons (mkppc h) open)
                   closed))
            ((closepath)
             (loop (cdr src)
                   '() 
                   (cons `(subpath 
                           closed?: #t
                           points: ,(reverse open))
                         closed))))))))
