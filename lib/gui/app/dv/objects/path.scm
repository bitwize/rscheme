
(define-class <path-graphic> (<leaf-object>)
  (subpaths type: <vector> init-value: '#())
  (insideness type: <symbol> init-value: 'non-zero)
  (dashing init-value: #f))

(define-class <subpath> (<object>)
  (in-path type: <path-graphic>)
  (closed? type: <boolean> init-value: #f)
  (path-points type: <vector> init-value: '#()))

(define-class <path-point> (<object>)
  (in-subpath type: <subpath>)
  (constraint type: <symbol> init-value: 'corner)   ; in (smooth curve corner)
  (position type: <point>)
  (in-handle type: <size> init-value: $zero-size)   ; relative to posn
  (out-handle type: <size> init-value: $zero-size)) ; relative to posn

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Painting
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (zero-size? s)
  (and (zero? (dx s)) (zero? (dy s))))

(define (pick-on-subpath (self <subpath>) owner pt ctm)
  (pick-on-path owner pt ctm
		(vector->list
                 (build-subpath-points self $identity-transform))))

		 ;(vector-map position (path-points self))

(define-method pick-list* ((self <path-graphic>) pt ctm)
  (let ((r '()))
    (vector-for-each
     (lambda (sp)
       (let ((psp (pick-on-subpath sp self pt ctm)))
	 (if (not (null? psp))
	     (set! r (append psp r)))))
     (subpaths self))
    r))

(define-method paint-artwork* ((self <path-graphic>) dev)
  (build-path-on-device self dev)
  (stroke dev))

(define-method paint-object* ((self <path-graphic>) dev)
  (with-gstate-saved
   dev
   (lambda ()
     (if (dashing self)
         (setdash dev (car (dashing self)) (cdr (dashing self))))
     (build-path-on-device self dev)
     ;
     (let ((fc (get-property self 'fill-color #f))
	   (sc (get-property self 'stroke-color #f)))
       (if fc
           (with-gstate-saved
            dev
            (lambda ()
              (closepath dev)
              (setcolor dev (device-color dev fc))
              (fill dev))))
       ; (not (eq? sc 'none))  ;what is this supposed to support?
       (if sc
           (begin
             (setcolor dev (device-color dev sc))
             (if (get-property self 'stroke-width #f)
                 (setlinewidth dev (get-property self 'stroke-width)))
	     (stroke dev)))))))

;;;
;;;  Build a vector of points which starts at the beginning of the
;;;  subpath and ends at the end
;;;

(define (build-subpath-points (sp <subpath>) ctm)
  (apply
   vector-append
   (vector 
    (transform (position (vector-ref (path-points sp) 0)) ctm))
   (map
    (lambda (k)
      (path-fragment-points
       (vector-ref (path-points sp) (- k 1))
       (vector-ref (path-points sp) k)
       ctm))
    (cdr (range (vector-length (path-points sp)))))))

(define (build-path-on-device (self <path-graphic>) dev)
  (vector-for-each
   (lambda ((sp <subpath>))
     (let (((ppv <vector>) (path-points sp)))
       (moveto dev (position (vector-ref ppv 0)))
       (let loop ((i 1)
		  (prev-pp (vector-ref ppv 0)))
	 (if (< i (vector-length ppv))
	     (let ((pp (vector-ref ppv i)))
	       (if (or (not (zero-size? (out-handle prev-pp)))
		       (not (zero-size? (in-handle pp))))
		   (curveto dev
			    (point+ (position prev-pp)
				    (out-handle prev-pp))
			    (point+ (position pp)
				    (in-handle pp))
			    (position pp))
		   (lineto dev (position pp)))
	       (loop (+ i 1) pp))
	     (begin
	       ;; all done with subpath... close it if needed
	       (if (closed? sp)
		   (closepath dev)))))))
   (subpaths self)))

;;; handle IDs are structured as follows:
;;;
;;;                         <--12 bits--> 2 bits
;;;      +-----------------+-------------+----+
;;;      |  subpath #      | path point# |grip|
;;;      +-----------------+-------------+----+
;;;
;;;  where grip = 00 for main point, 01 for in-handle, 10 for out-handle
;;;

;;;  return three values:  <subpath> <path-point> <symbol>
;;;  where <symbol> is one of: position, in, out

(define (handle->pathpoint (self <path-graphic>) id)
  (let* ((k-subpath (logical-shift-right id 14))
         (k-path-point (bitwise-and (logical-shift-right id 2) #xFFF))
         (k-grip (bitwise-and id #b11))
         (subpath (vector-ref (subpaths self) k-subpath))
         (pathpoint (vector-ref (path-points subpath) k-path-point)))
    (values subpath
            pathpoint
            (vector-ref '#(position in out) k-grip))))

(define (pathpoint->handle (self <path-point>))
  (let ((k-path-point (vmemq self (path-points (in-subpath self))))
        (k-subpath (vmemq (in-subpath self)
                          (subpaths (in-path (in-subpath self))))))
    (dm "pathpoint->handle ~s => ~s ~s" self k-subpath k-path-point)
    (+ (logical-shift-left k-subpath 14)
       (logical-shift-left k-path-point 2))))

(define-method accum-handles ((self <path-graphic>) accum)
  (let ((subpath-id 0))
    (vector-for-each
     (lambda (sp)
       (let ((pathpoint-id 0))
	 (vector-for-each
	  (lambda (pp)
	    (accum self (position pp) pathpoint-id)
	    (set! pathpoint-id (+ pathpoint-id #b100)))
	  (path-points sp))
	 (set! subpath-id (+ subpath-id #b100000000000))))
     (subpaths self))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Dragging the whole and the parts
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-method start-active-drag ((self <path-graphic>)
                                  (in-view <open-view>)
                                  (initial-pt <point>))
  (start-active-drag-handle self in-view -1 initial-pt))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Placement Tool
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (drawpen-button-press (view <open-view>)
			      (at <point>)
			      modifier-state)
  ;; there are two basic cases, depending on whether or not there
  ;; is already an open point selected.
  ;; (the modal interaction is quite similar, except that if we
  ;; are extending an open point, we also draw the line segment
  ;; being created)
  (let ((x (open-point-selected view)))
    (if x
        (extendpen-button-press view at modifier-state x)
        (newpen-button-press view at modifier-state))))

(define (newpen-button-press (view <open-view>)
                             (at <point>)
                             modifier-state)
  (let* ((parent (page-contents (view-page (underlying-object view))))
	 (fwd-ctm (invert-transform (window-ictm view)))
	 (up (refine-point view at))                ; snap to geometry
         (dp (transform up fwd-ctm))
         (path (make <path-graphic>
                     parent-object: parent
                     in-document: (in-document parent)
                     origin: $zero-point
                     graphic-bounding-box: $zero-rect
                     subpaths: '#()))
         (subpath (make <subpath>
                        in-path: path))
         (point (make <path-point>
                      in-subpath: subpath
                      constraint: 'smooth
                      position: up)))
    (set-property! path 'stroke-color 'black)
    (set-subpaths! path (vector subpath))
    (set-path-points! subpath (vector point))
    (do-select view path 0)
    (set-active-drag-proc! view (start-active-drag-handle path view #b10 dp))))

(define (extendpen-button-press (view <open-view>)
                                (at <point>)
                                modifier-state
                                (open-point <path-point>))
  (let* ((parent (page-contents (view-page (underlying-object view))))
	 (fwd-ctm (invert-transform (window-ictm view)))
	 (up (refine-point view at))                ; snap to geometry
         (dp (transform up fwd-ctm))
         (subpath (in-subpath open-point))
         (ppoint (make <path-point>
                      in-subpath: subpath
                      constraint: 'smooth
                      position: up))
         (h (+ #b10 (append-path-point! subpath ppoint))))
    (set-active-drag-proc! view (start-active-drag-handle (in-path subpath)
                                                          view h dp))))
     
(define-method start-active-drag-handle ((self <path-graphic>)
                                         (in-view <open-view>)
                                         handle-id
                                         (initial-pt <point>))
  (generic-dragger 
   self in-view handle-id initial-pt
   final-proc: (if (< handle-id 0)
                   #f
                   (lambda (final-pt)
                     (bind ((sp pp grip (handle->pathpoint self handle-id)))
                       (do-select in-view pp 0))))))
;;

(define (whole-path-mover (self <path-graphic>) start-pt)
  (lambda ((p <point>))
    (point- p start-pt)))

(define (path-grip-mover grip (pp <path-point>))
  (case grip
    ((position)
     (lambda ((p <point>))
       (set-position! pp p)
       $zero-size))
    ((in)
     (lambda ((p <point>))
       (set-in-handle! pp (point- p (position pp)))
       (if (eq? (constraint pp) 'smooth)
           (set-out-handle! pp (size* (in-handle pp) -1)))
       $zero-size))
    ((out)
     (lambda ((p <point>))
       (set-out-handle! pp (point- p (position pp)))
       (if (eq? (constraint pp) 'smooth)
           (set-in-handle! pp (size* (out-handle pp) -1)))
       $zero-size))))

(define-method make-adjuster ((self <path-graphic>)
                               handle-id
                               o->d
                               initial-thunk)
  (if (< handle-id 0)
      (mha
       (whole-path-mover self (initial-thunk))
       (path-draw-proc/whole self o->d))
      (bind ((sp (pp <path-point>) grip (handle->pathpoint self handle-id)))
        (mha
         ;;
         ;; the mouse-motion procedure, which operates in
         ;; constrained user space
         ;;
         (path-grip-mover grip pp)
         ;;
         ;; the draw proc producer
         ;;
         ;;  if (and (eq? (constraint pp) 'corner) (not (eq? grip 'position)))
         ;;  only the curve on the side of the grip is being changed
         ;;
         (path-draw-proc/all sp pp o->d handle-id)))))

;;;   the draw proc procedure, when the whole thing is moving

(define (path-draw-proc/whole path o->d)
  (lambda (shift)
    (let* ((ctm (concatenate-transform 
                 o->d
                 (translate $identity-transform (size->point shift))))
           (segs (vector-map (lambda (sp)
                               (explode-coords 
                                (vector->list
                                 (build-subpath-points sp ctm))))
                             (subpaths path))))
      (lambda (win gc)
        (vector-for-each
         (lambda (segment)
           (draw-lines win gc segment))
         segs)))))

;;;   the draw proc producer, when both fore and aft segments
;;;   may be changing

(define (path-draw-proc/all sp pp o->d handle-id)
  (let* ((num-pp (vector-length (path-points sp)))
         (k-path-point (bitwise-and (logical-shift-right handle-id 2) 
                                    #xFFF))
         (prev-pp (if (> k-path-point 0)
                      (vector-ref (path-points sp) (- k-path-point 1))
                      #f))
         (next-pp (if (< (+ k-path-point 1) num-pp)
                      (vector-ref (path-points sp) (+ k-path-point 1))
                      #f))
         (coordvec (cond
                    ((and prev-pp next-pp)
                     (lambda ()
                       (vector-append
                        (path-fragment-points prev-pp pp o->d)
                        (path-fragment-points pp next-pp o->d))))
                    (prev-pp
                     (lambda ()
                       (path-fragment-points prev-pp pp o->d)))
                    (next-pp
                     (lambda ()
                       (path-fragment-points pp next-pp o->d)))
                    (else
                     (lambda ()
                       '#())))))
    ;(dm "DRAG PATHPOINT ~s ~s ~s" prev-pp pp next-pp)
    (lambda (shift)
      (assert (zero-size? shift))
      (let* ((curve-pts (explode-coords (vector->list (coordvec))))
             (posn (device-point (transform (position pp) o->d)))
             (in-h (if (zero-size? (in-handle pp))
                       #f
                       (device-point (transform (point+ (position pp)
                                                        (in-handle pp))
                                                o->d))))
             (out-h (if (zero-size? (out-handle pp))
                        #f
                        (device-point (transform (point+ (position pp)
                                                         (out-handle pp))
                                                 o->d))))
             (hlines (cond
                      ((and in-h out-h)
                       (explode-coords (list in-h posn out-h)))
                      (in-h
                       (explode-coords (list in-h posn)))
                      (out-h
                       (explode-coords (list posn out-h)))
                      (else
                       '()))))
        ;(dm "  CURVE ~s" curve-pts)
        ;(dm "  POSN ~s  in-h ~s  out-h ~s" posn in-h out-h)
        ;(dm "  HLINES ~s" hlines)
        (lambda (win gc)
          (draw-lines win gc curve-pts)
          (let ((gc2 (transient-gc-nodash win)))
            (if (pair? hlines)
                (draw-lines win gc2 hlines))
            (draw-handle* posn win gc2)
            (if in-h (draw-handle* in-h win gc2))
            (if out-h (draw-handle* out-h win gc2))))))))

(define (path-fragment-points (from <path-point>)
                              (to <path-point>)
                              ctm)
  (if (and (zero-size? (out-handle from))
           (zero-size? (in-handle to)))
      (vector (transform (position to) ctm))
      (let* ((p0 (transform (position from) ctm))
             (p1 (transform (point+ (position from) (out-handle from)) ctm))
             (p2 (transform (point+ (position to) (in-handle to)) ctm))
             (p3 (transform (position to) ctm))
             (c (curv (x p0) (y p0) 
                      (x p1) (y p1)
                      (x p2) (y p2)
                      (x p3) (y p3))))
        (dm "  PATH FRAGMENT ~a  ~a  ~a  ~a" p0 p1 p2 p3)
        (vector-map (lambda (k)
                      (if (eq? k 1)
                          p3
                          (point-on c k)))
                    '#(0.125 0.25 0.375 0.5 0.625 0.75 0.875 1)))))
   
;;; add a new path-point to an open subpath
;;; returns the id of the new handle

(define (append-path-point! (sp <subpath>) (pp <path-point>))
  (let ((subpath-id (logical-shift-left (vmemq sp (subpaths (in-path sp))) 14))
	(ppv (path-points sp)))
    (set-path-points! sp (vector-append ppv (vector pp)))
    (+ subpath-id (logical-shift-left (vector-length ppv) 2))))

;;;  determine if an open point is selected, and return it if so
;;;  otherwise, return #f
;;;  (an open point is a <path-point> in an unclosed <subpath>
;;;  that is at one (either) end of of the sub-path)

(define (open-point-selected (view <open-view>))
  ;; XXX temporary implementation, it's an approximation
  (let* ((sel (key-sequence (current-selection view)))
	 (sel1 (if (pair? sel) (car sel) #f)))
    (if (instance? sel1 <path-graphic>)
	(vector-last (path-points (vector-last (subpaths sel1))))
        (if (instance? sel1 <path-point>)
            sel1
            #f))))

(define (vector-last (v <vector>))
  (vector-ref v (sub1 (vector-length v))))

;;;
;;; hack for testing...
;;;

(add-major-mode!
 (make <major-mode>
       name: 'pen
       button-press-proc: drawpen-button-press))

(define-interactive (pen-mode view)
  (interactive (owner))
  (set-major-mode! view (get-major-mode 'pen)))

(graphic-set-key #\M-4 pen-mode)

;;;

(define (transient-gc-nodash win)
  (get-property win 'transient-gc-nodash (create-transient-gc-nodash win)))

(define (create-transient-gc-nodash win)
  (let* ((scrn (drawable-screen win))
	 (gc (create-gcontext drawable: win
			      function: 'boole-xor
			      ; this hilight technique is taken from
			      ; X11 (Xlib - vol.1) (O'Reilly) p.205
			      foreground: (bitwise-xor
					   (screen-black-pixel scrn)
					   (screen-white-pixel scrn))
			      line-width: 0
			      background: (screen-white-pixel scrn))))
    (set-property! win 'transient-gc gc)
    gc))

;;;

;;; need to support all the <path-point> options...

(define (path-point-from-extern (in <subpath>) 
				offset
				#key x y
				     (in-dx default: 0)
				     (in-dy default: 0)
				     (out-dx default: 0)
				     (out-dy default: 0))
  (make <path-point>
	in-subpath: in
	position: (point+ (make-point x y) offset)
	in-handle: (if (and (eq? in-dx 0) (eq? in-dy 0))
		       $zero-size
		       (make-size in-dx in-dy))
	out-handle: (if (and (eq? out-dx 0) (eq? out-dy 0))
			$zero-size
			(make-size out-dx out-dy))))

(define (subpath-from-extern (in <path-graphic>)
			     offset
			     #key points
			          (closed? default: #f))
  (let ((sp (make <subpath>
		  closed?: closed?
		  in-path: in)))
    (set-path-points! 
     sp
     (list->vector
      (map (lambda (pp)
	     (case (car pp)
	       ((path-point)
		(apply path-point-from-extern sp offset (cdr pp)))
	       (else
		(em "not a path-point: ~s" pp))))
	   points)))
    sp))

(define (dashing-from-extern dashing)
  (if (pair? dashing)
      (begin
        (assert (every? real? dashing))
        (cons (list->vector dashing) 0))
      #f))

(define (path-from-extern in-group
			  (offset <size>)
			  #key subpaths
                               (dashing default: #f)
			       (stroke-color default: #f)
			       (stroke-width default: #f)
			       (fill-color default: #f))
  (let ((p (make <path-graphic>
		 in-document: (in-document in-group)
		 parent-object: in-group
                 dashing: (dashing-from-extern dashing)
		 graphic-bounding-box: (make-rect 0 0 0 0))))
    (set-subpaths! 
     p
     (list->vector
      (map (lambda (sp)
	     (case (car sp)
	       ((subpath)
		(apply subpath-from-extern p offset (cdr sp)))
	       (else
		(em "not a subpath: ~s" sp))))
	   subpaths)))
    (if stroke-color (set-property! p 'stroke-color stroke-color))
    (if fill-color (set-property! p 'fill-color fill-color))
    (if stroke-width (set-property! p 'stroke-width stroke-width))
    (recompute-graphic-bounding-box! p)
    p))

(define (paste-path-from-extern extern group offset)
  (apply path-from-extern group offset (cdr extern)))

;;;

(define-method recompute-graphic-bounding-box! ((self <path-graphic>))
  (let ((xmin 0)
	(xmax 0)
	(ymin 0)
	(ymax 0)
	(first? #t))
    (vector-for-each
     (lambda ((sp <subpath>))
       (vector-for-each
	(lambda ((pp <path-point>))
	  ;; NEED TO TAKE INTO ACCOUNT in/out HANDLES
	  (let ((x (x (position pp)))
		(y (y (position pp))))
	    (if first?
		(begin
		  (set! xmin x)
		  (set! xmax x)
		  (set! ymin y)
		  (set! ymax y)
		  (set! first? #f))
		(begin
		  (set! xmin (min xmin x))
		  (set! xmax (max xmax x))
		  (set! ymin (min ymin y))
		  (set! ymax (max ymax y))))))
	(path-points sp)))
     (subpaths self))
    (let ((r (bbox-rect xmin ymin xmax ymax)))
      (set-graphic-bounding-box! self r)
      r)))

;;;
;;;  Procedures to implement the <selectable-object> protocol
;;;  on <path-point> objects
;;;

(define-method draw-handle ((self <path-point>) id at view win gc)
  (draw-handle* at win gc)
  (if (not (eq? (bitwise-and id #b11) #b00))
      (let ((o->d (concatenate-transform
                   (concatenate-transform
                    (translate $identity-transform
                               (size->point
                                (size* 
                                 (point->size
                                  (view-origin (underlying-object view)))
                                 -1)))
                    (view-ctm (underlying-object view)))
                   (compute-view-ctm-for self view))))
        (dm "transform ~s => ~s"
            (position self)
            (transform (position self) o->d))
        (let ((p (device-point (transform (position self) o->d))))
          (draw-line win gc
                     (x at)
                     (y at)
                     (x p)
                     (y p))))))

(define-method compute-view-ctm-for ((self <path-point>) view)
  (compute-view-ctm-for (in-path (in-subpath self)) view))

(define-method paint-artwork* ((self <path-point>) dev)
  (paint-artwork* (in-path (in-subpath self)) dev))

(define-method accum-handles ((self <path-point>) proc)
  (accum-handles (in-path (in-subpath self)) proc)
  (let ((id (pathpoint->handle self)))
    (dm "base id ~b : ~a ... ~a"
        id
        (point+ (position self) (in-handle self))
        (point+ (position self) (out-handle self)))
    (proc self ; (in-path (in-subpath self))
          (point+ (position self) (in-handle self))
          (+ id #b01))
    (proc self ; (in-path (in-subpath self))
          (point+ (position self) (out-handle self))
          (+ id #b10))))

(define-method status-line-when-sel ((self <path-point>))
  (let ((id (pathpoint->handle self)))
    (format #f "~a/~d/~d"
            (status-line-when-sel (in-path (in-subpath self)))
            (logical-shift-right id 14)
            (bitwise-and (logical-shift-right id 2) #xFFF))))

(define-method start-active-drag-handle ((self <path-point>)
                                         (in-view <open-view>)
                                         handle-id
                                         (initial-pt <point>))
  (start-active-drag-handle (in-path (in-subpath self))
                            in-view
                            handle-id
                            initial-pt))

;;;

(define-method explode->list ((self <path-graphic>))
  (unlink-graphic self)
  (let ((sc (get-property self 'stroke-color #f))
        (fc (get-property self 'fill-color #f)))
    (map
     (lambda (sp)
       (let ((pg (make <path-graphic>
                       parent-object: (parent-object self)
                       in-document: (in-document self)
                       subpaths: (vector sp)
                       insideness: (insideness self)
                       origin: (origin self)
                       graphic-bounding-box: $zero-rect)))
         (if sc (set-property! pg 'stroke-color sc))
         (if fc (set-property! pg 'fill-color fc))
         pg))
     (vector->list (subpaths self)))))
