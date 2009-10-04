
;;; convert window device to sheet device coordinates
;;; by translating by `view-origin'
;;; [see also window->user-point in mouse.scm]

(define (window->sheet (view <open-view>) pt)
  (let (((o <point>) (view-origin (underlying-object view))))
    (make-point (+ (x pt) (x o))
		(+ (y pt) (y o)))))

(define-method pick-list ((self <open-view>) at) ;; `at' in sheet device coords
  (let* (((v <view>) (underlying-object self)))
    (dm "pick at ~s" at)
    (pick-list (page-contents (view-page v)) at (view-ctm v))))

(define-method pick-list ((self <group>) (pt <point>) ctm)
  (next-method self pt (concatenate-transform ctm (general-transform self))))

(define-method pick-list ((self <graphic-object>) (pt <point>) ctm)
  (let ((t (get-property self 'transform #f)))
    (pick-list* self pt (translate (if t (concatenate-transform ctm t) ctm)
                                   (origin self)))))

;;;

(define-method bounding-box ((self <graphic-object>))
  (inexact->exact
   (ceiling
    (or (bounding-box* self $identity-transform) 
	$zero-rect))))

;;; note that by allowing `bounding-box*' to return #f instead
;;; of an empty rect and using `merge-rect', we support having
;;; empty groups.  I don't know if that's useful...

(define (merge-rect a b)
  (if (and a b)
      (union-rect a b)
      (or a b)))

(define-method bounding-box* ((self <group>) ctm)
  (let ((ctm (concatenate-transform ctm (general-transform self)))
	(r #f))
    (for-each
     (lambda (elem)
       (let ((ebb (bounding-box* elem ctm)))
	 ;(format #t "   (bounding-box* ~s) => ~s\n" elem ebb)
	 (set! r (merge-rect r ebb))))
     (group-contents self))
    ;(format #t "(bounding-box* ~s) => ~s\n" self r)
    r))

(define-method bounding-box* ((self <graphic-object>) ctm)
  (transform 
   (offset-rect
    (graphic-bounding-box self)
    (x (origin self))
    (y (origin self)))
   ctm))

;;;

(define-method pick-list* ((self <group>) pt ctm)
  (apply append (map (rcurry pick-list pt ctm) 
		     (group-contents self))))

;;;  a `path', for the purposes of `pick-on-path' is an open path
;;;  in object-local coords (ie, without the `ctm' applied),
;;;  represented as a list of points

(define (pick-on-path owner (pt <point>) ctm path)
  (let loop ((p (map (lambda (p)
		       (transform p ctm))
		     path)))
    (if (null? (cdr p))
	'()
	(let ((r (pick-on-segment pt (car p) (cadr p))))
	  (if r
	      (list (cons r owner))
	      (loop (cdr p)))))))

(define (pick-on-segment (pt <point>) (from <point>) (to <point>))
  (let ((r (distance (make <line> from: from to: to) pt)))
    (if (< r *pick-closeness*)
	r
	#f)))

(define *pick-closeness* 3)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-method paint-object ((self <graphic-object>) dev)
  (with-gstate-saved
   dev
   (lambda ()
     (let ((t (get-property self 'transform #f)))
       (if t (concat dev t)))
     (translate dev (origin self))
     (paint-object* self dev))))

(define-method paint-artwork ((self <graphic-object>) dev)
  (with-ctm-saved
   dev
   (lambda ()
     (let ((t (get-property self 'transform #f)))
       (if t (concat dev t)))
     (translate dev (origin self))
     (paint-artwork* self dev))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   <group>
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-method paint-artwork* ((self <group>) dev)
  (concat dev (general-transform self))
  (for-each (rcurry paint-object dev) (group-contents self)))

(define-method paint-object* ((self <group>) dev)
  (concat dev (general-transform self))
  (for-each (rcurry paint-object dev) (group-contents self)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   `compute-view-ctm-for'  returns a O->U transform.  By
;;;   default, this is an identity transform.
;;;
;;;   Note that the object's `origin' is expressed in object
;;;   coordinates.
;;;

(define-method compute-view-ctm-for ((self <root-group>) in-view)
  (make-affine-transform))

  ;;(view-ctm (underlying-object in-view)))

(define-method compute-view-ctm-for ((self <user-group>) in-view)
  (translate (concatenate-transform
	      (compute-view-ctm-for (parent-object self) in-view)
	      (general-transform self))
	     (origin self)))

(define-method compute-view-ctm-for ((self <leaf-object>) in-view)
  (let ((t (get-property self 'transform #f))
        (p (compute-view-ctm-for (parent-object self) in-view)))
    (translate (if t (concatenate-transform p t) p)
               (origin self))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (redraw-status-view (self <open-view>))
  (let* ((xw (status-window self))
	 (scrn (drawable-screen xw))
	 (gc (create-gcontext drawable: xw
			      foreground: (screen-black-pixel scrn)
			      background: (screen-black-pixel scrn))))
    (set-gcontext-font! gc (get-property (drawable-display xw) 'status-font))
    (clear-area xw)
    (draw-glyphs xw gc 5 12 (current-status-line self))
    (free-gcontext gc)))

(define-method set-status-line! ((self <open-view>) (str <string>))
  (clear-area (status-window self) exposures?: #t)
  (display-force-output (drawable-display (status-window self)))
  (set-current-status-line! self str))

(define (redraw-open-view (self <open-view>))
  (with-x11-device (content-window self)
                   (lambda (dev)
                     (redraw-open-view* self dev))))

;;; the default way of drawing handles for graphic objects

(define-syntax (draw-handle* at win gc)
  (draw-rectangle win gc (- (x at) 1) (- (y at) 1) 3 3 #t))

(define-method draw-handle ((self <graphic-object>) id at view win gc)
  (draw-handle* at win gc))

(define (redraw-open-view* (self <open-view>) dev)
  ;;
  (if (need-to-recompute-handles self)
      (begin
        (set-need-to-recompute-handles! self #f)
        (update-handles self)))
  ;;
  (let ((old-cur-g (current-geometry self)))
    ;;  geometry points are inserted by the underlying (e.g., x11) driver
    (set-owner! dev self)
    (set-current-geometry! self (make-geometry-table))
    ;;
    ;; draw the page:
    ;;    1. grid lines
    ;;    2. page frame
    ;;    3. page content
    ;;
    (draw-grid-points self)
    (draw-page-frame self)
    ;;
    (with-gstate-saved
     dev
     (lambda ()
       (let* (((v <view>) (underlying-object self))
	      ((o <point>) (view-origin v)))
	 (translate* dev (make-point (- (x o)) (- (y o))))
	 (concat* dev (view-ctm v)))
       ;; draw page contents
       (paint-object (page-contents
                      (view-page
		       (underlying-object self)))
		     dev)))
    ;
    (with-gstate-saved
     dev
     (lambda ()
       ;;------------------------------------------------------------
       ;;   draw the artwork associated with the current selection
       ;;   in the selection color (i.e., blue)
       ;;------------------------------------------------------------
       (setcolor dev (selection-color self))
       ; we put the `view-origin' translation into the device ctm here
       ; instead of in the computation of `compute-view-ctm-for', so
       ; the handle points are in device sheet coordinates instead of
       ; device window coords
       ; (!! which means we can avoid update-handles on pan !! yay!)
       (let* (((v <view>) (underlying-object self))
	      ((o <point>) (view-origin v)))
	 (translate* dev (make-point (- (x o)) (- (y o))))
	 (concat* dev (view-ctm v)))
       ;;
       (table-for-each
	(current-selection self)
	(lambda (h k v)
	  (with-ctm-saved
	   dev
	   (lambda ()
	     (concat dev (compute-view-ctm-for k self))
	     (paint-artwork* k dev)))))
       ;
       (let ((xw (x-window dev))
	     (xgc (x-gc dev))
	     (xlate (size* (point->size 
			    (view-origin (underlying-object self))) 
			   -1)))
	 (table-for-each
	  (current-handles self)
	  (lambda (h k v)
            ;(dm "draw-handle: ~s [~s] ~s" (car v) (cdr v) k)
            (draw-handle (car v) (cdr v)
                         (device-point (point+ k xlate))
                         self xw xgc))))))
    ; this way of disabling updating geometry I don't exactly
    ; like...  What's a better way to handle the problem of
    ; self-snapping (ie, as the object moves, it's geometry points
    ; move and it keeps snapping to itself)
    (if (active-drag-proc self)
	(set-current-geometry! self old-cur-g))))


;;;
;;;  updates the handles geometry table
;;;  (that table maps points in device space to owner/id pairs)
;;;

(define (update-handles (self <open-view>))
  (set-current-handles! self (make-geometry-table))
  ;
  (table-for-each
   (current-selection self)
   (lambda (h k v)
     (let ((ctm (concatenate-transform 
                 (view-ctm (underlying-object self))
                 (compute-view-ctm-for k self))))
       (accum-handles
	k
	(lambda (owner point id)
          ;(dm "Handle ~d at: ~a ( ~a )" id point (transform point ctm))
	  (table-insert! (current-handles self)
			 (transform point ctm)
			 (cons owner id))))))))

(define (selection-state dev item)
  (table-lookup (current-selection (owner dev)) item))
