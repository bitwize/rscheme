;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  persistent object model
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-class <line-graphic> (<leaf-object>)
  (line-start type: <point>)
  (line-end type: <point>))

(define-method status-line-when-sel ((self <line-graphic>))
  (format #f "Line ~d" (id self)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   <line-graphic>
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-method pick-list* ((self <line-graphic>) pt ctm)
  (pick-on-path self pt ctm (list (line-start self) (line-end self))))

(define-method paint-artwork* ((self <line-graphic>) dev)
  (moveto dev (line-start self))
  (lineto dev (line-end self))
  (stroke dev))

(define-method accum-handles ((self <line-graphic>) accum)
  (accum self (line-start self) 0)
  (accum self (line-end self) 1))

(define-method paint-object* ((self <line-graphic>) dev)
  (let ((sw (get-property self 'stroke-width #f))
        (sc (get-property self 'stroke-color #f)))
    ;
    (define (render)
      (moveto dev (line-start self))
      (lineto dev (line-end self))
      (if sw (setlinewidth dev sw))
      (if sc (setcolor dev sc))
      ;;
      (stroke dev))
    ;;
    ;;
    (if sw
        (with-gstate-saved dev render)
        (render))))

(define-class <handle-adjuster> (<object>)
  (update-graphic-proc type: <function>)
  (get-stroker type: <function>))

;;;
;;;  The `click-only-proc' is called if the mouse activity
;;;  amounts to only a click and not an actual drag
;;;

(define (generic-dragger self
                         (in-view <open-view>)
                         handle-id
                         (initial-pt <point>)
                         #key (click-only-proc default: #f)
                              (final-proc default: #f))
  ;; during a drag, we use the object coordinates that were
  ;; in place at the beginning of the drag.  This avoids accumulation
  ;; of error during the many small changes of a drag.
  ;; `o->d' converts from (initial) object coords to device coords
  (let* ((o->u (compute-view-ctm-for self in-view))
         (o->d (concatenate-transform
                (invert-transform (window-ictm in-view))
                o->u))
	 (adjuster (make-adjuster self
                                  handle-id 
                                  o->d
                                  ; `initial-thunk'... proc to compute
                                  ; coordinates of initial point in 
                                  ; object space
                                  (lambda ()
                                    (inverse-transform 
                                     (refine-point in-view initial-pt)
                                     o->u))))
         (o-shift $zero-size)
	 (win (content-window in-view))
	 (gc (transient-gc win))
         ;; set up a box; if the mouse ever leaves this box,
         ;; then we regard the activity as a drag and not a click
         (click-only-trigger (if click-only-proc
                                 (make-rect (- (x initial-pt) 3)
                                            (- (y initial-pt) 3)
                                            6
                                            6)
                                 #f))
	 (last #f))
    ;
    (define (update-self new-pt)
      (if (and click-only-trigger
               (not (point-in-rect? click-only-trigger new-pt)))
          (set! click-only-trigger #f))
      (if click-only-trigger
          #f
          (let ((newp (inverse-transform (refine-point in-view new-pt) o->u)))
            (set! o-shift ((update-graphic-proc adjuster) newp))
            (dm 801 "new o-shift (~s => ~s): ~s" new-pt newp o-shift)
            #t)))
    ;
    (vector
     ;; mouse-motion handler
     (lambda ((new-pt <point>) flags)
       (if (update-self new-pt)
           (let ((outliner ((get-stroker adjuster) o-shift)))
             (if last (last win gc))
             (set! last outliner)
             (last win gc)
             (flush-client))))
     ;; mouse-up handler
     (lambda ((new-pt <point>) flags)
       (if last (last win gc))
       (if (update-self new-pt)
           (begin
             (if final-proc
                 (final-proc new-pt))
             (mark-as-dirty (in-document in-view))
             (set-origin! self (point+ (origin self) o-shift))
             (clear-all-areas (in-document in-view)))
           (if click-only-proc
               (click-only-proc flags)))))))

;;; `u' -- a procedure of one argument, a point in
;;;        OBJECT coordinates, which is supposed to
;;;        update the object to reflect the
;;;        movement of the handle to to that new
;;;        point.  The procedure returns the size
;;;        SHIFT by which the object's origin
;;;        should be moved.
;;;
;;; `o' -- a procedure of one argument, a size
;;;        SHIFT, which returns a procedure of two
;;;        arguments (a WINDOW and a GC) which,
;;;        when called, will draw the outline of
;;;        the object given that the handle has
;;;        moved by SHIFT

(define (mha u o)
  (make <handle-adjuster>
        update-graphic-proc: u
        get-stroker: o))

(define make-handle-adjuster mha)

(define-method make-adjuster ((self <line-graphic>)
                               handle-id
                               o->d
                               initial-thunk)
  (mha
   (case handle-id
     ((0)
      (lambda ((p <point>))
        (set-line-start! self p)
        $zero-size))
     ((1)
      (lambda ((p <point>))
        (set-line-end! self p)
        $zero-size))
     ((-1)
      (let ((start0 (line-start self))
            (end0 (line-end self))
            (p0 (initial-thunk)))
        (lambda ((p <point>))
          (let ((sh (chop (point- p p0))))
            (set-line-start! self (point+ start0 sh))
            (set-line-end! self (point+ end0 sh))
            $zero-size)))))
   (lambda (shift)
     (let ((pts (explode-coords
                 (vector->list
                  (map-object-points shift
                                     o->d
                                     (vector (line-start self)
                                             (line-end self)))))))
       (lambda (win gc)
         (draw-lines win gc pts))))))

(define-method start-active-drag-handle ((self <line-graphic>)
					 (in-view <open-view>)
					 handle-id
					 (initial-pt <point>))
  (generic-dragger self in-view handle-id initial-pt))

(define-method start-active-drag ((self <line-graphic>) 
				  (in-view <open-view>)
				  (initial-pt <point>)) ;; device-sheet coords
  (start-active-drag-handle self in-view -1 initial-pt))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   line-drawing tool
;;;
;;;   (nb, this is really a graphic command functionality)
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Here's an experimental implementation that reuses the adjuster
;;; code.  One consequence is that it creates the target object
;;; *BEFORE* the button release.  Any problem with that?

(define (drawline-button-press (in-view <open-view>)
                               (dp0 <point>) ;; window device coords
                               modifier-state)
  (let* (;; `ctm' converts window-device coords to user coords
	 (ctm (window-ictm in-view))
	 (fwd-ctm (invert-transform ctm)) ;; and back again
	 (up0 (refine-point in-view dp0)) ;; snap to geometry (window d.c.)
         (dp0 (transform up0 fwd-ctm))
	 (last #f)
         (self (make <line-graphic>
                     in-document: (in-document
                                   (page-contents
                                    (view-page
                                     (underlying-object in-view))))
                     parent-object: (page-contents
                                     (view-page
                                      (underlying-object in-view)))
                     line-start: up0
                     line-end: up0
                     graphic-bounding-box: $zero-rect)))
    (dm "LINE ~s" self)
    (do-select in-view self 0)
    (set-active-drag-proc!
     in-view
     (start-active-drag-handle self in-view 1 dp0))))

(add-major-mode!
 (make <major-mode>
       name: 'draw-line
       button-press-proc: drawline-button-press))

(define-interactive (draw-line-mode view)
  (interactive (owner))
  (set-major-mode! view (get-major-mode 'draw-line)))

(graphic-set-key #\M-3 draw-line-mode)

;;;

(define-method externalize ((self <line-graphic>))
  `(line start-x: ,(x (line-start self))
	 start-y: ,(y (line-start self))
	 end-x: ,(x (line-end self))
	 end-y: ,(y (line-end self))))

(define (paste-line-from-extern extern group offset)
  (apply (lambda (#key start-x start-y end-x end-y
                       (stroke-color default: #f)
		       (stroke-width default: #f)) 
	   (let ((sw (or stroke-width 1)) ;; stroke weight
		 (ls (point+ (make-point start-x start-y) offset))
		 (le (point+ (make-point end-x end-y) offset)))
	     (make <line-graphic>
		   in-document: (in-document group)
		   parent-object: group
		   properties: (vector-append
                                (if stroke-width
                                    (vector 'stroke-width stroke-width)
                                    '#())
                                (if stroke-color
                                    (vector 'stroke-color stroke-color)
                                    '#()))
		   graphic-bounding-box: (inset-rect
					  (bbox-rect (x ls) (y ls)
						     (x le) (y le))
					  (- sw)
					  (- sw))
		   line-start: ls
		   line-end: le)))
	 (cdr extern)))


