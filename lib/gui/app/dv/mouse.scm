
;;;
;;;  `refine-point' takes a window device point and applies
;;;  the various, weighted constraints, snaps, grids, 
;;;  etc. that are currently active (which may vary during
;;;  a drag, e.g., if the shift key is depressed, in which
;;;  case `constrain' should be a constraint procedure that
;;;  returns a user coordinate)
;;;
;;;  Returns the user-space point.  May update the cursor
;;;  according to the type of refinement applied.
;;;

(define (refine-point (in <open-view>) (pt <point>) #optional constrain)
  (let* ((geom (table-lookup (current-geometry in) pt))
	 ;; `ictm' converts window-device coords to user coords
	 (ictm (translate (invert-transform (view-ctm (underlying-object in)))
                          (view-origin (underlying-object in))))
         (u (transform pt ictm))
         (grid (snap-to-grid/u in u))
         (con (and constrain (constrain u)))
         (p (or grid geom con u))
         (curs (cdr (assq (if (eq? p geom)
                              'snap
                              (name (current-major-mode in)))
                          (mode-cursors in)))))
    (if (set-window-cursor! in curs)
        (play-sound (if (eq? p geom)
                        "Tink"
                        "Basso")))
    #|(dm "refine-point: ~s ==[~a]==> ~s" 
        pt
        (cond
         ((eq? p grid) "grid")
         ((eq? p geom) "geom")
         ((eq? p con) "rel ")
         ((eq? p u) "user")
         (else "?"))
        p)|#
    p))

#|
;; `tweak-point' operates in window sheet coordinates

(define (tweak-point (in <open-view>) (pt0 <point>))
  (let* ((snp (table-lookup (current-geometry in) pt0))
	 (pt (if snp
		 (snap-to-grid in snp) ;XXX really want to snap this to grid?
		 (snap-to-grid in pt0)))
	 (cmode (if snp 'snap (name (current-major-mode in)))))
    (dm "tweak-point: ~s ..> ~s ~s" pt0 snp pt)
    (set-window-cursor! in (cdr (assq cmode (mode-cursors in))))
    pt))
|#

;;;

(define (my-button-press-handler display
				 #rest ignore
				 #key code
                                      window
				      event-window
				      x y
				      state)
  (dm 116 "button-press in ~s at (~d ~d) ~s S: ~s" event-window x y code state)
  (case code
    ((1)
     (let ((h (get-property event-window 'button-press #f)))
       (if h
           (apply h event-window (make-point x y) state '())
           (let ((ov (window->open-view event-window)))
             (if ov
                 ((button-press-proc (current-major-mode ov))
                  ov (make-point x y) state))))))
    ((3)
     ;; context-sensitive menu
     (let ((ov (window->open-view event-window)))
       (if ov
           (context-sensitive-press ov event-window 
                                    (make-point x y) 
                                    state))))))

(define (my-motion-notify-handler display 
				  #rest ignore
				  #key window
				       event-window
				       x
				       y
				       state)
  (let ((h (get-property event-window 'button-motion #f)))
    (if h
	(h window (make-point x y) state)
	(let ((ov (window->open-view event-window)))
	  (if ov
	      (let ((drg (active-drag-proc ov)))
		(if drg
		    (begin
		      (if (vector? drg)
			  (set! drg (vector-ref drg 0)))
		      (drg (make-point x y) state)))))))))

(define (my-button-release-handler display
				   #rest ignore
				   #key window
				        event-window
					x
					y
					state)
  (let ((h (get-property event-window 'button-release #f)))
    (if h
	(h window (make-point x y) state)
	(let ((ov (window->open-view event-window)))
	  (if ov
	      (begin
		(if (vector? (active-drag-proc ov))
		    ((vector-ref (active-drag-proc ov) 1)
		     (make-point x y)
		     state))
		; cancel any currently active drag
		(set-active-drag-proc! ov #f)
		; and reset the cursor
		(reset-cursor! ov)))))))

(define-method status-line-when-sel ((self <graphic-object>))
  (format #f "Graphic ~d" (id self)))

;;;
;;;  make a GC for transient drawing,
;;;  as for drawing new boxes and lines and stuff

(define (transient-gc win)
  (get-property win 'transient-gc (create-transient-gc win)))

(define (create-transient-gc win)
  (let* ((scrn (drawable-screen win))
	 (gc (create-gcontext drawable: win
			      function: 'boole-xor
			      dashes: 3
			      ; this hilight technique is taken from
			      ; X11 (Xlib - vol.1) (O'Reilly) p.205
			      foreground: (bitwise-xor
					   (screen-black-pixel scrn)
					   (screen-white-pixel scrn))
			      line-width: 0
			      line-style: 'on-off-dash
			      background: (screen-white-pixel scrn))))
    (set-property! win 'transient-gc gc)
    gc))

;;;
;;;  construct a temporary procedure for converting (raw) window
;;;  coordinates into user coordinates (with snapping)
;;;

(define (get-window->user-conv-proc (view <open-view>))
  (let* ((fwd-ctm (view-ctm (underlying-object view)))
	 ;; `ctm' converts window-device coords to user coords
	 (ctm (translate (invert-transform fwd-ctm)
			 (view-origin (underlying-object view)))))
    (values (lambda (at)
	      (refine-point view at))
	    ctm)))

;;;
;;;  convert one point
;;;

(define (window->user-point (view <open-view>) (at <point>))
  ((get-window->user-conv-proc view) at))

;;;

;;;  relocate the `near' point so that it is on a valid
;;;  shift-constrained line from `from'

(define $PI (atan 0 -1))

(define (shift-constraint-filter (near <point>) (from <point>))
  (let* ((d (point- near from))
	 (a (atan (dy d) (dx d)))
	 (near-a (modulo (inexact->exact (round (/ a (/ $PI 4)))) 8)))
    ;(dm "Near angle index: ~d" near-a)
    (case near-a
      ((0 4) (make-point (x near) (y from)))
      ((2 6) (make-point (x from) (y near)))
      ((1 5) (let ((t (/ (+ (dx d) (dy d)) 2)))
	       (point+ from (make-size t t))))
      ((3 7) (let ((t (/ (- (dx d) (dy d)) 2)))
	       (point+ from (make-size t (- t))))))))
