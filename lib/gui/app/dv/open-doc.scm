
(define-class <transient-object> (<object>)
  underlying-object)

(define-class <open-document> (<transient-object>)
  (file-name type: <string>)
  (properties type: <vector> init-value: '#())
  (open-views type: <list> init-value: '())
  (dirty? init-value: #f))

(define-method to-string ((self <open-document>)) (file-name self))

(define-method mark-as-clean ((self <open-document>) #optional file)
  (if file
      (set-file-name! self file))
  (if (dirty? self)
      (begin
	(dm "~s is now clean" self)
	(set-dirty?! self #f))))

(define-method mark-as-dirty ((self <open-document>))
  (if (not (dirty? self))
      (begin
	(dm "~s is now dirty" self)
	(set-dirty?! self #t))))

;;;
;;;  map <document> objects to <open-document> objects
;;;

(define *document-table* (make-object-table))

(define-method transient-for ((self <document>))
  (table-lookup *document-table* self))

(define-class <open-view> (<transient-object>)
  (in-document type: <open-document>)
  (main-window type: <x-window>)
  (status-window type: <x-window>)
  (content-window type: <x-window>)
  (current-cursor init-value: #f)
  (content-window-size type: <size>)
  (current-selection type: <hash-table>)
  (need-to-recompute-handles init-value: #t)
  (current-handles type: <geometry-table>)  ;; active handles
  ;;
  ;; `current-geometry' is a geometry table mapping window sheet
  ;; coordinates to USER PAGE coordinates (or will be, anyway...)
  ;;
  (current-geometry type: <geometry-table>) ;; all geometry (snaps)
  (bottom-scrollbar type: <horz-scrollbar>)
  (right-scrollbar type: <vert-scrollbar>)
  (selection-color type: <fixnum>)
  (page-frame-color type: <fixnum>)
  (grid-color type: <fixnum>)
  (grid-gcontext)
  ;;
  ;;  the `grid-ctm' transforms device (view, not sheet) coordinates
  ;;  into the unit-size spacing of the grid, incorporating its
  ;;  translation, spacing, and the user->device transformation
  ;;
  (grid-ctm init-value: #f)  ; cache of grid tranform
  ;
  (mode-cursors type: <list>)
  ;
  (active-drag-proc init-value: #f)
  (current-status-line type: <string> init-value: "ok")
  (current-major-mode init-value: #f)
  (visible-grid init-value: #f)
  ;
  (paste-offset type: <size> init-value: $zero-size))

(define-method dirty? ((self <open-view>))
  (dirty? (in-document self)))

(define-method mark-as-dirty ((self <open-view>))
  (mark-as-dirty (in-document self)))

;;;

(define-thread-var *current-view* #f)

(define-inline (current-view)
  *current-view*)

(define (set-current-view! (view <open-view>))
  (set! *current-view* view))

(define (call-with-view (view <open-view>) thunk)
  (thread-let ((*current-view* view))
    (thunk)))
;;;

(define (clear-all-areas (self <open-document>))
  (for-each (lambda (ov)
	      (set-need-to-recompute-handles! ov #t)
	      (clear-area (content-window ov) exposures?: #t))
	    (open-views self)))

;;;

(define-method context-sensitive-press ((self <open-view>) in-win at state)
  ;; figure out the context
  (let ((ctx (list (list "Object: Foo" #f)
                   (list "Path: Bar" #f))))
    ;; pop up the menu
    (pop-up-menu (drawable-screen (main-window self))
                 "Context Menu"
                 (make-menu-cells
                  (on-display (current-client))
                  (list->vector ctx))
                 at
                 in-win)))

(define (clear-current-selection! (self <open-view>))
  (for-each (lambda (k)
	      (table-remove! (current-selection self) k))
	    (key-sequence (current-selection self)))
  (values))

(define (add-to-current-selection! (self <open-view>) item)
  (table-insert! (current-selection self) item #t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (get-lt-color scrn c) (getcolor scrn c screen-white-pixel))
(define (get-dk-color scrn c) (getcolor scrn c screen-black-pixel))

(define (getcolor scrn c dflt)
  (handler-case
    (alloc-color (screen-default-colormap scrn) c)
   ((<x-error>)
    (format #t "Could not allocate background color (~a)!\n" c)
    (dflt scrn))))

(define *stath* 16)
(define *statw* 48)

(define $grid-color (make-color red: 0.9 green: 0.9 blue: 1))

;;;
;;;   lay out an open view
;;;   Returns, respectively:
;;;             Outer window frame
;;;             Content window frame
;;;             Status window frame
;;;             Horizontal Scrollbar frame
;;;             Vertical Scrollbar frame

#|
(define (open-view-layout doc frame)
  (bind ((frame-x frame-y content-w content-h (rect->values frame))
         (w (+ content-w *stath* 1))
         (h (+ content-h *stath* 1)))
    (values (make-rect frame-x frame-y w h)
            (make-rect 0 0 (- w *stath* 1) (- h *stath* 1))
            (make-rect -1 (- h *stath* 1) *statw* *stath*)
            (make-rect *statw* (- h *stath* 1) (- w *statw* *stath* 2) 16)
            (make-rect (- w *stath* 1) -1 16 (- h *stath* 1)))))
|#

(define (open-view-layout doc frame)
  (bind ((frame-x frame-y content-w content-h (rect->values frame))
         (w (+ content-w *stath* 1))
         (h (+ content-h (* *stath* 2) 1)))
    (values (make-rect frame-x frame-y w h)
            (make-rect 0 0 content-w content-h)
            (make-rect -1 (- h *stath*) w *stath*)
            (make-rect -1 content-h (- w *stath* 1) 16)
            (make-rect (- w *stath* 1) -1 16 (- h *stath*)))))


(define (open-view-window (doc <open-document>) (self <view>))
  (bind ((scrn (on-screen (current-client)))
         (window-frame
          contentwin-frame 
          statwin-frame 
          horzsb-frame
          vertsb-frame (open-view-layout doc (view-frame self)))
	 (frame-x frame-y content-w content-h (rect->values (view-frame self)))
	 ;(w (+ content-w *stath* 1))
	 ;(h (+ content-h *stath* 1))
	 (cbg (getcolor scrn "lightgray" screen-white-pixel))
	 (cbd (getcolor scrn "slategray" screen-black-pixel))
	 (gridcolor (getcolor scrn $grid-color screen-black-pixel))
	 (sel (getcolor scrn "dodgerblue" screen-black-pixel))
	 (win (create-window parent: (screen-root scrn)
			     x: (origin-x window-frame)
			     y: (origin-y window-frame)
			     width: (size-width window-frame)
			     height: (size-height window-frame)
			     event-mask: '(structure-notify 
					   key-press
					   key-release
					   focus-change)
			     background: (screen-white-pixel scrn)))
	 ; create the status view
	 (statwin (create-window parent: win
				 x: (origin-x statwin-frame)
				 y: (origin-y statwin-frame)
				 width: (size-width statwin-frame)
				 height: (size-height statwin-frame)
				 background: cbg
				 border: cbd
				 event-mask: '(exposure)
				 border-width: 1))
	 ; create the bottom scrollbar
	 (vx (view-extent self))
	 (botscr (make-horz-scrollbar x: (origin-x horzsb-frame)
				      y: (origin-y horzsb-frame)
				      width: (size-width horzsb-frame)
				      parent: win
				      position: (+ (x (view-origin self))
						   (/ content-w 2))
				      min-value: 0
				      max-value: (width vx)
				      value-scope: content-w))
	 ; create the right scrollbar
	 (rtscr (make-vert-scrollbar x: (origin-x vertsb-frame)
				     y: (origin-y vertsb-frame)
				     height: (size-height vertsb-frame)
				     parent: win
				     position: (+ (y (view-origin self))
						  (/ content-h 2))
				     min-value: 0
				     max-value: (height vx)
				     value-scope: content-h))
	 (cursors (make-mode-cursors (screen-root scrn)))
	 ; create window for the content view
	 (cont (create-window parent: win
			      x: (origin-x contentwin-frame)
			      y: (origin-y contentwin-frame)
			      background: (screen-white-pixel scrn)
			      width: (size-width contentwin-frame)
			      height: (size-height contentwin-frame)
			      cursor: (cdr (assq 'select cursors))
			      event-mask: '(exposure 
					    button-press
					    button-release
					    button-motion))))
    (dm "content window: ~s" contentwin-frame)
    ;
    (change-property win
		     "WM_NAME" 
		     (format #f "~a <~a>"
			     (file-name doc)
			     (name self))
		     "STRING" 8)
    ;
    (map-window statwin)
    (map-window cont)
    (map-window win)
    ;
    ;
    (let ((ov (make <open-view>
		    in-document: doc
		    mode-cursors: cursors
		    underlying-object: self
		    current-selection: (make-object-table)
		    current-handles: (make-geometry-table)
		    current-geometry: (make-geometry-table)
		    current-major-mode: (get-major-mode 'select)
		    bottom-scrollbar: botscr
		    right-scrollbar: rtscr
		    status-window: statwin
		    selection-color: sel
                    grid-color: gridcolor
                    page-frame-color: (screen-black-pixel scrn)
		    grid-gcontext: (create-gcontext 
                                    drawable: cont
                                    foreground: gridcolor
                                    stipple: (gray-stipple scrn))
		    ;page-frame-color: cbg
		    main-window: win
		    paste-offset: (make-size 10 10)
		    content-window-size: (size contentwin-frame)
		    content-window: cont)))
      ;
      (add-close-handler win (lambda (info)
                               (close-view-with-review ov)))
      ;
      ; establish app pointers for all our X windows
      (set-property! cont 'open-view ov)
      (set-property! win 'open-view ov)
      (set-property! statwin 'open-view ov)
      ; set up the action procedures for the scrollbars
      (set-action! botscr
		   (lambda (sender)
		     (do-pan ov (make-size (- (scrollbar-value-range sender)
					      (x (view-origin self)))
					   0))))
      (set-action! rtscr
		   (lambda (sender)
		     (do-pan ov (make-size 0
					   (- (scrollbar-value-range sender)
					      (y (view-origin self)))))))
      (set-property! win
		     'key-state
		     (make <key-state>
			   initial-keymap: (list *graphic-keymap*
						 *global-keymap*)
			   owner: ov))
      ;
      (set-property! cont 'exposure-thunk (lambda ()
					    (redraw-open-view ov)))
      (set-property! statwin
		     'exposure-thunk
		     (lambda ()
		       (redraw-status-view ov)))
      (set-open-views! doc (cons ov (open-views doc)))
      (set-open-views! (current-client)
		       (cons ov (open-views (current-client))))
      ov)))

(define (window-exposure-thunk w)
  (get-property w 'exposure-thunk #f))
      
(define (window->open-view w)
  (get-property w 'open-view #f))

(define (open-document (doc <document>) #optional (file default: "Untitled"))
  (let ((od (make <open-document>
		  file-name: file
		  underlying-object: doc)))
    (table-insert! *document-table* doc od)
    (for-each
     (lambda (v)
       (open-view-window od v))
     (document-views doc))
    (display-force-output (on-display (current-client)))
    od))

(define-method window-ictm ((self <open-view>))
  (window-ictm (underlying-object self)))

(define-method set-window-cursor! ((self <open-view>) to)
  (if (eq? (current-cursor self) to)
      #f
      (begin
        (set-window-cursor! (content-window self) to)
        (set-current-cursor! self to)
        #t)))
