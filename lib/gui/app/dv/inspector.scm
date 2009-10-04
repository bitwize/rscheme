

(define-class <inspector> (<object>)
  for-client
  inspector-window
  inspector-cmap
  (inspector-locked? init-value: #f)
  (inspector-target init-value: #f)
  (sub-inspectors type: <list> init-value: '())
  sub-inspector-doc
  sub-inspector-page
  (active-sub-inspector init-value: #f))

(define-class <sub-inspector> (<object>)
  (owner type: <inspector>)
  (name type: <string>)
  sub-inspector-icon
  sub-inspector-icon-sel
  inspector-window)

(define *doc-icon* (make-graphic-image
		    from: (read-png-image "images/doc-insp.png")))
(define *page-icon* (make-graphic-image
		     from: (read-png-image "images/page-insp.png")))
(define *obj-icon* (make-graphic-image
		    from: (read-png-image "images/obj-insp.png")))

(define (make-sub-inspector-window (self <inspector>))
  (let ((scrn (on-screen (for-client self))))
    (create-window parent: (inspector-window self)
		   x: 0
		   y: 29
		   width: 220
		   height: 271
		   background: (vector-ref
				(pixels (get-bound-colors
					 (inspector-cmap self)
					 *tab-colors*))
				1)
		   event-mask: '(exposure))))

(define (make-doc-sub-inspector (self <inspector>))
  (let* ((win (make-sub-inspector-window self))
	 (scrn (drawable-screen win))
	 (cmap (screen-default-colormap scrn))
	 (gc (create-gcontext drawable: win
			      foreground: (screen-black-pixel scrn))))
    (set-gcontext-font! gc (get-property (drawable-display win)
					 'inspection-font))
    (set-property!
     win
     'exposure-thunk
     (lambda ()
       (dm 333 "doc exposed")
       (clear-area win)
       (let ((open-view (next-owner (for-client self))))
	 (if open-view
	     (let ((doc (in-document (underlying-object open-view))))
	       (draw-glyphs win gc 5 16
			    (format #f "~d pages" 
				    (vector-length (document-pages doc)))))))))
    (make <sub-inspector>
      owner: self
      name: "Document"
      sub-inspector-icon: *doc-icon*
      sub-inspector-icon-sel: *doc-icon*
      inspector-window: win)))

(define (make-page-sub-inspector (self <inspector>))
  (let ((win (make-sub-inspector-window self)))
    (make <sub-inspector>
      owner: self
      name: "Page"
      sub-inspector-icon: *page-icon*
      sub-inspector-icon-sel: *page-icon*
      inspector-window: win)))

(define-method make-target-sub-inspectors ((self <object>) (insp <inspector>))
  '())

(define (reset-inspector-target (self <inspector>) target)
  (let ((subs (list (sub-inspector-doc self)
		    (sub-inspector-page self))))
    (if target
	(set! subs (append subs (make-target-sub-inspectors target self))))
    (set-sub-inspectors! self subs)
    (set-active-sub-inspector! self (car subs))
       ;; unmap all the inactive ones
    (for-each (lambda (si)
		(unmap-window (inspector-window si)))
	      (cdr (sub-inspectors self)))
       ;; map the active one
    (map-window (inspector-window (active-sub-inspector self)))
    (need-to-redraw (inspector-window self))
    (values)))

(define (make-inspector #optional target)
  (let* ((scrn (on-screen (current-client)))
	 (cmap (screen-default-colormap scrn))
	 (win (create-window parent: (screen-root scrn)
			     x: 32
			     y: 16
			     width: 220
			     height: 300
			     event-mask: '(exposure)
			     background: (vector-ref
					   (pixels (get-bound-colors
						    cmap
						    *tab-colors*))
					   3)))
	 (insp (make <inspector>
		 for-client: (current-client)
		 inspector-window: win
		 inspector-cmap: cmap
		 sub-inspector-doc: #f
		 sub-inspector-page: #f)))
    ;;
    (let ((gc (create-gcontext drawable: win
                               foreground: (screen-black-pixel scrn)
                               background: (screen-white-pixel scrn))))
      (set-gcontext-font! gc (get-property (drawable-display win)
					   'menu-font))
      (let ((t (make-tab-control win
				 (make-rect 0 10 220 19)
				 '("Document"
				   "Page"
				   "Selection"))))
	(set-selected-tab! t (vector-ref (tab-cells t) 0))
	(set-property! 
	 win
	 'exposure-thunk
	 (lambda ()
	   (clear-area win)))
	;(draw-object t win gc)
	;(if (active-sub-inspector insp))
	;(draw-glyphs win gc 5 16 (name (active-sub-inspector insp)))
	;;
	(set-sub-inspector-doc! insp (make-doc-sub-inspector insp))
	(set-sub-inspector-page! insp (make-page-sub-inspector insp))
	   ;;
	(change-property win "WM_NAME" "Inspector" "STRING" 8)
        (reset-inspector-target insp target)
        (map-window win)))))

(define (t)
  (make-inspector)
  (flush-client))
