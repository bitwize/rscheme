(define-class <dialog-item> (<object>) :abstract
  (properties type: <vector> init-value: '#())
  name
  dialog-item-window)

(define-class <dialog-button> (<dialog-item>)
  action-thunk)

;;;  a metaobject for dialog items -- instances are managers
;;;  for the things that can appear in descriptors, and know
;;;  how to size and create redraw thunks for their instances

(define-class <dialog-class> (<object>)
  name
  build-exposure-thunk
  compute-size)

(define *dialog-item-types* (make-symbol-table))

;;;

(define (query-dialog title descriptor response-callback)
  (let* ((scrn (on-screen (current-client)))
	 (dpy (on-display (current-client)))
	 (size (compute-dialog-elem-size descriptor))
	 (colorv (get-scrollbar-colors scrn))
	 (ditems '())
	 (add-ditem (lambda ((item <dialog-item>))
		      (set! ditems (cons item ditems))))
	 (w (create-window parent: (screen-root scrn)
			   x: 100
			   y: 100
			   width: (dx size)
			   height: (dy size)
			   event-mask: '(key-press
					 key-release
					 focus-change
					 exposure)
			   background: (vector-ref colorv 2))))
    (change-property w "WM_NAME" title "STRING" 8)
    (let ((xt (build-dialog-exposure-thunk descriptor w colorv $zero-point
					   add-ditem)))
      (set-property! w 'response-callback 
		     (lambda (a)
		       (unmap-window w) ;; XXX need to really free it
		       ;; XXX and also all the other resources we alloc'd
		       (response-callback a)))
      (set-property! w
		     'exposure-thunk
		     (lambda ()
		       (clear-area w)
		       (xt))))
    ; fix up any dialog-item cross-references
    (let ((alist (map (lambda (di)
			(cons (name di) di))
		      ditems)))
      (for-each 
       (lambda (di)
	 (for-each 
	  (lambda (xr)
	    (let ((p (assq (cdr xr) alist)))
	      (if p
		  (set-property! di (car xr) (cdr p))
		  (wm 713 "~s: (~s) xref `~s' not resolved" 
		      (name di) 
		      (car xr)
		      (cdr xr)))))
	  (or (get-property di 'xrefs) '())))
       ditems))
    ;
    (map-window w)
    (flush-client)
    w))

(define (get-client-font fnt)
  (get-property (on-display (current-client)) fnt))

;;; For some reason, this is just hugely wrong
;;; if we use `text-extents'
;;;
;;; for example, for f=[dialog-label-font]
;;;  ("-adobe-helvetica-medium-r-normal--12-*-*-*-*-*-*-*")
;;;
;;;   (text-extents f "Inp") = 9
;;;
;;;   but (char-width f #\I) = 3  +
;;;       (char-width f #\n) = 7  +
;;;       (char-width f #\p) = 7  =  17
;;;
;;;  ...so we bail and sume the individual char widths
 
(define (text-box str font)
  (bind ((fnt (get-client-font font))
	 (width asc desc left right (text-extents fnt str)))
    (make-size (reduce + 0 (map (lambda (ch)
				  (char-width fnt (char->integer ch)))
				(string->list str)))
	       (+ asc desc))))

(define (build-dialog-exposure-thunk item win colorv (at <point>) add-ditem)
  ;(dm "build-thunk: ~#*40s" item)
  (case (car item)
    ((inset)
     (build-dialog-exposure-thunk (caddr item)
				  win colorv 
				  (point+ at 
					  (make-size (cadr item)
						     (cadr item)))
				  add-ditem))
    ((vert)
     (if (null? (cdr item))
	 (lambda ())
	 (let* ((sz (compute-dialog-elem-size (cadr item)))
		(this (build-dialog-exposure-thunk 
		       (cadr item)
		       win
		       colorv
		       at
		       add-ditem))
		(succ (build-dialog-exposure-thunk 
		       (cons 'vert (cddr item))
		       win
		       colorv
		       (point+ at (make-size 0 (dy sz)))
		       add-ditem)))
	   (lambda ()
	     (this)
	     (succ)))))
    ((horz)
     (if (null? (cdr item))
	 (lambda ())
	 (let* ((sz (compute-dialog-elem-size (cadr item)))
		(this (build-dialog-exposure-thunk 
		       (cadr item)
		       win
		       colorv
		       at
		       add-ditem))
		(succ (build-dialog-exposure-thunk 
		       (cons 'horz (cddr item))
		       win
		       colorv
		       (point+ at (make-size (dx sz) 0))
		       add-ditem)))
	   (lambda ()
	     (this)
	     (succ)))))
    ((big-label)
     (let ((fnt (get-client-font 'dialog-big-font))
	   (gc (create-gcontext drawable: win
				foreground: (screen-black-pixel 
					     (drawable-screen win))))
	   (lbl (cadr item)))
       (bind ((width asc desc left right (text-extents fnt lbl)))
	 (set-gcontext-font! gc fnt)
	 (lambda ()
	   (draw-glyphs win gc (x at) (+ asc (y at)) lbl)))))
    ((label)
     (let ((fnt (get-client-font 'dialog-label-font))
	   (gc (create-gcontext drawable: win
				foreground: (screen-black-pixel 
					     (drawable-screen win))))
	   (lbl (cadr item)))
       (bind ((width asc desc left right (text-extents fnt lbl)))
	 (set-gcontext-font! gc fnt)
	 (lambda ()
	   (draw-glyphs win gc (x at) (+ asc (y at)) lbl)))))
    ((horz-rule)
     ; skip for now
     (lambda ()))
    ((input)
     (build-dialog-input-text-exposure item win colorv at add-ditem))
    ((button)
     (let* ((sz (compute-dialog-elem-size item))
	    (butwin (create-window parent: win
				   x: (x at)
				   y: (y at)
				   width: (dx sz)
				   height: (dy sz)
				   background: (vector-ref colorv 2)
				   event-mask: '(button-press
						 button-release
						 button-motion
						 exposure)))
	    (fnt (get-client-font 'dialog-label-font))
	    (gc (create-gcontext drawable: win
				 foreground: (screen-black-pixel 
					      (drawable-screen win))))
	    (lbl (cadr item))
	    (fm (make-rect 0 0 (dx sz) (dy sz)))
	    (label-x $button-left-margin)
	    (label-y (+ $button-top-margin
			; I had (font-descent) in here too, for centering,
			; but it seems that font-ascent is bigger than
			; the X height (perhaps for umlaut characters?)
			;(font-descent fnt) ; for centering
			(font-ascent fnt))))
       (map-window butwin)
       (bind ((width asc desc left right (text-extents fnt lbl)))
	 (set-gcontext-font! gc fnt)
	 (set-property! butwin
			'button-press
			(lambda (in at state)
			  ;; for now, do no tracking -- instant action
			  ((get-property win 'response-callback)
			   (cadr (memq 'value: item)))))
	 (set-property! butwin
			'exposure-thunk
			(lambda ()
			  (draw-bezeled butwin gc
					fm
					colorv
					'(0 bottom right
					    3 left top
					    1 bottom right
					    ;1 left top ; test balance
					    #|2 middle|#))
			  (set-gcontext-foreground! gc (vector-ref colorv 0))
			  (draw-glyphs butwin gc label-x label-y lbl)))
	 (lambda ()))))
    (else
     (lambda ()))))

(define $button-left-margin 6)
(define $button-right-margin 6)
(define $button-top-margin 4)
(define $button-bottom-margin 4)

(define (compute-dialog-elem-size item)
  (case (car item)
    ((inset)
     (let ((s (compute-dialog-elem-size (caddr item))))
       (make-size (+ (dx s) (* 2 (cadr item)))
		  (+ (dy s) (* 2 (cadr item))))))
    ((vert)
     (reduce (lambda (max-size sub-item)
	       (let ((ss (compute-dialog-elem-size sub-item)))
		 (make-size (max (dx max-size) (dx ss))
			    (+ (dy max-size) (dy ss)))))
	     (make-size 0 0)
	     (cdr item)))
    ((horz)
     (reduce (lambda (max-size sub-item)
	       (let ((ss (compute-dialog-elem-size sub-item)))
		 (make-size (+ (dx max-size) (dx ss))
			    (max (dy max-size) (dy ss)))))
	     (make-size 0 0)
	     (cdr item)))
    ((big-label)
     (text-box (cadr item) 'dialog-big-font))
    ((label)
     (text-box (cadr item) 'dialog-label-font))
    ((horz-rule)
     (make-size 10 4))
    ((button)
     (let ((s (text-box (cadr item) 'dialog-label-font))
	   (fnt (get-client-font 'dialog-label-font)))
       ;(dm "ascent = ~d, descent = ~d" (font-ascent fnt) (font-descent fnt))
       (make-size (+ $button-left-margin $button-right-margin (dx s))
		  (+ $button-top-margin
		     ;(font-descent fnt)
		     (font-ascent fnt)
		     (font-descent fnt)
		     $button-bottom-margin))))
    ((input)
     (compute-dialog-input-elem-size item))
    (else (em 410 "Unknown dialog element: ~s" item))))

(define *exit-ok?*
  '(inset
    5
    (vert
     (big-label "Are you sure you want to exit?")
     (horz-rule)
     (horz (inset 3 (button "No" value: #f))
	   (inset 3 (button "Yes" :default value: #t)))
     (horz (label "Input: ") (input 40 name: inp succ: out))
     (horz (label "Output: ") (input 12 name: out pred: inp succ: style))
     (horz (label "Style: ") (input 12 name: style pred: out)))))

(define (t)
  (with-client (car *all-clients*)
   (lambda ()
     (query-dialog "Exit OK?"
		   *exit-ok?*
		   (lambda (answ)
		     (if answ
			 (process-exit 0)))))))

(define-class <cancelled> (<condition>)
  cancelled-what)

(define-method display-object ((self <cancelled>) port)
  (format port "Cancelled `~a'\n" (cancelled-what self)))

(define (verify-something title text)
  (let ((a (call-with-current-continuation
	    (lambda (exit)
	      (query-dialog title
			    `(inset
			      5
			      (vert
			       (big-label ,text)
			       (horz-rule)
			       (horz (inset 3 (button "Cancel" value: cancel))
				     (inset 3 (button "No" value: #f))
				     (inset 3 (button "Yes" :default 
						      value: #t)))))
			    (lambda (answ)
			      (dm "Verify (~a) => ~s" title answ)
			      (exit answ)))
	      (app-event-loop (current-client))))))
    (if (eq? a 'cancel)
	(signal (make <cancelled>
		      cancelled-what: title))
	a)))
