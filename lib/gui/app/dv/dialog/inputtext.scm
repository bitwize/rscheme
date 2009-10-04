;;;

(define-class <dialog-input> (<dialog-item>)
  (dialog-input-contents type: <string> init-value: "" setter: set-di-c!)
  (dialog-input-point type: <fixnum> init-value: 0)
  (dialog-input-mark type: <fixnum> init-value: 0))

;;;

(define *dialog-text-keymap* (make-keymap))

(define (dialog-set-key key-spec proc)
  (keymap-set-key *dialog-text-keymap* key-spec proc))

(define (set-dialog-input-contents! (self <dialog-input>) (text <string>))
  (set-di-c! self text)
  (let ((m (string-length text)))
    (set-dialog-input-point! self m)
    (set-dialog-input-mark! self m))
  (need-to-clear (dialog-item-window self)
		 (get-property (dialog-item-window self) 'exposure-thunk)))

;;;

(define (dlg-insert di str)
  (let ((pt (dialog-input-point di))
	(mk (dialog-input-mark di))
	(text (dialog-input-contents di)))
    (set-dialog-input-contents!
     di
     (string-append (substring text 0 (min pt mk))
		    str
		    (substring text (max pt mk))))
    (set-dialog-input-mark! di (+ (min pt mk) 1))
    (set-dialog-input-point! di (+ (min pt mk) 1))))

(define-interactive (dlg-insert-char-self di e)
  (interactive :synchronous (owner) (event))
  (dlg-insert di (string (car e))))

(define (dlg-delete-region di from to)
  (let ((text (dialog-input-contents di)))
    (set-dialog-input-contents!
     di
     (string-append (substring text 0 from)
		    (substring text to)))
    (set-dialog-input-mark! di from)
    (set-dialog-input-point! di from)))

(define-interactive (dlg-delete-next di)
  (interactive :synchronous (owner))
  (let ((pt (dialog-input-point di))
	(mk (dialog-input-mark di))
	(text (dialog-input-contents di)))
    (if (= pt mk)
	(if (< pt (string-length text))
	    (dlg-delete-region di pt (+ pt 1)))
	(dlg-delete-region di (min pt mk) (max pt mk)))))

(define-interactive (dlg-delete-prev di)
  (interactive :synchronous (owner))
  (let ((pt (dialog-input-point di))
	(mk (dialog-input-mark di))
	(text (dialog-input-contents di)))
    (if (= pt mk)
	(if (> pt 0)
	    (dlg-delete-region di (- pt 1) pt))
	(dlg-delete-region di (min pt mk) (max pt mk)))))

(define (di-goto di pt)
  (set-dialog-input-point! di pt)
  (set-dialog-input-mark! di pt)
  (need-to-clear (dialog-item-window di)
		 (get-property (dialog-item-window di) 'exposure-thunk)))

(define-interactive (dlg-goto-sol di)
  (interactive :synchronous (owner))
  (di-goto di 0))

(define-interactive (dlg-goto-eol di)
  (interactive :synchronous (owner))
  (di-goto di (string-length (dialog-input-contents di))))

(define-interactive (dlg-goto-prev di)
  (interactive :synchronous (owner))
  (di-goto di (max 0 (- (dialog-input-point di) 1))))

(define-interactive (dlg-goto-next di)
  (interactive :synchronous (owner))
  (di-goto di (min (+ (dialog-input-point di) 1)
		   (string-length (dialog-input-contents di)))))

(define-interactive (dlg-goto-pred-field di)
  (interactive :synchronous (owner))
  (let ((fld (get-property di 'pred #f)))
    (if fld
	(let ((w (dialog-item-window fld)))
	  (set-input-focus (drawable-display w) w 'none)))))
      

(define-interactive (dlg-goto-succ-field di)
  (interactive :synchronous (owner))
  (let ((succ (get-property di 'succ #f)))
    (if succ
	(let ((w (dialog-item-window succ)))
	  ; this'll get trashed by focus-in, though...
	  (set-dialog-input-mark! succ 0)
	  (set-input-focus (drawable-display w) w 'none)))))
      

;;;----------------------------------------------------------------------
;;;
;;;			     Key bindings
;;;

(for-each
 (lambda (k)
   (dialog-set-key (integer->char k) dlg-insert-char-self))
 (list-tail (range 127) 32))

(dialog-set-key #\C-a dlg-goto-sol)
(dialog-set-key #\C-e dlg-goto-eol)
(dialog-set-key #\C-f dlg-goto-next)
(dialog-set-key #\C-b dlg-goto-prev)

(dialog-set-key #\C-h dlg-delete-prev)
(dialog-set-key #\del dlg-delete-prev)
(dialog-set-key 'backspace dlg-delete-prev)
(dialog-set-key 'delete dlg-delete-prev)
(dialog-set-key #\C-d dlg-delete-next)

(dialog-set-key #\tab dlg-goto-succ-field)
(dialog-set-key 'tab dlg-goto-succ-field)

#|
(dialog-set-key #\cr dlg-finish)
(dialog-set-key 'return dlg-finish)
|#

(define (build-dialog-input-text-exposure item win colorv at add-ditem)
  (let* ((sz (compute-dialog-elem-size item))
	 (inpwin (create-window parent: win
				x: (+ (x at) 1)
				y: (+ (y at) 1)
				width: (- (dx sz) 2)
				height: (- (dy sz) 2)
				background: (vector-ref colorv 3)
				event-mask: '(button-press
					      button-release
					      button-motion
					      key-press
					      key-release
					      exposure
					      focus-change)))
	 (scrn (drawable-screen win))
	 (gc (create-gcontext drawable: win))
	 (gcsel (create-gcontext drawable: win
				 foreground: (get-dk-color scrn
							   (make-color
							    red: 0.8
							    green: 0.8
							    blue: 0.9))))
	 (gctxt (create-gcontext drawable: win
				 foreground: (screen-black-pixel scrn)))
	 (gcxor (create-gcontext drawable: win
				 function: 'boole-xor
				 foreground: (bitwise-xor
					      (screen-black-pixel scrn)
					      (screen-white-pixel scrn))
				 line-width: 1))
	 (cursor-drawn? #f)
	 (fm (make-rect (x at) (y at) (dx sz) (dy sz)))
	 (di (make <dialog-input>
		   name: 'unnamed
		   dialog-item-window: inpwin)))
    ;
    (define (parse-args width #key name (succ default: #f) (pred default: #f))
      (set-name! di name)
      (let ((xr (append (if pred (list (cons 'pred pred)))
			(if succ (list (cons 'succ succ))))))
	(set-property! di 'xrefs xr)))
    ;
    (define (X x)   ; convert position to coordinate 
      (+ 3 (* x 7)))
    (define (unX x) ; convert coordinate to position
      (min (string-length (dialog-input-contents di))
	   (max 0 (inexact->exact (round (/ (- x 3) 7))))))
    ;
    (define (render-sel)
      (let ((pt (dialog-input-point di))
	    (mk (dialog-input-mark di)))
	(cond
	 ((< mk pt)
	  (draw-rectangle inpwin
			  gcsel
			  (X mk)
			  2
			  (- (X pt) (X mk))
			  13
			  #t))
	 ((> mk pt)
	  (draw-rectangle inpwin
			  gcsel
			  (+ 1 (X pt))
			  2
			  (- (X mk) (+ 1 (X pt)))
			  13
			  #t)))))
    ;
    (define (render-cursor)
      (draw-rectangle inpwin
		      gcxor
		      (X (dialog-input-point di))
		      2 1 13
		      #t))
    ;
    (define (redraw)
      (clear-area inpwin)
      (if cursor-drawn?  ;; really want to ask focus-in?
	  (render-sel))
      (let ((t (dialog-input-contents di)))
	(if (> (string-length t) 0)
	    (draw-glyphs inpwin gctxt 3 12 t))
	(if cursor-drawn?
	    (render-cursor))))
    ;
    (apply parse-args (cdr item))
    ;
    (set-property! inpwin 'exposure-thunk redraw)
    ;
    (set-gcontext-font! gctxt (get-client-font 'default-text-font))
    (add-ditem di)
    (map-window inpwin)
    (set-property! inpwin
		   'button-press
		   (lambda (win at state)
		     (set-dialog-input-mark! di (unX (x at)))
		     (set-dialog-input-point! di (unX (x at)))
		     (if cursor-drawn?
			 (redraw)
			 (set-input-focus (drawable-display win)
					  inpwin
					  'none))))
    (set-property! inpwin
		   'button-motion
		   (lambda (win at state)
		     (set-dialog-input-point! di (unX (x at)))
		     (redraw)
		     ; I don't understand why this is needed...
		     ; the main event loop should flush-client itself...
		     (flush-client)))
    (set-property! inpwin
		   'key-state
		   (make <key-state>
			 initial-keymap: (list *dialog-text-keymap*
					       *global-keymap*)
			 owner: di))
    (set-property! inpwin
		   'focus-in
		   (lambda (win mode kind)
		     (set! cursor-drawn? #t)
		     (redraw)))
    (set-property! inpwin
		   'focus-out
		   ; it doesn't seem to be picking up focus-out's
		   ; to other (non-appl) windows...
		   (lambda (win mode kind)
		     (set! cursor-drawn? #f)
		     (redraw)))
    ; return the thunk for drawing the part of us that is in the
    ; outer X window, ie, the bezel
    (lambda ()
      (draw-bezeled win gc
		    fm
		    colorv
		    '(1 top left
			2 bottom right)))))

(define (compute-dialog-input-elem-size item)
  (let ((fnt (get-client-font 'default-text-font)))
    (make-size (+ 2 (* (cadr item) (char-width fnt (char->integer #\m))))
	       (+ 2 2
		  (font-ascent fnt)
		  (font-descent fnt)))))
