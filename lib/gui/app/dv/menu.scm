(define-syntax (if-own-menu a b) b) ;; use override-redirect for TL menu?

(define-color-list *menu-colors*
  "black"
  "rgbi:0.166/0.166/0.2"
  "rgbi:0.333/0.333/0.4"
  "rgbi:0.5/0.5/0.667"
  "white")

(define (get-menu-title-colors scrn)
  (pixels (get-bound-colors (screen-default-colormap scrn) *menu-colors*)))

(define (pop-up-menu scrn title (items <vector>) at in-window)
  (bind ((root (screen-root scrn))
         (itemh (size-height (frame (vector-ref items 0))))
         (x0 y0 (translate-coordinates in-window (x at) (y at) root))
         (w (create-window parent: root
                           x: x0
                           y: y0
                           width: (apply max (map (lambda (c)
                                                    (size-width (frame c)))
                                                  (vector->list items)))
                           height: (* itemh (vector-length items))
                           override-redirect: #t
                           save-under: #t
                           border-width: 0
                           event-mask: '(exposure 
                                         button-motion
                                         button-press
                                         button-release)))
	 (cv (get-scrollbar-colors scrn))
         (gc (create-gcontext drawable: w)))
    (map-window w)
    (set-property! w 
                   'exposure-thunk
                   (lambda ()
                     (let loop ((i 0))
                       (if (< i (vector-length items))
                           (begin
                             (draw-menu-cell (vector-ref items i) w gc cv)
                             (loop (+ i 1)))))))
    (set-property! w
                   'button-release
                   (lambda (in at state)
                     (unmap-window w)))))

(define (make-menu scrn title (items <vector>) #optional at)
  (let* ((itemh (size-height (frame (vector-ref items 0))))
	 (maxw (size-width (frame (vector-ref items 0))))
	 (cv (get-scrollbar-colors scrn))
	 (cv2 (if-own-menu (get-menu-title-colors scrn) #f))
	 (win (create-window parent: (screen-root scrn)
			     override-redirect: (if at
                                                    #t
                                                    (if-own-menu #t #f))
			     x: (if at (x at) 16)
			     y: (if at (y at) 16)
			     background: (vector-ref cv 2)
			     width: maxw
			     height: (* itemh (+ (if-own-menu 1 0)
						 (vector-length items)))
			     event-mask: '(button-press
					   button-release
					   button-motion
					   key-press
					   key-release
					   exposure
					   structure-notify)))
	(gc (create-gcontext drawable: win
			     foreground: (screen-black-pixel scrn)
			     background: (screen-white-pixel scrn)))
	(menu-font (get-property (drawable-display win) 'menu-font)))
    (set-property! win 'key-state (make <key-state>
					initial-keymap: (list *global-keymap*)
					owner: 'next-owner))
    (change-property win "WM_NAME" title "STRING" 8)
    (add-close-handler win (lambda (info)
                             (call-interactive exit-client-with-review)))
    (set-property! win
		   'button-press
		   (lambda (in at state)
		     ;; for now, do no tracking -- instant action buttons
		     (let loop ((i 0))
		       (if (< i (vector-length items))
			   (let ((mc (vector-ref items i)))
			     (if (point-in-rect? (frame mc) at)
				 (if (action mc)
				     (call-interactive (action mc)
						       'next-owner
						       #f))
				 (loop (+ i 1))))))))
    (if-own-menu 
     (begin)
     (set-gcontext-font! gc menu-font))
    (set-property! win
		   'exposure-thunk
		   (let ((tf (make-rect 0 0 maxw itemh))
			 (menu-title-font (if-own-menu
					   (get-property 
					    (drawable-display win) 
					    'menu-title-font)
					   #f)))
		     (lambda ()
		       (if-own-menu
			(begin
			  (set-gcontext-font! gc menu-title-font)
			  (draw-menu-title title tf win gc cv2)
			  (set-gcontext-font! gc menu-font))
			(begin))
		       (let loop ((i 0))
			 (if (< i (vector-length items))
			     (begin
			       (draw-menu-cell (vector-ref items i) win gc cv)
			       (loop (+ i 1))))))))
    (map-window win)
    win))

(define-class <menu-cell> (<object>)
  (frame type: <rect>)
  (title type: <string>))

(define-class <command-cell> (<menu-cell>)
  key-equiv
  action)

(define-class <submenu-cell> (<menu-cell>)
  submenu)

#|
(define-method draw-menu-cell ((cell <command-cell>) win gc colorv)
  (next-method)
  (if (key-equiv cell)  (draw-glyphs win gc ...)))

(define-method draw-menu-cell ((cell <submenu-cell>) win gc colorv)
  (next-method)
  (draw-image "->" ...))
|#
  

; only used when `if-own-menu' evaluates its first argument

(define (draw-menu-title title frame win gc colorv)
  (draw-bezeled win gc
		frame
		colorv
		'(0 bottom right
		  3 left top
		  1 bottom right
		  2 middle))
  (set-gcontext-foreground! gc (vector-ref colorv 4))
  (draw-glyphs win gc
	       6 (- (limit-y frame) 6)
	       title))


(define-method draw-menu-cell ((cell <menu-cell>) win gc colorv)
  (draw-bezeled win gc
		(frame cell)
		colorv
		'(0 bottom right
		  3 left top
		  1 bottom right
		  2 middle))
  (set-gcontext-foreground! gc (vector-ref colorv 0))
  (draw-glyphs win gc
	       6 (- (limit-y (frame cell)) 6)
	       (title cell))
  (draw-menu-cell-extra cell win gc colorv))

(define-method draw-menu-cell-extra ((cell <menu-cell>) win gc colorv)
  (values))

(define-method draw-menu-cell-extra ((cell <command-cell>) win gc colorv)
  (if (key-equiv cell)
      (draw-glyphs win gc
		   (- (limit-x (frame cell))
		      (car (key-equiv cell))
		      6)
		   (- (limit-y (frame cell)) 6)
		   (cdr (key-equiv cell)))))

(define (make-menu-cells dpy (items <vector>))
  (let* ((n (vector-length items))
	 (c (make-vector n))
	 (fnt (get-property dpy 'menu-font))
	 (ke (make-dequeue))
	 (maxw 40))
    (vector-for-each
     (lambda (descr)
       (let ((w (+ 24 (text-extents fnt (car descr)))))
	 (if (pair? (cddr descr))
	     (let ((kew (text-extents fnt (caddr descr))))
	       (dequeue-push-back! ke kew)
	       (set! w (+ w (- kew 8)))))
	 (set! maxw (max maxw w))))
     items)
    (let loop ((i 0))
      (if (< i n)
	  (let ((descr (vector-ref items i)))
	    (vector-set! c i (make <command-cell>
				   frame: (make-rect 0 
						     (* (if-own-menu
							 (+ i 1)
							 i)
							20)
						     maxw 
						     20)
				   key-equiv: (if (pair? (cddr descr))
						  (cons
						   (dequeue-pop-front! ke)
						   (caddr descr))
						  #f)
				   action: (make-menu-action descr)
				   title: (car descr)))
	    (loop (+ i 1)))
	  c))))

(define (make-menu-action descr)
  ; eagerly evaluate the symbol as a function name
  (eval (cadr descr)))

(define-interactive (open-test-doc)
  (interactive)
  (open-document (make-test-doc)))

(define-interactive (open-new-doc)
  (interactive)
  (open-document (make-new-doc)))

(global-set-key #\M-n open-new-doc)
(global-set-key #\M-t open-test-doc)

(define (open-main-menu (client <client>))
  (set-main-menu!
   client
   (make-menu (on-screen client)
	      "dV"
	      (make-menu-cells 
	       (on-display client)
	       '#(("About..." show-about-window)
		  ("New" open-new-doc "M-n")
		  ("Save" save-file "^x ^s")
		  ("Save As..." save-as-file)
		  ("Load..." load-file "^x ^f")
		  ("Test Doc" open-test-doc "M-t")
		  ("Quit" exit-client-with-review "^x ^c")))))
  (flush-client client))
