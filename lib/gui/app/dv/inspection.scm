
(define (make-inspection-window from-window title)
  (let* ((scrn (drawable-screen from-window))
	 (win (create-window parent: (screen-root scrn)
			     x: 0
			     y: 0
			     width: 370
			     height: 180
			     event-mask: '(exposure
					   focus-change
					   key-press
					   key-release)
			     background: (screen-white-pixel scrn))))
    (change-property win "WM_NAME" title "STRING" 8)
    (map-window win)
    (let ((lines '#())
          (gc (create-gcontext drawable: win
                               foreground: (screen-black-pixel scrn)
                               background: (screen-white-pixel scrn))))
      (set-gcontext-font! gc (get-property (drawable-display win)
                                  'inspection-font))
      (set-property! 
       win
       'exposure-thunk
       (lambda ()
         (clear-area win)
         (let loop ((i 0))
           (if (< i (vector-length lines))
               (begin
                 (draw-glyphs win gc 5 (+ (* i 16) 14) (vector-ref lines i))
                 (loop (+ i 1)))))))
      ; return a procedure for adding a line to the inspector
      (lambda (str)
        (set! lines (vector-append lines (vector str)))
        (clear-area win exposures?: #t)
        (display-force-output (drawable-display win))))))

(define-interactive (inspect win sel)
  (interactive (owner) (selection))
  (for-each
   (lambda (item)
     (let* ((lbl (status-line-when-sel item))
	    (append-line (make-inspection-window (main-window win) lbl)))
       (for-each
	append-line
	(string-split
	 (with-output-to-string
	   (lambda ()
	     (print item)))
	 #\newline))))
   sel))

(graphic-set-key #\M-q inspect)
