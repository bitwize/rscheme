
(define-class <minibuffer> (<object>)
  (minibuffer-window type: <x-window>)
  (minibuffer-window-gc type: <x-gcontext>)
  ;
  (minibuffer-black type: <fixnum>)
  (minibuffer-light-bezel type: <fixnum>)
  (minibuffer-dark-bezel type: <fixnum>)
  ;
  (minibuffer-input-window type: <x-window>)
  (minibuffer-input-window-text-gc type: <x-gcontext>)
  (minibuffer-input-window-cursor-gc type: <x-gcontext>)
  ;
  (minibuffer-input-window-x type: <fixnum>)
  (minibuffer-input-window-frame type: <rect>)
  (minibuffer-prompt type: <string>)
  (minibuffer-contents type: <string> init-value: "" setter: set-mb-c!)
  (minibuffer-point type: <fixnum> init-value: 0)
  (minibuffer-cursor-drawn? type: <boolean> init-value: #t)
  ;
  (minibuffer-mailbox init-function: make-mailbox))

(define (set-minibuffer-contents! (self <minibuffer>) (text <string>))
  (set-mb-c! self text)
  (set-minibuffer-point! self (string-length text))
  (need-to-clear (minibuffer-input-window self)
		 (lambda ()
		   (redraw-minibuffer-input-window self))))

;  (clear-area (minibuffer-input-window self) exposures?: #t))


(define (redraw-minibuffer-window (self <minibuffer>))
  (clear-area (minibuffer-window self))
  (let ((gc (minibuffer-window-gc self)))
    (draw-bezel-border (minibuffer-input-window-frame self)
		       (minibuffer-window self)
		       gc
		       (minibuffer-light-bezel self)
		       (minibuffer-dark-bezel self))
    (set-gcontext-foreground! gc (minibuffer-black self))
    (draw-glyphs (minibuffer-window self) gc 5 14 (minibuffer-prompt self))))

(define (redraw-minibuffer-input-window (self <minibuffer>))
  (let ((w (minibuffer-input-window self)))
    (clear-area w)
    (let ((text (minibuffer-contents self)))
      (if (not (string=? text ""))
	  (draw-glyphs w (minibuffer-input-window-text-gc self) 3 12 text))
      (if (minibuffer-cursor-drawn? self)
	  (draw-rectangle w
			  (minibuffer-input-window-cursor-gc self)
			  (+ 3 (* (minibuffer-point self) 7)) 2 1 13
			  #t)))))

(define (make-minibuffer-window prompt)
  (bind ((scrn (on-screen (current-client)))
	 (beige (getcolor scrn "gray87" screen-white-pixel))
	 (light (getcolor scrn "gray66" screen-white-pixel))
	 (dark (getcolor scrn "gray33" screen-black-pixel))
	 (black (screen-black-pixel scrn))
	 (prompt-font (get-property (on-display (current-client))
				    'minibuffer-prompt-font))
	 (xdiv (+ 3 (text-extents prompt-font prompt)))
	 (win (create-window parent: (screen-root scrn)
			     x: 0
			     y: 0
			     width: 370
			     height: (+ 14 2 2)
			     event-mask: '(exposure
					   key-press
					   key-release
					   focus-change)
			     background: beige))
	 (f (make-rect xdiv 2 (- 370 xdiv 2) 14))
	 (inwin (create-window parent: win
			       x: (origin-x f)
			       y: (origin-y f)
			       width: (size-width f)
			       height: (size-height f)
			       event-mask: '(exposure)
			       background: (screen-white-pixel scrn))))
    (change-property win "WM_NAME" "Minibuffer:0" "STRING" 8)
    ;
    (let ((gc (create-gcontext drawable: win
			       foreground: (screen-black-pixel scrn)
			       background: (screen-white-pixel scrn)))
	  (igc (create-gcontext drawable: win
				foreground: (screen-black-pixel scrn)
				background: (screen-white-pixel scrn)))
	  (igcx (create-gcontext drawable: win
				 function: 'boole-xor
				 foreground: (bitwise-xor
					      (screen-black-pixel scrn)
					      (screen-white-pixel scrn))
				 line-width: 1)))
      (set-gcontext-font! igc (get-property (drawable-display win)
				   'minibuffer-text-font))
      (set-gcontext-font! gc prompt-font)
      (let ((mb (make <minibuffer>
		      minibuffer-prompt: prompt
		      ;
		      minibuffer-window: win
		      minibuffer-window-gc: gc
		      ;
		      minibuffer-black: (screen-black-pixel scrn)
		      minibuffer-light-bezel: light
		      minibuffer-dark-bezel: dark
		      ;
		      minibuffer-input-window: inwin
		      minibuffer-input-window-text-gc: igc
		      minibuffer-input-window-cursor-gc: igcx
		      ;
		      minibuffer-input-window-x: xdiv
		      minibuffer-input-window-frame: f)))
	(set-property! win 'minibuffer mb)
	(set-property! win
		       'exposure-thunk
		       (lambda ()
			 (redraw-minibuffer-window mb)))
	(set-property! inwin
		       'exposure-thunk
		       (lambda ()
			 (redraw-minibuffer-input-window mb)))
	(map-window inwin)
	(map-window win)
	(flush-client)
	mb))))

;; `r' is the inside rectangle

(define (draw-bezel-border (r <rect>) win gc light dark)
  (let ((x (origin-x r))
	(y (origin-y r))
	(w (size-width r))
	(h (size-height r)))
    (set-gcontext-foreground! gc dark)
    (draw-rectangle win gc x (- y 1) (+ w 1) 1 #t) ; top
    (draw-rectangle win gc (- x 1) (- y 1) 1 (+ h 1) #t) ; left
    (set-gcontext-foreground! gc light)
    (draw-rectangle win gc (- x 1) (+ y h) (+ w 1) 1 #t) ; bottom
    (draw-rectangle win gc (+ x w) y 1 (+ h 1) #t) ; right
    (values)))

;;;

(define (read-from-minibuffer (prompt-string <string>)
			      #key (initial-contents default: "")
			           (keymap default: (list *text-keymap*
							  *global-keymap*))
				   (read-proc default: identity))
  (let ((mb (make-minibuffer-window prompt-string)))
    (set-property! (minibuffer-window mb)
		   'key-state
		   (make <key-state>
			 initial-keymap: keymap
			 owner: mb))
    (if (string? initial-contents)
	(if (not (string=? initial-contents ""))
	    (set-minibuffer-contents! mb initial-contents))
	(begin
	  (if (not (string=? (car initial-contents) ""))
	      (set-minibuffer-contents! mb (car initial-contents)))
	  (set-minibuffer-point! (max 0
				      (min (string-length 
					    (car initial-contents))
					   (cdr initial-contents))))))
    (receive-message! (minibuffer-mailbox mb))))

;; M-x

(define-interactive (execute-extended-command owner event)
  (interactive (owner) (event))
  (let ((str (read-from-minibuffer "Command:")))
    (let ((f (handler-case
	      (eval (read (open-input-string str)))
	      ((<condition>)
	       #f))))
      (if (procedure? f)
	  (call-interactive f owner event)
          (let* ((x (format #f "~#@60s" f))
                 (d (make-string (string-length x) #\-)))
            (newline)
            (format #t "   +-~a-+\n" d)
            (format #t "   | ~a |\n" x)
            (format #t "   +-~a-+\n" d)
            (newline))))))

;
(define (mb-chpoint mb pt)
  (set-minibuffer-point! mb pt)
  (clear-area (minibuffer-input-window mb) exposures?: #t))

(define-interactive (finish mb)
  (interactive :synchronous (owner))
  (unmap-window (minibuffer-window mb))
  (send-message! (minibuffer-mailbox mb) (minibuffer-contents mb)))

(define-interactive (goto-sol mb)
  (interactive :synchronous (owner))
  (mb-chpoint mb 0))

(define-interactive (goto-prev mb)
  (interactive :synchronous (owner))
  (mb-chpoint mb (max 0 (- (minibuffer-point mb) 1))))

(define-interactive (goto-next mb)
  (interactive :synchronous (owner))
  (mb-chpoint mb 
	      (min (string-length (minibuffer-contents mb))
		   (+ (minibuffer-point mb) 1))))

(define-interactive (goto-eol mb)
  (interactive :synchronous (owner))
  (mb-chpoint mb (string-length (minibuffer-contents mb))))

(define-interactive (delete-prev-char mb)
  (interactive :synchronous (owner))
  (let ((pt (minibuffer-point mb))
	(text (minibuffer-contents mb)))
    (if (> pt 0)
	(begin
	  (set-minibuffer-contents!
	   mb
	   (string-append (substring text 0 (- pt 1))
			  (substring text pt)))
	  (set-minibuffer-point! mb (- pt 1))))))

(define-interactive (insert-char-self mb e)
  (interactive :synchronous (owner) (event))
  (let ((ch (car e))
	(pt (minibuffer-point mb))
	(text (minibuffer-contents mb)))
    (set-minibuffer-contents!
     mb
     (string-append (substring text 0 pt)
		    (string ch)
		    (substring text pt)))
    (set-minibuffer-point! mb (+ pt 1))))

;;;

(for-each
 (lambda (k)
   (textual-set-key (integer->char k) insert-char-self))
 (list-tail (range 127) 32))

(textual-set-key #\C-a goto-sol)
(textual-set-key #\C-e goto-eol)
(textual-set-key #\C-f goto-next)
(textual-set-key #\C-b goto-prev)

(textual-set-key #\C-h delete-prev-char)
(textual-set-key #\del delete-prev-char)
(textual-set-key 'backspace delete-prev-char)
(textual-set-key 'delete delete-prev-char)

(textual-set-key #\cr finish)
(textual-set-key 'return finish)

(global-set-key #\M-x execute-extended-command)
