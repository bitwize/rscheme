;;;
;;;  This key mapping facility stores a `keyboard-mapping' property
;;;  on the display object, which should be updated using
;;;  `update-keyboard-mapping' whenever a keyboard-mapping-change
;;;  event arrives
;;;

(define (get-keyboard-mapping dpy)
  (get-property dpy
		'keyboard-mapping
		(let ((m (keyboard-mapping dpy)))
		  (set-property! dpy 'keyboard-mapping m)
		  m)))

(define *keysym-control-tbl*
  '((#xFF08 backspace)
    (#xFF0D return)
    (#xFF50 home)    ;; home
    (#xFF52 up)      ;; up
    (#xFF54 down)    ;; down
    (#xFF51 left)    ;; left
    (#xFF53 right)   ;; right
    (#xFFFF delete)))

(define (keysym->event-name chc)
  (let ((a (assq chc *keysym-control-tbl*)))
    (if a
	(cadr a)
	(if (and (> chc 1) (< chc 128))
	    (integer->char chc)
	    (symbol-append "keysym-" chc)))))

;;;  converts an X key event (keycode + state) into an
;;;  event descriptor, which is either a character or a symbol
;;;  a la GNU emacs.

(define (key->event display (code <fixnum>) (state <fixnum>))
  (let* ((ks (vector-ref (get-keyboard-mapping display) code))
	 (ix (min (sub1 (vector-length ks))
		  (bitwise-and state 1)))
	 (event (keysym->event-name (vector-ref ks ix)))
	 (with-ctl (if (control-state? state)
		       (prepend-control-status event)
		       event))
	 (with-meta (if (meta-state? state)
			(prepend-meta-status with-ctl)
			with-ctl)))
    (dm 101 "KEY (keycode ~d) (state ~b) (event ~s) => ~s\n"
	    code
	    state
	    event
	    with-meta)
    with-meta))

(define (shift-state? state)
  (eq? #b1 (bitwise-and state #b1)))

(define (shiftlock-state? state)
  (eq? #b10 (bitwise-and state #b10)))

(define (control-state? state)
  (eq? #b100 (bitwise-and state #b100)))

(define (meta-state? state)
  (eq? #b1000 (bitwise-and state #b1000)))

(define (cap-state? state)
  (not (eq? 0 (bitwise-and state #b11))))

;;;

(define (prepend-control-status event)
  (if (char? event)
      (integer->char (- (char->integer (char-upcase event)) 64))
      (symbol-append "C-" event)))

(define (prepend-meta-status event)
  (if (char? event)
      (integer->char (+ 128 (char->integer event)))
      (symbol-append "M-" event)))
