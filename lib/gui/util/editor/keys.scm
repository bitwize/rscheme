(define (control-state? state)
  (eq? #b100 (bitwise-and state #b100)))

(define (meta-state? state)
  (eq? #b1000 (bitwise-and state #b1000)))

(define (shift-state? state)
  (eq? #b1 (bitwise-and state #b1)))

(define (shiftlock-state? state)
  (eq? #b10 (bitwise-and state #b10)))

(define (cap-state? state)
  (not (eq? 0 (bitwise-and state #b11))))
(define *keysym-control-tbl*
  '((#xFF08 backspace)
    (#xFF0D return)
    (#xFF50 home) ;; home
    (#xFF51 left) ;; left
    (#xFF52 up) ;; up
    (#xFF53 right) ;; right
    (#xFF54 down) ;; down
    (#xFFFF delete)))

(define (keysym->event-name chc)
  (let ((a (assq chc *keysym-control-tbl*)))
    (if a
	(cadr a)
	(if (and (> chc 1) (< chc 128))
	    (integer->char chc)
	    (symbol-append "keysym-" chc)))))

(define (key->char (code <fixnum>) (state <fixnum>))
  (let* ((ks (vector-ref (x-keyboard-mapping *display*) code))
	 (ix (min (sub1 (vector-length ks))
		  (bitwise-and state 1)))
	 (event (keysym->event-name (vector-ref ks ix)))
	 (with-ctl (if (control-state? state)
		       (prepend-control-status event)
		       event))
	 (with-meta (if (meta-state? state)
			(prepend-meta-status with-ctl)
			with-ctl)))
    with-meta))

(define (prepend-control-status event)
  (if (char? event)
      (integer->char (- (char->integer (char-upcase event)) 64))
      (symbol-append "C-" event)))

(define (prepend-meta-status event)
  (if (char? event)
      (integer->char (+ 128 (char->integer event)))
      (symbol-append "M-" event)))
