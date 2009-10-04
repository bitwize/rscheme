;;;
;;;  major modes
;;;

(define-class <major-mode> (<object>)
  (name type: <symbol>)
  (button-press-proc type: <function>)
  (cursor init-value: #f))

(define *major-modes* '())

(define (add-major-mode! (m <major-mode>))
  (set! *major-modes* (cons (cons (name m) m) *major-modes*)))

(define (get-major-mode (name <symbol>))
  (let ((a (assq name *major-modes*)))
    (if a
	(cdr a)
	(error "~s: no such major mode" name))))

(define (set-major-mode! (self <open-view>) (m <major-mode>))
  (set-current-major-mode! self m)
  (reset-cursor! self))

(define (reset-cursor! (self <open-view>))
  (let* ((m (current-major-mode self))
	 (c (assq (name m) (mode-cursors self))))
    (if c
	(set-window-cursor! self (cdr c))
	(error "~s: major mode has no cursor" (name m)))))

(load "major/select.scm")
(load "major/zoom.scm")

(define-interactive (select-mode view)
  (interactive (owner))
  (set-major-mode! view (get-major-mode 'select)))

(graphic-set-key #\M-1 select-mode)
