;; windowing interface to Tk library

,(use threads)

(load "geom.scm")

(define *gui-interp* (make-tcl-interp))

(define (gui-eval fmt . args)
  (tcl-eval *gui-interp* (apply format #f fmt args)))


(define (init-windows)
  (let ((w (tk-create-main-window #f "rscheme" "RScheme")))
    (tk-make-window-exist w)
    (run-threads
     (make <thread>
	   thunk: handle-tk-events))))


(define-class <gui-object> (<object>)
  containing-object)

(define-generic-function gui-action)
(define-generic-function gui-click)
(define-generic-function gui-drag)
(define-generic-function draw-yourself)

(define-class <tk-object> (<gui-object>)
  tk-name
  tk-tag)    ;; bound to the callback

;; note that the fact that <window> inherits from <tk-object>
;; instead of <gui-object> is a hidden fact

(define-class <window> (<tk-object>)
  title
  frame)

(define-method initialize ((self <window>))
  (set-tk-names! self "win")
  (gui-eval "toplevel ~a -geometry ~ax~a+~a~a"
	    (tk-name self)
	    (width (frame self))
	    (height (frame self))
	    (x (frame self))
	    (y (frame self)))
  (gui-eval "wm title ~a {~a}" (tk-name self) (title self))
  self)

(define *all-tags* 0)
(define (identity x) x)

(define *tag->object* (make-table eq? identity))

(define (set-tk-names! (self <tk-object>) (basename <string>))
  (if (not (tk-name self))
      (set-tk-name! self
		    (string-append (if (containing-object self)
				       (tk-name (containing-object self))
				       "")
				   "."
				   basename
				   (number->string *all-tags*))))
  (set-tk-tag! self *all-tags*)
  (table-insert! *tag->object* *all-tags* self)
  (set! *all-tags* (+ *all-tags* 1)))

(define (handle-tk-events)
  (let loop ()
    (for-each (lambda (evt)
		;; look up the object
		(let ((o (table-lookup *tag->object* (cadr evt)))
		      (type (car evt)))
		  (if o
		      (case type
			((0) (gui-action o))
			((1) (gui-click o (caddr evt) (cadddr evt)))
			((2) (gui-drag o (caddr evt) (cadddr evt)))
			(else
			 (format #t "unknown event type => ~s\n" evt)))
		      (format #t "ill event => ~s\n" evt))))
	      (gui *gui-interp* 3))
    ;; redraw all the slates that need it
    (for-each (lambda (c)
		(table-remove! *slates-needing-redraw* c)
		(redraw-canvas c))
	      (table-keys->list *slates-needing-redraw*))
    (thread-sleep 0.2)
    (loop)))

(define-class <button> (<tk-object>)
  title)

(define-method initialize ((self <button>))
  (set-tk-names! self "button")
  (gui-eval "button ~a -text {~a}"
	    (tk-name self)
	    (title self))
  (gui-eval "pack ~a" (tk-name self))
  (gui-eval "bind ~a <Button-1> {delayed-callback 0 ~a}"
	    (tk-name self)
	    (tk-tag self))
  self)

(define-method gui-action ((self <gui-object>))
  ;; default behavior -- do nothing
  ;; (for now, issue a warning)
  (format #t "(gui-action ~s) -- NOP\n" self)
  self)

(define-method gui-click ((self <gui-object>) (x <number>) (y <number>))
  (format #t "(gui-click ~s ~d ~d) -- NOP\n" self x y)
  self)

(define-method gui-drag ((self <gui-object>) (x <number>) (y <number>))
  (format #t "(gui-drag ~s ~d ~d) -- NOP\n" self x y)
  self)

(define-class <group> (<tk-object>)
  (group-elements '()))

(define-method initialize ((self <group>))
  (set-tk-names! self "group")
  (gui-eval "frame ~a -relief groove" (tk-name self))
  (gui-eval "pack ~a -fill x" (tk-name self))
  self)

(define-class <list-box> (<tk-object>)
  (list-elements '())
  renderer)

(define-method initialize ((self <list-box>))
  (set-tk-names! self "listbox")
  (gui-eval "listbox ~a" (tk-name self))
  (gui-eval "pack ~a" (tk-name self))
  self)

(define (list-box-add! (self <list-box>) item)
  (set-list-elements! self (append (list-elements self) (list item)))
  (gui-eval "~a insert end {~a}"
	    (tk-name self)
	    ((renderer self) item)))


;; slates & panels
;; (only support slates for now)

(define *slates-needing-redraw* (make-object-table))

(define-class <slate> (<tk-object>)
  frame)

(define-method initialize ((self <slate>))
  (set-tk-names! self "canvas")
  (gui-eval "canvas ~a -width ~d -height ~d"
	    (tk-name self)
	    (width (frame self))
	    (height (frame self)))
  (gui-eval "pack ~a" (tk-name self))
  (gui-eval "bind ~a <Button-1> {delayed-callback 1 ~a %x %y}"
	    (tk-name self)
	    (tk-tag self))
  (gui-eval "bind ~a <Button1-Motion> {delayed-callback 2 ~a %x %y}"
	    (tk-name self)
	    (tk-tag self))
  (table-insert! *slates-needing-redraw* self 1)
  self)

;; a context for accumulating drawing commands

(define-class <temp-context> (<object>)
  (tk-name '#uninit <string>)
  (tk-proc '#uninit <closure>)
  (current-point '#uninit <point>)
  (current-color 'black))

(define (move-to (p <point>))
  (set-current-point! (fluid-ref *context*) p))

(define (set-color (c <symbol>))
  (set-current-color! (fluid-ref *context*) c))

(define (draw-string (s <string>))
  (let ((c (fluid-ref *context*)))
    ((tk-proc c) "create"
		 "text"
		 (x (current-point c))
		 (y (current-point c))
		 "-text" s
		 "-tags" "all"
		 "-outline" (current-color c))))

(define (frame-rect (r <rect>))
  (let ((c (fluid-ref *context*)))
    ((tk-proc c) "create"
		 "rectangle"
		 (x r)
		 (y r)
		 (max-x r)
		 (max-y r)
		 "-tags" "all"
		 "-outline" (current-color c))))


(define (redraw-canvas (self <slate>))
  (fluid-let ((*context* (make <temp-context>
			       tk-name: (tk-name self)
			       tk-proc: (gui *gui-interp* 4 (tk-name self))
			       current-point: (make <point> x: 0 y: 0))))
    (gui-eval "~a delete all" (tk-name self))
    (draw-yourself self)))

;; default is a NOP

(define-method draw-yourself ((self <slate>)))

(define (update-slate (self <slate>))
  (table-insert! *slates-needing-redraw* self 2))
