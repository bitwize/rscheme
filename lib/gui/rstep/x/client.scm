
;;; the <client> represents a connection to a
;;; GUI server

(define-class <client> (<object>)
  name
  (properties init-value: '#())
  on-display
  on-screen
  use-colormap  ;; usu. the screen's default colormap
  (client-keyboard-map init-value: #f)
  (main-menu init-value: #f)
  (focus-window init-value: #f)
  (windows init-value: '())
  (atom-symbol-table init-function: make-fixnum-table)
  (resource-settings init-function: make-symbol-table)
  resource-tables
  (pending-updates init-function: make-object-table)
  (event-loop-thread init-value: #f)
  (active-color-wells init-value: '())
  (color-changers init-function: make-color-table)
  (all-windows init-value: '()))

(define-thread-var *client* #f)
(define *all-clients* '())

(define (with-client (c <client>) thunk)
  (thread-let ((*client* c))
    (thunk)))

(define (display->client dpy)
  (get-property dpy 'client))

(define (current-client)
  *client*)

(define (flush-client #optional (client default: (current-client)))
  (display-force-output (on-display client)))

(define (open-client-on-x dpy cnxn scrn)
  (let ((client (make <client>
                      name: dpy
                      on-display: cnxn
                      on-screen: scrn
                      resource-tables: (make-resource-tables)
                      use-colormap: (screen-default-colormap scrn))))
    (dm 102 "initializing client: ~s..." client)
    (set-property! cnxn 'client client)
    (set-client-keyboard-map! client (keyboard-mapping cnxn))
    (load-settings client)
    (set! *all-clients* (cons client *all-clients*))
    (with-client
     client
     (lambda ()
       (thread-resume (make-thread event-loop "RStep"))))
    client))
  
(define (open-client #optional dpy)
  (bind ((dpy (or dpy (getenv "DISPLAY")))
	 (cnxn scrn (connect-to-x-display dpy)))
    (open-client-on-x dpy cnxn scrn)))
         
(define (load-settings client)
  (for-each
   (lambda (ent)
     (table-insert! (resource-settings client)
		    (car ent)
		    (cadr ent)))
   (call-with-input-file "RStep.defaults" read)))

(define (application-font)
  (get-font-resource 'application.font (current-client)))

(define-method remove-active-color-well! ((self <client>) well)
  (if (memq well (active-color-wells self))
      (begin
	(set-active-color-wells! self (delq well (active-color-wells self)))
	(resign-live well))))

(define-method add-active-color-well! ((self <client>) well excl?)
  (if excl?
      (begin
	(for-each resign-live (active-color-wells self))
	(set-active-color-wells! self '())))
  (if (become-live well)
      (begin
	(set-active-color-wells! self (cons well (active-color-wells self)))
	(if excl?
	    (set-color! (get-color-picker self) (color well))))))

(define-method get-color-picker ((self <client>))
  (let ((p (get-property self 'color-picker #f)))
    (or p
	(let ((w (make-window frame: (make-rect 50 50 80 100)
			      title: "Color")))
	  (make-color-picker 
	   frame: (make-rect 0 0 80 100)
	   parent: (content-view w))))))
