
(define-thread-var *client* #f)

;;;  a <client> represents a display connection 

(define-class <client> (<object>)
  (properties init-value: '#())
  display-name
  on-display      ;; an X display
  on-screen       ;; an X screen
  using-colormap  ;; usu. (screen-default-colormap on-screen)
  (client-keyboard-mapping init-value: #f) ; filled in by initialize
  (main-menu init-value: #f)
  (toolbox-menu init-value: #f)
  (resource-vector type: <vector> init-value: '#())
  (focus-window init-value: #f)
  atom-symbol-table
  active-font
  (next-owner init-value: #f)
  (open-views type: <list> init-value: '())
  (client-kill-ring type: <list> init-value: '())
  (exit-mbox type: <mailbox>)
  (client-exit type: <future>)
  (style-dictionary type: <symbol-table>)
  (style-set))

(define-method write-object ((self <client>) port)
  (format port "#[<client> ~a]" (display-name self)))

(define (with-client (c <client>) thunk)
  (thread-let ((*client* c))
    (thunk)))

(define (display->client dpy)
  (get-property dpy 'client))

(define (current-client)
  *client*)

(define (flush-client #optional (client default: (current-client)))
  (display-force-output (on-display client)))

(define (make-client dpy)
  (bind ((cnxn scrn (connect-to-x-display dpy))
	 (mb (make-mailbox)))
    (make <client>
	  display-name: dpy
	  on-display: cnxn
	  on-screen: scrn
	  active-font: (default-text-font)
	  atom-symbol-table: (make-table eq? integer->hash)
	  exit-mbox: mb
	  client-exit: (future (receive-message! mb))
	  using-colormap: (screen-default-colormap scrn)
          style-dictionary: (default-style-dictionary)
          style-set: (default-style-set))))
    

;;;

(define (connect-to-x-display dpy)
  (bind ((dpy-host dpy-srvr dpy-scrn (parse-display-var dpy)))
    (dm 100 "opening display ~a:~d.~d" dpy-host dpy-srvr dpy-scrn)
    (let* ((cnxn (open-display dpy-host display: dpy-srvr))
	   (r (display-roots cnxn)))
      (if (and (>= dpy-scrn 0)
	       (< dpy-scrn (length r)))
	  (values cnxn (list-ref r dpy-scrn))
	  (em 191
	      "screen `~d' out of range (~d screens available on ~a:~d)"
	      dpy-scrn (length r) dpy-host dpy-srvr)))))

;;

(define dpy-var-fmt2 (with-module regex (unformat->proc "~a:~d")))
(define dpy-var-fmt3 (with-module regex (unformat->proc "~a:~d.~d")))

(define (parse-display-var str)
  (define (fxhost s)
    (if (string=? s "")
	(with-module unixm (hostname))
	s))
  (bind ((host dpy scrn (dpy-var-fmt3 str)))
    (if host
	(values (fxhost host) dpy scrn)
	(bind ((host dpy (dpy-var-fmt2 str)))
	  (if host
	      (values (fxhost host) dpy 0)
	      (values (fxhost str) 0 0))))))

;;;

(define (atom->symbol (atom <fixnum>))
  (let ((tbl (atom-symbol-table (current-client))))
    (or (table-lookup tbl atom)
	(let ((n (string->symbol (atom-name (on-display (current-client))
                                            atom))))
	  (table-insert! tbl atom n)
	  n))))

;;;

(define *all-clients* '())

(define-method initialize ((self <client>))
  (dm 102 "initializing client ~s..." self)
  (set-property! (on-display self) 'client self)
  (set-client-keyboard-mapping! self (keyboard-mapping (on-display self)))
  ;
  ;  bind the application resources to server/screen-specific values
  ;
  (let ((fnttab (make-string-table))
	(dpy (on-display self)))
    (for-each
     (lambda (f)
       (let ((font (or (table-lookup fnttab (cadr f))
		       (open-font dpy (cadr f)))))
	 (table-insert! fnttab (cadr f) font)
	 (set-property! dpy (car f) font)))
     *app-fonts*))
  ;
  ;  lock down all the colors
  ;
  (init-default-colormap-pixels (on-screen self))
  (with-client self slurp-colormap)
  ;
  ;  start the event loop
  ;
  (spawn-loop self)
  ;
  ;  start a thread to open the main menu
  ;
  (thread-resume
   (make-thread
    (lambda ()
      (dm 104 "opening main menu...")
      (open-main-menu self))))
  (set! *all-clients* (cons self *all-clients*)))
#|
  (queue-event (on-display self)
	       'execute
	       thunk: (lambda ()
			(dm 105 "opening toolbox...")
			(make-toolbox))))
|#