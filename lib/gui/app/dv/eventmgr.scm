;,(use rs.sys.threads.manager)

(load "winconfig.scm")
(load "keys.scm")
(load "mouse.scm")
(load "exposure.scm")
(load "focus.scm")
(load "clientmsg.scm")

(define (my-event-handler exit-loop #rest event-slots #key display event-key)
  (case event-key
    ((exposure)
     (apply my-exposure-handler display event-slots))
    ((button-press) 
     (apply my-button-press-handler display event-slots))
    ((button-release) 
     (apply my-button-release-handler display event-slots))
    ((key-press) 
     (apply my-key-press-handler display event-slots))
    ((key-release) 
     (apply my-key-release-handler display event-slots))
    ((motion-notify) 
     (apply my-motion-notify-handler display event-slots))
    ((configure-notify)
     (apply my-configure-notify-handler display event-slots))
    ((focus-in)
     (apply my-focus-in-handler display event-slots))
    ((focus-out)
     (apply my-focus-out-handler display event-slots))
    ((client-message)
     (apply my-client-message-handler display event-slots))
    ((exit-loop) ;; a special internal event
     ;(dm "exit-loop")
     (exit-loop #t))
    ((execute) ;; an internal event
     (apply my-execute display event-slots))
    (else
     (wm 139 "~s event ignored" event-key)))
  ;; tell `process-event' that we handled the event, 
  ;; so that it doesn't loop w/o returning
  #t)

(define (my-execute dpy #rest r #key thunk)
  (dm "execute: ~s" thunk)
  (thunk))

(define (make-event-loop-handler client exit)
  (lambda (condition next-handler)
    (let ((p (current-error-port)))
      (format p
	      "** event loop for ~s encountered condition **\n** ~a"
	      client
	      condition)
      (format p "~a\n" (make-string 72 #\-))
      (with-module repl (apply-backtrace condition))
      (format p "~a\n" (make-string 72 #\-))
      (flush-output-port (current-error-port))
      (exit))))

(define (do-one-event client dpy exit-loop)
  (call-with-current-continuation
   (lambda (exit)
     (handler-bind (<condition> (make-event-loop-handler client exit))
       ;; `process-event' contains an implicit force-output
       ;; before it gets an event
       (process-event
	dpy 
	handler: (lambda args
		   ;(dm "handling => ~s" args)
		   (apply my-event-handler exit-loop args)))))))

(define (run-client-loop (client <client>) dpy exit-loop)
  ;; this loop is exited non-locally by a special `exit-loop'
  ;; event handler
  (let loop ()
    ;
    ;(dm "in client-loop...")
    (do-one-event client dpy exit-loop)
    ;
    (if (> (table-size *clear-table*) 0)
	(begin
	  (table-for-each *clear-table*
			  (lambda (h k v)
                            (dm "Clearing: ~s ~s" k v)
			    ;(clear-area k)
			    (v)))
	  (set! *clear-table* (make-object-table))))
    (loop)))

(define (app-event-loop (client <client>))
  (let* ((dpy (on-display client))
	 (v (call-with-current-continuation
	     (lambda (exit-loop)
	       (run-client-loop client dpy exit-loop)))))
    (dm 109 "app-event-loop: client ~s exiting" client)
    ;;
    (display-force-output dpy)
    (send-message! (exit-mbox client) v)
    ;; need thread safety here!
    (set! *all-clients* (delq client *all-clients*))))

;;    (if (null? *all-clients*)
;;	(process-exit 0))

(define (spawn-loop (client <client>))
  (thread-resume 
   (make-thread (lambda ()
		  (with-client client
			       (lambda ()
				 (app-event-loop client))))
		(string-append "event-loop " (display-name client)))))

;;;

(define *clear-table* (make-object-table))

(define (need-to-clear win thunk)
  (table-insert! *clear-table* win thunk))

(define (need-to-redraw win)
  (need-to-clear win (get-property win 'exposure-thunk)))

(define (need-to-clear-and-redraw win)
  (need-to-clear win
                 (lambda ()
                   (clear-area win)
                   ((get-property win 'exposure-thunk)))))
