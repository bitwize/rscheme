
(define-class <event-loop-terminated> (<condition>)
  for-client)

(define (event-loop)
  (let* ((client (current-client))
	 (dpy (on-display client)))
    (set-event-loop-thread! client (current-thread))
    (handler-case
     (let loop ()
       (if (process-one-event client dpy rstep-x-handler)
	   (loop)
	   (dm 108 "event-loop: loop for ~s done" client)))
     ((<event-loop-terminated>)
      (dm 109 "event-loop: loop for ~s terminated" client)))
    (values)))

(define (track-mouse self #key mouse-moved mouse-up)
  (call-with-current-continuation
   (lambda (exit)
     ;;
     (define (tracking-button-release #rest info #key event-window x y state
				      root-x root-y)
       (dm "mouse-up: ~s ~s (~x)" x y state)
       (exit (mouse-up (make-point x y) state (make-point root-x root-y))))
     ;;
     (define (tracking-motion-notify #rest info #key event-window x y state
				     root-x root-y)
       ;(dm "mouse-moved: ~s ~s (~x)" x y state)
       (mouse-moved (make-point x y) state (make-point root-x root-y))
       #t)
     ;;
     (define (tracking-x-handler #rest info #key event-key)
       (case event-key
	 ((button-release)
	  (apply tracking-button-release info))
	 ((motion-notify)
	  (apply tracking-motion-notify info))
	 (else
	  (apply rstep-x-handler info))))
     ;;
     (dm 111 "event-loop: track-mouse for ~s" self)
     (let* ((client (for-client self))
	    (dpy (on-display client)))
       (let loop ()
	 (if (process-one-modal-event client dpy tracking-x-handler)
	     (loop)
	     (dm 112 "event-loop: track-mouse for ~s done" self)))))))

;;;

(define (process-all-exposures (client <client>))
  ;; slurp up all the `update' events
  (let ((dpy (on-display client)))
    (let loop ()
      (if (memq (process-event dpy
                               handler: x-event-key
                               peek?: #t)
                '(exposure configure-notify))
          (begin
            (process-one-event* client (on-display client) rstep-x-handler)
            (loop))))))

;;;

(define (flush-updates (client <client>))
  (dm "flushing updates...")
  (process-all-exposures client)
  (dm "~d pending updates" (table-size (pending-updates client)))
  (if (> (table-size (pending-updates client)) 0)
      (let ((t (pending-updates client)))
	(set-pending-updates! client (make-object-table))
	(table-for-each t
			(lambda (h k v)
			  (draw k (if (eq? v #t) '() (list v))))))))

(define (process-one-event (client <client>) dpy handler-proc)
  (flush-updates client)
  (process-one-event* client dpy handler-proc)
  (flush-updates client)
  #t)

(define (process-one-event* (client <client>) dpy handler-proc)
  (handler-case
   (process-event dpy handler: handler-proc)
   ((<condition> condition: c)
    (wm 103 "** error in event loop handler\n** ~a" c)
    (let ((s (get-property c 'stack #f)))
      (if s
	  (print (vm-continuation-reg s)))))))

(define (process-one-modal-event (client <client>) dpy handler-proc)
  (flush-updates client)
  (process-event dpy handler: handler-proc)
  (flush-updates client)
  #t)

(define (x-event-key #rest info #key event-key)
  event-key)

(define (rstep-x-handler #rest info #key display event-key)
  (dm 110 "event-loop: ~s" event-key)
  (case event-key
    ((exposure)
     (apply rstep-exposure display info))
    ((motion-notify)
     (apply rstep-motion-notify display info))
    ((button-press)
     (apply rstep-button-press display info))
    ((button-release)
     (apply rstep-button-release display info))
    ((enter-notify)
     (apply rstep-enter-notify display info))
    ((leave-notify)
     (apply rstep-leave-notify display info))
    ((focus-out)
     (apply rstep-focus-out display info))
    ((focus-in)
     (apply rstep-focus-in display info))
    ((configure-notify)
     (apply rstep-configure-notify display info))
    ((key-press)
     (apply rstep-key-press display info))
    ((key-release)
     (apply rstep-key-release display info))
    (else
     #t)))

(define-method do-configure-notify ((self <object>) new-frame above)
  (dm 111 "do-configure-notify: ~s" self))

(define (rstep-configure-notify display 
				#rest ignore
				#key window x y width height above-sibling)
  (let ((p (get-property window 'rstep-window #f)))
    (if p
	(do-configure-notify p (make-rect x y width height) above-sibling))
    #t))

(define (rstep-focus-in display #rest ignore #key window mode kind)
  (let ((p (get-property window 'rstep-window #f))
	(c (current-client)))
    (if (focus-window c)
	(did-resign-focus (focus-window c)))
    (set-focus-window! (current-client) p)
    (if p
	(did-become-focus p))
    #t))

(define (rstep-focus-out display #rest ignore #key window mode kind)
  (let ((c (current-client)))
    (if (focus-window c)
	(did-resign-focus (focus-window c)))
    (set-focus-window! c #f)
    #t))

(define-method draw ((self <object>))
  (values))

(define (rstep-exposure display 
			#rest ignore
			#key window x y width height count)
  (if (eq? count 0)
      (let ((p (get-property window 'rstep-view #f)))
	(dm "exposed: ~s ~s" window p)
	(if p
            (let* ((t (pending-updates (current-client)))
                   (was (table-lookup t p)))
              (cond
               ((eq? was #t)
                #f)                     ; do nothing -- already all exposed
               ((eq? was #f)
                (table-insert! t p (make-rect x y width height)))
               (else
                (table-insert! t p (union-rect
                                    was 
                                    (make-rect x y width height)))))))))
  #t)

(define (rstep-button-press display
			    #rest ignore
			    #key event-window
				 x y
				 state)
  (let ((p (get-property event-window 'rstep-view #f)))
    (dm "button-press ~s ~s (~s ~s)" x y 
        (class-name (object-class x))
        (class-name (object-class y)))
    (if p
	(mouse-down p (make-point x y) state))
    #t))

(define (rstep-button-release display
			      #rest ignore
			      #key window
			           event-window
				   x y
				   state)
  (let ((p (get-property event-window 'rstep-view #f)))
    (if p
	(mouse-up p (make-point x y) state))
    #t))

(define (rstep-key-press display
                         #rest ignore
                         #key event-window
                         state
                         code)
  (let ((p (get-property event-window 'rstep-view #f)))
    (dm "key-press ~s ~s" code state)
    (if p
	(key-down p code state))
    #t))

(define (rstep-key-release display
                           #rest ignore
                           #key event-window
                           state
                           code)
  (let ((p (get-property event-window 'rstep-view #f)))
    (dm "key-release ~s ~s" code state)
    (if p
	(key-up p code state))
    #t))

(define (rstep-enter-notify display
			    #rest ignore
			    #key window
			    x y
			    state)
  (let ((p (get-property window 'rstep-view #f)))
    (if p
	(mouse-enter p (make-point x y) state))
    #t))

(define (rstep-leave-notify display
			    #rest ignore
			    #key window
			    x y
			    state)
  (let ((p (get-property window 'rstep-view #f)))
    (if p
	(mouse-leave p (make-point x y) state))
    #t))

(define (rstep-motion-notify display
			     #rest ignore
			     #key event-window
			     x y
			     state)
  ;(format #t "motion: ~s\n" ignore)
  (let ((p (get-property event-window 'rstep-view #f)))
    (if p
	(mouse-moved p (make-point x y) state))
    #t))

;;;
