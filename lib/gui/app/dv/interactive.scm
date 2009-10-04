
;; map underlying procedures to interactive gateways

(define *interactive-wrappers* '())

;; list the interactive procedures that want to run in the main event loop

(define *synchronous-interactions* '())

;; record all the compiled interaction wrappers

(define *interactive-wrapper-dict* '())

(define (get-interactive-wrapper descr)
  (let ((a (assoc descr *interactive-wrapper-dict*)))
    (if a
	(cdr a)
	(let ((proc (compile-interaction-spec descr)))
	  (set! *interactive-wrapper-dict*
		(cons (cons descr proc) *interactive-wrapper-dict*))
	  proc))))

(define (expect-type item class)
  (if (instance? item class)
      item
      (em "expected ~s to be a ~s" item (name class))))

(define (compile-interaction-spec spec)
  ;; don't really compile... just return an interpreter
  (lambda (proc owner event)
    ;
    ; special indirection: if owner = 'next-owner, then use the next
    ; owner instead -- this hack is to support the menu frame being
    ; the owner; its commands need to apply to the topmost
    ; document window!
    ;
    (if (eq? owner 'next-owner)
	(set! owner (next-owner (current-client))))
    ;
    (apply proc
	   (map (lambda (s)
		  (case (car s)
		    ((client) 
                     (current-client))
		    ((owner)
		     owner)
		    ((event)
		     (or event (error "interactive: no event")))
		    ((open-view)
		     (cond
                      ((instance? owner <open-view>)
                       owner)
                      (owner
                       (or (window->open-view owner)
                           (error "interactive: owner not a main view")))
                      (error "interactive: no owner")))
		    ((open-document)
		     (if owner
                         (expect-type (in-document owner)
                                      <open-document>)
			 (error "interactive: no owner")))
		    ((document)
		     ;; owner is an <open-view>, so `document' is:
		     (if owner
			 (expect-type (underlying-object (in-document owner))
                                      <document>)
			 (error "interactive: no owner")))
		    ((selection)
		     (key-sequence (current-selection owner)))
		    ((minibuffer)
		     (let ((str (read-from-minibuffer (caddr s))))
		       (case (cadr s)
                         ((<number:length>) 
                          (let ((v (string->number str)))
                            (cond
                             ((eq? v #f)
                              (error "could not parse quantity ~s" v))
                             ((number? v)
                              (quantity* v *default-length-units*))
                             ((and (quantity? v)
                                   (number? 
                                    (quantity/ v *default-length-units*)))
                              v)
                             (else
                              (error "expected a length, not ~s" v)))))
			 ((<number>) (string->number str))
			 ((<string>) str)
			 ((<symbol>) (string->symbol str))
			 (else
			  (error "unknown interaction (minibuffer type): ~s"
				 (cadr s))))))
		    ((click)
		     (if (pair? (cdr s))
			 (get-click (cadr s))
			 (get-click)))
		    (else (error "invalid interaction spec: ~s" s))))
		(cdr spec)))))
  
(define-macro (define-interactive (name . args) . body)
  (assert (and (pair? body)
	       (pair? (car body))
	       (eq? (caar body) 'interactive)))
  (let ((i-desc (car body))
	(xtra '())
	(uname (symbol-append "u." name)))
    (if (and (pair? (cdr i-desc))
	     (eq? (cadr i-desc) ':synchronous))
	(begin
	  (set! i-desc (cons 'interactive (cddr i-desc)))
	  (set! xtra `((set-synchronous! ,name)))))
    `(begin
       (define (,uname ,@args) ,@(cdr body))
       ; indirect through a var name so the proc can be redefined
       ; after a pointer is captured (for debugging purposes)
       (define (,name . all-args) (apply ,uname all-args))
       ,@xtra
       (set-interactive-wrapper! ,name (get-interactive-wrapper ',i-desc)))))

(define (set-synchronous! proc)
  (set! *synchronous-interactions* (cons proc *synchronous-interactions*)))

(define (synchronous? proc)
  (and (memq proc *synchronous-interactions*) #t))

(define (set-interactive-wrapper! proc wrap)
  (set! *interactive-wrappers* (cons (cons proc wrap) *interactive-wrappers*))
  (values))

;;

(define (get-click #optional prompt)
  (let* ((view (my-owner))
	 (win (content-window view))
	 (saved-sl (current-status-line view))
	 (saved-bp (get-property win 'button-press #f)))
    (if prompt
	(set-status-line! view prompt))
    (let ((pt (call-with-current-continuation
	       (lambda (exit)
		 (set-property! win
				'button-press 
				 (lambda (win at state)
				   (exit at)))
		 (app-event-loop (current-client))))))
      (if saved-bp
	  (set-property! win 'button-press saved-bp)
	  (remove-property! win 'button-press))
      (set-status-line! view saved-sl)
      pt)))
				 
;;;

(define-thread-var *interactive-owner*)
(define-thread-var *interactive-event*)

(define (my-owner)
  *interactive-owner*)

(define (my-event)
  *interactive-event*)

;;;  we make `owner' and `event' be optional, defaulting to
;;;  `(my-owner)' and `(my-event)', to support having
;;;  one interactive procedure call another without having
;;;  to specify the owner and event explicitly (because
;;;  that would then require taking them as args, w/o the
;;;  dynamic mechanism; with the dynamic mechansim, the call
;;;  site would just look like (call-interactive foo (my-owner) (my-event))
;;;  anyway)
;;;
;;;  first used to let `save-file' call `save-file-as' when the
;;;  document is untitled

(define (call-interactive (proc <function>) #optional 
			  (owner default: (my-owner)) 
			  (event default: (my-event)))
  (dm "call-interactive: ~a" (name proc))
  (let ((wrap (assq proc *interactive-wrappers*)))
    (if wrap
	(thread-let ((*interactive-owner* owner)
		     (*interactive-event* event))
          (if (synchronous? proc)
	      ((cdr wrap) proc owner event)
	      (thread-resume
	       (make-thread
		(lambda ()
                  (handler-case
                   ((cdr wrap) proc owner event)
                   ((<condition> condition: c)
                    (dm "call-interactive: ~a FAILED!" (name proc))
                    (em 809 "~a failed: ~a" (name proc) c)
                    (with-module repl (apply-backtrace c)))))
		(name proc)))))
	(em 810 "~s: not interactive" proc))))

(define (dv-eval expr)
  (with-module repl
    (eval-in-envt expr *self*)))
;;;


#|
;;; disconnect a client.
;;; usually (this impl, always) when all clients disconnect, the
;;; program exits

(define-interactive (exit-client client)
  (interactive (client))
  (dm 107 "exit-client ~s" client)
  ;
  (unmap-window (main-menu client))
  (display-force-output (on-display client))
  ;
  ;; need thread safety here!
  (set! *all-clients* (delq client *all-clients*))
  (if (null? *all-clients*)
      (process-exit 0)))
|#
