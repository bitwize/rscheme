
(define (invalid-event display handler event-data sent?)
  (wm 402 "invalid event received: ~d" 
	  (bvec-ref event-data 0))
  #t)

;;;
;;;  Each possible event code (ie, 2..127) has an associated
;;;  procedure which knows how to destructure the 32-byte event
;;;  and invoke the handler with the appropriate arguments
;;;

(define *event-unwrappers* (make-vector 128 invalid-event))
(define *event-keys* (make-vector 128 #f))

;;; Like `delq!', but removes the following element as well

(define (delq2! key list)
  (let loop ((prev #f) (l list))
    (if (pair? l)
	(let (((l <pair>) l))
	  (if (eq? (car l) key)
	      (if prev
		  (begin
		    (set-cdr! prev (cddr l))
		    (loop prev (cddr l)))
		  (begin
		    (set! list (cddr l))
		    (loop #f (cddr l))))
	      (loop l (cdr l))))
	(if prev
	    list
	    '()))))

;;; Queue an internal event

(define (queue-event (self <x-display>) 
		     event-key
		     #rest event-slots 
		     #key (append? default: #t))
  ; note that we strip out the `append?', if specified -- does CL do this
  ; automatically?  We don't support #all-keys, quite
  (let ((descr (cons event-key (delq2! 'append?: event-slots)))
	((q <mailbox>) (event-queue self)))
    (if append?
	(send-message! q descr)
	(send-message/prepend! q descr))
    (values)))

;;;  NOTE:  timeout is not implemented yet.
;;;  also, I think the intended CLX behavior is to leave items
;;;  in the queue events for which the handler returns #f, although
;;;  it is not particularly clear...  That is not implemented either.

(define (process-event (self <x-display>) #key handler
		                               (timeout default: #f)
					       (peek? default: #f)
					       (discard? default: #f)
					       (force-output? default: #t))
  ;;
  (define (discard-rest)
    (let loop ()
      (if (get-next-event-if-present self)
          (loop))))
  ;;
  (if force-output?
      (display-force-output self))
  ;;
  (let loop ((peeked-past '()))
    (bind ((next (if timeout
                     (get-next-event-if-present self)
                     (get-next-event self))))
      (if (not next)
          (begin
            (if peek?
                (for-each (lambda (e)
                            (unget-event self e))
                          peeked-past))
            #f)
          ;(dm 101 "X event => ~s" next)
          (let ((r (if (pair? next)
                       ;; handle "internal" events
                       (apply handler
                              display: self
                              event-key: (car next)
                              (cdr next))
                       ;; "external" events need to be unwrapped
                       ((vector-ref *event-unwrappers*
                                    (bitwise-and (bvec-ref next 0) #x7F))
                        self
                        handler
                        next
                        (>= (bvec-ref next 0) 128)))))
            (if r
                (begin
                  ;; flush everything that's left, if requested
                  (if discard?
                      (discard-rest))
                  ;; repopulate the event queue, if requested
                  (if peek?
                      (begin
                        (unget-event self next)
                        (for-each (lambda (e)
                                    (unget-event self e))
                                  peeked-past)))
                  r)
                (loop (cons next peeked-past))))))))

(define-macro (internal-declare-event codes . slot-declarations)
  `(begin
     ,@(map (lambda (c)
	      `(vector-set! *event-keys* ,(cadr c) ',(car c)))
	    codes)
     ,@(map (lambda (c)
	      `(vector-set! *event-unwrappers*
			    ,(cadr c)
			    ,(make-event-unwrapper 
			      (cadr c) (car c) slot-declarations)))
	    codes)))

(define (gui.x%add-event-unwrapper (name <symbol>) (decls <list>))
  (let ((code (vmemq name *event-keys*)))
    (vector-set!
     *event-unwrappers*
     code
     (gui.x%eval (make-event-unwrapper code name decls)))))

(define (gui.x%eval expr)
  (with-module
      repl
    (with-module
	mlink
      (eval-in-envt expr (top-level-envt (get-module 'gui.x))))))

(define (clx-type-width t)
  (cadr (assq t '((card8 u1:)
		  (int8 s1:)
		  (card16 u2:)
		  (int16 s2:)
		  (card32 u4:)
		  (window u4:)
		  (drawable u4:)
		  (boolean u1:)))))

(define (expand-event-args specs)
  (let loop ((s specs)
	     (unpack-spec '())
	     (keys '()))
    (if (null? s)
	(values (reverse unpack-spec) (reverse keys))
	(if (null? (cdar s))
	    (loop (cdr s)
		  (cons* '-
			 (gen-event-slot-computation (caar s) '-) 
			 unpack-spec)
		  keys)
	    (if (pair? (cddar s))
		(loop (cons* (list (caar s) (cadar s))
			     (cons (caar s) (cddar s))
			     (cdr s))
		      unpack-spec
		      keys)
		(bind ((type (caar s))
		       (var (cadar s))
		       (decoder-kwd cnv-code (gen-event-slot-computation
					       type
					       var)))
		  (loop (cdr s)
			(cons* (cadar s) 
			       decoder-kwd
			       unpack-spec)
			(cons* cnv-code
			       (symbol->keyword var)
			       keys))))))))

(define (gen-event-slot-computation type-expr var)
  (if (symbol? type-expr)
      ;; a simple type expression like `window' or `card8'
      (case type-expr
	((window drawable)
	 (values 'u4: `(table-lookup (xid-table display) ,var)))
	((atom)
	 (values 'u4: var))
	((boolean)
	 (values 'u1: `(not (eq? ,var 0))))
	(else
	 (values (case type-expr
		   ((int8) 's1:)
		   ((int16) 's2:)
		   ((card8) 'u1:)
		   ((card16) 'u2:)
		   ((card32) 'u4:)
		   (else (error "~s: invalid simple type-expr" type-expr)))
		 var)))
      ;; a complex type expression like `(member (0 normal) (1 while-grabbed))'
      (case (car type-expr)
	((member8)
	 (values
	  'u1:
	  `(case ,var
	     ,@(let ((k 0))
		 (map (lambda (c)
			(set! k (+ k 1))
			(if (pair? c)
			    (let ((on-value (car c))
				  (result (cadr c)))
			      `((,on-value) ',result))
			    `((,(- k 1)) ',c)))
		      (cdr type-expr))))))
	(else 
	 (error "~s: invalid type expr constructor" (car type-expr))))))

(define (make-event-unwrapper code name decls)
  (bind ((unpack-spec pass-kwds (expand-event-args decls)))
    `(lambda ',(symbol-append "unwrap-" name "-event")
       (display handler event-data sent?)
       (with-unpacked event-data
		      ,unpack-spec
		      ; why `apply*'? see CR 593
		      ; "Combo of LEXREF function can get bad code generated"
		      (apply*
		       ; common arguments...
		       display: display
		       event-key: ',name
		       send-event?: sent?
		       ,@pass-kwds
		       '()
		       handler)))))

(define-macro (declare-event event-codes . slot-declarations)
  (if (pair? event-codes)
      `(begin ,@(map (lambda (c)
		       `(declare-event ,c ,@slot-declarations))
		     event-codes))
      `(gui.x%add-event-unwrapper ',event-codes ',slot-declarations)))

;;; we can't use `declare-event' because of evaluation order problems
;;; (ie, this module isn't defined until after the define-module
;;; completes, and gui.x%eval needs this module to be around)

(internal-declare-event ((exposure 12))
			(card8)
			(card8)
			(card16)
			(window window)
			(card16 x y width height count))

(internal-declare-event ((motion-notify 6))
			(card8)         ; event number
			(boolean hint?) ; detail (0=Normal 1=Hint)
			(card16)        ; sequence-number
			(card32 time)
			(window root event-window window)
			(int16 root-x root-y x y)
			(card16 state)
			(boolean same-screen?))

(internal-declare-event ((configure-notify 22))
			(card8)  ; event type
			(card8)  ; unused
			(card16) ; sequence number
			(window event-window window above-sibling)
			(int16 x y)
			(card16 width height border-width)
			(boolean override-redirect?))

(internal-declare-event ((reparent-notify 21))
			(card8)  ; event type
			(card8)  ; unused
			(card16) ; sequence number
			(window event-window window parent)
			(int16 x y)
			(boolean override-redirect?))

(internal-declare-event ((map-notify 19))
			(card8)  ; event type
			(card8)  ; unused
			(card16) ; sequence number
			(window event-window window)
			(boolean override-redirect?))

(internal-declare-event ((no-exposure 14))
			(card8)  ; event type
			(card8)  ; unused
			(card16)  ; sequence number
			(drawable drawable)
			(card16 minor)
			(card8 major))

(internal-declare-event ((unmap-notify 18))
			(card8)  ; event type
			(card8)  ; unused
			(card16) ; sequence number
			(window event-window window)
			(boolean configure?))

(internal-declare-event ((button-press 4)
			 (button-release 5)
			 (key-press 2)
			 (key-release 3))
			(card8)         ; event number
			(card8 code)    ; detail (button number?)
			(card16)        ; sequence-number
			(card32 time)
			(window root event-window window)
			(int16 root-x root-y x y)
			(card16 state)
			(boolean same-screen?))

(internal-declare-event ((focus-in 9)
			 (focus-out 10))
			(card8) ; event number
			((member8 ancestor
				  virtual
				  inferior
				  nonlinear
				  nonlinear-virtual
				  pointer
				  pointer-root
				  none)
			 kind)
			(card16) ; sequence number
			(window window)
			((member8 normal grab ungrab while-grabbed) mode))

(internal-declare-event ((enter-notify 7)
			 (leave-notify 8))
			(card8) ; event code
			((member8 ancestor
				  virtual
				  inferior
				  nonlinear
				  nonlinear-virtual)
			 kind)
			(card16) ; sequence number
			(card32 time)
			(window root window child)
			(int16 root-x root-y x y)
			(card16 state)
			((member8 normal grab ungrab)))

;;;---------------------------------------------------------------------
;;; special handling for ClientMessage

(define (unwrap-client-message display handler event-data sent?)
  (with-unpacked event-data
		 (u1: -
		  u1: fmt
		  u2: -
		  u4: win
		  u4: typ)
    (let ((w (table-lookup (xid-table display) win)))
      (apply* display: display
	      window: w
	      event-window: w
	      event-key: 'client-message
	      type: typ
	      format: fmt
	      data: (case fmt
		      ((8) (vector-map (lambda (k)
					 (xbo-read-u1 event-data k))
				       '#(12 13 14 15 16 17 18 19 20 
					  21 22 23 24 25 26 27 28 
					  29 30 31)))
		      ((16) (vector-map (lambda (k)
					  (xbo-read-u2 event-data k))
					'#(12 14 16 18 20 22 24 26 28 30)))
		      ((32) (vector-map (lambda (k)
					  (xbo-read-u4 event-data k))
					'#(12 16 20 24 28))))
	      '()
	      handler))))

(vector-set! *event-keys* 33 'client-message)
(vector-set! *event-unwrappers* 33 unwrap-client-message)

;;;---------------------------------------------------------------------

#|
KeyPress            2
KeyRelease          3
ButtonPress         4
ButtonRelease       5
MotionNotify	    6
EnterNotify	    7
LeaveNotify	    8
FocusIn		    9
FocusOut	    10
KeymapNotify	    11
Expose              12
GraphicsExposure    13
NoExposure	    14
VisibilityNotify    15
CreateNotify	    16
DestroyNotify	    17
UnmapNotify	    18
MapNotify	    19
MapRequest	    20
ReparentNotify      21
ConfigureNotify     22
ConfigureRequest    23
GravityNotify	    24
ResizeRequest	    25
CirculateNotify	    26
CirculateRequest    27
PropertyNotify	    28
SelectionClear	    29
SelectionRequest    30
SelectionNotify	    31
ColormapNotify	    32
ClientMessage	    33
MappingNotify	    34
|#
