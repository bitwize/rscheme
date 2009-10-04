(define-module-extend gui.x ()

;;;
;;;  An alternative interface to the event stream
;;;

(define-class <xev> (<object>) :abstract
  xev-display
  (xev-send-event? init-value: #f))

(define-class <xev-unknown> (<xev>)
  xev-data)
  
(define-class <xev-inout> (<xev>) :abstract
  xev-sequence
  xev-time
  xev-window
  xev-kind
  xev-root
  xev-child
  xev-root-position
  xev-position
  xev-state
  xev-grab)

(define-class <xev-enter-notify> (<xev-inout>))
(define-class <xev-leave-notify> (<xev-inout>))

(define-class <xev-focus> (<xev>) :abstract
  xev-sequence
  xev-kind
  xev-window
  xev-mode)

(define-class <xev-focus-in> (<xev-focus>))
(define-class <xev-focus-out> (<xev-focus>))

(define-class <xev-updown> (<xev>) :abstract
  xev-sequence
  xev-code
  xev-time
  xev-window
  xev-event-window
  xev-root
  xev-root-position
  xev-position
  xev-state
  xev-same-screen?)

(define-class <xev-button-press> (<xev-updown>))
(define-class <xev-button-release> (<xev-updown>))
(define-class <xev-key-press> (<xev-updown>))
(define-class <xev-key-release> (<xev-updown>))

(define-class <xev-configure-notify> (<xev>)
  xev-sequence
  xev-event-window
  xev-window
  xev-above-sibling
  xev-frame
  xev-border-width
  xev-override-redirect?)

(define-class <xev-exposure> (<xev>)
  xev-sequence
  xev-window
  xev-frame
  xev-count)


;;;

(define (unknown-event-object (self <x-display>) data sent?)
  (make <xev-unknown>
        xev-display: self
        xev-send-event?: sent?
        xev-data: data))
        
(define *oo-event-unwrappers* (make-vector 128 unknown-event-object))

(define (every-other l)
  (if (or (null? l)
          (null? (cdr l)))
      '()
      (cons (car l) (every-other (cddr l)))))

(define-macro (declare-oo-event specs decls slots)
  (bind ((unpack-spec vars (expand-event-args decls)))
    `(begin
       ,@(map
          (lambda (spec)
            (let ((name (car spec))
                  (code (cadr spec))
                  (class (caddr spec)))
              ;; XXX (1) evaluate (xid-table display) only once
              ;; XXX (2) inline `make' operation
              ;; XXX (3) inline xid-table lookup operation
              `(vector-set! 
                *oo-event-unwrappers*
                ,code
                (lambda ',(symbol-append "oo-unwrap-" name "-event")
                  (display data sent?)
                  (with-unpacked
                   data
                   ,unpack-spec
                   (let (,@(map (lambda (k v)
                                  (list (keyword->symbol k) v))
                                (every-other vars)
                                (every-other (cdr vars))))
                     (make ,class
                           xev-display: display
                           xev-send-event?: sent?
                           ,@(apply append (map (lambda (s)
                                                  (list 
                                                   (symbol->keyword (car s))
                                                   (cadr s)))
                                                slots)))))))))
          specs))))

(declare-oo-event ((focus-in 9 <xev-focus-in>)
                   (focus-out 10 <xev-focus-out>))
                  ;;-decls-
                  ((card8) ; event number
                   ((member8 ancestor
                             virtual
                             inferior
                             nonlinear
                             nonlinear-virtual
                             pointer
                             pointer-root
                             none)
                    kind)
                   (card16 seq) ; sequence number
                   (window window)
                   ((member8 normal grab ungrab while-grabbed) mode))
                  ;;-slots-
                  ((xev-sequence seq)
                   (xev-kind kind)
                   (xev-window window)
                   (xev-mode mode)))

(declare-oo-event ((button-press 4 <xev-button-press>)
                   (button-release 5 <xev-button-release>)
                   (key-press 2 <xev-key-press>)
                   (key-release 3 <xev-key-release>))
                  ;;-decls-
                  ((card8)         ; event number
                   (card8 code)    ; detail (button number?)
                   (card16 seq)    ; sequence-number
                   (card32 time)
                   (window root event-window window)
                   (int16 root-x root-y win-x win-y)
                   (card16 state)
                   (boolean same-screen?))
                  ;;
                  ;;-slots-
                  ((xev-sequence seq)
                   (xev-code code)
                   (xev-time time)
                   (xev-window window)
                   (xev-event-window event-window)
                   (xev-root root)
                   (xev-root-position (with-module
                                          graphics.geometry
                                        (make-point root-x root-y)))
                   (xev-position (with-module
                                     graphics.geometry
                                   (make-point win-x win-y)))
                   (xev-state state)
                   (xev-same-screen? same-screen?)))

(declare-oo-event ((enter-notify 7 <xev-enter-notify>)
                   (leave-notify 8 <xev-leave-notify>))
                  ;;-decls-
                  ((card8) ; event code
                   ((member8 ancestor
                             virtual
                             inferior
                             nonlinear
                             nonlinear-virtual)
                    kind)
                   (card16       seq) ; sequence number
                   (card32       time)
                   (window       root window child)
                   (int16        root-x root-y win-x win-y)
                   (card16       state)
                   ((member8 normal grab ungrab) grab))
                  ;;-slots-
                  ((xev-sequence seq)
                   (xev-time time)
                   (xev-window window)
                   (xev-kind kind)
                   (xev-root root)
                   (xev-child child)
                   (xev-root-position (with-module
                                          graphics.geometry
                                        (make-point root-x root-y)))
                   (xev-position (with-module
                                     graphics.geometry
                                   (make-point win-x win-y)))
                   (xev-state state)
                   (xev-grab grab)))

(declare-oo-event ((configure-notify 22 <xev-configure-notify>))
                  ((card8)  ; event type
                   (card8)  ; unused
                   (card16 seq) ; sequence number
                   (window event-window window above-sibling)
                   (int16 fx fy)
                   (card16 fwidth fheight border-width)
                   (boolean override-redirect?))
                  ((xev-sequence seq)
                   (xev-event-window event-window)
                   (xev-window window)
                   (xev-above-sibling above-sibling)
                   (xev-frame (with-module
                                  graphics.geometry
                                (make-rect fx fy fwidth fheight)))
                   (xev-border-width border-width)
                   (xev-override-redirect? override-redirect?)))

(declare-oo-event ((exposure 12 <xev-exposure>))
                  ((card8)
                   (card8)
                   (card16 seq)
                   (window window)
                   (card16 fx fy fwidth fheight count))
                  ((xev-sequence seq)
                   (xev-window window)
                   (xev-frame (with-module
                                  graphics.geometry
                                (make-rect fx fy fwidth fheight)))
                   (xev-count count)))

(define (process-event-object (self <x-display>) 
                              #key handler
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
          (let* ((ev (if (instance? next <xev>)
                         next
                         ((vector-ref *oo-event-unwrappers*
                                      (bitwise-and (bvec-ref next 0) #x7F))
                          self
                          next
                          (>= (bvec-ref next 0) 128))))
                 (r (handler ev)))
            (if r
                (begin
                  ;; flush everything that's left, if requested
                  (if discard?
                      (discard-rest))
                  ;; repopulate the event queue, if requested
                  (if peek?
                      (begin
                        (unget-event self ev)
                        (for-each (lambda (e)
                                    (unget-event self e))
                                  peeked-past)))
                  r)
                (loop (cons ev peeked-past))))))))

(&module (export process-event-object
                 <xev>
                 <xev-enter-notify>
                 <xev-leave-notify>
                 <xev-button-press>
                 <xev-button-release>
                 <xev-key-press>
                 <xev-key-release>
                 <xev-focus>    ; abstract
                 <xev-focus-in>
                 <xev-focus-out>
                 <xev-configure-notify>
                 <xev-exposure>
                 <xev-unknown>
                 xev-display
                 xev-send-event?
                 xev-sequence
                 xev-data
                 xev-time
                 xev-window
                 xev-kind
                 xev-root
                 xev-child
                 xev-root-position
                 xev-position
                 xev-state
                 xev-grab
                 xev-mode
                 xev-event-window
                 xev-above-sibling
                 xev-frame
                 xev-border-width
                 xev-override-redirect?
                 xev-count
                 ))
)
