;;;
;;;  A simple text editor for gui.x
;;;

,(use gui.x
      graphics.color
      syscalls
      graphics.geometry
      rs.util.properties
      tables
      rs.sys.threads.manager)

(define-class <editor-display> (<object>)
  x-display
  x-screen
  x-colormap
  ;;
  (styles init-function: make-string-table)
  (app-color-window-background* init-value: #f)
  (app-color-cursor-blink* init-value: #f)
  (app-font-body-text* init-value: #f)
  (app-font-control-text* init-value: #f)
  (x-keyboard-mapping* init-value: #f))

(define-method x-keyboard-mapping ((self <editor-display>))
  (or (x-keyboard-mapping* self)
      (let ((m (keyboard-mapping (x-display self))))
        (set-x-keyboard-mapping*! self m)
        m)))

(define-method hash-code ((self <editor-display>))
  (transient->hash self))

(define-macro (declare-app-font name key)
  `(define-method ,(symbol-append "app-font-" name) ((self <editor-display>))
     (or (,(symbol-append "app-font-" name "*") self)
         (let ((f (open-font (x-display self) ,key)))
           (,(symbol-append "set-app-font-" name "*!") self f)
           f))))
  
(define-macro (declare-app-color name default-expr)
  `(define-method ,(symbol-append "app-color-" name) ((self <editor-display>))
     (or (,(symbol-append "app-color-" name "*") self)
         (let ((c ,default-expr))
           (,(symbol-append "set-app-color-" name "*!") self c)
           c))))

;;;

(declare-app-font body-text
  "-adobe-courier-medium-r-normal--12-120-*-*-*-*-*-*")

(declare-app-font control-text
  "-adobe-helvetica-medium-r-normal--12-120-*-*-*-*-*-*")

     
(declare-app-color window-background (screen-white-pixel (x-screen self)))
(declare-app-color cursor-blink (alloc-color (x-colormap self) "dark blue"))

(define-thread-var *display* #f)

(define-class <text-face> (<object>)
  name
  x-font
  (x-foreground init-value: #f)
  (x-background init-value: #f))

(define (getfont dpy xname)
  (open-font (x-display dpy) xname))

(define (deffont name xname)
  (table-insert! (styles *display*) name (getfont *display* xname)))

(define (defcolor name (xcolor <color>))
  (table-insert! (styles *display*) name (alloc-color (x-colormap *display*)
                                                      xcolor)))

#|
(define (declare-face name font fg bg)
  (make <text-face>
        name: name
        x-font: 
  (alloc-color (x-colormap *display*) 
|#

(define (xhandle-window-specific-event #rest r
                                       #key 
                                       window
                                       event-key)
  (cond
   ((get-property window event-key #f)
    => (lambda (f)
         ;(format #t "F ~s\n" f)
         (apply f r)))
   (else
    (format #t "no window-specific handler: ~s on ~s\n" event-key window))))

(define (xhandle-event-window-specific-event #rest r
                                             #key 
                                             event-window
                                             event-key)
  (cond
   ((get-property event-window event-key #f)
    => (lambda (f)
         ;(format #t "F ~s\n" f)
         (apply f r)))
   (else
    (format #t "no window-specific handler: ~s on ~s\n" 
            event-key event-window))))
                                   
(define (xhandler #rest r #key display event-key)
  (handler-case
                                        ;(format #t "* ~s\n" r)
   (case event-key
     ((enter-notify
       leave-notify
       exposure
       focus-in
       focus-out) (apply xhandle-window-specific-event r))
     ((button-press
       button-release
       motion-notify
       key-press
       key-release) (apply xhandle-event-window-specific-event r))
     (else
      (format #t "event ~s + ~s\n" event-key r)))
   ((<condition> condition: err)
    (format #t "+------------------------------------------\n")
    (format #t "|  Error in X Event Handler for ~s\n" event-key)
    (format #t "+------------------------------------------\n")
    (for-each (lambda (p)
                (format #t "|  ~a  ~s\n" (car p) (cdr p)))
              (keyword-list->assoc-list r))
    (format #t "+------------------------------------------\n")
    (format #t "|  The error was:\n|  ~a" err)))
  #t)

(define (go)
  (let* ((d (open-display "localhost"))
         (s (car (display-roots d))))
    (set! *display* (make <editor-display>
                          x-display: d
                          x-screen: s
                          x-colormap: (screen-default-colormap s)))
    (thread-resume
     (make-thread
      (lambda ()
        (let loop ((i 0))
          (process-event d handler: xhandler)
          (loop (+ i 1))))
      "x-event"))))
