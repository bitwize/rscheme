;(load "_keywords.scm")
;(load "module.scm")

,(use gui.x
      rs.util.properties
      rs.sys.threads.manager)

(define d (open-display (with-module unixm (hostname))))

(define $colors '#(#xbbbbcc #xffffff #xcccccb #x808080))
(define $colors '#(32 1 3 8))

(define $steel-blue (make-color red: (/ 70 255)
				green: (/ 130 255)
				blue: (/ 180 255)))

(define s (car (display-roots d)))

(define (getcolor c dflt)
  (handler-case
    (alloc-color (screen-default-colormap s) c)
   ((<x-error>)
    (display "Could not allocate background color!\n")
    (dflt s))))

(define steelblue (getcolor "Steel Blue" screen-white-pixel))
(define aliceblue (getcolor "Alice Blue" screen-white-pixel))
(define aquamarine (getcolor "aquamarine" screen-white-pixel))

(define w (create-window parent: (screen-root s)
			 x: 50
			 y: 50
			 width: 120
			 height: 300
			 background: steelblue #|(vector-ref $colors 0)|#))

(define w1 (create-window parent: w
			  x: 10 
			  y: 10
			  width: 100
			  height: 20
			  border: (vector-ref $colors 2)
			  border-width: 1
			  event-mask: '(enter-window leave-window)
			  background: (vector-ref $colors 1)))

(define w2 (create-window parent: w
			  x: 10 
			  y: 35
			  width: 100
			  height: 20
			  border: (vector-ref $colors 2)
			  border-width: 1
			  event-mask: '(enter-window leave-window)
			  background: (vector-ref $colors 3)))

(map-window w)
(map-window w1)
(map-window w2)

(display-force-output d)

(define (xhandle-window-specific-event #rest r
                                       #key 
                                       window
                                       event-key)
  (cond
   ((get-property window event-key #f)
    => (lambda (f)
         ;(format #t "F ~s\n" f)
         (apply f r)))))
                                   
(define (xhandler #rest r #key display event-key)
  ;(format #t "* ~s\n" r)
  (case event-key
    ((enter-notify
      leave-notify) (apply xhandle-window-specific-event r))
    (else
     (format #t "event ~s + ~s\n" event-key r)))
  #t)

(thread-resume
 (make-thread
  (lambda ()
    (let loop ((i 0))
      (process-event d handler: xhandler)
      (loop (+ i 1))))
  "Xevent"))

(define (did-enter #key display event-key send-event? kind time root window child root-x root-y x y state)
  ;(format #t "ENTER ~s ~s\n" x y)
  (draw-rectangle window
                  (get-property window 'gc0)
                  1 1 10 10))

(define (did-leave #key display event-key send-event? kind time root window child root-x root-y x y state)
  ;(format #t "LEAVE ~s ~s\n" x y)
  (draw-rectangle window
                  (get-property window 'gc1)
                  1 1 10 10))

(define (setup (w <x-window>))
  (set-property! w
                 'gc0
                 (create-gcontext drawable: w
                                  foreground: aquamarine
                                  background: (screen-black-pixel s)))
  (set-property! w
                 'gc1
                 (create-gcontext drawable: w
                                  foreground: aliceblue
                                  background: (screen-black-pixel s)))

  (set-property! w 'enter-notify did-enter)
  (set-property! w 'leave-notify did-leave))

(setup w1)
(setup w2)

