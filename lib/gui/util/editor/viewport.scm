(define-thread-var *current-event* #f)

(define-class <event> (<object>)
  buffer
  viewport
  data)


(define-class <editor-viewport> (<object>)
  (lock type: <semaphore>)
  (keyboard-state-machine)
  (cursor init-value: #f)
  (clear-cursor init-value: #f)
  (viewport-active? init-value: #f)
  (x-window)
  (x-gc))


(define (editor ed)
  (let* ((fnt (app-font-body-text ed))
         (margin 3)
         (cw (char-width fnt #\M))
         (lineht (+ (font-ascent fnt) (font-descent fnt) 2))
         (buf (process-text $sample-text margin))
         (w (create-window parent: (screen-root (x-screen ed))
                           x: 50
                           y: 60
                           width: (* 80 cw)
                           height: (* 20 lineht)
                           event-mask: '(exposure
                                         enter-window leave-window 
                                         focus-change
                                         button-press
                                         button-release
                                         button1-motion
                                         key-press
                                         key-release)
                           background: (app-color-window-background ed)))
         (textgc (create-gcontext 
                  drawable: w
                  foreground: (screen-black-pixel (x-screen ed))
                  background: (screen-white-pixel (x-screen ed))
                  font: fnt))
         (vp (make <editor-viewport>
                   lock: (make-semaphore 1)
                   x-window: w
                   keyboard-state-machine: (make-key-state-machine)
                   x-gc: textgc))
         (t0 (time))
         (flipgc (create-gcontext 
                  drawable: w
                  foreground: (bitwise-xor
                               (app-color-cursor-blink ed)
                               (screen-white-pixel (x-screen ed)))
                  function: 'boole-xor))
         (listflip (listflipper w flipgc)))
    ;;
    (thread-resume
     (make-thread
      (lambda ()
        (let ((gc flipgc)
              (ch (font-ascent fnt)))
          (let loop ()
            (thread-sleep 0.2)
            (with-semaphore
             (lock vp)
             (let ((c (and (viewport-active? vp) (cursor vp))))
               (if c
                   (begin
                     (if (clear-cursor vp)
                         ((clear-cursor vp))
                         (let ((flip (flipper vp w gc (car c) (cadr c))))
                           (flip)
                           (set-clear-cursor! vp flip)))
                     (display-force-output (x-display ed))))))
            (loop))))
      "blink"))
    ;;
    (define (exposure #rest r #key x y width height count)
      (let ((t (time)))
        (format #t "exposure: ~a ~s ~d\n" 
                (- (time->epoch-seconds t) (time->epoch-seconds t0)) 
                (make-rect x y width height)
                count)
        (set! t0 t))
      (draw-buffer-lines buf w textgc margin (make-rect x y width height)))
    ;;
    (define (focus-in #rest r)
      (set-viewport-active?! vp #t))
    ;;
    (define (focus-out #rest r)
      (set-viewport-active?! vp #f)
      (if (clear-cursor vp)
          ((clear-cursor vp))))
    ;;
    (define (key-press #rest r #key code state)
      (let ((ch (key->char code state)))
        (format #t "key-press: ~s\n" ch)
        (with-semaphore
         (lock vp)
         (thread-let ((*current-event* (make <event>
                                             buffer: buf
                                             viewport: vp
                                             data: ch)))
           (if (clear-cursor vp)
               ((clear-cursor vp)))
          (process-key (keyboard-state-machine vp) *current-event*)))))
    ;;
    (define (key-release #rest r)
      (format #t "key up: ~s\n" r))
    ;;
    (define (cursor-at type line run delta)
      (case type
        ((in-run)
         (bind ((i x1 (char-index-in-run buf run delta)))
           (list line (make-point (+ (x run) x1) (y line)) run i)))
        ((after-end-of-line)
         (let* ((n (vector-length (runs line)))
                (last (and (> n 0) (vector-ref (runs line) (- n 1)))))
           (if last
               (list line (make-point (+ (x last) (width last))
                                      (y line))
                     last
                     (string-length (text last)))
               (list line (make-point margin (y line)) #f 0))))
        ((before-start-of-line)
         (let* ((n (vector-length (runs line)))
                (first (and (> n 0) (vector-ref (runs line) 0))))
           (list line (make-point margin (y line)) first 0)))
        (else
         (format #t "bad cursor find: ~s ~s ~s ~s" type line run delta)
         #f)))
    ;;
    (define (mouse->cursor x y)
      (bind ((type line run delta (locate-screen-point buf x y)))
        (cursor-at type line run delta)))
    ;;
    (define (button-press #rest r #key x y state code)
      (set-cursor! vp (mouse->cursor x y)))
    #|
      (define (goto type line run delta)
        (set-cursor! vp (cursor-at type line run delta)))
      ---
      (bind ((type line run delta (locate-screen-point buf x y)))
        (format #t "* button-press ~s (~s,~s) ~s ==> ~s ~s ~s ~s\n" 
                code x y state
                type line run delta)
        (goto type line run delta)
        (values))
      |#
    ;;
    (define (button-release #rest r #key x y state code)
      (format #t "* button-release ~s (~s,~s) ~s\n" code x y state)
      (listflip '())) 
    ;;
    (define (motion-notify #rest r #key x y state)
      (bind ((c (mouse->cursor x y))
             (l (hilite-current-selection (cursor vp) c)))
        ;(format #t "  sel ~s\n" l)
        (listflip l)))
    ;;
    (define (did-enter #rest r)
      (values))
    ;;
    (define (did-leave #rest r)
      (values))
    ;;
    (set-property! w 'enter-notify did-enter)
    (set-property! w 'leave-notify did-leave)
    (set-property! w 'focus-in focus-in)
    (set-property! w 'focus-out focus-out)
    (set-property! w 'key-press key-press)
    (set-property! w 'key-release key-release)
    (set-property! w 'exposure exposure)
    (set-property! w 'button-press button-press)
    (set-property! w 'button-release button-release)
    (set-property! w 'motion-notify motion-notify)
    ;;
    (map-window w)
    ;;
    (display-force-output (x-display ed))
    vp))


(define (locate-screen-point (buf <editor-buffer>) mouse-x mouse-y)
  (call-with-current-continuation
   (lambda (exit)
     (let ((first-line (and (> (vector-length (lines buf)) 0)
                            (vector-ref (lines buf) 0)))
           (last-line (and (> (vector-length (lines buf)) 0)
                           (vector-ref (lines buf) 
                                       (- (vector-length (lines buf)) 1)))))
       ;;
       (if (and first-line
                (<= mouse-y (- (y first-line) (height first-line))))
           (exit 'before-first-line))
       ;;
       (if (and last-line
                (>= mouse-y (- (y last-line) (height last-line))))
           (exit 'after-last-line))
       ;;
       (vector-for-each
        (lambda ((l <editor-line>))
          (if (and (>= mouse-y (- (y l) (height l)))
                   (< mouse-y (+ (y l) (depth l))))
              (let* ((runs (runs l))
                     (n (vector-length runs)))
                ;;
                (if (or (= n 0)
                        (< mouse-x (x (vector-ref runs 0))))
                    (exit 'before-start-of-line l))
                ;;
                (vector-for-each
                 (lambda ((r <buffer-text-run>))
                   (if (and (>= mouse-x (x r))
                            (< mouse-x (+ (x r) (width r))))
                       (exit 'in-run l r (- mouse-x (x r)))))
                 runs)
                (exit 'after-end-of-line l))))
        (lines buf))
       'unknown))))

(define (draw-buffer-lines (buf <editor-buffer>) win gc margin paint)
  #|
  ;;
  ;; draw coord system (inside margin), and all text baselines
  ;;
  (set-gcontext-foreground! gc (style-compile 'gray8))
  (draw-line win gc margin margin (+ 100 margin) margin)
  (draw-line win gc margin margin margin (+ 100 margin))
  ;;
  (vector-for-each
   (lambda ((l <editor-line>))
     (let* ((n (vector-length (runs l)))
            (lastrun (if (= n 0)
                         #f
                         (vector-ref (runs l) (- n 1)))))
       ;;
       (if lastrun
           (draw-line win gc
                      margin (y l)
                      (+ (x lastrun) (width lastrun)) (y l))
           (draw-line win gc
                      margin (y l)
                      (+ margin 20) (y l)))))
   (lines buf))
  |#
  ;;
  (let ((ix (compiled-style-index buf)))
    (vector-for-each
     (lambda ((l <editor-line>))
       (vector-for-each
        (lambda ((r <buffer-text-run>))
          ((vector-ref ix (style r)) win gc (x r) (y l) (width r)
           (height l)
           (depth l)
           (text r)))
        (runs l)))
   (lines buf))))

(define (flipper (self <editor-viewport>) win gc (line <editor-line>) pt)
  (let ((y (- (y pt) (height line)))
        (x (x pt))
        (h (+ (height line) (depth line))))
    ;;
    (lambda ()
      (draw-rectangle win gc x y 1 h #t)
      (set-clear-cursor! self #f))))

(define (fliplist win gc lst)
  (x-fillarea lst win gc))


;;;

(define (current-event)
  *current-event*)

(define (current-editor-buffer)
  (buffer (current-event)))

(define (current-point)
  (cursor (viewport (current-event))))

(define (insert-content (inserting <string>))
  (bind ((v (viewport (current-event)))
         (line pt run i (list->values (current-point)))
         (str (if run (text run) ""))
         (new-str (string-append (substring str 0 i)
                                 inserting
                                 (substring str i))))
    (set-text! run new-str)
    ;;
    (let ((dx (reformat-run (current-editor-buffer) line run)))
      (set-car! (cdddr (cursor v)) (+ i (string-length inserting)))
      (set-car! (cdr (cursor v)) (point+ pt (make-size dx 0)))
      (clear-area (x-window v)
                  x: (x run)
                  y: (- (y line) (height line))
                  width: (- (line-limit-x line) (x run))
                  height: (+ (height line) (depth line))
                  exposures?: #t))))

(define (kbd-insert-self)
  (format #t "insert ~s at ~s\n" key (current-point))
  (insert-content (string (data (current-event)))))

(define (kill-line)
  (values))

(define (clear-after-run v line run)
  (clear-area (x-window v)
              x: (x run)
              y: (- (y line) (height line))
              width: (- (line-limit-x line) (x run))
              height: (+ (height line) (depth line))
              exposures?: #t))

(define *last-event* #f)

(define (backward-delete-char)
  (bind ((v (viewport (current-event)))
         (cur (current-point))
         (line pt run i (list->values cur)))
    ;;
    (set! *last-event* (current-event))
    (if (= i 0)
        (if (eq? run (line-first-run line))
            ;; delete the line break
            (begin
              (clear-area (x-window v) exposures?: #t)
              )
            ;; delete the last char of the previous run
            (let* ((r (line-previous-run line run))
                   (str (text r))
                   (new-str (substring str 0 (sub1 (string-length str)))))
              (clear-after-run v line r)
              (if (string=? new-str "")
                  (bind ((dx r (delete-run (current-editor-buffer) line r)))
                    (set-car! (cdr cur) (point+ pt (make-size dx 0)))
                    (set-car! (cddr cur) r)
                    (set-car! (cdddr cur) (string-length (text r))))
                  (begin
                    (set-text! r new-str)
                    (let ((dx (reformat-run (current-editor-buffer) line r)))
                      (set-car! (cdr cur) (point+ pt (make-size dx 0))))))))
        (let* ((str (text run))
               (new-str (string-append (substring str 0 (- i 1))
                                       (substring str i))))
          (clear-after-run v line run)
          (format #t "old str = ~s  new str = ~s\n" str new-str)
          (if (string=? new-str "")
              ;; the whole run has been deleted
              (bind ((dx r (delete-run (current-editor-buffer) line run)))
                (set-car! (cdr cur) (point+ pt (make-size dx 0)))
                (set-car! (cddr cur) r)
                (set-car! (cdddr cur) (string-length (text r))))
              (begin
                ;; there is something left
                (set-text! run new-str)
                (let ((dx (reformat-run (current-editor-buffer) line run)))
                  (set-car! (cdddr (cursor v)) (- i 1))
                  (set-car! (cdr (cursor v)) (point+ pt (make-size dx 0))))))))))


(define (end-of-line)
  (bind ((cur (current-point))
         (line pt run i (list->values cur)))
    (set-car! (cdr cur) (make-point (line-limit-x line) (y line)))
    (set-car! (cddr cur) (line-last-run line))
    (set-car! (cdddr cur) (string-length (text (line-last-run line))))))

(define (beginning-of-line)
  (bind ((cur (current-point))
         (line pt run i (list->values cur)))
    (set-car! (cdr cur) (make-point (x (vector-ref (runs line) 0))
                                    (y line)))
    (set-car! (cddr cur) (vector-ref (runs line) 0))
    (set-car! (cdddr cur) 0)))

(define (redisplay)
  (reprocess-buffer! (current-editor-buffer))
  (clear-area (x-window (viewport (current-event)))
              exposures?: #t))

(define (hilite-current-selection from to)
  (let ((from-line (car from))
        (from-pt (cadr from))
        (from-run (caddr from))
        (from-index (cadddr from))
        ;;
        (to-line (car to))
        (to-pt (cadr to))
        (to-run (caddr to))
        (to-index (cadddr to))
        ;;
        (min-x 3)
        (max-x 200))
    ;;
    (define (bottom-y l)
      (+ (y l) (depth l)))
    (define (top-y l)
      (- (y l) (height l)))
    ;;
    ;;
    (define (box x1 y1 x2 y2)
      (if (= x1 x2)
          '()
          (list (make-rect x1 y1 (- x2 x1) (- y2 y1)))))
    ;;
    (if (eq? from-line to-line)
        (if (or (< (x to-run) (x from-run))
                (and (eq? from-run to-run)
                     (< to-index from-index)))
            (hilite-current-selection to from)
            ;;  an extent on a single line
            (box (x from-pt)
                 (top-y from-line)
                 (x to-pt)
                 (bottom-y from-line)))
        (if (< (y to-line) (y from-line))
            (hilite-current-selection to from)
            ;; spanning multiple lines
            (let ((y1 (bottom-y from-line))
                  (y2 (top-y to-line)))
              (if (= y1 y2) ; but only two lines
                  (append (box (x from-pt) (top-y from-line) max-x y1)
                          (box min-x y2 (x to-pt) (bottom-y to-line)))
                  (append (box (x from-pt) (top-y from-line) max-x y1)
                          (box min-x y1 max-x y2)
                          (box min-x y2 (x to-pt) (bottom-y to-line)))))))))

(define (listflipper win gc)
  (let ((save '()))
    (lambda (rect-list)
      (if rect-list
          (if (not (equal? rect-list save))
              (if (null? rect-list)
                  (begin
                    (if (pair? save)
                        (fliplist win gc save))
                    (set! save '()))
                  (begin
                    (if (pair? save)
                        (x-fillarea (rect-list-xor save rect-list) win gc)
                        (fliplist win gc rect-list))
                    (set! save rect-list))))
          (set! save '())))))

(define (rect-list-xor a b)
  ;;
  (define (rect-list-area l)
    (case (length l)
      ((1) (rect-area (car l)))
      ((2) (area-union (rect-area (car l))
                       (rect-area (cadr l))))
      ((3) (area-union (area-union (rect-area (car l))
                                   (rect-area (cadr l)))
                       (rect-area (caddr l))))))
  ;;
  (let ((A (rect-list-area a))
        (B (rect-list-area b)))
    ;(format #t "XOR ~s\n    ~s\n" a b)
    (let ((x (area-xor A B)))
      ;(format #t "    = ~s\n" (map path-points (subpaths x)))
      x)))

#|

(define A (list (make-rect 0 0 30 10)))
(define B (list (make-rect 0 0 40 10)))

(rect-list-subtract A B)

((make-rect 45 99 155 16) 
 (make-rect 3 115 197 16)        
 (make-rect 3 131 0 16))

(rect-list-xor (list (make-rect 150 147 7 16))
               (list (make-rect 143 147 14 16)))

|#

