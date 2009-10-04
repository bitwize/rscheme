
(define-macro (debug-text-frame expr)
  `(begin ,expr))
;'(values)

(define-class <text-frame> (<object>)
  frame
  (flow init-value: 'a)
  (primary-sidebar init-value: #f)      ; one of {#f left right inside outside}
  (secondary-sidebar? init-value: #f)
  (primary-sidebar-width init-value: 72)
  (secondary-sidebar-width init-value: 72)
  (sidebar-gap init-value: 6)
  (num-columns init-value: 1)
  (column-gap init-value: 18))

(define-method render ((self <list>) dev)
  (for-each (lambda (member)
              (render member dev))
            self))

(define-method render ((self <text-frame>) dev)
  (if *show-outlines*
      (render-outline self dev)))

(define-method render-outline ((self <text-frame>) dev)
  (with-gstate-saved 
   dev
   (lambda ()
     (setlinewidth dev 0.25)
     (setcolor dev (device-color dev '(gray 0.5)))
     (rectstroke dev (frame self))
     ;(setlinewidth dev 0.25)
     (let ((sb (primary-sidebar-subframe self)))
       (if sb
           (rectstroke dev sb)))
     (let ((sb (secondary-sidebar-subframe self)))
       (if sb
           (rectstroke dev sb)))
     (for-each (lambda (col)
                 (rectstroke dev col))
               (column-subframes self)))))

(define-method column-subframes ((self <text-frame>))
  (let* ((left-side-take (if (primary-sidebar self)
                             (+ (primary-sidebar-width self)
                                (sidebar-gap self))
                             0))
         (n (num-columns self))
         (w (- (size-width (frame self)) left-side-take))
         (x (+ (origin-x (frame self))
               left-side-take))
         (colw (/ (- w (* (column-gap self) (- n 1))) n)))
    (map (lambda (i)
           (make-rect (+ x 
                         (* i colw)
                         (if (> i 0)
                             (* i (column-gap self))
                             0))
                      (origin-y (frame self))
                      colw
                      (size-height (frame self))))
         (range n))))

(define-method secondary-sidebar-subframe ((self <text-frame>))
  #f)

(define-method primary-sidebar-subframe ((self <text-frame>))
  (if (primary-sidebar self)
      ;; XXX ignore sidebar types for now -- always left!
      (make-rect (origin-x (frame self))
                 (origin-y (frame self))
                 (primary-sidebar-width self)
                 (height (frame self)))
      #f))

;;;

;;;

#|
(define-method iterate-over-space ((self <list>) rs)
  (let loop ((y self))
    (if (null? y)
        (values)
        (bind ((type subtype (request-peek rs)))
          (if (and (eq? type 'break)
                   (eq? type 'frame))
              (request-read rs)
              (begin
                (iterate-over-space (car y) rs)
                (loop (cdr y))))))))

(define-method iterate-over-space ((self <vector>) rs)
  ...)
|#

;;; process a list of text-frames

(define (iterate-over-flow (frames <list>) pg rs)
  (let loop ((l frames))
    (bind ((type subtype (request-peek rs)))
      (debug-text-frame
       (format #t "flow peek: ~s ~s in frames ~s\n" type subtype l))
      (cond
       ;;
       ((and (eq? type 'break) (eq? subtype 'page))
        ;; consume the request, return "ok", and exit
        (request-read rs)
        (debug-text-frame
         (format #t "returning ok-page\n"))
        (request-return rs 'ok-page)
        (debug-text-frame
         (format #t "need another...\n")))
       ;;
       ((eq? type 'vskip)
        (if (eq? l frames)          ; ignore vspace at beginning of frame
            (begin
              (request-read rs)
              (request-return rs 'ok-skip)
              (loop l))
            (if (pair? l)
                (begin
                  (iterate-over-frame (car l) pg rs)
                  (loop (cdr l)))
                (values))))
       ;;
       ((pair? l)
        ;; real work to do, and a way to do it...
        (iterate-over-frame (car l) pg rs)
        (loop (cdr l)))
       (else
        ;; real work to do, but we can't do it...
        (values))))))
  
;;; process a text-frame

(define (iterate-over-frame (self <text-frame>) pg rs)
  (let loop ((c (range (num-columns self))))
    (debug-text-frame
     (format #t "columns ~s\n" c))
    (bind ((type subtype (request-peek rs)))
      (debug-text-frame
       (format #t "frame peek: ~s ~s\n" type subtype))
      (cond
       ;;
       ((and (eq? type 'break) (eq? subtype 'frame))
        ;; it's a break at our level; return "ok", exit
        ;; consume the request and exit
        (request-read rs)
        (request-return rs 'ok-frame))
       ;;
       ((and (eq? type 'break) 
             (not (memq subtype '(column))))
        ;; it's a break of some structure at a higher level than us...
        ;; don't consume the request; just exit
        (values))
       ;;
       ((pair? c)
        (iterate-over-column self (car c) pg rs)
        (loop (cdr c)))
       ;;
       (else
        ;; real work to do, but we can't do it...
        (values))))))

;;; process a column

(define (iterate-over-column (self <text-frame>) col pg rs)
  (let ((h (height (frame self)))
        (sf (make <placement-subframe>
                  page: pg
                  frame: self
                  column: col))
        (ystack (list (cons 0 1))))     ; (0 . 1) is a marker token
    ;;
    (define (belongs-to-this-iteration? stack)
      ;; something of a hack to handle the case that the
      ;; stack is pushed, and *then* a new page is cut.  Unfortunately,
      ;; it still only comes out right in a few cases; to wit, if the
      ;; page is cut before the sidebar text is emitted.  Otherwise,
      ;; the sidebar text winds up on the first page and the in-column
      ;; text winds up on the second page.
      (eq? (last stack) (last ystack)))
    ;;
    (let loop ((y 0))
      (debug-text-frame
        (format #t "baseline ~s\n" y))
      (bind ((type subtype (request-peek rs)))
        (debug-text-frame
         (format #t "column peek: ~s ~s\n" type subtype))
        (cond
         ((and (eq? type 'break) (eq? subtype 'column))
          ;; it's a break at our level; consume, return "ok", and exit
          (request-read rs)
          (request-return rs 'ok-column))
         ((eq? type 'break)
          ;; it's a break at some other (i.e., higher) level; just exit
          (values))
         ;;
         ((eq? type 'vskip)
          (if (< (+ y subtype) h)
              (begin
                (request-read rs)
                (request-return rs y)
                (loop (+ y subtype)))))
         ((eq? type 'ypush)
          (request-read rs)
          (request-return rs (cons y ystack))
          (loop y))
         ((eq? type 'ypop)
          (request-read rs)
          (request-return rs)
          (if (belongs-to-this-iteration? subtype)
              (let ((top (car subtype)))
                (set! ystack (cdr subtype))
                (loop top))
              (loop 0)))
         ;;
         (else
          ;; it's a proper request
          ;; XXX note: this won't actually work when we have multiple cols
          (let ((use-sf (if (memq subtype '(sidebar/first-baseline
                                            sidebar/first-baseline*
                                            sidebar/top-edge
                                            sidebar/last-baseline))
                            (begin
                              (make <placement-subframe>
                                    page: pg
                                    frame: self
                                    column: 'across-primary-sidebar))
                            (begin
                              sf))))
            (if (< (+ y type) h)
                (begin
                  (request-read rs)
                  (request-return rs use-sf (+ y type))
                  (loop (+ y type)))))))))))

#|
(define (t)
  (make-request-stream
   (lambda (rs)
     (iterate-over-flow (chapter-start-page) rs))))
|#
