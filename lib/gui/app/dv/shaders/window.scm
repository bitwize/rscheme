
;;;

(define-instance-variable *width* 200pt <number:length>)
(define-instance-variable *height* 300pt <number:length>)
(define-instance-variable *title* "Untitled" <string>)
(define-instance-variable *title-height* 16pt <number:length>)
(define-instance-variable *title-font* (Helvetica Regular 12) <font>)

;;;

(define-procedure (handles)
  (scale *width* *height*)
  (handlef 0 0 'sw)             ; 0
  (handlef 1 0 'se)             ; 1
  (handlef 0 1 'nw)             ; 2
  (handlef 1 1 'ne)             ; 3
  (handlef 0.5 0 's)            ; 4
  (handlef 1 0.5 'e)            ; 5
  (handlef 0.5 1 'n)            ; 6
  (handlef 0 0.5 'w))           ; 7

(define-procedure (tweak h x0 y0)
  (let ((w0 *width*)
        (h0 *height*))
    (let-syntax ((s-change (syntax-form (y)
                             (let ((dy (- y y0)))
                               (set! *height* (- h0 dy))
                               dy)))
                 (w-change (syntax-form (x)
                             (let ((dx (- x x0)))
                               (set! *width* (- w0 dx))
                               dx)))
                 (n-change (syntax-form (y)
                             (let ((dy (- y y0)))
                               (set! *height* (+ h0 dy))
                               0)))
                 (e-change (syntax-form (x)
                             (let ((dx (- x x0)))
                               (set! *width* (+ w0 dx))
                               0))))
      (case h
        ((0) (lambda (x y) (values (w-change x) (s-change y))))
        ((1) (lambda (x y) (values (e-change x) (s-change y))))
        ((2) (lambda (x y) (values (w-change x) (n-change y))))
        ((3) (lambda (x y) (values (e-change x) (n-change y))))
        ((4) (lambda (x y) (values 0 (s-change y))))
        ((6) (lambda (x y) (values 0 (n-change y))))
        ((5) (lambda (x y) (values (e-change x) 0)))
        ((7) (lambda (x y) (values (w-change x) 0)))
        (else #f)))))
    

;; show the artwork in outline

(define-procedure (artwork)
  (gsaved
   (rectpath 0 0 *width* *height*)
   (stroke))
  (gsaved
   (rectpath 0 (- *height* *title-height*) *width* *title-height*)
   (stroke))
  (setfont *title-font*)
  (moveto 4 (+ (- *height* *title-height*) 2))
  (show *title*))

(define-procedure (draw)
  (gsaved
   (rectpath 0 0 *width* *height*)
   (stroke))
  (gsaved
   (rectpath 0 (- *height* *title-height*) *width* *title-height*)
   (fill))
  (setcolor 1)
  (setfont *title-font*)
  (moveto 4 (+ (- *height* *title-height*) 2))
  (show *title*))

(define-procedure (outline)
  (outline/rect 0 0 *width* *height*)
  (outline/line 0 (- *height* *title-height*) 
                *width* (- *height* *title-height*)))

