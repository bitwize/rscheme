;;;
;;;  Shader: Dataflow Node
;;;

(define-instance-variable *name* "Node" <string>)
;(define-instance-variable *ports* () <object>)
(define-instance-variable *width* 2cm <number:length>)
(define-instance-variable *height* 1cm <number:length>)
(define-instance-variable *name-font* (Times Roman 12) <font>)

;;;

(define-procedure (draw-label)
  (moveto (/ (- *width*
                (string-width *name-font* *name*))
             2)
          (/ (- *height* (font-size *name-font*)) 2))
  (setfont *name-font*)
  (show *name*))

;;;

(define-procedure (outline)
  (rectpath 0 0 *width* *height*)
  (stroke))

(define-procedure (artwork)
  (outline)
  (draw-label))

(define-procedure (draw)
  (outline)
  (draw-label))

(define-procedure (handles)
  (handle 0 0)
  (handle *width* 0)
  (handle 0 *height*)
  (handle *width* *height*))

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
        (else #f)))))
