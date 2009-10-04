;;;
;;;  Shader: Permanent Storage (Disk Drive)
;;;
;;;  Instance variables
;;;
(define-instance-variable *height* 1cm <number:length>)
(define-instance-variable *width* 1cm <number:length>)
(define-instance-variable *squish* 2 <real>)
(define-instance-variable *bodyfill* white <fill-style>)
(define-instance-variable *topfill* white <fill-style>)
(define-instance-variable *stroke* black <stroke-style>)

;;

(define-procedure (trace art?)
  ;;
  ;; Draw the main part of the body
  ;;
  (scale *width* (/ *height* *squish*))
  (gsaved
   (moveto 0 *squish*)
   (lineto 0 0)
   (arc 1/2 0 1/2 180 360)
   (lineto 1 *squish*)
   (arc 1/2 *squish* 1/2 0 180)
   (closepath)
   (scale (/ 1.0 *width*) (/ *squish* *height*))
   (if art?
       (stroke)
       (begin
         (fill *bodyfill*)
         (stroke *stroke*))))
  ;;
  ;;  Draw the top disk
  ;;
  (gsaved
   (moveto 0 *squish*)
   (arc 1/2 *squish* 1/2 180 360)
   ;(lineto 1/2 1/2)
   ;(closepath)
   (scale (/ 1.0 *width*) (/ *squish* *height*))
   ;(scale (/ *width*) (/ *squish* *height*))
   (if art?
       (stroke)
       (begin
         (fill *topfill*)
         (stroke *stroke*)))))

;;
(define-procedure (handles)
  (scale *width* (/ *height* *squish*))
  (handle 0 0)                                  ; 0
  (handle 1 0)                                  ; 1
  (handle 0 *squish*)                           ; 2
  (handle 1 *squish*)                           ; 3
  (handle 1/2 -1/2))

;;
(define-procedure (draw)
  (trace #f))

;;
(define-procedure (artwork)
  (trace #t))

;;

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
        ((4) (let ((s0 *squish*))
               (lambda (x y)
                 (set! *squish* (/ h0 (* -2 y)))
                 (values 0 0))))
        (else #f)))))

(define-procedure (outline)
  (trace #t))

