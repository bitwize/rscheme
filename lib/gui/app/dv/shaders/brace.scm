;;;
;;;   RScheme/dv shader:        brace
;;;

;;;
;;;   Instance variables
;;;   ==================

(define-instance-variable *stepin* 2pt <number:length>)
(define-instance-variable *radius* 3pt <number:length>)
(define-instance-variable *from* (0 0) <point>)
(define-instance-variable *to* (72 0) <point>)

;;;
;;;   Instance methods
;;;   ================

(define-procedure (trace)
  ;; set up the coordinate system so that (0,0) is at the midpoint,
  ;; (-w,0) is at the start, and (w,0) is at the end
  (let ((w (lay-flat (point-average *from* *to*) *to*))
        (r *radius*))
    ;; draw the starting piece
    (moveto (- w) (- (+ *stepin* r)))
    (lineto (- w) (- r))
    (arcn (- r w) (- r) r 180 90)
    (lineto (- r) 0)
    (arc (- r) r r 270 360)
    (arc r r r 180 270)
    (lineto (- w r) 0)
    (arcn (- w r) (- r) r 90 0)
    (lineto w (- (+ *stepin* r)))
    (stroke)))

(define-procedure (draw)    (trace))
(define-procedure (artwork) (trace))
(define-procedure (outline) (trace))

(define-procedure (handles)
  (handle *from*)               ; 0
  (handle *to*))                ; 1

(define-procedure (tweak h x0 y0)
  (case h
    ((0) (lambda (x y) (set! *from* (make-point x y)) (values 0 0)))
    ((1) (lambda (x y) (set! *to* (make-point x y)) (values 0 0)))
    (else #f)))

