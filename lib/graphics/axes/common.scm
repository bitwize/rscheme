
(define $major-tic-mark (make <graphic-tic-mark>
                              inside-far-side?: #t
                              inside-length: 5
                              outside-length: 2))

(define $minor-tic-mark (make <graphic-tic-mark>
                              inside-far-side?: #t
                              inside-length: 3
                              outside-length: 0))

(define $y-label-default-style (make <graphic-tic-label>
                                     font: (get-text-font "Times"
                                                          "Italic"
                                                          8)))

(define $x-label-default-style (make <graphic-tic-label>
                                     anchor: '(center top)
                                     font: (get-text-font "Times"
                                                          "Italic"
                                                          8)))

(define (tic-label-style orientation)
  (case orientation
    ((x) $x-label-default-style)
    ((y) $y-label-default-style)))

(define (linear-scale-axis #key 
                           from 
                           to
                           label
                           (value-units default: #f)
                           (orientation default: 'x)
                           (size default: 100))
  (bind ((axis-min axis-max num-major minor-per-major (linear-auto-range from to))
         (a (make <graphic-axis>
                  label: label
                  size: (case orientation
                          ((y) (make-size 0 size))
                          ((x) (make-size size 0)))
                  outside-unit: (case orientation
                                  ((y) (make-size -1 0))
                                  ((x) (make-size 0 -1)))
                  value-units: value-units
                  value-units-prefix-mode: 'decimal
                  lower-value-limit: axis-min
                  upper-value-limit: axis-max))
         (num-minor (+ 1 (* minor-per-major (- num-major 1))))
         (major-delta (/ (- axis-max axis-min) (- num-major 1)))
         (minor-delta (/ (- axis-max axis-min) (- num-minor 1))))
    ;;
    (set-label-decimal-digits! a 
                               (max 0 (inexact->exact
                                       (- (floor (log10 major-delta))))))
    ;;
    (add-tic-mark! a
                   (make <graphic-tic-set>
                         container: a
                         count: num-major
                         generator: (lambda (i)
                                      (+ axis-min (* i major-delta)))
                         style: (tic-label-style orientation)))
    ;;
    (add-tic-mark! a
                   (make <graphic-tic-set>
                         container: a
                         count: num-major
                         generator: (lambda (i)
                                      (+ axis-min (* i major-delta)))
                         style: $major-tic-mark))
    (add-tic-mark! a
                   (make <graphic-tic-set>
                         container: a
                         count: num-minor
                         generator: (lambda (i)
                                      (+ axis-min (* i minor-delta)))
                         style: $minor-tic-mark))
    ;;
    a))


(define (log-scale-axis #key 
                        from 
                        to
                        label
                        (value-units default: #f)
                        #|
                        lower-value-limit
                        upper-value-limit
                        |#
                        (orientation default: 'x)
                        (size default: 100))
  ;;
  (bind ((axis-min axis-max num-major minor-per-major (log-auto-range from to))
         (a (make <graphic-axis>
                 label: label
                 size: (case orientation
                         ((y) (make-size 0 size))
                         ((x) (make-size size 0)))
                 outside-unit: (case orientation
                                 ((y) (make-size -1 0))
                                 ((x) (make-size 0 -1)))
                 value-units: value-units
                 value-units-prefix-mode: 'decimal
                 lower-value-limit: axis-min ; lower-value-limit
                 upper-value-limit: axis-max ; upper-value-limit
                 transfer-function: log
                 inverse-transfer-function: exp)))
    ;;
    (add-tic-mark! a
                   (make <graphic-tic-set>
                         container: a
                         count: num-major
                         generator: (lambda (i)
                                      (* axis-min (expt 10.0 i)))
                         style: (tic-label-style orientation)))
    (add-tic-mark! a
                   (make <graphic-tic-set>
                         container: a
                         count: num-major
                         generator: (lambda (i)
                                      (* axis-min (expt 10.0 i)))
                         style: $major-tic-mark))
    ;;
    (add-tic-mark! a
                   (make <graphic-tic-set>
                         container: a
                         count: (+ 1 (* minor-per-major (- num-major 1)))
                         generator: (lambda (i)
                                      (* axis-min
                                         (expt 10.0 
                                               (quotient i minor-per-major))
                                         (+ 1 (modulo i minor-per-major))))
                         style: $minor-tic-mark))
    ;;
    a))


;;;

(define (axis-bbox (self <graphic-axis>) frame)
  (union-rect (tic-marks-bbox self frame)
              (compute-axis-label-geometry self)))
  
(define (tic-marks-bbox (self <graphic-axis>) frame)
  (union-rects
   (map (lambda (t)
          (tic-mark-set-bbox t frame))
        (tic-marks self))))
  
(define (draw-axis (self <graphic-axis>) dev frame)
  (for-each
   (lambda (t)
     (draw-tic-mark-set t dev frame))
   (tic-marks self))
  ;;
  (bind ((box renderer (compute-axis-label-geometry self)))
    (renderer dev)))

(define (rotate-rect (r <rect>))
  (transform r (rotate $identity-transform 90)))

;;; axis-standoff: figure out how far away the axis label has to go to
;;; avoid clobbering the (major) tic labels

(define (axis-standoff (self <graphic-axis>))
  ;; we don't care about the width and height, because we're just going
  ;; to look at the reverse offset
  (let* ((b (tic-marks-bbox self (make-rect 0 0 100 100)))
         (s (make-size (min (origin-x b) 0) (min (origin-y b) 0))))
    (inner-product s (outside-unit self))))

(define (compute-axis-label-geometry (self <graphic-axis>))
  (bind ((axis-label (cons 'span (compute-title self)))
         (tbox (rendering-bbox axis-label (label-font self) $zero-point))
         (orient (axis-orientation self))
         (w (size-width tbox))
         (h (size-height tbox))
         (c (size+ (size* (size self) 0.5)
                   (size* (outside-unit self) 
                          (+ (axis-standoff self)
                             ;; add 25% of the font size for leading
                             (* 0.25 (font-size (label-font self)))))))
         (origin (case orient
                   ((x) (make-point (- (dx c) (/ w 2))
                                    (- (dy c) h)))
                   ((y) (make-point (- (dx c) h)
                                    (- (dy c) (/ w 2))))))
         (bbox (case orient
                 ((x) (make-rect (x origin) (y origin) w h))
                 ((y) (make-rect (- (x origin) h) (y origin) w h)))))
    ;;
    (values bbox
            (lambda (dev)
              (with-gstate-saved
               dev
               (lambda ()
                 (translate dev origin)
                 (if (eq? orient 'y)
                     (rotate dev 90))
                 (do-rendering axis-label
                               (label-font self)
                               dev
                               $zero-point)))))))
    
(define (axis-orientation (self <graphic-axis>))
  (if (> (abs (dx (size self))) (abs (dy (size self))))
      'x
      'y))

    
    

(define-class <full-2d-graph> (<object>)
  (origin type: <point>)
  (x-axis type: <graphic-axis>)
  (y-axis type: <graphic-axis>))
  
(define-method frame ((self <full-2d-graph>))
  (make-rect (x (origin self))
             (y (origin self))
             (dx (size (x-axis self)))
             (dy (size (y-axis self)))))

(define-method graph-bbox ((self <full-2d-graph>))
  (let* ((f (frame self))
         (f0 (make-rect 0 0 (size-width f) (size-height f)))
         (xf (offset-rect (axis-bbox (x-axis self) f0)
                          (origin-x f)
                          (origin-y f)))
         (yf (offset-rect (axis-bbox (y-axis self) f0)
                          (origin-x f)
                          (origin-y f))))
    ;;(format #t "x-axis bbox: ~s\n" xf)
    ;;(format #t "y-axis bbox: ~s\n" yf)
    ;;
    (union-rect (union-rect f xf) yf)))

(define-method plot-graph ((self <full-2d-graph>) dev proc)
  #|
  (format #t "===X===\n")
  (print (x-axis self))
  (format #t "===Y===\n")
  (print (y-axis self))
  (format #t "=======\n")
  |#
  (with-gstate-saved
   dev
   (lambda ()
     (setlinewidth dev 0.25)
     (let* ((f (frame self))
            (f0 (make-rect 0 0 (size-width f) (size-height f))))
       (rectstroke dev f)
       (translate dev (origin self))
       (draw-axis (x-axis self) dev f0)
       (draw-axis (y-axis self) dev f0)
       (setlinewidth dev 1)
       ;;
       (rectclip dev f0)
       (proc dev
             (lambda (a b)
               ;; this only works for X/Y aligned and orthogonal axes...
               (make-point (x (point-on (x-axis self) a))
                           (y (point-on (y-axis self) b)))))))))
   

