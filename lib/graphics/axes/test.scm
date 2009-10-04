
                        
(define (test1)
  (log-scale-axis from: 3 
                  to: 9342521
                  label: "File Size"
                  value-units: "B"
                  orientation: 'y
                  size: 300))


#|
(define (test1)
  (let ((a (make <graphic-axis>
                 label: "File Size"
                 size: (make-size 0 300)
                 label-font: (get-text-font "Times" "Italic" 10)
                 value-units: "B"
                 value-units-prefix-mode: 'binary
                 lower-value-limit: 1
                 upper-value-limit: 1e7
                 transfer-function: (lambda (x) (/ (log x) (log 10)))
                 inverse-transfer-function: (lambda (l) (expt 10 l)))))
    ;;
    (add-tic-mark! a
                   (make <graphic-tic-set>
                         container: a
                         count: 8
                         generator: (lambda (i)
                                      (* (case (modulo i 3)
                                           ((0) 1)
                                           ((1) 10)
                                           ((2) 100))
                                         (expt 1024.0 (quotient i 3))))
                         style: (make <graphic-tic-label>
                                      font: (get-text-font "Times"
                                                           "Italic"
                                                           8))))
    ;;
    (add-tic-mark! a
                   (make <graphic-tic-set>
                         container: a
                         count: 71
                         generator: (lambda (i)
                                      (let ((i (+ i 10)))
                                        (* (/ (modulo i 10) 10)
                                           (case (modulo (quotient i 10) 3)
                                             ((0) 1)
                                             ((1) 10)
                                             ((2) 100))
                                           (expt 1024.0 (quotient i 30)))))
                         style: (make <graphic-tic-mark>
                                      inside-length: 20
                                      outside-length: 0)))


    ;;
    a))
|#

(define (test2)
  (let ((a (make <graphic-axis>
                 label: "Supply Current"
                 size: (make-size 0 300)
                 label-font: (get-text-font "Times" "Italic" 10)
                 value-units: "A"
                 value-units-prefix-mode: 'decimal
                 lower-value-limit: 0
                 upper-value-limit: 1.5
                 label-decimal-digits: 3)))
    (add-tic-mark! a
                   (make <graphic-tic-set>
                         container: a
                         count: 16
                         generator: (lambda (i)
                                      (* i 0.1))
                         style: (make <graphic-tic-label>
                                      font: (get-text-font "Helvetica"
                                                           "Regular"
                                                           8))))
    (add-tic-mark! a
                   (make <graphic-tic-set>
                         container: a
                         count: 16
                         generator: (lambda (i)
                                      (* i 0.1))
                         style: (make <graphic-tic-mark>)))

    a))

(define (test3)
  (let ((a (make <graphic-axis>
                 label: "Time"
                 size: (make-size 500 0)
                 outside-unit: (make-size 0 -1)
                 label-font: (get-text-font "Times" "Italic" 10)
                 value-units: "s"
                 value-units-prefix-mode: 'decimal
                 lower-value-limit: 1
                 upper-value-limit: 3
                 label-decimal-digits: 1)))
    ;;
    (define (make-tics from to per style)
      (make <graphic-tic-set>
            container: a
            count: (+ (/ (- to from) per) 1)
            generator: (lambda (i)
                         (+ from (* i per)))
            style: style))
    ;;
    (define (minor style)
      (add-tic-mark! a (make-tics 1 3 1/50 style)))
    (define (major style)
      (add-tic-mark! a (make-tics 1 3 1/10 style)))
    ;;
    (major (make <graphic-tic-label>
                 anchor: '(center top)
                 font: (get-text-font "Helvetica"
                                      "Regular"
                                      8)))
    (major (make <graphic-tic-mark>
                 inside-length: 4
                 outside-length: 0))
    (minor (make <graphic-tic-mark>
                 inside-length: 2
                 outside-length: 0))
    a))

(define (test-axes)
  (let ((p (open-ps-device "/tmp/a.ps")))
    (startpage p)
    ;;
    (let ((g (make <full-2d-graph>
                   origin: (make-point 36 (+ 400 36))
                   x-axis: (test3)
                   y-axis: (test2))))
      (with-gstate-saved
       p
       (lambda ()
         (setcolor p (device-color p '(rgb 0.8 0.8 1)))
         (rectfill p (graph-bbox g))))
      (plot-graph g
                  p
                  (lambda (dev P)
                    (moveto dev (P 1 0.1))
                    (for-each (lambda (j)
                                (lineto dev (P (+ (/ j 10) 1.1)
                                               (+ 0.8 (* 0.4 (sin (/ j 3.1)))))))
                              (range 18))
                    (stroke dev))))
    ;;
    (plot-graph (make <full-2d-graph>
                      origin: (make-point 36 36)
                      x-axis: (test3)
                      y-axis: (test1))
                p
                (lambda (dev P)
                  (moveto dev (P 1 5))
                  (for-each (lambda (j)
                              (lineto dev (P (+ (/ j 10) 1.1)
                                             (+ 10100
                                                (* 10000 (sin (/ j 3.1)))))))
                            (range 18))
                  (stroke dev)))
    ;;
    #|
    (translate p (make-point 36 36))
    (with-gstate-saved
     p
     (lambda ()
       (setlinewidth p 0.25)
       (rectstroke p (make-rect 0 0 (if x (dx (size x)) 200) (dy (size y))))
       (if x (draw-axis x p))
       (draw-axis y p)))
    ;;
    |#
    (endpage p)
    (close-graphics-device p)))
