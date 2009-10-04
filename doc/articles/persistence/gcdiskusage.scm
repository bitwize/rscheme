,(use graphics.axes)

(define *data:phase-changes* #f)
(define *data:file-sizes* #f)

(define (subset-data x)
  ;(let ((t (car x))) (and (>= t 1116656139.208574) (<= t 1116675491.085739)))
  #t)

(call-with-input-file
    "gcdiskusage.dat"
  (lambda (port)
    ;;
    (define (to-hours lst)
      (map (lambda (e)
             (cons (/ (car e) 3600) (cdr e)))
           lst))
    ;;
    (set! *data:phase-changes* (to-hours (select subset-data (read port))))
    (set! *data:file-sizes* (to-hours (select subset-data (read port))))))

;;

(define *phase-label-font* (get-text-font "Helvetica" "Oblique" 6))

(define (draw)
  ;;
  (translate (make-point (* 72 6) 72))
  (rotate 90)
  (translate (make-point -100 36))
  ;;
  (let ((minx (min (car (car *data:file-sizes*))
                   (car (car *data:phase-changes*))))
        (maxx (max (car (last *data:file-sizes*))
                   (car (last *data:phase-changes*))))
        (maxy (reduce1 max (map (lambda (fs)
                                  (+ (cadr fs) (caddr fs)))
                                *data:file-sizes*))))
    ;;
    (let* ((f (make-rect 100 100 350 150))
           (g (make <full-2d-graph>
                   origin: (origin f)
                   x-axis: (linear-scale-axis from: 0
                                              to: (- maxx minx)
                                              label: "Time"
                                              orientation: 'x
                                              value-units: "hours"
                                              size: (size-width f))
                   y-axis: (linear-scale-axis from: 0
                                              to: maxy
                                              label: "File Size"
                                              value-units: "B"
                                              orientation: 'y
                                              size: (size-height f))))
          (outside '()))
      ;;
      (plot-graph
       g
       (current-graphics-device)
       (lambda (dev @)
         (gsaved
          ;;
          (for-each
           (lambda (p)
             (set! outside (cons (list (x (@ (- (car p) minx) 0)) 
                                       (caddr p))
                                 outside)))
           *data:phase-changes*))
         ;;
         (gsaved
          (setdash '#(1 1) 0)
          (setcolor (device-color '(rgb 1 0 0)))
          (stroke-data *data:file-sizes* (lambda (p) (@ (- (car p) minx)
                                                        (cadr p))))
          (setcolor (device-color '(rgb 0 0 1)))
          (stroke-data *data:file-sizes* (lambda (p) (@ (- (car p) minx)
                                                        (caddr p)))))
         (setlinejoin 'round)
         (setlinecap 'round)
         (stroke-data *data:file-sizes* (lambda (p) (@ (- (car p) minx)
                                                       (+ (cadr p)
                                                          (caddr p)))))))
      ;;
      (setlinewidth 1)
      (setlinecap 'round)
      (setcolor (device-color '(rgb 0 0.333 0)))
      (let ((x0 0)
            (y0 0)
            (labeled-yet (vector #f #f #f #f #f #f #f))
            (phase-label '#("idle" "packing" "prep" "pending" "tscan" "pscan" "reclaim")))
        ;;
        (setfont *phase-label-font*)
        (for-each
         (lambda (t)
           (let* ((yi (vmemq (cadr t) '#(IDLE PACKING PREP PENDING TSCAN PSCAN RECLAIM)))
                  (y (* yi 5)))
             ;;
             (if (not (vector-ref labeled-yet yi))
                 (begin
                   (vector-set! labeled-yet yi #t)
                   (gsaved
                    (setcolor (device-color 'black))
                    (rshow (- (+ (car t) (origin-x f)) 2) (- (+ y (limit-y f) 5) 1.5)
                           (vector-ref phase-label yi)))))
             ;;
             (moveto (+ x0 (origin-x f)) (+ y0 (limit-y f) 5))
             (lineto (+ (car t) (origin-x f)) (+ y0 (limit-y f) 5))
             (set! x0 (car t))
             (set! y0 y)
             (stroke)))
         (reverse outside))
        ;;
        (moveto (+ x0 (origin-x f)) (+ y0 (limit-y f) 5))
        (lineto (limit-x f) (+ y0 (limit-y f) 5))
        (stroke)
        ))))

(define (stroke-data lst xform)
  (let ((n 0))
    (for-each
     (lambda (item)
       (if (eq? n 0)
           (moveto (xform item))
           (lineto (xform item)))
       (set! n (+ n 1))
       (if (> n 1400)
           (begin
             (stroke)
             (set! n 0))))
     (cdr lst))
    (if (> n 0)
        (stroke))))

#|
Here's how to extract the gcdiskusage.dat data from
the log output produced by setting the RS_LPGC=<filename>
environment variable:

perl -ne 'print "($1 $2 $3)\n" if m/(\d+.\d*) 463-3201T phase ([A-Z]+) ([A-Z]+)\n/;' pgc.dat

perl -e '$s1=0; $s2=0; $t0=0; 
  print "(\n";
  while (<>) { 
  if (m/(\d+.\d*) 462-5400T stat \((\d+) (\d+)\)\n/) {
    $t = $1;
    $x = $2;
    $y = $3;
    if ((($t - $t0) > 60) || ($x < $s1) || ($y < $s2)) {
      print " ($1 $2 $3)\n";
      $t0 = $t;
      $s1 = $x;
      $s2 = $y;
    }
  } }
  print ")\n";' pgc.dat 

|#
