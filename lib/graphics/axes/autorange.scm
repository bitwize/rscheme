
;;;
;;;  Figure out the display range and major/minor tic points
;;;  for a given range of values
;;;
;;;  Returns
;;;     lower-value-limit
;;;     upper-value-limit
;;;     num-major
;;;     minor-per-major

(define (linear-auto-range from to)
  (if (= from to)
      (if (> from 0)
          (linear-auto-range 0 to)
          (if (< from 0)
              (linear-auto-range from 0)
              (linear-auto-range 0 1)))
      (bind ((range (abs (- to from)))
             (scale denom (estimate-step range))
             (step (/ (expt 10 scale) denom)))
        ;; if the origin (zero) is within 10 steps, suck it up
        ;; and recalculate our limits
        (cond
         ((and (> from 0) (<= (- from (* 10 step)) 0))
          (linear-auto-range 0 to))
         ((and (< to 0) (>= (+ to (* 10 step)) 0))
          (linear-auto-range from 0))
         (else
          (let ((ilow (inexact->exact (floor (/ from step))))
                (iupp (inexact->exact (ceiling (/ to step)))))
          (values (* step ilow)
                  (* step iupp)
                  (+ (- iupp ilow) 1)
                  (if (> (- iupp ilow) 6) 5 10)
                  ;;
                  (list step scale denom))))))))

(define (estimate-step range)
  (let* ((scale (inexact->exact (floor (log10 range))))
         (step (expt 10.0 scale))
         (count (/ range (expt 10.0 scale))))
    (cond
     ((< count 1.6) (values scale 5))           ; 0.0  0.2  0.4  0.6  0.8  1.0
     ((< count 3)   (values scale 2))           ; 0  5  10
     ((> count 8)   (values (+ scale 1) 5))     ; 0  2  4  6  8  10
     (else          (values scale 1)))))        ; 0 1 2 3 4 5 6 7 8 9 10

(define (log10 x)
  (/ (log x) (log 10)))

(define (log-auto-range from to)
  (cond
   ((<= to 0)
    (log-auto-range from 1))
   ((<= from 0)
    (log-auto-range 1 to))
   ((< to from)
    (log-auto-range to from))
   (else
    (let* ((log-from (inexact->exact (floor (log10 from))))
           (log-to (inexact->exact (ceiling (log10 to))))
           (step (ceiling (/ (+ 1 (- log-to log-from))
                             $log-axis-max-major-tics))))
      (set! step 1)     ; if step winds up being, e.g., 2, things don't work
                        ; e.g., (log-auto-range 1 14e6) => 5 major tics
                        ; which doesn't draw the axis right (I think we need
                        ; to separate out the notion of major tics from
                        ; label tics, and then we could two step=2 label
                        ; tics and keep the major tics at step=1
      (values (expt 10 log-from)
              (expt 10 log-to)
              (+ (/ (- log-to log-from) step) 1)
              9
              (list log-from log-to step))))))

(define $log-axis-max-major-tics 8)


#|

        --- I don't think this really works right yet... ---
;;;
        
(define (log2 x)
  (/ (log x) (log 2)))

;;; the trick will be making this "snap" to major tics on
;;; 10-binary-powers, e.g., 1K, 1M, 1G

(define (binary-log-auto-range from to)
  (cond
   ((<= to 0)
    (log-auto-range from 1))
   ((<= from 0)
    (log-auto-range 1 to))
   ((< to from)
    (log-auto-range to from))
   (else
    (let* ((log-from (inexact->exact (floor (log2 from))))
           (log-to (inexact->exact (ceiling (log2 to))))
           (step (ceiling (/ (+ 1 (- log-to log-from))
                             $log-axis-max-major-tics))))
      (values (expt 2 log-from)
              (expt 2 log-to)
              (+ (/ (- log-to log-from) step) 1)
              8
              (list log-from log-to step))))))

(define (try-binary-log-step from-power to-power step)
  (values (inexact->exact (floor (/ from-power step)))
          (inexact->exact (ceiling (/ to-power step)))))
|#

