
(define-constant $default-size (make-size 0 100))
(define-constant $default-outside-unit (make-size -1 0))

(define-constant $default-tic-label-font (get-text-font "Helvetica"
                                                        "Regular"
                                                        8))


(define-class <graphic-axis> (<object>)
  ;;
  ;; A custom way to compute the label for a given user coordinate
  ;;  If set, should be a procedure of one argument and some keywords
  ;;  which returns a span body, e.g.,
  ;;   (set-compute-label-hook! x-axis (lambda (self x #key scale scaled suffix)
  ;;                                     `("[" ,(to-string x) "]"))
  ;;
  (compute-label-hook init-value: #f)
  ;; The axis origin is relative to the graph origin, 
  ;; because we translate to the graph origin before drawing
  ;; anything
  (origin type: <point> init-value: $zero-point)
  (size type: <size> init-value: $default-size)
  (outside-unit type: <size> init-value: $default-outside-unit)
  ;;
  (tic-marks init-value: '())
  ;;
  ;;  The label for the entire axis
  ;;
  (label init-value: #f)
  (label-font init-value: $default-tic-label-font)
  (label-decimal-digits init-value: 0)
  ;;
  ;;  The abbreviated name of the value unit, used on value labels
  ;;
  (value-units init-value: #f) ; e.g., "A" for Amps, or "B" for Bytes
  ;;
  ;; The value units prefix mode determines how prefixes
  ;; are added to the value labels.
  ;;
  ;;    none - no prefixes are added
  ;;    decimal - latix prefixes (p, n, u, m, K, M, G, T) 
  ;;              may be added, where K=1000
  ;;    binary  - latix prefixes (p, n, u, m, K, M, G, T)
  ;;              may be added, where K=1024
  ;;
  (value-units-prefix-mode init-value: 'none)   ; none, decimal, binary
  ;;
  ;; These define the limits of the axis in subject value space
  ;;
  (lower-value-limit type: <real>)
  (upper-value-limit type: <real>)
  ;;
  ;; The transfer function is used to implement non-linear graph
  ;; scale.  The transfer-function takes a subject value and transforms
  ;; it into graphic (linear-on-paper) coordinate value.  The 
  ;; inverse-transfer-function goes the other way...
  ;;
  ;;  ((transfer-function GRAPHIC-AXIS) SUBJECT-VALUE) => LINEAR-COORD
  ;;  ((inverse-transfer-function GRAPHIC-AXIS) LINEAR-COORD) => SUBJECT-VALUE
  ;;
  ;;  for example, to implement a log scale, set
  ;;  transfer-function to (lambda (x) (/ (log x) (log 10)))
  ;;  and inverse-transfer-function to (lambda (x) (expt 10 x))
  ;;
  (transfer-function init-value: identity)
  (inverse-transfer-function init-value: identity)
  ;;
  (cached-axis-common-unit-scale init-value: '#uninit))

(define (add-tic-mark! (self <graphic-axis>) mark)
  (set-tic-marks! self (append (tic-marks self) (list mark))))

;;;


;;; Observation...
;;;
;;;   Sometimes you want to fix the unit suffix for the entire
;;;   axis, e.g., if we are looking at tic labels at 800mA, 900mA, 1000mA,
;;;   and 1100mA, you don't want to write it as 800mA, 900mA, 1A, 1.1A.
;;;
;;;   Other times you want to allow it to vary on a per label basis, e.g.,
;;;   on a log scale you want to be able to show labels as
;;;   1HZ, 10HZ, 100HZ, 1KHZ, 10KHZ, 100KHZ, 1MHZ.
;;;
;;;   The heuristic we use is that if the label we want to use at the
;;;   end points differ by more than one scale factor, then we let it
;;;   float, otherwise we fix it at the smaller of the two scales.
;;;
;;;   If we are using such a fixed scale factor, then it is 
;;;   saved in `cached-axis-common-unit-scale'.  This applies
;;;   to scientific notation exponents as well as latix prefixes,

(define (axis-common-unit-scale (self <graphic-axis>))
  (if (eq? (cached-axis-common-unit-scale self) '#uninit)
      (let ((s0 (pick-scale self (lower-value-limit self)))
            (s1 (pick-scale self (upper-value-limit self))))
        (if (or (not s0) (not s1))
            (set-cached-axis-common-unit-scale! self (or s0 s1))
            (if (<= (abs (- s0 s1)) 1)
                (set-cached-axis-common-unit-scale! self (min s0 s1))
                (set-cached-axis-common-unit-scale! self #f)))))
  (cached-axis-common-unit-scale self))
  
(define-method point-on ((self <graphic-axis>) x)
  (let* ((fn (transfer-function self))
         (b (fn (lower-value-limit self)))
         (a (/ (- (fn x) b)
               (- (fn (upper-value-limit self)) b))))
    (point+ (origin self) (size* (size self) a))))

(define (compute-label-at (self <graphic-axis>) x)
  (let* ((s (or (axis-common-unit-scale self) 
                (pick-scale self x)
                0))
         (u (value-units self))
         (suffix (if (axis-common-unit-scale self)
                     '()        ; include units in the title, not on each label
                     (scale->text (value-units-prefix-mode self) s u)))
         (scaled (if (= s 0)
                     x
                     (/ x (case (value-units-prefix-mode self)
                            ((none) (expt 10 s))
                            ((decimal) (expt 1000.0 s))
                            ((binary) (expt 1024.0 s)))))))
    ;;
    (if (compute-label-hook self)
        ((compute-label-hook self) self x scale: s scaled: scaled suffix: suffix)
        `(,(number->fixed-point-string scaled (label-decimal-digits self))
          ,@suffix))))

(define (scale->text mode s u)
  (if (= s 0) 
      (if u
          (list u)
          '())
      (case mode
        ((none)
         (if u
             `(" " (symbol "\264") " 10" (sup ,(number->string s))
               " " ,u)
             `(" " (symbol "\264") " 10" (sup ,(number->string s)))))
        ((decimal binary)
         (let ((a (string-ref "pnum KMGT"
                              (+ (min (max s -4) 4) 4))))
           `(,(string a) ,u))))))

(define (number->fixed-point-string x nd)
  (case nd
    ((0) (number->string (inexact->exact (round x))))
    ((1) (~ "~.1f" (/ (round (* x 10)) 10)))
    ((2) (~ "~.2f" (/ (round (* x 100)) 100)))
    ((3) (~ "~.3f" (/ (round (* x 1000)) 1000)))
    (else (format #f (~ "~~.~df" nd) x))))

(define (pick-scale (self <graphic-axis>) x)
  (let ((x (abs x)))
    (if (= x 0)
        #f
        (case (value-units-prefix-mode self)
          ((none)
           (inexact->exact (floor (/ (log x) (log 10)))))
          ((decimal)
           (inexact->exact (floor (/ (log x) (log 1000)))))
          ((binary)
           (inexact->exact (floor (/ (log x) (log 1024)))))))))
    
;;;

(define (compute-title (self <graphic-axis>))
  (let ((s (axis-common-unit-scale self)))
    (if (and s
             (value-units self)
             (not (string=? (value-units self) "")))
        `(,(label self)
          " (" 
          ,@(scale->text (value-units-prefix-mode self)
                         s
                         (value-units self))
          ")")
        (list (label self)))))

;;;
