(define *row-height* 15)
(define *col-width* 20)
(define *frame-inset* 1)
(define *header-gap* 4)
(define *frame-x-shift* 1)
(define *frame-y-shift* 3)

(define *cal-month-dx* (+ (* 7 *col-width*) 10))
(define *cal-month-dy* (+ (* 7 *row-height*) 25))


;;;

(define (apply-annotation dev frame major minor)
  ;;;
  ;;;  Configure the graphics state for the given annotation
  ;;;
  ;;;     major = 'circle or 'square
  ;;;     minor = colorspec
  ;;;
  (setlinewidth dev 0.5)
  (translate dev (make-point 0 -1.5))
  (case major
    ((circle)
     (moveto dev (make-point (limit-x frame) (center-y frame)))
     (arc dev
          (point-average (lower-left frame) (upper-right frame))
          (/ (min (size-width frame) (size-height frame)) 2)
          0
          360))
    ((square)
     (moveto dev (lower-left frame))
     (lineto dev (lower-right frame))
     (lineto dev (upper-right frame))
     (lineto dev (upper-left frame))
     (closepath dev)))
  ;;
  (setcolor dev minor))

(define (show-at/ralign dev font pt str)
  (moveto dev (make-point (- (x pt) (string-width font str)) (y pt)))
  (show dev str))

(define (show-at/center dev font pt str)
  (moveto dev (make-point (- (x pt) (/ (string-width font str) 2)) (y pt)))
  (show dev str))
  
  

(define-method render-eps ((self <month-graphic>) dev)
  (setfont dev *text-font*)
  (let ((num-weeks (vector-length (_week-matrix self))))
    ;;
    (define (show-row y items annots)
      (let loop ((i 0))
        (if (< i 7)
            (let ((label (vector-ref items i))
                  (annot (or (vector-ref annots i) '()))
                  (frame (inset-rect
                          (make-rect (+ (* *col-width* i)
                                        *col-width*
                                        (- (min *row-height* *col-width*))
                                        *frame-x-shift*)
                                     (- y *frame-y-shift*)
                                     (min *row-height* *col-width*)
                                     (min *row-height* *col-width*))
                          1 1)))
              ;(format #t "annot[~d] => ~s  (~s)\n" i annot label)
              ;; pre-annot
              (if (assq 'fill annot)
                  (with-gstate-saved
                   dev
                   (lambda ()
                     (let ((a (assq 'fill annot)))
                       (apply-annotation dev frame (cadr a) (caddr a)))
                     (fill dev))))
              ;; label
              (if label
                  (show-at/center dev
                                  *text-font*
                                  (make-point (center-x frame) y)
                                  (to-string label)))
              ;; post-annot
              (if (assq 'stroke annot)
                  (with-gstate-saved
                   dev
                   (lambda ()
                     (let ((a (assq 'stroke annot)))
                       (apply-annotation dev frame (cadr a) (caddr a)))
                     (stroke dev))))
              (loop (+ i 1))))))
    ;;
    (with-gstate-saved
     dev
     (lambda ()
       ;(translate dev (make-point 0 (* num-weeks *row-height*)))
       (with-gstate-saved
        dev
        (lambda ()
          (setcolor dev '(gray 0.5))
          (let ((y (/ *header-gap* -1)))
            (moveto dev (make-point 4 y))
            (lineto dev (make-point (+ 4 (* *col-width* 7)) y)))
          (stroke dev)))
       (show-row 0 '#("S" "M" "T" "W" "T" "F" "S")
                 (make-vector 7 '()))
       (let loop ((i 0)
                  (y (- (+ *header-gap* *row-height*))))
         (if (< i num-weeks)
             (let ((row (vector-ref (_week-matrix self) i)))
               (show-row y 
                         row
                         (vector-map (lambda (d)
                                       (if d
                                           (vector-ref (_annotations self) 
                                                       (- d 1))
                                           '()))
                                     row))
               (loop (+ i 1)
                     (- y *row-height*)))))))))

;;;  Render a particular month (named `month-name' and with body
;;;  graphics `month-graphic') onto the given device (`dev') at
;;;  coordinates [0,0] in user space

(define (render-1-month dev month-name month-graphic)
  (with-gstate-saved
   dev
   (lambda ()
     (setfont dev *month-font*)
     (setcolor dev '(gray 0.5))
     (rectfill dev (make-rect 4 13 (- *cal-month-dx* 10) 15))
     (setcolor dev '(gray 1))
     (show-at/center dev
                     *month-font*
                     (make-point (/ *cal-month-dx* 2) 16.5)
                     month-name)))
  (render-eps month-graphic dev))

;;;  Render an entire year in the form of 3 months wide
;;;  by 4 rows high (each row is a calendar quarter)

(define (render-year dev year months)
  (setfont dev *text-font*)
  (for-each
   (lambda (y)
     (for-each
      (lambda (x)
        (with-gstate-saved
         dev
         (lambda ()
           (let ((m (list-ref months (+ x (* y 3)))))
             (translate dev (make-point (* x *cal-month-dx*)
                                        (* (- 3 y) *cal-month-dy*)))
             (render-1-month dev (car m) (cdr m))))))
      ;; three columns
      '(0 1 2)))
   ;; four rows
   '(0 1 2 3)))

