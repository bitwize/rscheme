,(use tables)
,(use rs.util.msgs)
,(use rs.util.properties)
,(use graphics.geometry)
,(use graphics.afm)

(load "../../gui/app/dv/font.scm")
(load "../../gui/app/dv/dev/device.scm")
(load "../../gui/app/dv/dev/virtual.scm")
(load "../../gui/app/dv/dev/bbox/driver.scm")
(load "../../gui/app/dv/dev/ps/driver.scm")

(define-class <month-graphic> (<object>)
  start-date
  (_week-matrix type: <vector>)
  (_annotations type: <vector>))

(define *first-weeks* '((sunday         #(1 2 3 4 5 6 7))
                        (monday         #(#f 1 2 3 4 5 6))
                        (tuesday        #(#f #f 1 2 3 4 5))
                        (wednesday      #(#f #f #f 1 2 3 4))
                        (thursday       #(#f #f #f #f 1 2 3))
                        (friday         #(#f #f #f #f #f 1 2))
                        (saturday       #(#f #f #f #f #f #f 1))))

(define (trim-trailing-empty x)
  (let ((n (vector-length x)))
    (if (equal? (vector-ref x (- n 1))
                '#(#f #f #f #f #f #f #f))
        (subvector x 0 (- n 1))
        x)))

(define (week-matrix start-date num-days)
  (bind ((matrix (vector (make-vector 7 #f)
                         (make-vector 7 #f)
                         (make-vector 7 #f)
                         (make-vector 7 #f)
                         (make-vector 7 #f)
                         (make-vector 7 #f)))
         (week0 day0 (date->week start-date)))
    ;;
    (let loop ((i 0))
      (if (< i num-days)
          (bind ((w d (date->week (date+ start-date i))))
            (vector-set!
             (vector-ref matrix (- w week0))
             d
             (+ i 1))
            (loop (+ i 1)))
          (trim-trailing-empty
           (trim-trailing-empty matrix))))))

(define (make-month year month)
  (let* ((start-date (ymd->date year month 1))
         (num-days (date- (ymd->date (if (= month 12)
                                         (+ year 1)
                                         year)
                                     (+ (modulo month 12) 1)
                                     1)
                          start-date)))
    (make <month-graphic>
          start-date: start-date
          _week-matrix: (week-matrix start-date num-days)
          _annotations: (make-vector num-days '()))))

;;;

(define-method set-annotation! ((self <month-graphic>) 
                                (day <fixnum>)
                                annotation)
  (vector-set! (_annotations self) 
               (- day 1) 
               (cons annotation
                     (vector-ref (_annotations self) (- day 1)))))

(load "month_eps.scm")                  

(define $dv-version "0.99")

(define *text-font* (get-text-font "Helvetica" "Regular" 9))
(define *month-font* (get-text-font "Helvetica" "Bold" 12))

(define *long-month-name*
  '#("January" 
     "February" 
     "March" 
     "April"
     "May" 
     "June" 
     "July" 
     "August"
     "September" 
     "October" 
     "November" 
     "December"))


(define (tt)
  (let ((months (map (lambda (mm)
                       (cons
                        (vector-ref *long-month-name* mm)
                        (make-month 2003 (+ mm 1))))
                     (range 12))))
    (let ((bb (with-bbox-device (lambda (dev)
                                  (render-year dev 2003 months)))))
      (format #t "bbox: ~s\n" bb)
      (let ((dev (open-eps-device "/tmp/year.eps" bb)))
        (render-year dev 2003 months)
        (close-ps-device dev)))))

#|
(define (t)
  (let ((m (make-month 2003 1)))
    (set-annotation! m 1 '(stroke circle (gray 0.5)))
    (set-annotation! m 2 '(fill square (gray 0.5)))
    (set-annotation! m 10 '(fill square (gray 0.667)))
    (set-annotation! m 10 '(stroke circle (gray 0)))
    (set-annotation! m 24 '(fill square (gray 0.667)))
    (let ((bb (with-bbox-device (lambda (dev)
                                  (render-eps m dev)))))
      (format #t "bbox: ~s\n" bb)
      (let ((d (open-eps-device "/tmp/jan.eps" bb)))
        (render-eps m d)
        (close-ps-device d)))))

(define (tpt-month y m paydays holidays)
  (let ((mg (make-month y m)))
    (for-each
     (lambda (pd)
       (if (= (car pd) m)
           (set-annotation! mg (cadr pd) '(fill square (gray 0.667)))))
     paydays)
    ;
    (for-each
     (lambda (hd)
       (if (= (car hd) m)
           (set-annotation! mg (cadr hd) '(stroke circle (gray 0.333)))))
     holidays)
    ;
    mg))
  
(define *paydays-2003*
  '((1 10)
    (1 24)
    (2 7)
    (2 21)
    (3 7)
    (3 21)
    (4 4)
    (4 18)
    (5 2)
    (5 16)
    (5 30)
    (6 13)
    (6 27)
    (7 11)
    (7 25)
    (8 8)
    (8 22)
    (9 5)
    (9 19)
    (10 3)
    (10 17)
    (10 31)
    (11 14)
    (11 26)
    (12 12)
    (12 26)))

(define *holidays-2003*
  '((1 1)
    (2 17)
    (7 4)
    (5 26)
    (9 1)
    (11 27)
    (11 28)
    (12 24)
    (12 25)))

(define (tt)
  (let ((months (map (lambda (mm)
                       (cons
                        (vector-ref *long-month-name* mm)
                        (tpt-month 2003 (+ mm 1) 
                                   *paydays-2003*
                                   *holidays-2003*)))
                     (range 12))))
    (let ((bb (with-bbox-device (lambda (dev)
                                  (render-year dev 2003 months)))))
      (format #t "bbox: ~s\n" bb)
      (let ((dev (open-eps-device "/tmp/year.eps" bb)))
        (render-year dev 2003 months)
        (close-ps-device dev)))))
      
|#
