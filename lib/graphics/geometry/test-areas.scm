,(use syscalls)

(define $dv-version "areatests (1.0)")

,(use tables
      graphics.geometry
      rs.util.properties
      rs.util.msgs
      graphics.afm
      graphics.color
      graphics.device
      graphics.fontmgr
      rs.sys.multimethod)

;(load "~/p/sf-from-tsunami/library/dev/gui/app/dv/export.scm")

(define (t1)
  (list 1
        (cons 'A (rect-area (make-rect 1 1 5 3)))
        (cons 'B (rect-area (make-rect 4 2 3 3)))))

(define (t2)
  (list 2
        (cons 'A (simple-polygon-area (make-point 2 0)
                                      (make-point 6 0)
                                      (make-point 6 6)
                                      (make-point 2 6)
                                      (make-point 2 4)
                                      (make-point 4 4)
                                      (make-point 4 2)
                                      (make-point 2 2)))
        (cons 'B (rect-area (make-rect 1 1 2 4)))))

(define (t3)
  (list 3
        (cons 'A (path-subtract
                  (rect-area (make-rect 1 1 5 4))
                  (rect-area (make-rect 2 2 3 2))))
        (cons 'B (rect-area (make-rect 3 0 1 3)))))

(define (t4)
  (list 4
        (cons 'A (path-subtract
                  (rect-area (make-rect 1 1 5 4))
                  (rect-area (make-rect 2 3 3 1))))
        (cons 'B (rect-area (make-rect 3 0 1 2)))))

(define (t5)
  (list 5
        (cons 'A (rect-area (make-rect 1 1 2 3)))
        (cons 'B (rect-area (make-rect 3 0 2 2)))))

(define (ra x y w h)
  (rect-area (make-rect x y w h)))

(define (t6)
  (list 6
        (cons 'A (path-subtract
                  (ra 1 1 11 6)
                  (ra 2 2 3 3)))
        (cons 'B (path-add
                  (ra 3 3 1 1)
                  (path-subtract
                   (ra 7 2 4 3)
                   (ra 9 3 1 1))))))
              
(define (t7)
  (list 7 
        (cons 'A (area-union (ra 2 0 5 1) (ra 0 1 3 1)))
        (cons 'B (ra 3 1 1 1))))

(define (t8)
  (let* ((a (ra 2 0 1 1))
         (a (area-union a (ra 3 0 1 1)))
         (a (area-union a (ra 4 0 1 1)))
         (a (area-union a (ra 5 0 1 1)))
         (a (area-union a (ra 6 0 1 1)))
         (a (area-union a (ra 0 1 1 1)))
         (a (area-union a (ra 1 1 1 1)))
         (a (area-union a (ra 2 1 1 1))))
  (list 8 (cons 'A a) (cons 'B (ra 3 1 1 1)))))

(define (poly . args)
  (let loop ((p '())
             (a args))
    (if (null? a)
        (apply simple-polygon-area (reverse! p))
        (loop (cons (make-point (car a) (cadr a)) p) (cddr a)))))
    
(define (atest j a b)
  (list j (cons 'A a) (cons 'B b)))
        
(define (t9)
  (atest 9 (poly 1 1  6 1  6 4  4 4  4 3  3 3  3 4  1 4) (ra 2 4 3 2)))

(define (t10)
  (atest 10 (poly 1 1  6 1  6 4  4 4  4 3  3 3  3 4  1 4) (ra 2 2 3 2)))

(define (t11)
  (atest 11 (ra 1 1 5 2) (ra 4 1 2 2)))

(define (t12)
  (atest 12 (ra 4 1 2 2) (ra 1 1 5 2)))
  
#|
(define (ta2 #optional fn)
  (bind ((elist vx (areas->edge-list ((or fn t1))))
         (all-edges (unify-edges (car elist) (cadr elist) vx)))
    (for-each (lambda ((v <vertex>))
                (fix-up-participants! v))
              (vertices vx))
    (crunch-contours all-edges)))
    

(define w (open-preview-window))

(define (splat f)
  (let ((l (ta2 f)))
    (clear w)
    (show-results w l)
    l))

;;;
|#

(define (annotate dev at type #optional extra setback)
  (with-gstate-saved
   dev
   (lambda ()
     (translate dev at)
     (scale dev (/ 1 7) (/ 1 7))
     (setlinewidth dev 0.5)
     ;;
     (case type
       ((x)
        (moveto dev (make-point 1 0))
        (arc dev (make-point 0 0) 1 0 360)
        (closepath dev)
        (moveto dev (make-point -1 -1))
        (lineto dev (make-point 1 1))
        (moveto dev (make-point 1 -1))
        (lineto dev (make-point -1 1)))
       ((square)
        (moveto dev (make-point -1 -1))
        (lineto dev (make-point 1 -1))
        (lineto dev (make-point 1 1))
        (lineto dev (make-point -1 1))
        (closepath dev))
       ((circle)
        (moveto dev (make-point 1 0))
        (arc dev (make-point 0 0) 1 0 360)
        (closepath dev))
       ((arrow)
        (let* (((d <size>) (normalize extra))
               ((n <size>) (make-size (- (dy d)) (dx d))))
          (if setback
              (translate dev (size->point (size* d (- setback)))))
          (moveto dev (make-point 0 0))
          (lineto dev (point+ (size->point (size* d -3.3)) (size* n 1.25)))
          (lineto dev (point+ (size->point (size* d -3.3)) (size* n -1.25)))
          (closepath dev)))
       (else
        (error "Unknown annotation")))
     ;;
     (if (eq? type 'arrow)
         (fill dev)
         (begin
           (with-gstate-saved
            dev
            (lambda ()
              (setcolor dev (device-color dev 'white))
              (fill dev)))
           (stroke dev))))))

(define-method show-contour-orientations ((self <path-bounded-area>) dev)
  (for-each
   (lambda ((sp <area-subpath>))
     (let* ((pp (path-points sp))
            (p0 (position (car pp)))
            (p1 (position (cadr pp))))
       ;;
       (format #t "    p0=~s p1=~s  avg=~s\n" p0 p1 (point-average p0 p1))
       (annotate dev (point-average p0 p1)
                 'arrow
                 (point- p1 p0)
                 -1.7)))
   (subpaths self)))
  
(define-method render-contour ((self <path-bounded-area>) dev)
  (let ((x '()))
    (for-each
     (lambda ((sb <area-subpath>))
       (let ((pp (path-points sb)))
         (set! x (cons (position (car pp)) x))
         (moveto dev (position (car pp)))
         (let loop ((l (cdr pp)))
           (if (null? l)
               (closepath dev)
               (begin
                 (set! x (cons (position (car l)) x))
                 (lineto dev (position (car l)))
                 (loop (cdr l)))))))
     (subpaths self))
    x))

(define *spacing* (make-size 94 72))

(define (grid-lines dev)
  (setlinewidth dev 0.05)
  (setcolor dev (device-color dev '(cmyk 1 0 0 0)))
  ;;
  (moveto dev (make-point -0.5 0))
  (lineto dev (make-point 12 0))
  (moveto dev (make-point 0 -0.5))
  (lineto dev (make-point 0 7))
  (stroke dev))

(define (render-test-case-cell dev k contour bg)
  (format #t "================= test case cell ~s ============\n" k)
  (with-gstate-saved
   dev
   (lambda ()
     (translate dev (make-point (* k (width *spacing*)) 0))
     (setlinewidth dev 1)
     (rectstroke dev (make-rect 0 0
                                (width *spacing*)
                                (height *spacing*)))
     (translate dev (make-point 9 9))
     (scale dev 6 6)
     (grid-lines dev)
     ;;
     (for-each (lambda (c color)
                 (setcolor dev (device-color dev color))
                 (render-contour c dev)
                 (stroke dev))
               bg
               '((cmyk 1 1 0 0)
                 (cmyk 0 1 1 0)
                 (cmyk 0 1 0 0)
                 (cmyk 0 0 0.5 0.5)
                 (cmyk 1 0 1 0)))
     ;;
     (setcolor dev (device-color dev 'black))
     (let ((l (render-contour contour dev)))
       (with-gstate-saved
        dev
        (lambda ()
          (setcolor dev (device-color dev '(gray 0.667)))
          (fill dev)))
       (setlinewidth dev 0.1)
       (stroke dev)
       ;;
       (setcolor dev (device-color dev 'black))
       (show-contour-orientations contour dev)
       (for-each (lambda (p) (annotate dev p 'circle)) l)))))
     

(define (render-test-case dev a b name)
  (format #t "*********** TEST CASE ~s *************\n" name)
  (render-test-case-cell dev 0 a '())
  (render-test-case-cell dev 1 b '())
  (render-test-case-cell dev 2 (area-union a b) (list a b))
  (render-test-case-cell dev 3 (area-intersection a b) (list a b))
  (render-test-case-cell dev 4 (area-subtract a b) (list a b))
  (render-test-case-cell dev 5 (area-xor a b) (list a b)))
  

;;;

(define (area-test-page dev suite-title page-num tests)
  (let ((f (get-text-font "Times" "Roman" 12))
        (fi (get-text-font "Times" "Italic" 10)))
    ;;
    (startpage dev page-num)
    (with-gstate-saved
     dev
     (lambda ()
       (translate dev (make-point 36 18))
       ;;
       (setfont dev fi)
       (moveto dev (make-point 0 (+ 14 (* 10 (height *spacing*)))))
       (show dev suite-title)
       ;;
       (setfont dev f)
       ;;
       (with-gstate-saved
        dev
        (lambda ()
          (let ((w (width *spacing*)))
            (translate dev (make-point 0 (+ 2 (* 10 (height *spacing*)))))
            ;;
            (moveto dev (make-point 0 0))
            (show dev "A")
            (moveto dev (make-point w 0))
            (show dev "B")
            (moveto dev (make-point (* 2 w) 0))
            (show dev "(union A B)")
            (moveto dev (make-point (* 3 w) 0))
            (show dev "(intersection A B)")
            (moveto dev (make-point (* 4 w) 0))
            (show dev "(minus A B)")
            (moveto dev (make-point (* 5 w) 0))
            (show dev "(xor A B)"))))
                                        ;
       ;;
       (for-each
        (lambda (k test-case-thunk)
          (let* ((test-case (test-case-thunk))
                 (name (format #f "T~d" (car test-case)))
                 (h (height *spacing*)))
            (with-gstate-saved
             dev
             (lambda ()
               (translate dev (make-point 0 (* (- 10 (+ 1 k)) h)))
               (moveto dev (make-point (- -4 (string-width f name)) (/ h 2)))
               (show dev name)
               (render-test-case dev
                                 (cdr (cadr test-case))
                                 (cdr (caddr test-case))
                                 name)))))
        (range (length tests))
        tests)))
    (endpage dev)))
  
(define (go)
  (let ((dev (open-ps-device "/tmp/areas.ps"))
        (title (format #f "[graphics/geometry/test-areas.scm] ~a"
                       (time->string
                        (time)
                        "%Y-%m-%d %H:%M:%S %Z"))))
    ;;
    (area-test-page dev title "1" (list t1 t2 t3 t4 t5 t6 t7 t8 t9 t10))
    (area-test-page dev title "2" (list t11 t12))
    (close-graphics-device dev)))

;(define A (ra 3 0 1 1))
;(define B (ra 4 0 1 1))
