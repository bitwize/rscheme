;;;
;;;  This file defines a test jig we can use for testing
;;;  various geometry features...
;;;
;;;  Usage:
;;;    rsf -q testjig.scm
;;;    top[0]=>(test-jig I S sine)
;;;    gv /tmp/test-geom.ps


(define $dv-version "geomtests (1.0)")

,(use graphics.geometry 
      graphics.color 
      rs.util.msgs 
      rs.util.properties 
      tables 
      graphics.afm

      graphics.device
      graphics.fontmgr)

;(load "~/rscheme/sf/library/dev/gui/app/dv/export.scm")


(define-method render-contour ((self <path-bounded-area>) dev)
  ;; this may be subsumed by graphics.device's generic areapath operator
  (for-each
   (lambda ((sb <area-subpath>))
     (let ((pp (path-points sb)))
       (moveto dev (position (car pp)))
       (let loop ((l (cdr pp)))
         (if (null? l)
             (closepath dev)
             (begin
               (lineto dev (position (car l)))
               (loop (cdr l)))))))
   (subpaths self)))

(define *spacing* (make-size 108 72))

(define (grid-lines dev)
  (setlinewidth dev 0.05)
  (setcolor dev (device-color dev '(cmyk 0.667 0 0 0)))
  ;;
  (moveto dev (make-point -0.5 0))
  (lineto dev (make-point 12 0))
  (moveto dev (make-point 0 -0.5))
  (lineto dev (make-point 0 7))
  (stroke dev))



  

(define-thread-var *current-device*)

(define (current-device) *current-device*)

;;;

(define (test-jig . thunks)
  (let ((dev (open-ps-device "/tmp/test-geom.ps"))
        (f (get-text-font "Times" "Roman" 12)))
    ;;
    ;;
    (for-each (lambda (p)
                (with-gstate-saved
                 dev
                 (lambda ()
                   (startpage dev)
                   (translate dev (make-point 36 18))
                   (setfont dev f)
                   (with-gstate-saved
                    dev
                    (lambda ()
                      (thread-let ((*current-device* dev))
                        (p))))
                   (endpage dev))))
              thunks)
    ;;
    (close-graphics-device dev)))

(define (show-ellipse self #optional color)
  (let ((dev (current-device)))
    (with-gstate-saved
     dev
     (lambda ()
       (if color (setcolor dev (device-color dev color)))
       ;;
       (let ((l (ellipse->bezier-segments self)))
         (setlinewidth dev 1)
         (moveto dev (start-point (car l)))
         (curveto dev
                  (first-handle (car l))
                  (second-handle (car l))
                  (end-point (car l)))
         (curveto dev
                  (first-handle (cadr l))
                  (second-handle (cadr l))
                  (end-point (cadr l)))
         (curveto dev
                  (first-handle (caddr l))
                  (second-handle (caddr l))
                  (end-point (caddr l)))
         (curveto dev
                  (first-handle (cadddr l))
                  (second-handle (cadddr l))
                  (end-point (cadddr l)))
         (closepath dev)
         (stroke dev))))))

(define (stroke-curve l #key (color default: #f) (closed? default: #t))
  (let ((dev (current-device)))
    (with-gstate-saved
     dev
     (lambda ()
       (if color (setcolor dev (device-color dev color)))
       ;; trace the curve itself
       (setlinewidth dev 1)
       (moveto dev (start-point (car l)))
       (for-each (lambda (s)
                   (curveto dev
                            (first-handle s)
                            (second-handle s) 
                            (end-point s)))
                 l)
       (if closed?
           (closepath dev))
       (stroke dev)))))
  
(define (show-curve-vector self #optional color)
  (let ((dev (current-device)))
    (with-gstate-saved
     dev
     (lambda ()
       (if color (setcolor dev (device-color dev color)))
       ;; trace the curve itself
       (setlinewidth dev 2)
       (moveto dev (start-point self))
       (let ((e (normalize (point- (end-point self) (second-handle self)))))
         (curveto dev (first-handle self) (second-handle self) 
                  (point+ (end-point self) (size* e -4))))
       (stroke dev)
       ;; draw the start point
       (annotate dev (start-point self) 'circle)
       ;; draw the end arrow
       ;(format #t "end arrow at ~s\n" (end-point self))
       (annotate dev (end-point self) 'arrow (point- (end-point self)
                                                     (second-handle self)))))))

(define (show-curve self #optional color)
  (let ((dev (current-device)))
    (with-gstate-saved
     dev
     (lambda ()
       (if color (setcolor dev (device-color dev color)))
       ;; trace the curve itself
       (setlinewidth dev 2)
       (moveto dev (start-point self))
       (curveto dev (first-handle self) (second-handle self) (end-point self))
       (stroke dev)
       ;; show the control handles
       (setcolor dev (device-color dev 'black))
       (setlinewidth dev 0.5)
       (moveto dev (start-point self))
       (lineto dev (first-handle self))
       (moveto dev (end-point self))
       (lineto dev (second-handle self))
       (stroke dev)
       ;;
       (annotate dev (start-point self) 'square)
       (annotate dev (end-point self) 'square)
       (annotate dev (first-handle self) 'circle)
       (annotate dev (second-handle self) 'circle)))))

;;;

(define (annotate dev at type #optional extra)
  (with-gstate-saved
   dev
   (lambda ()
     (translate dev at)
     ;;
     (case type
       ((x)
        (moveto dev (make-point 2 0))
        (arc dev (make-point 0 0) 2 0 360)
        (closepath dev)
        (moveto dev (make-point -2 -2))
        (lineto dev (make-point 2 2))
        (moveto dev (make-point 2 -2))
        (lineto dev (make-point -2 2)))
       ((square)
        (moveto dev (make-point -2 -2))
        (lineto dev (make-point 2 -2))
        (lineto dev (make-point 2 2))
        (lineto dev (make-point -2 2))
        (closepath dev))
       ((circle)
        (moveto dev (make-point 2 0))
        (arc dev (make-point 0 0) 2 0 360)
        (closepath dev))
       ((arrow)
        (let* (((d <size>) (normalize extra))
               ((n <size>) (make-size (- (dy d)) (dx d))))
          (moveto dev (make-point 0 0))
          (lineto dev (point+ (size->point (size* d -8)) (size* n 3)))
          (lineto dev (point+ (size->point (size* d -8)) (size* n -3)))
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

(define (show-calligraphic curve
                           pen-angle-0 pen-angle-1
                           pen-width-0 pen-width-1
                           pen-aspect-0 pen-aspect-1
                           parity)
  (define (pen t)
    (make-ellipse angle: (+ pen-angle-0 (* t (- pen-angle-1 pen-angle-0)))
                  major-axis: (+ pen-width-0 (* t (- pen-width-1 pen-width-0)))
                  aspect: (+ pen-aspect-0 (* t (- pen-aspect-1 pen-aspect-0)))
                  origin: (point-on curve t)))
  ;;
  (let* ((E0 (pen 0))
         (E1 (pen 1))
         (EM (pen 1/2))
         (t0 (ellipse-tangent-at E0 (size* (tangent-on curve 0) parity)))
         (t1 (ellipse-tangent-at E1 (size* (tangent-on curve 1) parity)))
         (tm (ellipse-tangent-at EM (size* (tangent-on curve 1/2) parity)))
         (e0 (point-on E0 t0))
         (e1 (point-on E1 t1))
         (em (point-on EM tm)))
    ;;
    (show-ellipse E0 '(gray 0.8))
    (show-ellipse EM '(gray 0.8))
    (show-ellipse E1 '(gray 0.8))
    ;;
    (show-curve curve)
    (let ((path (make-bezier start-point: e0
                             end-point: e1
                             mid-point: em
                             start-tangent: (tangent-on curve 0)
                             end-tangent: (tangent-on curve 1)))
          (start-cap (cons E0 t0))
          (end-cap (cons E1 t1)))
      ;;
      (format #t "start-cap t = ~s       end-cap t = ~s\n" t0 t1)
      (show-curve path '(gray 0.777))
      ;;
      (annotate (current-device) e0 'x)
      (annotate (current-device) em 'x)
      (annotate (current-device) e1 'x)
      ;;
      (values start-cap
              path
              end-cap))))

#|
(define (bez-offset)
  (bind ((c2 (curv 100 100 -25 -25 125 -25 0 100))
         (c1 (curv 0 0 10 100 150 150 100 10))
         (c1a c1b (subdivide c1))
         (c c1a))
    ;;
    (define (pma f pt #optional (follow default: ","))
      (format #t "  ~a -> ~a, ~a -> ~a~a\n"
              (format #f f "X")
              (x pt)
              (format #f f "Y")
              (y pt)
              follow))
    ;;
    (translate (current-device) (make-point 36 36))
    ;;
    (let* ((pen-angle 120)
           (pen-width 20)
           (pen-aspect-ratio 1/2)
           (e (make-ellipse angle: pen-angle
                            major-axis: pen-width
                            aspect: pen-aspect-ratio))
           (em (point+ (point-on c 1/2)
                       (point->size (point-on e 0))))
           (t0 (ellipse-tangent-at e (point- (start-point c)
                                             (first-handle c))))
           (t1 (ellipse-tangent-at e (point- (second-handle c)
                                             (end-point c))))
           (e0 (point+ (point-on e t0)
                       (point->size (start-point c))))
           (e1 (point+ (point-on e t1)
                       (point->size (end-point c)))))
      ;;
      (show-ellipse (translate e (start-point c)) '(gray 0.8))
      (show-ellipse (translate e (point-on c 1/2)) '(gray 0.8))
      (show-ellipse (translate e (end-point c)) '(gray 0.8))
      ;;
      (show-curve c)
      ;;
      (format #t "=========================================\n\n")
      (format #t "soln /. {\n")
      (pma "~a0" (start-point c))
      (pma "~a1" (first-handle c))
      (pma "~a2" (second-handle c))
      (pma "~a3" (end-point c))
      (pma "E0~a" e0)
      (pma "EM~a" em)
      (pma "E1~a" e1 " }")
      (format #t "\n\n=========================================\n\n")
      ;;
      
      ;;(show-curve (offset-bezier c1a 10) '(gray 0.666))
      ;;(show-curve (offset-bezier c1a -10) '(gray 0.333))
      #|
      (show-curve (curv -13.1545 11.3641 -7.47539 68.1552 35.5846 105.793 63.8269 112.854)
                  '(gray 0.777))
      |#
      ;; XXX fit-curve is now inside `make-bezier'
      (show-curve (fit-curve e0 em e1 c) '(gray 0.777))
      ;;
      (annotate (current-device) e0 'x)
      (annotate (current-device) em 'x)
      (annotate (current-device) e1 'x)
      ;;
      (showpage (current-device)))))
|#

(define (range->point-set r)
  (let ((n (caddr r)))
    (list->vector
     (map (lambda (k)
            (+ (car r)
               (* (/ k (- n 1))
                  (- (cadr r) (car r)))))
          (range n)))))

(define (show-axis ctm x-spec y-spec)
  (let* ((xset (range->point-set x-spec))
         (yset (range->point-set y-spec))
         (dev (current-device))
         (x0 (vector-ref xset 0))
         (x1 (vector-ref xset (sub1 (vector-length xset))))
         (y0 (vector-ref yset 0))
         (y1 (vector-ref yset (sub1 (vector-length yset)))))
    ;
    (define (line x0 y0 x1 y1)
      (moveto dev (transform (make-point x0 y0) ctm))
      (lineto dev (transform (make-point x1 y1) ctm))
      (stroke dev))
    ;;
    (define (arrow x0 y0 x1 y1 x2 y2)
      (moveto dev (transform (make-point x0 y0) ctm))
      (lineto dev (transform (make-point x1 y1) ctm))
      (lineto dev (transform (make-point x2 y2) ctm))
      (fill dev))
    ;;
    (with-gstate-saved
     dev
     (lambda ()
       (setlinewidth dev 0.5)
       (line x0 0 x1 0)
       (line 0 y0 0 y1)
       (arrow x1 -1 (+ x1 3) 0 x1 1)
       (arrow -1 y1 0 (+ y1 3) 1 y1)
       ;;
       ;;
       (vector-for-each
        (lambda (x)
          (line x -2 x 2))
        (subvector xset 0 (sub1 (vector-length xset))))
       (vector-for-each
        (lambda (y)
          (line -2 y 2 y))
        (subvector yset 0 (sub1 (vector-length yset))))))))

(define (show-abstract-line (a <point>) (b <point>) (clip <rect>))
  (bind ((dev (current-device))
         (p q (clip-to-rect a b clip)))
    (with-gstate-saved
     dev
     (lambda ()
       (setdash dev '#(1 2) 0)
       (setlinewidth dev 0.25)
       (rectclip dev clip)
       (moveto dev p)
       (lineto dev q)
       (stroke dev)))))
     

(define (show-vector (at <point>) (dir <size>) #optional (scale default: 1))
  (let ((dev (current-device)))
    (with-gstate-saved
     dev
     (lambda ()
       (setcolor dev (device-color dev '(rgb 1 0 0)))
       (if (> (inner-product dir dir) 0)
           (let* ((d (normalize dir))
                  (n (make-size (- (dy d)) (dx d))))
             (moveto dev at)
             (lineto dev (point+ (point+ at dir) (size* d (* scale -2))))
             (stroke dev)
             
             (moveto dev (point+ at dir))
             (lineto dev (point+
                          (point+ (point+ at dir)
                                  (size* n (* scale 1)))
                          (size* d (* scale -3))))
             (lineto dev (point+
                          (point+ (point+ at dir)
                                  (size* n (* scale -1)))
                          (size* d (* scale -3))))
             (closepath dev)
             (fill dev)))
       (annotate dev at 'circle)))))
       
(define (ellipse-subdiv)
  (for-each
   (lambda (info)
     (let ((angle (caddr info))
           (fig-x (+ 120 (* (car info) 150)))
           (fig-y (+ 120 (* (cadr info) 180))))
       (let* ((e (make-ellipse origin: (make-point fig-x fig-y)
                               major-axis: 72
                               aspect: 1/2
                               angle: angle))
              (l (ellipse->bezier-segments e)))
         (show-axis (rotate (translate (make-affine-transform)
                                       (make-point fig-x fig-y))
                            angle)
                    '(-81 81 19)
                    '(-81 81 19))
         ;;
         (stroke-curve l color: '(gray 0.8))
         ;;
         (for-each (lambda (sub)
                     (for-each show-curve (subdivide e sub)))
                   (cdddr info)))))
   ;;
   '((0 0 15 (0.1 0.7))
     (1 0 30 (0.3 0.9))
     (2 0 45 (0 1))
     
     (0 1 80 (0 1/2))
     (1 1 90 (0.1 0.2))
     (2 1 110 (0.1 0.4) (0.6 0.7))

     (0 2 170 (0.4 0.6))
     (1 2 180 (0.6 0.8))
     )))



(define (ellipses)
  (for-each
   (lambda (info)
     (let ((angle (caddr info))
           (fig-x (+ 120 (* (car info) 150)))
           (fig-y (+ 120 (* (cadr info) 150))))
       (let* ((e (make-ellipse origin: (make-point fig-x fig-y)
                               major-axis: 72
                               aspect: 1/2
                               angle: angle))
              (l (ellipse->bezier-segments e)))
         (show-axis (rotate (translate (make-affine-transform)
                                       (make-point fig-x fig-y))
                            angle)
                    '(-81 81 19)
                    '(-81 81 19))
         ;;
         (for-each show-curve l)
         (let ((t (ellipse-top-at e)))
           (show-vector (point-on e t)
                        (size* (normalize (tangent-on e t)) 20))))))
   '((0 0 15)
     (1 0 30)
     (2 0 45)
     
     (0 1 80)
     (1 1 90)
     (2 1 110)

     (0 2 170)
     (1 2 180)
     (2 2 190)

     (0 3 255)
     (1 3 270)
     (2 3 285)
     )))

#|
(define e (make-ellipse origin: (make-point 150 150)
                          major-axis: 72
                          aspect: 1/2
                          angle: 30))
|#

#|
(define (subpath->bezier-list p)
  (let ((pp (path-points p)))
    (map (lambda (k)
           (let* ((p0 (position (vector-ref pp k)))
                  (p3 (position (vector-ref pp (+ k 1))))
                  (d1 (out-handle (vector-ref pp k)))
                  (d2 (in-handle (vector-ref pp (+ k 1))))
                  (p1 (point+ p0 d1))
                  (p2 (point+ p3 d2)))
             `(curv ,(x p0) ,(y p0)
                    ,(x p1) ,(y p1)
                    ,(x p2) ,(y p2)
                    ,(x p3) ,(y p3))))
         (range (sub1 (vector-length pp))))))
|#

(define-class <calligraphic-path> (<object>)
  path
  pen-angle
  pen-width
  pen-aspect)

(define (show-calligraphic-path (self <calligraphic-path>))
  ;; XXX TODO : handle corners in path correctly by
  ;;            angling the inner corner and radiusing the
  ;;            outer corner along the ellipse
  ;;
  (define (expand l)
    (if (pair? l)
        l
        (map (lambda (p) l)
             (cons #t (path self)))))
  ;;
  (define (this z)
    (if (pair? (car z))
        (cadar z) 
        (car z)))
  (define (next z)
    (if (pair? (cadr z))
        (caadr z)
        (cadr z)))
  ;;
  (define (process-path parity)
    (let loop ((p (path self))
               (a (expand (pen-angle self)))
               (w (expand (pen-width self)))
               (z (expand (pen-aspect self)))
               (r '())
               (start-cap #f)
               (end-cap #f))
      (if (null? p)
          (values (reverse! r) start-cap end-cap)
          (bind ((scap path ecap (show-calligraphic (car p)
                                                    (this a) (next a)
                                                    (this w) (next w)
                                                    (this z) (next z)
                                                    parity)))
            (loop (cdr p)
                  (cdr a)
                  (cdr w)
                  (cdr z)
                  (cons path r)
                  (or start-cap scap)
                  ecap)))))
  ;;
  (bind ((P0 S0 E0 (process-path 1))
         (P1 S1 E1 (process-path -1)))
    ;;
    (format #t "start-cap:  (~s ~s)\n" (cdr S0) (cdr S1))
    (print (car S0))
    (print (car S1))
    ;;
    (format #t "  end-cap:  (~s ~s)\n" (cdr E0) (cdr E1))
    (print (car E0))
    (print (car E1))
    ;;
    (append P0
            (ecap (car E0) (cdr S0) (cdr S1))
            (reverse-path P1)
            (ecap (car S0) (cdr S1) (cdr S0)))))

(define (ecap e t0 t1)
  (if (< t1 t0)
      (ecap e t0 (+ t1 1))
      (if (> t1 1)
          (append (ecap e t0 1)
                  (ecap e 0 (- t1 1)))
          (subdivide e (list t0 t1)))))

(define (reverse-path l)
  (let loop ((l l)
             (r '()))
    (if (null? l)
        r
        (loop (cdr l)
              (cons (reverse-bezier (car l)) r)))))
             

(define (S)
  (let ((shape (list
                (curv 245.584 560.472 221.584 584.472 192. 574. 182. 560.)
                (curv 182. 560. 172. 546. 191.936 526.68 217.936 526.68) 
                (curv 217.936 526.68 243.936 526.68 258.896 506.2 259.92
                      484.696)
                (curv 259.92 484.696 260.944 463.192 234. 464. 214. 464.) 
                (curv 214. 464. 194. 464. 181.024 482.368 175.952 491.864)))
        (T (translate
            (translate (scale $identity-transform 5) 
                       (make-point -217.936 -526.68))
            (make-point 48 75))))
    (let ((calpath (make <calligraphic-path>
                         path: (map (lambda (c)
                                      (let ((x (transform c T)))
                                        (show-curve c)
                                        x))
                                    shape)
                         pen-angle: 45
                         pen-width: '(8 20 20 20 10 8)
                         pen-aspect: '1/3)))
      ;;
      (let ((z (show-calligraphic-path calpath)))
        (endpage (current-device))
        (startpage (current-device))
        (for-each show-curve z)))))

(define (I*)
  (let ((shape (list
                (curv 249.085 450.378 259.085 478.378 266.78 483.146
                      279.887 483.146)
                (curv 279.887 483.146 292.995 483.146 296.927 462.174
                      291.684 435.96) 
                (curv 291.684 435.96 286.441 409.746 273.152 342.389
                      268 322) 
                (curv 268 322 262.848 301.611 274 286 286 286) 
                (curv 286 286 298 286 308.136 315.994 314 336)))
        (T (if #t
               $identity-transform
               (translate
                (translate (scale $identity-transform 4)
                           (make-point -249.085 -450.378))
                (make-point 30 30)))))
    ;;
    (make <calligraphic-path>
          path: (map (lambda (c)
                       (let ((x (transform c T)))
                                        ;(show-curve c)
                         x))
                     shape)
          pen-angle: 0
          ;; we have some discontinuous widths and aspects; it's
          ;; up to us to make sure it works out...
          pen-width: '(2 (2 6) 10 10 (6 2) 2)
          pen-aspect: '(1 (1 1/3) 1/3 1/3 (1/3 1) 1))))

(define (I)
  (let ((z (show-calligraphic-path (I*))))
    ;(subdivide (subdivide (subdivide (I*) 0) 1) 0)
    (endpage (current-device))
    (startpage (current-device))
    (for-each show-curve z)))

(define (list-head l k)
  (let loop ((i k)
             (r '())
             (l l))
    (if (= i 0)
        (reverse! r)
        (loop (- i 1) (cons (car l) r) (cdr l)))))

(define (linear-divide f k)
  (if (pair? f)
      (append (list-head f (+ k 1))
              (list (/ (+ (list-ref f k)
                          (list-ref f (+ k 1)))
                       2))
              (list-tail f (+ k 1)))
      f))
;;
(define-method subdivide ((self <calligraphic-path>) k)
  (bind ((p q (subdivide (list-ref (path self) k))))
    (make <calligraphic-path>
          path: (append (list-head (path self) k)
                        (list p q)
                        (list-tail (path self) (+ k 1)))
          pen-angle: (linear-divide (pen-angle self) k)
          pen-width: (linear-divide (pen-width self) k)
          pen-aspect: (linear-divide (pen-aspect self) k))))

(define (coincidences)
  ;;; test cases for `coincident-edges' in contours.scm
  (let ((y 600)
        (k 0)
        (dev (current-device))
        (xposn '((75 < 0) (100 = 0) (130 < 1) (200 = 1) (220 > 1))))
    (setfont dev (get-text-font "Times" "Roman" 9))
    ;;
    (translate dev (make-point 72 0))
    ;;
    (with-gstate-saved
     dev
     (lambda ()
       (translate dev (make-point 0 600))
       (setlinewidth dev 0.5)
       (moveto dev (make-point 100 0))
       (lineto dev (make-point 100 -450))
       (moveto dev (make-point 200 0))
       (lineto dev (make-point 200 -450))
       (stroke dev)))
    ;;
    (define (showcase t0 t1 x0 x1 label)
      (with-gstate-saved
       dev
       (lambda ()
         (set! y (- y 16))
         (set! k (+ k 1))
         (translate dev (make-point 0 y))
         (moveto dev (make-point -36 -4))
         (show dev label)
         (moveto dev (make-point 240 -4))
         (show dev (format #f "(Case ~d)" k))
         (show-vector (make-point x0 0) 
                      (make-size (- x1 x0) 0)))))
    ;;
    (for-each
     (lambda (t0)
       (for-each
        (lambda (t1)
         (let* ((x0 (car t0))
                (x1 (car t1))
                (l (format #f "t0 ~j & t1 ~j" (cdr t0) (cdr t1))))
           (if (and (equal? t0 t1)
                    (not (eq? (cadr t0) '=)))
               (begin
                 (showcase t0 t1 (- x0 10) (+ x0 10)
                           (format #f "~a & t0 < t1" l))
                 (showcase t0 t1 (+ x0 10) (- x0 10)
                           (format #f "~a & t0 > t1" l)))
               (showcase t0 t1 x0 x1 l))))
        xposn))
     xposn)))
     

;;

(define (minipage label f thunk)
  (let ((dev (current-device)))
    (with-gstate-saved
     dev
     (lambda ()
       (translate dev (make-point (origin-x f) (origin-y f)))
       (with-gstate-saved
        dev
        (lambda ()
          (setlinewidth dev 0.5)
          (rectstroke dev (make-rect 0 0 (size-width f) (size-height f)))))
       (setfont dev $minipage-label-font)
       (moveto dev (make-point 0 -9))
       (show dev label)
       (thunk)))))

(define $minipage-label-font (get-text-font "Times" "Italic" 8))
  
(define (bezier-intersections)
  (define (p k)
    (make-rect (+ 50 (* 200 (modulo k 2)))
               (+ 50 (* 224 (quotient k 2)))
               200 200))
  
  (minipage 
   "Bezier Intersections #1"
   (p 0)
   (lambda ()
     (let ((c (curv 10 10 20 110 160 160 110 20))
           (ls (list (make-line 10 50 160 15)
                     (make-line 25 70 160 80))))
       (show-curve c)
       (for-each (lambda (l)
                   (show-vector (from l) (point- (to l) (from l))))
                 ls)
       ;;
       (for-each
        (lambda (l)
          (for-each 
           (lambda (i)
             (annotate (current-device) (point-on l (cadr i)) 'square)
             (annotate (current-device) (point-on c (car i)) 'circle))
           (intersection-parameter c l)))
        ls))))
  ;;
  (minipage "Bezier Fit #1"
            (p 1)
            (lambda ()
              (let ((pts (list (make-point 30 30)
                               (make-point 40 50)
                               (make-point 60 60)
                               (make-point 90 40))))
                (show-curve (make-bezier
                             start-point: (car pts)
                             end-point: (last pts)
                             mid-points: (list (cadr pts) (caddr pts))))
                (for-each
                 (lambda (p) (annotate (current-device) p 'circle))
                 pts))))
  (minipage 
   "Bezier Intersections #2" (p 2)
   (lambda ()
     (let ((c (curv 25 50 125 195 55 5 150 100))
           (ls (list (make-line 57.5 170 105.5 55)
                     (make-line 25 70 160 80))))
       (show-curve c)
       (for-each (lambda (l)
                   (show-vector (from l) (point- (to l) (from l))))
                 ls)
       ;;
       (for-each
        (lambda (l)
          (format #t "line ~s:\n" l)
          (for-each 
           (lambda (i)
             (let ((lp (point-on l (cadr i)))
                   (cp (point-on c (car i))))
               (annotate (current-device) lp 'square)
               (annotate (current-device) cp 'circle)
               (format #t "at intersection ~s => distance ~s\n"
                       i (distance lp cp))))
           (intersection-parameter c l)))
        ls)))))


#|
(load "polynomial.scm")

(define-method real-part ((self <real>)) self)
(define-method imag-part ((self <real>)) 0)

(define (bez-intersect (c <bezier-curve>) (l <line>))
  (bind ((c2 T (fixup l c))
         (xp yp (bezier->polynomials c2)))
    (apply append
           (map (lambda (r)
                  (if (> (abs (imag-part r))
                         (/ (abs (real-part r)) 100))
                      '()
                      (let ((t (magnitude r)))
                        (list (list t (point-on c t))))))
                (apply compute-cubic-roots (reverse yp))))))

(define (fixup (self <line>) friend)
  (let* ((t (rotate-and-scale (translate $identity-transform
                                         (from self))
                              (point- (to self) (from self))))
         (i (invert-transform t)))
    (values (transform friend i) t)))
|#


(define (subdivide2 (a <bezier-curve>) t0 t1)
  (if (< t0 t1)
      (bind ((l0 r0 (if (= t0 0) (values #f a) (subdivide-at a t0)))
             (l1 r1 (if (= t1 1) r0 (subdivide-at r0 (/ (- t1 t0)
                                                        (- 1 t0))))))
        l1)
      (reverse-bezier (subdivide2 a t1 t0))))


(define (bez-coincidence)
  (let ((dev (current-device)))
    ;;
    (define (co (a <bezier-curve>) (b <bezier-curve>))
      (let ((t0 (intersection-parameter a (start-point b)))
            (t1 (intersection-parameter a (end-point b)))
            (t2 (intersection-parameter b (start-point a)))
            (t3 (intersection-parameter b (end-point a))))
        (values t0 t1 t2 t3)))
    ;;
    (define (coinc c t0 t1 t2 t3)
      (let ((a (subdivide2 c t0 t1))
            (b (subdivide2 c t2 t3)))
        (show-curve-vector (transform a (translate $identity-transform
                                                   (make-point 0 5))))
        (show-curve-vector (transform b (translate $identity-transform
                                                   (make-point 0 -5))))
        (bind ((t0 t1 t2 t3 (co a b)))
          (define (L x)
            (if (and (>= x 0) (<= x 1))
                (sprintf-float "%.1f" 10 (+ x 0.0))
                '-))
          (moveto dev (make-point 110 -4))
          (show dev (format #f "~a, ~a, ~a, ~a" 
                            (L t0) (L t1) (L t2) (L t3))))))
    ;;
    (define (show-case* c k t0 t1 t2 t3)
      (with-gstate-saved
       dev
       (lambda ()
         (translate dev (make-point 48 (+ 48 (* 50 k))))
         ;;(show-axis $identity-transform '(-10 100 12) '(-10 100 12))
         (coinc c t0 t1 t2 t3))))
    (define (show-case c k t0 t1 t2 t3)
      (show-case* c k t0 t1 t2 t3)
      (show-case* c (+ k 1) t0 t1 t3 t2))
    ;;
    (let ((c (curv 0 0 50 30 50 -30 100 0)))
      (show-case c 0  0 1  0.3 0.7)
      (show-case c 2  0.3 0.7  0 1)
      (show-case c 4  0 0.7  0.3 1)
      (show-case c 6  0.3 1  0 0.6)
      (show-case c 8  0 0.4  0.6 1)
      ;;
      )))

(define (tight-bbox c)
  (if (< (flatness c) 0.01)
      (bbox c)
      (bind ((l r (subdivide c)))
        (union-rect (tight-bbox l) (tight-bbox r)))))

(define (fit-to-box c)
  (let* ((b (tight-bbox c))
         (ms (/ 100 (max (size-width b)
                         (size-height b)))))
    (transform c 
               (translate (scale $identity-transform ms)
                          (make-point (- (origin-x b))
                                      (- (origin-y b)))))))

(define (bezier-axis-plots)
  (define (rot90 s)
    (make-size (- (dy s)) (dx s)))
  ;;
  (define (slew c)
    (transform c
               (invert-transform
                (rotate-and-scale $identity-transform 
                                  (point- (end-point c)
                                          (start-point c))))))
  ;;
  (define (baxis-case label y c)
    (let* (((c <bezier-curve>) (fit-to-box (slew c)))
           (dev (current-device))
           #|
           (T (rotate-and-scale $identity-transform 
                                (point- (end-point c)
                                        (start-point c))))
           |#
           (t-marks (select (lambda (t)
                              (and (real? t)
                                   (>= t 0)
                                   (<= t 1)))
                            (bezier-quadratic-roots c))))
      ;;
      (bezier-axis-plots* (format #f "Bezier Self-Intersection #~a" label)
                          c
                          t-marks
                          y)))
  ;;
  (baxis-case "1" -20 (curv 111.0625 670.875 
                          216.5 703.4375 
                          3.5 698.9375 
                          214.5625 660.375))
  (baxis-case "2" (- 150 20) (curv 111.0625 601.375
                            334.5 597 157.5 625.5 214.5625 590.875))
  (baxis-case "3" (- 300 20) (curv 0 0 200 0 100 200 100 -100))
  (baxis-case "4" (- 450 20) (curv 111.0625 540.625
                            229.5 570 -58.5 523.5 214.5625 530.125))
  (baxis-case "5" (- 600 20) (curv 111.0625 540.625
                            229.5 570 
                            -58.5 (+ 523.5 20) 
                            214.5625 (+ 530.125 20))))

(define (bezier-axis-plots* label c t-marks y0)
  (let ((dev (current-device)))
    (minipage 
     label
     (make-rect 50 (+ y0 50) 120 120)
     (lambda ()
       (translate dev (make-point 10 10))
       (with-gstate-saved 
        dev
        (lambda ()
          (setcolor dev (device-color dev '(rgb 0 0 1)))
          (setlinewidth dev 0.2)
          (rectstroke dev (make-rect 0 0 100 100))))
       (show-curve-vector c)
       (for-each
        (lambda (t)
          (setlinewidth dev 0.5)
          (annotate dev (point-on c t) 'circle))
        t-marks)))
    ;;
    (minipage
     "x(t)"
     (make-rect (+ 50 120) (+ y0 50) 120 120)
     (lambda ()
       (translate dev (make-point 10 60))
       (with-gstate-saved 
        dev
        (lambda ()
          (setcolor dev (device-color dev '(rgb 0 0 1)))
          (show-axis $identity-transform '(0 100 11) '(-50 50 11))))
       (simple-plot (lambda (t)
                      (x (point-on c t))))
       (for-each
        (lambda (t)
          (let ((p (point-on c t)))
            (setlinewidth dev 0.5)
            (annotate dev (make-point (* t 100) (/ (x p) 2)) 'circle)))
        t-marks)))
    ;;
    (minipage
     "y(t)"
     (make-rect (+ 50 (* 2 120)) (+ y0 50) 120 120)
     (lambda ()
       (translate dev (make-point 10 60))
       (with-gstate-saved 
        dev
        (lambda ()
          (setcolor dev (device-color dev '(rgb 0 0 1)))
          (show-axis $identity-transform '(0 100 11) '(-50 50 11))))
       (simple-plot (lambda (t)
                      (y (point-on c t))))
       (for-each
        (lambda (t)
          (let ((p (point-on c t)))
            (setlinewidth dev 0.5)
            (annotate dev (make-point (* t 100) (/ (y p) 2)) 'circle)))
        t-marks)))))

;;;  Plot the function fn: [0,1] -> [-1,1]
;;;  over the range 0,1

(define (simple-plot fn)
  (let ((dev (current-device)))
    (for-each
     (lambda (t)
       (let ((z (make-point (* t 100) (/ (fn t) 2))))
         (if (= t 0)
             (moveto dev z)
             (lineto dev z))))
     (cons 0 (map (lambda (it)
                    (/ (+ it 1) 50.0))
                  (range 50))))
    (setlinewidth dev 2)
    (stroke dev)))

;;;



(define *sample-points* (append '(0)
                                (map (lambda (t)
                                       (/ t 50.0))
                                     (cdr (range 50)))
                                '(1)))

#|
(search-for-minimum (lambda (x) (max-sine-error a: 0.603589 b: x)) 0.5 0.6 10)
|#

(define (search-for-minimum proc from to n)
  (let loop ((from-x from)
             (from-y (proc from))
             (to-x to)
             (to-y (proc to))
             (i 0))
    (let ((x (/ (+ from-x to-x) 2)))
      (format #t "[~d] ~d (~d) -- ~d (~d)\n" i from-x from-y to-x to-y)
      (if (= i n)
          x
          (let ((y (proc x)))
            (format #t "\t\ty(~d) = ~d\n" x y)
            (if (and (>= y from-y)
                     (>= y to-y))
                (if (< from-y to-y)
                    from-x
                    to-x)
                (if (> (abs (- y from-y)) (abs (- y to-y)))
                    (loop x y to-x to-y (+ i 1))
                    (loop from-x from-y x y (+ i 1)))))))))
                    
                    

(define (max-sine-error #rest r)
  (let ((curv (car (apply sine-wave-beziers r))))
    (let loop ((t *sample-points*)
               (max-err #f)
               (max-err-t #f))
      (if (null? t)
          (values max-err max-err-t)
          (let ((err (distance curv (make-point (* (car t) (/ $Pi 2))
                                                (sin (* (car t) (/ $Pi 2)))))))
            (if (or (not max-err) (> err max-err))
                (begin
                  (loop (cdr t) err (car t)))
                (loop (cdr t) max-err max-err-t)))))))

(define (max-matrix a b scale)
  (vector-map
   (lambda (y)
     (vector-map
      (lambda (x)
        (sprintf-float "%.3f" 30 (max-sine-error (+ a (* x scale))
                                                 (+ b (* y scale)))))
      '#(0 1 2 3 4 5 6 7 8 9 10)))
   '#(0 1 2 3 4 5 6 7 8 9 10)))

(define (bezdistance)
  (let ((visit '())
        (p (make-bezier start-point: (make-point 160 30)
                        first-handle: (make-point 60 240)
                        second-handle: (make-point 340 240)
                        end-point: (make-point 240 30)))
        ;; this just so happens to be approx. the estimated '-20' offset curve
        (q (make-bezier start-point: (make-point 142 21)
                        first-handle: (make-point 28 260)
                        second-handle: (make-point 372 260)
                        end-point: (make-point 258 21)))
        (tx 0.4))
    ;;
    (define (drawcurv c)
      (let ((dev (current-device)))
        (moveto dev (start-point c))
        (curveto dev (first-handle c) (second-handle c) (end-point c))))
    ;;
    (minipage
     "Distance from B(t) to A, as a function of t"
     ;;
     (make-rect 36 (+ 300 36 18) 400 100)
     (lambda ()
       (minigraph (make-rect 5 5 390 90)
                  xmin: 0
                  xmax: 1
                  xstep: 0.05
                  function: (lambda (t)
                              (bind ((z (point-on p t))
                                     (dist pt tx (distance q z t)))
                                (set! visit (cons (list z pt (point-on q t)) 
                                                  visit))
                                dist)))))
    ;;
    (minipage
     (~ "Closeness for t=~d" tx)
     (make-rect 36 (+ 300 36 18 18 100) 400 220)
     (lambda ()
       (let* ((t tx)
              (dev (current-device))
              (b (point-on p t))
              (v (tangent-on q t))
              (tang (make <line>
                          from: (point-on q t)
                          to: (point+ (point-on q t) v)))
              (ortho (make <line>
                           from: b
                           to: (point+ b (make-size (- (dy v)) (dx v))))))
         (drawcurv q)
         (stroke dev)
         (with-gstate-saved
          dev
          (lambda ()
            (setcolor dev (device-color dev '(rgb 0 0 1)))
            (show-abstract-line (from tang) (to tang) (make-rect 0 0 400 220))
            (setcolor dev (device-color dev '(rgb 1 0 0)))
            (show-abstract-line (from ortho) (to ortho) (make-rect 0 0 400 220))
            (annotate dev b 'circle)))
         ;;
         (for-each
          (lambda (inter)
            (annotate dev (point-on q (car inter)) 'circle))
          (intersection-parameter q ortho)))))
       ;;
    (minipage
     "Two Bezier Curves"
     (make-rect 36 36 400 300)
     (lambda ()
       (bind ((dev (current-device)))
         ;;
         (show-curve p)
         (show-curve q)
         ;;
         (setlinewidth dev 0.2)
         (for-each (lambda (v)
                     (moveto dev (car v))
                     (lineto dev (cadr v))
                     (annotate dev (car v) 'circle)     ; p[t]
                     (annotate dev (cadr v) 'square)    ; closest point on q
                     (annotate dev (caddr v) 'x))       ; q[t]
                   visit))))
    ;;
    (values)))

(define (sine)
  (minipage
   "Sin(x)"
   (make-rect 50 50 420 420)
   (lambda ()
     (translate (current-device) (make-point 10 210))
     (let ((mx (scale $identity-transform (make-point (/ 400 (/ $Pi 2)) 200)))
           (curv (car (sine-wave-beziers))))
       (show-axis $identity-transform '(0 400 11) '(-200 200 11))
       ;;
       (show-curve (transform curv mx))
       (setcolor (current-device)
                 (device-color (current-device) '(gray 0.667)))
       (simple-plot* (scale mx (make-point (/ $Pi 2) 1))
                     (lambda (t)
                       (sin (* t (/ $Pi 2)))))
       (bind ((err err-at (max-sine-error)))
         (annotate (current-device)
                   (transform (point-on curv err-at) mx) 'circle)))))
  (minipage
   "Distance to Bezier Curve"
   (make-rect 50 650 420 100)
   (lambda ()
     (let ((c (transform (car (sine-wave-beziers))
                         (rotate
                          (scale $identity-transform
                                 (make-point 200 100))
                          -25)))
           (test-points (list (make-point 0 0)
                              (make-point 50 50)
                              (make-point 100 50)
                              (make-point 150 50)
                              (make-point 200 50)
                              (make-point 250 50)
                              (make-point 300 50)
                              (make-point 350 50)
                              (make-point 400 50)
                              (make-point 175 10)
                              (make-point 225 10))))
       (stroke-curve (list c) closed?: #f)
       (for-each
        (lambda (p)
          (bind ((dist pt t (distance c p)))
            (moveto (current-device) p)
            (lineto (current-device) pt)
            (stroke (current-device))
            (annotate (current-device) p 'square)
            (annotate (current-device) pt 'circle)
            ;;
            (moveto (current-device) (point+ p (make-size 3 -4)))
            (show (current-device) (sprintf-float "%.1f" 20 dist))))
        test-points))))
  ;;
  (minipage
   "Wave pattern"
   (make-rect 50 500 420 100)
   (lambda ()
     (let* ((mx (scale $identity-transform (make-point (/ 30 $Pi) 3)))
            (wave (map (lambda (seg)
                         (transform seg mx))
                       (append
                        (sine-wave-beziers)
                        (sine-wave-beziers cycle: 1)
                        (sine-wave-beziers cycle: 2)))))
       (translate (current-device) (make-point 10 50))
       (for-each
        (lambda (y)
          (with-gstate-saved
           (current-device)
           (lambda ()
             (translate (current-device) (make-point 0 y))
             (stroke-curve wave closed?: #f))))
        (map (lambda (iy)
               (* iy 3))
             (range 10)))))))

(define (simple-plot* matrix fn)
  (let ((dev (current-device)))
    ;;
    (moveto dev (transform (make-point 0 (fn 0)) matrix))
    (for-each
     (lambda (t)
       (lineto dev (transform (make-point t (fn t)) matrix)))
     (map (lambda (i)
            (/ (+ i 1) 50.0))
          (range 49)))
    (lineto dev (transform (make-point 1 (fn 1)) matrix))
    ;;
    (setlinewidth dev 1)
    (stroke dev)))


(define (cloudpath)
  (list (curv 220 187 137 185 -22 237 0 350)
        (curv 0 350 22 462 130 487 175 454)
        (curv 175 454 220 422 105 612 325 662)
        (curv 325 662 544 712 622 632 624 574)
        (curv 624 574 665 632 816 709 921 599)
        (curv 921 599 1026 489 814 384 859 399)
        (curv 859 399 904 414 1041 394 996 207)
        (curv 996 207 951 20 547 165 592 137)
        (curv 592 137 637 110 524 -27 384 0)
        (curv 384 0 245 27 235 77 220 187)))

(define (cloudstroke dev rect)
  (areastroke dev
              (transform (segments->area (list (cloudpath)))
                         (translate-and-scale-uniform
                          (make-rect 0 0 1000 1000) 
                          rect))))

(define (Cloud)
  (minipage
   "Cloud"
   (make-rect 36 36 350 350)
   (lambda ()
     (let ((d *current-device*))
       (translate d (make-point 25 25))
       ;;
       (let* ((t (translate-and-scale-uniform (make-rect 0 0 1000 1000)
                                              (make-rect 0 0 300 300)))
              (tc (map (lambda ((s <bezier-curve>))
                         (transform s t))
                       (cloudpath))))
         ;
         (setcolor d (device-color d '(rgb 0 0 1)))
         (rectstroke d (union-rects (map (lambda (s)
                                           (bbox s 0.25))
                                         tc)))
         ;;
         (setcolor d (device-color d 'black))
         (for-each
          (lambda ((s <bezier-curve>))
            (show-curve s '(rgb 1 0 0)))
          tc)
         ;;
         (let ((c1 (car tc)))
           (annotate d
                     (first-handle c1)
                     'arrow
                     (point- (first-handle c1)
                             (start-point c1))))))))
  ;;
  (minipage
   "Cloud Test #1"
   (make-rect 36 (+ 350 36 36) 350 200)
   (lambda ()
     (let ((d *current-device*))
       (with-gstate-saved
        d
        (lambda ()
          (setlinewidth d 0.25)
          (setcolor d (device-color d '(gray 0.667)))
          (rectstroke d (make-rect 10 10 50 50))))
       ;;
       (cloudstroke d (make-rect 10 10 50 50))
       (cloudstroke d (make-rect 100 30 50 50))
       (cloudstroke d (make-rect 200 130 50 50))
       (rectstroke d (make-rect 270 40 50 100))
       ;;
       (arrowstroke d (list (make-point 225 140)
                            (make-point 225 100)
                            (make-point 270 100))
                    radius: 5
                    setback: 0.5)
       ;;
       (setfont d (get-text-font "Times" "Italic" 8))
       (moveto d (make-point (+ 225 5) (+ 100 2)))
       (show d "a-pointer")
       (values))))
  (values))

#|
(define (Cloud)
  ;; This approach is way too regular to "look right"...
  (let ((curves (map (lambda (c)
                       (transform c (scale $identity-transform (/ 200))))
                     (list 
                      (curv 0 0     40 60   180 20   200 0)
                      (curv 200 0   220 -20   220 -40  200 -60)))))
    (minipage
     "Template"
     (make-rect 36 36 250 200)
     (lambda ()
       (let ((d *current-device*))
         (translate d (make-point 25 100))
         (annotate d (make-point 0 0) 'circle)
         (annotate d (make-point 200 0) 'circle)
         ;;
                                        ;(moveto d (make-point 0 0))
         (let ((S (scale $identity-transform 200)))
           (for-each (lambda (c)
                       (show-curve (transform c S)))
                     curves)))))
    ;;
    (for-each
     (lambda (spec)
       (let* ((n (car spec))
              (j (cadr spec))
              (x 36)
              (y (+ 36 250 (* j 120)))
              (radius 40))
         (define (polar a)
           (make-point (* (cos a) radius) (* (sin a) radius)))
         ;;
         (minipage
          (~ "Test Cloud (n=~d)" n)
          (make-rect x y 100 100)
          (lambda ()
            (let ((d *current-device*))
              (translate d (make-point 50 50))
              (annotate d $zero-point 'circle)
              ;;
              ;; Draw the polygon frame
              ;;
              (with-gstate-saved
               d
               (lambda ()
                 (setlinewidth d 0.2)
                 (setcolor d (device-color d '(gray 0.667)))
                 ;;
                 (moveto d (polar 0))
                 (for-each 
                  (lambda (i)
                    (lineto d (polar (* 2 $Pi (/ (+ i 1) n)))))
                  (range n))
                 (stroke d)))
              ;;
              ;;  Apply the curve to the polygon
              ;;
              (let ((interior-angle (* 180 (- 1 (/ 2 n)))))
                (for-each 
                 (lambda (i)
                   (let* ((t $identity-transform)
                          (t (translate t (polar (* 2 $Pi (/ i n)))))
                                        ;(t (rotate t (* 180 (- 1 (/ 2 n)))))
                          (t (rotate t (+ (* i (/ 360 n))
                                          (- (- 180 (/ interior-angle 2))))))
                          (t (scale t (regular-polygon-side n radius)))
                          )
                     (stroke-curve
                      (map (lambda (c)
                             (transform c t))
                           curves)
                      closed?: #f)))
                 (range n))))))))
     '((5 0)
       (7 1)))))
|#
                           
                           #|(transform (car (sine-wave-beziers))
                         (scale (translate $identity-transform
                                           (make-point 20 10))
                                (make-point 100 150)))
                         |#

(define (minigraph-xvalues xmin xmax xstep)
  (let loop ((r '())
             (x xmax))
    (if (> (/ (abs (- x xmin)) xstep) 0.01)
        (loop (cons x r) (- x xstep))
        (cons xmin r))))
           

   
(define (minigraph frame #key xmin xmax xstep function)
  (let* ((xvalues (minigraph-xvalues xmin xmax xstep))
         (yvalues (map function xvalues))
         (ymin (reduce1 min yvalues))
         (ymax (reduce1 max yvalues))
         (dev (current-device))
         (sx (/ (size-width frame) (- xmax xmin)))
         (sy (/ (size-height frame) (- ymax ymin))))
    ;;
    (define (gp x y)
      (make-point (* sx (- x xmin))
                  (* sy (- y ymin))))
    ;;
    (with-gstate-saved
     dev
     (lambda ()
       (moveto dev (lower-left frame))
       (lineto dev (upper-left frame))
       (stroke dev)
       ;;
       (setfont dev (get-text-font "Helvetica" "Regular" 8))
       (moveto dev (point+ (lower-left frame) (make-size 1 -3)))
       (show dev (to-string ymin))
       (moveto dev (upper-left frame))
       (show dev (to-string ymax))
       ;;
       (setlinewidth dev 0.1)
       ;;
       (translate dev (origin frame))
       (moveto dev (gp (car xvalues) (car yvalues)))
       (for-each (lambda (x y)
                   (lineto dev (gp x y)))
                 (cdr xvalues)
                 (cdr yvalues))
       (stroke dev)))))

(define (bezier-offsets)
  ;;
  ;; show how an offset to a bezier curve is constructed
  ;;
  (define (annotated-bezoffset dev dist f a graph-error?)
    ;;
    (define (cross a b)
      (let ((d (point- a b)))
        (normalize (make-size (dy d) (- (dx d))))))
    ;;
    (define (offset-line l d)
      (make <line>
            from: (point+ (from l) d)
            to: (point+ (to l) d)))
    ;;
    (define (intersect a b)
      (bind ((p h t (clip-to-segment (from a) (to a) 
                                     (from b) (cross (to b) (from b)))))
        (or p
            (error "~s..~s and ~s..~s do not intersect"
                   (from a) (to a)
                   (from b) (to b)))))
    ;;
    (define (offpoint p d)
      (offset-point p (dx d) (dy d)))
    ;;
    (define (drawcurv c)
      (moveto dev (start-point c))
      (curveto dev (first-handle c) (second-handle c) (end-point c)))
                                        ;
    (show-curve a)
    ;;
    (let* ((p1 (start-point a))
           (p2 (first-handle a))
           (p3 (second-handle a))
           (p4 (end-point a))
           (n1 (cross p2 p1))
           (n2 (cross p3 p2))
           (n3 (cross p4 p3))
           (l1 (offset-line (make <line> from: p1 to: p2)
                            (size* n1 dist)))
           (l2 (offset-line (make <line> from: p2 to: p3)
                            (size* n2 dist)))
           (l3 (offset-line (make <line> from: p3 to: p4)
                            (size* n3 dist)))
           (q (bezier-offset a dist)))
      ;;
      (with-gstate-saved
       dev
       (lambda ()
         (setcolor dev (device-color dev '(rgb 0.5 0.5 1)))
         (drawcurv q)
         (drawcurv (bezier-offset a (- dist)))
         (stroke dev)))
      ;;
      (annotate dev (intersect l1 l2) 'circle)
      (annotate dev (intersect l2 l3) 'circle)
      ;;
      (show-vector p3 (point- p4 p3) 2)
      (show-vector p2 (point- p3 p2) 2)
      (show-vector p1 (point- p2 p1) 2)
      ;;
      ;;
      (show-abstract-line (from l1) (to l1) f)
      (show-abstract-line (from l2) (to l2) f)
      (show-abstract-line (from l3) (to l3) f)
      ;;
      ;;
      (show-vector p1 (size* n1 dist) 2)
      (show-vector p2 (size* n1 dist) 2)
      (show-vector p2 (size* n2 dist) 2)
      (show-vector p3 (size* n2 dist) 2)
      (show-vector p3 (size* n3 dist) 2)
      (show-vector p4 (size* n3 dist) 2)
      ;;
      (if graph-error?
          (with-gstate-saved
           dev
           (lambda ()
             (let ((visit '()))
               (minigraph 
                (make-rect 150 5 45 45)
                xmin: 0
                xmax: 1
                xstep: 0.1
                function: (lambda (t)
                            (bind ((z (point-on q t))
                                   (dist pt tx (distance a z t)))
                              (set! visit (cons (list z pt (point-on a t)) visit))
                              dist)))
               ;;
               (setlinewidth dev 0.2)
               (for-each (lambda (v)
                           (moveto dev (car v))
                           (lineto dev (cadr v))
                           (annotate dev (car v) 'circle)
                           (annotate dev (caddr v) 'x)
                           (annotate dev (cadr v) 'square))
                         visit)))))
      (values)))
  ;;
  (minipage
   "Offset to a Bezier Curve #1"
   (make-rect 36 36 200 200)
   (lambda ()
     (let ((dev (current-device))
           (f (make-rect 0 0 200 200))
           (a (make-bezier start-point: (make-point 20 10)
                           first-handle: (make-point 30 60)
                           second-handle: (make-point 100 150)
                           end-point: (make-point 150 180)))
           (dist -10))
       ;;
       (annotated-bezoffset dev -10 f a #t))))
  ;;
  (minipage
   "Offset to a Bezier Curve #2: Bend too tight"
   (make-rect 36 (+ 36 250) 200 200)
   (lambda ()
     (let ((dev (current-device))
           (f (make-rect 0 0 200 200))
           (a (make-bezier start-point: (make-point 80 15)
                           first-handle: (make-point 30 120)
                           second-handle: (make-point 170 120)
                           end-point: (make-point 120 15)))
           (dist -10))
       ;;
       (annotated-bezoffset dev -10 f a #t))))
  ;;
  (minipage
   "Offset to a Bezier Curve #2: Subdivided"
   (make-rect (+ 36 250) (+ 36 250) 200 200)
   (lambda ()
     (bind ((dev (current-device))
            (f (make-rect 0 0 200 200))
            (p q (subdivide (make-bezier start-point: (make-point 80 15)
                                         first-handle: (make-point 30 120)
                                         second-handle: (make-point 170 120)
                                         end-point: (make-point 120 15))))
            (dist -10))
       ;;
       (annotated-bezoffset dev -10 f p #t)
       (annotated-bezoffset dev -10 f q #f))))
  ;;
  (values))


(define (regular-polygon-side n r)
  (* 2 r (sin (/ $Pi n))))


(define (test-all)
  ;; send output to /tmp/test-geom.ps
  (test-jig
   ellipse-subdiv ellipses
   I I* S
   coincidences
   bezier-intersections
   bez-coincidence
   bezier-axis-plots
   sine
   Cloud))
  
