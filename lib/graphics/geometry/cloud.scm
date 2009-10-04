#|
(define (showcloud)
  (let ((prev (make-point 5.18404 4.45412)))
    ;;
    (define (mcurv prev step)
      (bind ((a b c (list->values step))
             (p (if (instance? (car prev) <point>)
                    (car prev)
                    (end-point (car prev)))))
        (cons (curv (x p) (y p)
                    (car a) (cadr a)
                    (car b) (cadr b)
                    (car c) (cadr c))
              prev)))
    ;;
    (cdr
     (reverse
      (reduce mcurv
              (list (make-point 5.18404 4.45412))
              '(((3.53404 4.40412) (0.334045 5.45412) (0.784045 7.70412))
                ((1.23404 9.95412) (3.38404 10.4541) (4.28404 9.80412))
                ((5.18404 9.15412) (2.88404 12.9541) (7.28404 13.9541))
                ((11.684 14.9541) (13.234 13.3541) (13.284 12.2041))
                ((14.1 13.3541) (17.134 14.9041) (19.234 12.7041))
                ((21.334 10.5041) (17.084 8.40412) (17.984 8.70412))
                ((18.884 9.00412) (21.634 8.60412) (20.734 4.85412))
                ((19.834 1.10412) (11.734 4.00412) (12.634 3.45412))
                ((13.534 2.90412) (11.284 0.154115) (8.48404 0.704115))
                ((5.68404 1.25412) (5.48544 2.25217) (5.18544 4.45217))))))))

(define (normalcloud)
  (let ((t (translate-and-scale-uniform (cloudbox)
                                        (make-rect 0 0 1000 1000))))
    (map (lambda (c)
           (transform c t))
         (showcloud))))

(define-method integerify! ((self <bezier-curve>))
  (integerify! (start-point self))
  (integerify! (first-handle self))
  (integerify! (second-handle self))
  (integerify! (end-point self))
  self)

(define-method integerify! ((self <point>))
  (gvec-set! self 0 (round (gvec-ref self 0)))
  (gvec-set! self 1 (round (gvec-ref self 1)))
  self)

(define (clouddump)
  (map (lambda (c)
         (let ((p0 (start-point c))
               (p1 (first-handle c))
               (p2 (second-handle c))
               (p3 (end-point c)))
           (list (x p0) (y p0)
                 (x p1) (y p1)
                 (x p2) (y p2)
                 (x p3) (y p3))))
       (map integerify! (normalcloud))))

|#


(define (cloudbox)
  (union-rects
   (map
    (lambda ((s <bezier-curve>))
      (bbox s 0.25))
    (cloudpath))))


(define (Cloud)
  (minipage
   "Details"
   (make-rect 36 250 200 200)
   (lambda ()
     (let ((d *current-device*))
       (translate d (make-point 30 30))
       ;;
       (let ((t (translate-and-scale-uniform (cloudbox)
                                             (make-rect 0 0 140 140))))
         ;
         (rectstroke
          d
          (union-rects
           (map
            (lambda ((s <bezier-curve>))
              (let ((c (transform s t)))
                (show-curve c '(rgb 1 0 0))
                (bbox c 0.25)))
            (select (lambda (i)
                      (instance? i <bezier-curve>))
                    (showcloud)))))
         ;;
         (let ((c1 (car (showcloud))))
           (annotate d
                     (transform (first-handle c1) t) 
                     'arrow
                     (point- (transform (first-handle c1) t)
                             (transform (start-point c1) t))))))))
  ;;
  (minipage
   "Template"
   (make-rect 36 36 200 200)
   (lambda ()
     (let ((d *current-device*))
       (translate d (make-point 100 100))
       (scale d 5 5)
       ;;
       (define (pair->point p)
         (make-point (car p) (cadr p)))
       ;;
       (moveto d (make-point 5.18404 4.45412))
       ;;
       (for-each
        (lambda (curv)
          (bind ((a b c (list->values curv)))
            (curveto d
                     (pair->point a)
                     (pair->point b)
                     (pair->point c))))
        '(((3.53404 4.40412) (0.334045 5.45412) (0.784045 7.70412))
          ((1.23404 9.95412) (3.38404 10.4541) (4.28404 9.80412))
          ((5.18404 9.15412) (2.88404 12.9541) (7.28404 13.9541))
          ((11.684 14.9541) (13.934 13.3541) (13.284 12.2041))
          ((12.634 11.0541) (17.134 14.9041) (19.234 12.7041))
          ((21.334 10.5041) (17.084 8.40412) (17.984 8.70412))
          ((18.884 9.00412) (21.634 8.60412) (20.734 4.85412))
          ((19.834 1.10412) (11.734 4.00412) (12.634 3.45412))
          ((13.534 2.90412) (11.284 0.154115) (8.48404 0.704115))
          ((5.68404 1.25412) (5.48544 2.25217) (5.18544 4.45217))))
       ;;
       (closepath d)
       (stroke d)))))

