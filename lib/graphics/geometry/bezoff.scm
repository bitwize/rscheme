(define-module-extend graphics.geometry ()

;;; FIXED:

(define (bezier-converge-step (self <bezier-curve>) (b <point>) t)
  (let ((v (tangent-on self t)))
    (caar
     (select-min
      (lambda (item)
        (distance (point-on self (car item)) b))
      (intersection-parameter
       self
       (make <line>
             from: b
             to: (point+ b (make-size (- (dy v)) (dx v)))))))))

;;; NEW:

(define (bezier-offset (self <bezier-curve>) dist)
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
  (let* ((p1 (start-point self))
         (p2 (first-handle self))
         (p3 (second-handle self))
         (p4 (end-point self))
         (n1 (cross p2 p1))
         (n2 (cross p3 p2))
         (n3 (cross p4 p3))
         ;;
         (l1 (offset-line (make <line> from: p1 to: p2)
                          (size* n1 dist)))
         (l2 (offset-line (make <line> from: p2 to: p3)
                          (size* n2 dist)))
         (l3 (offset-line (make <line> from: p3 to: p4)
                          (size* n3 dist))))
    ;(format #t "n1 n2 n3 ~s ~s ~s\n" n1 n2 n3)
    ;;
    (make <bezier-curve>
          start-point: (offpoint p1 (size* n1 dist))
          first-handle: (intersect l1 l2)
          second-handle: (intersect l2 l3)
          end-point: (offpoint p4 (size* n3 dist)))))

(&module
 (export bezier-offset)))
