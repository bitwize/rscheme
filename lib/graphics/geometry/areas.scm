;;;
;;;   a path as defined by a set of subpaths,
;;;   each of which is a list of <line> or <bezier-curve> segments
;;;
;;;   Note that this is a kind of "dual" representation relative
;;;   to the <path-bounded-area> representation, which is in terms
;;;   of vertices

(define-class <segment-path> (<object>)
  segments)

;;;
;;;

(define-class <area-subpath> (<object>)
  (path-points type: <list> init-value: '()))

(define-class <area-subpath-vertex> (<object>)
  (position type: <point>))

(define-class <area-subpath-bezier-vertex> (<area-subpath-vertex>)
  (in-handle type: <size> init-value: $zero-size)   ; relative to posn
  (out-handle type: <size> init-value: $zero-size)) ; relative to posn

(define-class <path-bounded-area> (<area>)
  (subpaths type: <list> init-value: '()))


(define (subpath-neg (self <area-subpath>))
  (make <area-subpath>
        path-points: (reverse (path-points self))))

;;;
;;;  primitives for constructing path-bounded areas
;;;

(define (rect-area (r <rect>))
  (let-syntax ((sv (syntax-form (p)
                     (make <area-subpath-vertex> position: p))))
    (make <path-bounded-area>
      subpaths: (list
                 (make <area-subpath>
                   path-points: (list (sv (origin r))
                                      (sv (lower-right r))
                                      (sv (upper-right r))
                                      (sv (upper-left r))))))))

(define (simple-polygon-area . lst)     ; a list of points
  (let-syntax ((sv (syntax-form (p)
                     (make <area-subpath-vertex> position: p))))
    (let loop ((v '())
               (l lst))
      (if (null? l)
          (make <path-bounded-area>
                subpaths: (list
                           (make <area-subpath>
                                 path-points: (reverse! v))))
          (loop (cons (sv (car l)) v)
                (cdr l))))))
  
;;;
;;;  simple operations on path-bounded areas, useful for
;;;  constructing complex paths
;;;

(define (path-subtract (a <path-bounded-area>)
                       (b <path-bounded-area>))
  (make <path-bounded-area>
        subpaths: (append (subpaths a)
                          (map subpath-neg (subpaths b)))))

(define (path-neg (a <path-bounded-area>))
  (make <path-bounded-area>
        subpaths: (map subpath-neg (subpaths a))))

(define (path-add (a <path-bounded-area>)
                  (b <path-bounded-area>))
  (make <path-bounded-area>
        subpaths: (append (subpaths a)
                          (subpaths b))))


;;;

(define-method area-union ((a <path-bounded-area>)
                           (b <path-bounded-area>))
  (simplify-area (binary-area-eval a b (lambda (label) (eq? label #b1010)))))

(define-method area-intersection ((a <path-bounded-area>)
                                  (b <path-bounded-area>))
  (path-neg
   (simplify-area (binary-area-eval a b (lambda (label) (eq? label #b0101))))))

(define-method area-subtract ((a <path-bounded-area>)
                              (b <path-bounded-area>))
  (path-neg
   (simplify-area (binary-area-eval a b (lambda (label) (eq? label #b1001))))))

(define-method area-xor ((a <path-bounded-area>)
                         (b <path-bounded-area>))
  (simplify-area
   (area-subtract (area-union a b)
                  (area-intersection a b))))

;;;

(define (simplify-area (self <path-bounded-area>))
  (make <path-bounded-area>
        subpaths: (map simplify-subpath (subpaths self))))


(define (simplify-subpath (self <area-subpath>))
  (let loop ((p (path-points self))
             (r '()))
    (if (null? p)
        (let* ((l (car r))
               (z (reverse! r)))
          (make <area-subpath>
                path-points: (if (redundant-vertex? (car z) l (cadr z))
                                 (cdr z)
                                 z)))
        (if (and (pair? r)
                 (redundant-vertex? (car p)
                                    (car r)
                                    (if (pair? (cdr p))
                                        (cadr p)
                                        (car (path-points self)))))
            (loop (cdr p) r)
            (loop (cdr p) (cons (car p) r))))))

(define (redundant-vertex? (self <area-subpath-vertex>) previous next)
  (same-direction? (point- (position next)
                           (position self))
                   (point- (position self)
                           (position previous))))

(define (same-direction? (a <size>) (b <size>))
  (zero? (inner-product a (make-size (- (dy b)) (dx b)))))

(define-method print ((self <path-bounded-area>))
  (for-each (lambda (sp)
              (format #t "subpath:")
              (for-each (lambda (v)
                          (format #t " ~d,~d"
                                  (x (position v))
                                  (y (position v))))
                        (path-points sp))
              (newline))
            (subpaths self))
  self)

(define-method bbox ((self <path-bounded-area>))
  (union-rects (map bbox (subpaths self))))

(define-method bbox ((self <area-subpath>))
  (union-rects (map bbox (path-points self))))

(define-method bbox ((self <area-subpath-vertex>))
  (make-rect (x (position self))
             (y (position self))
             0 0))

(define-method bbox ((self <area-subpath-bezier-vertex>))
  (let* ((p0 (position self))
         (p1 (point+ p0 (in-handle self)))
         (p2 (point+ p0 (out-handle self))))
    ;;
    (bbox-rect (min (x p0) (x p1) (x p2))
               (min (y p0) (y p1) (y p2))
               (max (x p0) (x p1) (x p2))
               (max (y p0) (y p1) (y p2)))))
