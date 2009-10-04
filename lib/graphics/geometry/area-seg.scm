
;;;
;;;  Take a list of subpaths,
;;;  each subpath is a list of <line> or <bezier> segments,
;;;  and turn it into a <path-bounded-area>
;;;

(define-generic-function segment->subpath-vertex)
(define-generic-function transform-vertex)

(define (segments->area lst)
  (make <path-bounded-area>
        subpaths: (map segments->subpath lst)))

(define (segments->subpath lst)
  (let* (((v <vector>) (list->vector lst))
         (m (- (vector-length v) 1)))
    (let loop ((i m)
               (r '()))
      (let ((rn (cons (segment->subpath-vertex 
                       (vector-ref v (modulo (+ i m) (+ m 1)))
                       (vector-ref v i))
                      r)))
        (if (= i 0)
            (make <area-subpath>
                  path-points: rn)
            (loop (- i 1) rn))))))

(define-method segment->subpath-vertex ((prev <bezier-curve>) next)
  (if (instance? next <line>)
      (make <area-subpath-bezier-vertex>
            position: (end-point prev)
            in-handle: (point- (second-handle prev) (end-point prev))
            out-handle: $zero-size)
      (make <area-subpath-bezier-vertex>
            position: (end-point prev)
            in-handle: (point- (second-handle prev) (end-point prev))
            out-handle: (point- (first-handle next) (start-point next)))))


(define-method segment->subpath-vertex ((prev <line>) next)
  (if (instance? next <line>)
      (make <area-subpath-vertex>
            position: (to prev))
      (make <area-subpath-bezier-vertex>
            position: (to prev)
            in-handle: $zero-size
            out-handle: (point- (first-handle next) (start-point next)))))


  

(define-method area->segments ((self <rect>))
  (let ((x0 (origin-x self))
        (y0 (origin-y self))
        (w (size-width self))
        (h (size-height self)))
    (let ((x1 (+ x0 w))
          (y1 (+ y0 h)))
      (let ((p0 (make-point x0 y0))
            (p1 (make-point x1 y0))
            (p2 (make-point x1 y1))
            (p3 (make-point x0 y1)))
        ;; a single subpath
        (list 
         ;; comprised of 4 lines
         (list (make <line> from: p0 to: p1)
               (make <line> from: p1 to: p2)
               (make <line> from: p2 to: p3)
               (make <line> from: p3 to: p0)))))))

(define-method area->segments ((self <path-bounded-area>))
  (map (lambda (sp)
         (cons 'closed (subpath->segments sp)))
       (subpaths self)))

(define (subpath->segments (self <area-subpath>))
  (let loop ((l (cdr (path-points self)))
             (prev (car (path-points self)))
             (r '()))
    (if (null? l)
        (reverse! (cons (subpath-segment1 prev (car (path-points self))) r))
        (loop (cdr l)
              (car l)
              (cons (subpath-segment1 prev (car l)) r)))))

(define (subpath-segment1 a b)
  (if (instance? a <area-subpath-bezier-vertex>)
      (if (instance? b <area-subpath-bezier-vertex>)
          (make <bezier-curve>
                start-point: (position a)
                first-handle: (point+ (position a) (out-handle a))
                second-handle: (point+ (position b) (in-handle b))
                end-point: (position b))
          (make <bezier-curve>
                start-point: (position a)
                first-handle: (point+ (position a) (out-handle a))
                second-handle: (position b)
                end-point: (position b)))
      (if (instance? b <area-subpath-bezier-vertex>)
          (make <bezier-curve>
                start-point: (position a)
                first-handle: (position a)
                second-handle: (point+ (position b) (in-handle b))
                end-point: (position b))
          (make <line>
                from: (position a)
                to: (position b)))))

(define-method transform ((self <path-bounded-area>) ctm)
  (make <path-bounded-area>
        subpaths: (map (lambda (sp)
                         (transform-subpath sp ctm))
                       (subpaths self))))

(define (transform-subpath (self <area-subpath>) ctm)
  (make <area-subpath>
        path-points: (map (lambda (v)
                            (transform-vertex v ctm))
                          (path-points self))))

(define-method transform-vertex ((self <area-subpath-vertex>) ctm)
  (make <area-subpath-vertex>
        position: (transform (position self) ctm)))

(define-method transform-vertex ((self <area-subpath-bezier-vertex>) ctm)
  (make <area-subpath-bezier-vertex>
        position: (transform (position self) ctm)
        in-handle: (transform (in-handle self) ctm)     ; a `dtransform'
        out-handle: (transform (out-handle self) ctm))) ; a `dtransform'


;;;
;;;  interpret a sequence of postscript operators
;;;  represented as forms with the operator as the head,
;;;   e.g.:
;;;  
          #|
          (
           (moveto 19 706)
           (curveto 106 616 32 651 22 661)
           (lineto 674 367)
           (lineto 674 347)
           (lineto 183 139)
           (curveto 19 15 21 63 37 75)
           (lineto 0 15)
           (lineto 0 213)
           (lineto 19 213)
           (curveto 60 145 32 145 19 165)
           (curveto 99 153 86 148 72 145)
           (lineto 216 199)
           (lineto 216 461)
           (lineto 120 502)
           (curveto 53 521 67 521 93 514)
           (curveto 19 451 20 504 28 521)
           (lineto 0 451)
           (lineto 0 706)
           (closepath)
           (moveto 257 216)
           (lineto 532 331)
           (lineto 257 447)
           (closepath)
          )
          |#

(define (eval-postscript-path ops)
  (let loop ((pa '())                ; path accum
             (sp '())               ; subpath accum
             (cp #f)                ; current-point
             (l ops))
    (if (null? l)
        (make <segment-path>
              segments: (if (null? sp)
                            pa
                            (cons (reverse! sp) pa)))
        (let ((op (car l)))
          (case (car op)
            ((moveto)
             (loop pa sp (make-point (cadr op) (caddr op)) (cdr l)))
            ((lineto)
             (let ((p (make-point (cadr op) (caddr op))))
               (loop pa
                     (cons (make <line> from: cp to: p) sp)
                     p
                     (cdr l))))
            ((curveto)
             (bind ((x1 y1 x2 y2 x3 y3 (list->values (cdr op)))
                    (p (make-point x3 y3))
                    (b (make <bezier-curve>
                             start-point: cp
                             first-handle: (make-point x1 y1)
                             second-handle: (make-point x2 y2)
                             end-point: p)))
               (loop pa (cons b sp) p (cdr l))))
            ((closepath)
             (loop (cons (reverse! (if (and cp
                                            (pair? sp)
                                            (not (point=? cp (from (last sp)))))
                                       (cons (make <line>
                                                   from: cp
                                                   to: (from (last sp)))
                                             sp)
                                       sp))
                         pa)
                   '() #f (cdr l)))
            (else
             (error "postscript-path->segments: unknown postscript operator: ~s"
                    op)))))))

;;;


(define-method transform ((self <segment-path>) T)
  (make <segment-path>
        segments: (map (lambda (sp)
                         (map (lambda (seg)
                                (transform seg T))
                              sp))
                       (segments self))))

