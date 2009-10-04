(define-class <vertex> (<object>)
  (name init-value: #f)
  (position type: <point>)
  (participants type: <list> init-value: '()))

(define-method write-object ((self <vertex>) port)
  (format port "#[<vertex> ~d ~d ~a]"
          (x (position self))
          (y (position self))
          (name self)))

(define-method negative-label ((self <fixnum>))
  (logical-shift-left self 1))

(define-method list->label ((bits <fixnum>))
  bits)

(define-method list->label ((lst <list>))
  (bitwise-or
   (if (memq 'A lst) #b0001 0)
   (bitwise-or
    (if (memq '~A lst) #b0010 0)
    (bitwise-or
     (if (memq 'B lst) #b0100 0)
     (if (memq '~B lst) #b1000 0)))))

(define (label->list (l <fixnum>))
  (append (if (eq? (bitwise-and l #b0001) 0) '() '(A))
          (if (eq? (bitwise-and l #b0010) 0) '() '(~A))
          (if (eq? (bitwise-and l #b0100) 0) '() '(B))
          (if (eq? (bitwise-and l #b1000) 0) '() '(~B))))

(define-class <edge-side> (<object>) :abstract
  (label type: <fixnum>)
  owner
  (included? init-value: #f)
  (on-contour init-value: #f))

(define-class <contour> (<object>)
  (in-group init-value: #f)
  (label type: <fixnum>)
  (entry-point type: <edge-side>)
  (edges init-value: '()))              ; actually edge-sides

(define-method write-object ((self <contour>) port)
  (format port "#[<contour> ~s]" (label->list (label self))))

(define-class <edge-side-0> (<edge-side>))

(define-class <edge-side-1> (<edge-side>))

(define-method write-object ((self <edge-side-0>) port)
  (format port "#[~s:0 ~s]"
          (name (owner self))
          (label->list (label self))))

(define-method write-object ((self <edge-side-1>) port)
  (format port "#[~s:1 ~s]"
          (name (owner self))
          (label->list (label self))))

(define-class <edge> (<object>)
  (name init-value: #f)
  (from type: <vertex>)
  (to type: <vertex>)
  (side-0 type: <edge-side-0>)            ; goes from `from' to `to'
  (side-1 type: <edge-side-1>))           ; goes from `to' to `from'

(define-method arrival-vertex ((self <edge-side-0>))
  (to (owner self)))

(define-method arrival-vertex ((self <edge-side-1>))
  (from (owner self)))

(define-method departure-vertex ((self <edge-side-0>))
  (from (owner self)))

(define-method departure-vertex ((self <edge-side-1>))
  (to (owner self)))

(define (next-participant (self <vertex>) (edge <edge>))
  (let ((l (memq edge (participants self))))
    (if (pair? (cdr l))
        (cadr l)
        (car (participants self)))))
  
(define (next-edge-side (self <edge-side>))
  (let* ((t (arrival-vertex self))
         (n (next-participant t (owner self))))
    (if (eq? t (from n))
        (side-0 n)
        (side-1 n))))

(define (edge-side (self <edge>) s)
  (case s
    ((0) (side-0 self))
    ((1) (side-1 self))))

(define-method write-object ((self <edge>) port)
  (format port "#[<edge> ~s ~s=>~s]"
          (name self)
          (name (from self))
          (name (to self))))


(define-class <bezier-edge> (<edge>)
  (from-control type: <point>)
  (to-control type: <point>))

;;;

(define (remote-point (self <edge>) (p <vertex>))
  (let ((f (from self)))
    (if (eq? f p)
        (to self)
        f)))

(define (fix-up-participants! (self <vertex>))
  ;; this is only really necessary if there are more than 2
  (set-participants!
   self
   (map cdr (sort (map (lambda ((e <edge>))
                         (cons (angle-between self (remote-point e self))
                               e))
                       (participants self))
                  (lambda (a b)
                    (< (car a) (car b)))))))
  
(define $2Pi (* 2 $Pi))

(define-method angle-between ((a <vertex>) (b <vertex>))
  (let ((dy (- (y (position b))
               (y (position a))))
        (dx (- (x (position b))
               (x (position a)))))
    (atan dy dx)))


(define (edges-parallel? (e1 <edge>) (e2 <edge>))
  (same-direction? (point- (position (from e1)) (position (to e1)))
                   (point- (position (from e2)) (position (to e2)))))

(define (edges-coincident? (e1 <edge>) (e2 <edge>))
  (and (edges-parallel? e1 e2)
       (same-direction? (point- (position (from e2)) (position (to e1)))
                        (point- (position (from e1)) (position (to e1))))))

(define (angle-between-edges (e1 <edge-side>) (e2 <edge-side>))
  (let* ((d1 (point- (position (arrival-vertex e1))
                     (position (departure-vertex e1))))
         (d2 (point- (position (arrival-vertex e2))
                     (position (departure-vertex e2))))
         (p (inner-product (normalize d1) (normalize d2))))
    ;(format #t "~s => ~s   DOT => ~s\n" d1 d2 p)
    (acos p)))


;;;

(define (make-vertex x y name)
  (make <vertex>
    name: name
    position: (make-point x y)))

(define (unlink-edge! (self <edge>))
  (let ((f (from self))
        (t (to self)))
    (set-participants! f (delq self (participants f)))
    (set-participants! t (delq self (participants t)))
    self))


(define (make-edge f t name labels-0 labels-1)
  (let ((s0 (make <edge-side-0>
              label: (list->label labels-0)
              owner: #f))
        (s1 (make <edge-side-1>
              label: (list->label labels-1)
              owner: #f)))
    (let ((e (make <edge>
               name: name
               from: f
               to: t
               side-0: s0
               side-1: s1)))
      (set-participants! f (cons e (participants f)))
      (set-participants! t (cons e (participants t)))
      (set-owner! s0 e)
      (set-owner! s1 e)
      e)))

;;;


;;;

(define (make-contour-from (self <edge-side>))
  (let loop ((edges (list self))
             (p self)
             (l (label self)))
    (assert (not (included? p)))
    (set-included?! p #t)
    ;(format #t "  contour edge: ~s\n" p)
    (let ((n (next-edge-side p)))
      (if (eq? n self)
          (make <contour>
            label: l
            entry-point: self
            edges: (reverse! edges))
          (loop (cons n edges) 
                n
                (bitwise-or l (label n)))))))

;;;  Find all the contours given a set of edges

(define (crunch-contours lst)
  (let ((out '()))
    (for-each 
     (lambda ((e <edge>))
       (if (not (included? (side-0 e)))
           (set! out (cons (make-contour-from (side-0 e)) out)))
       (if (not (included? (side-1 e)))
           (set! out (cons (make-contour-from (side-1 e)) out))))
     lst)
    out))

;;;

(define (sum-angles (from <vertex>) (to <contour>))
  (let loop ((sum 0)
             (lst (edges to))
             (a (angle-between from (departure-vertex (car (edges to))))))
    (if (null? lst)
        sum
        (let ((c2 (angle-between from (arrival-vertex (car lst)))))
          #|
          (format #t "~s => ~s ... ~s ~s         ~s\n"
                  (departure-vertex (car lst))
                  (arrival-vertex (car lst))
                  a
                  c2
                  (- c2 a))
          |#
          (loop (+ sum (- c2 a)) 
                (cdr lst)
                c2)))))
  
(define (det a b c d)
  (- (* a d) (* b c)))

(define (cross (a <vertex>) (b <vertex>))
  (bind ((u1 u2 (point->values (position a)))
         (v1 v2 (point->values (position b))))
    (det u1 u2 v1 v2)))

(define (dot (a <vertex>) (b <vertex>))
  (bind ((u1 u2 (point->values (position a)))
         (v1 v2 (point->values (position b))))
    (+ (* u1 v1)
       (* u2 v2))))

(define (angle-between-edge-sides (a <edge-side>) (b <edge-side>))
  (bind ((c (position (arrival-vertex a)))
         (da (normalize (point- c (position (departure-vertex a)))))
         (db (normalize (point- (position (arrival-vertex b)) c))))
    (acos (inner-product da db))))

(define (normal-vector (self <edge-side>))
  (let ((f (position (departure-vertex self)))
        (t (position (arrival-vertex self))))
    (let ((d (point- t f)))
      (make-point (dy d) (- (dx d))))))
    
(define (wiggle (v <vertex>))
  (make <vertex>
    position: (point+ (position v)
                      (make-size (/ (- (random 999) 499) 1000.0)
                                 (/ (- (random 999) 499) 1000.0)))
    name: (name v)))

;;; Compute the winding number of the contour `to' around
;;; the point `from'.
;;;
;;;  <0 => `to' winds CW around `from'
;;;   0 => `from' is outside of `to'
;;;  >0 => `to' winds CCW around `from'

(define (winding-number (from <vertex>) (to <contour>))
  ;; use an infinite ray in the (1,0) direction (positive x axis)
  ;; from the `from' point
  (let loop ((lst (edges to))
             (w 0))
    (if (null? lst)
        w
        (let* ((n (normal-vector (car lst)))
               (a (point- (position (departure-vertex (car lst)))
                          (position from)))
               (b (point- (position (arrival-vertex (car lst)))
                          (position from)))
               (ap (if (> (dy a) 0) 'above 'below))
               (bp (if (> (dy b) 0) 'above 'below)))
          #|
          (format #t "~s ... normal = ~s\n" (car lst) n)
          (format #t "       ~s ~a\n" a ap)
          (format #t "       ~s ~a\n" b bp)
          |#
          (if (not (eq? ap bp))
              ;; if the edge goes from above to below the ray,
              ;; then figure out where it hits the x axis by
              ;; first computing the intersection parameter `t'
              (let* ((t (/ (dy a)
                           (- (dy a) (dy b))))
                     (xx (+ (* (dx a) (- 1 t))
                            (dx b))))
                ;(format #t "\t\t~a ==> x @ ~a\n" t xx)
                (if (> xx 0)
                    (loop (cdr lst)
                          (if (eq? ap 'above)
                              (- w 1)
                              (+ w 1)))
                    (loop (cdr lst) w)))
              (loop (cdr lst) w))))))

(define (unit-square)
  (let ((p0 (make-vertex 0 0 'p0))
        (p1 (make-vertex 1 0 'p1))
        (p2 (make-vertex 1 1 'p2))
        (p3 (make-vertex 0 1 'p3)))
    (bind ((a (make-edge p0 p1 'a '(~A) '(A)))
           (b (make-edge p1 p2 'b '(~A) '(A)))
           (c (make-edge p2 p3 'c '(~A) '(A)))
           (d (make-edge p3 p0 'd '(~A) '(A))))
      ;
      (fix-up-participants! p0)
      (fix-up-participants! p1)
      (fix-up-participants! p2)
      (fix-up-participants! p3)
      ;;
      (make <contour>
        label: #b10
        entry-point: (side-0 a)
        edges: (list (side-0 a)
                     (side-0 b)
                     (side-0 c)
                     (side-0 d))))))

(define (unit-square-cw)
  (let ((p0 (make-vertex 0 0 'p0))
        (p1 (make-vertex 1 0 'p1))
        (p2 (make-vertex 1 1 'p2))
        (p3 (make-vertex 0 1 'p3)))
    (bind ((a (make-edge p0 p1 'a '(~A) '(A)))
           (b (make-edge p1 p2 'b '(~A) '(A)))
           (c (make-edge p2 p3 'c '(~A) '(A)))
           (d (make-edge p3 p0 'd '(~A) '(A))))
      ;
      (fix-up-participants! p0)
      (fix-up-participants! p1)
      (fix-up-participants! p2)
      (fix-up-participants! p3)
      ;;
      (make <contour>
        label: #b01
        entry-point: (side-1 d)
        edges: (list (side-1 d)
                     (side-1 c)
                     (side-1 b)
                     (side-1 a))))))
;;;

(define (contour-area (self <contour>))
  ;; <http://astronomy.swin.edu.au/~pbourke/geometry/clockwise/>
  (let loop ((area 0)
             (lst (edges self)))
    (if (null? lst)
        (/ area 2)
        (let* ((a (departure-vertex (car lst)))
               (b (arrival-vertex (car lst)))
               (c (- (* (x (position a))
                        (y (position b)))
                     (* (x (position b))
                        (y (position a))))))
          ;(format #t "~s  -->  ~s    contrib ~s\n" a b c)
          (loop (+ area c)
                (cdr lst))))))

;;;

(define-method ccw? ((self <contour>))
  (>= (contour-area self) 0))

(define (starting-vertex (self <contour>))
  (departure-vertex (entry-point self)))

;;;

;;;(define cl (crunch-contours (t)))

;;;
;;; Definition:
;;;   A contour group is a set of contours which
;;;   are interconnected through edges and vertices
;;;   (i.e., the closure of reachability via edges
;;;   and vertices)
;;;
;;; Conjecture 1:
;;;    Each contour group has exactly one contour
;;;    which is oriented CCW
;;;
;;; Corollary:
;;;    For simple stuff, the CCW contour is labelled (~A), (~B), or (~A ~B)
;;;
;;; Conjecture 2:
;;;    The CCW contour in every contour group contains
;;;    all the other contours, which in turn are all siblings
;;;    of each other.
;;;
;;; Conjecture 3:
;;;    For any given contour group, the `inside' contours are disjoint
;;;    the union of the interior contours is equal to the `outside'
;;;    contour.  In other words, the inside contours form a partition
;;;    of the space bounded by the outside contour.

(define-class <contour-group> (<object>)
  (id init-value: #f)
  (outside init-value: #f)
  ;; `inside' is a list of pairs; each pair
  ;; has as it's car a CW <contour> that is inside the outside contour,
  ;; and as it's cdr a list (possibly empty) of contour groups
  ;; that are fully contained inside that contour
  (inside type: <list> init-value: '()))

(define-method write-object ((self <contour-group>) port)
  (format port "#[<cg> ~s {~s}]" (id self) (outside self)))

(define (make-contour-group (start <contour>))
  (let ((g (make <contour-group>))
        (q (make-dequeue))
        (cx (make-object-table))
        (vx (make-object-table)))
    ;;
    (define (visit (v <vertex>))
      (if (not (table-lookup vx v))
          (begin
            ;(format #t "visiting: ~s\n" v)
            (table-insert! vx v #t)
            (dequeue-push-back! q v)
            (values))))
    ;;
    (visit (departure-vertex (entry-point start)))
    ;;
    (let loop ()
      (if (dequeue-empty? q)
          (finish-laying-out-contour-group g (key-sequence cx))
          (let ((v (dequeue-pop-front! q)))
            (for-each (lambda ((c <contour>))
                        #|(if (not (table-lookup cx c))
                            (format #t "  including: ~s\n" c))|#
                        (table-insert! cx c #t))
                      (contours-intersecting-vertex v))
            (for-each visit (reachable-from-vertex v))
            (loop))))))

(define (finish-laying-out-contour-group (g <contour-group>) contours)
  (for-each (lambda ((c <contour>))
              (set-in-group! c g))
            contours)
  ;;
  (let ((o (select ccw? contours)))
    (assert (= (length o) 1))   ; Conjecture: exactly one CCW
    (set-inside! g (map (lambda (c)
                          (cons c '()))
                        (delq (car o) contours)))
    (set-outside! g (car o))
    ;;
    g))
  
(define (contours-intersecting-vertex (self <vertex>))
  (append! 
   (map (lambda ((e <edge>))
          (on-contour (side-0 e)))
        (participants self))
   (map (lambda ((e <edge>))
          (on-contour (side-1 e)))
        (participants self))))
           

;;; return a list of all vertices reachable from a given vertex

(define (reachable-from-vertex (self <vertex>))
  (append!
   (map (lambda ((e <edge>)) (to e)) (participants self))
   (map (lambda ((e <edge>)) (from e)) (participants self))))

(define (build-contour-groups contours)
  (let ((g '()))
    ;; set up edge->contour pointers
    (for-each
     (lambda ((c <contour>))
       (for-each
        (lambda ((e <edge-side>))
          (assert (not (on-contour e)))
          (set-on-contour! e c))
        (edges c)))
     contours)
    ;;
    (for-each 
     (lambda (c)
       (if (not (in-group c))
           (set! g (cons (make-contour-group c) g))))
     contours)
    ;;
    (for-each set-id! g (range (length g)))
    g))

;;;
;;;  Determine the relationship between contour groups
;;;

;;; Return #t iff the contour group `inner' is contained
;;; inside the contour group `outer' (which is to say, the
;;; `outer' group contains the `inner' group)

(define-generic-function contains?)

(define-method contains? ((outer <contour-group>) (inner <contour-group>))
  (contains? (outside outer) inner))

(define-method contains? ((outer <contour>) (inner <contour-group>))
  (let ((inner-pt (from (owner (entry-point (outside inner))))))
    (not (zero? (winding-number inner-pt outer)))))
  
(define (organize-contour-groups contour-groups)
  ;; [Foley, van Dam, et.al., p.944]
  ;; use a queue to process the contour groups, because we may
  ;; have to visit some more than once (i.e., until it is contained
  ;; by either 0 or 1 other unassigned ones)
  (let ((outermost '())
        (q (make-dequeue)))
    ;; fill the queue
    (for-each
     (lambda ((a <contour-group>))
       (dequeue-push-back! q a)
       (values))
     contour-groups)
    ;;
    (let loop ()
      (if (dequeue-empty? q)
          outermost
          (let* (((a <contour-group>) (dequeue-pop-front! q))
                 (is-inside-list (select (lambda ((c <contour-group>))
                                           (contains? c a))
                                         (vector->list (dequeue-state q)))))
            ;(format #t "PROCESSING ~s\n" a)
            ;(print is-inside-list)
            ;(newline)
            ;
            (case (length is-inside-list)
              ((1)
               (add-sub-group! (car is-inside-list) a))
              ((0)
               (set! outermost (add-sub-group* outermost a)))
              (else
               (dequeue-push-back! q a)))
            (loop))))))

#|      
     (let ((a <contour-group>))
       (let loop ((c contour-groups))
         (if (null? c)
             ;; not contained in anything...
             (set! outermost (cons a outermost))
             (if (eq? a (car c))
                 ;; keep looking
                 (loop (cdr c))
                 (if (contains? (car c) a)
                     ;; contained in (car c)
                     (add-sub-group! (car c) a)
                     ;; keep looking
                     (loop (cdr c)))))))
     contour-groups)
    outermost))
|#

;;; Once it has been established that `sub' is inside the outside
;;; of `self', then we know it must be inside one of the interior
;;; contours; we just have to find which one and add it there.
;;;
;;; Hopefully, it's more efficient to test against the outside
;;; contour before going and testing against the inside contours
;;; Worst case, we do two tests, best case is we avoid making a
;;; whole bunch of tests (well, really the worst case is that I'm
;;; wrong about Conjecture 3 and this doesn't even work correctly!)

(define (add-sub-group! (self <contour-group>) (sub <contour-group>))
  (let loop ((s (inside self)))
    (assert (pair? s))  ; we'd better find it somewhere in here!
    (let ((entry (car s)))
      (if (contains? (car entry) sub)
          ;; we found the inside contour that `sub' is inside...
          ;; check to see if `sub' is actually inside one of the
          ;; contour group already found to be inside that contour
          (let sub-sub-check ((l (cdr entry)))
            (if (null? l)
                (begin
                  (set-cdr! entry (cons sub (cdr entry)))
                  (values))
                (if (contains? (car l) sub)
                    (add-sub-group! (car l) sub)
                    (sub-sub-check (cdr l)))))
          (loop (cdr s))))))

(define (add-sub-group* (ilist <list>) (sub <contour-group>))
  (let loop ((s ilist))
    (if (null? s)
        (cons sub ilist)
        (if (contains? (car s) sub)
            (begin
              (add-sub-group! (car s) sub)
              ilist)
            (loop (cdr s))))))

;;;

(define (inset-contour (self <contour>) distance)
  (let* ((evec (list->vector (edges self)))
         (n (vector-length evec))
         (vertices (list->vector
                    (map departure-vertex (edges self)))))
    ;;
    (for-each
     (lambda (i)
       (let* (((e1 <edge-side>) (vector-ref evec (modulo (+ i -1 n) n)))
              ((e2 <edge-side>) (vector-ref evec i))
              (n1 (normalize (point->size (normal-vector e1))))
              (n2 (normalize (point->size (normal-vector e2))))
              (a (angle-between-edge-sides e1 e2))
              (u (normalize (size+ n1 n2)))
              (s (/ distance (sin (- (/ $Pi 2) (/ a 2)))))
              (v (vector-ref vertices i))
              (w (point+ (position v) (size* u s))))
         #|(format #t "~s / ~s ==> angle ~s s: ~s ... ~s\n" 
                 e1 e2
                 (* 180 (/ a $Pi))
                 s 
                 w)|#
         ;; note: this is only correct for right angles!
         (vector-set! vertices
                      i
                      (make <vertex>
                        position: w
                        name: (name v)))))
     (range n))
    ;;
    (let ((e (make-vector n #f)))
      (for-each
       (lambda (i)
         (vector-set! e i (make-edge
                           (vector-ref vertices i)
                           (vector-ref vertices (modulo (+ i 1) n))
                           (name (owner (vector-ref evec i)))
                           (label (vector-ref evec i))
                           '())))
       (range n))
      ;;
      (vector-for-each fix-up-participants! vertices)
      ;;
      (make <contour>
        label: (label self)
        entry-point: (side-0 (vector-ref e 0))
        edges: (map side-0 (vector->list e))))))

;;;


;;;
;;;  integrate my-weiler.scm with region.scm
;;;

(define-class <vertex-index> (<object>)
  (vertices init-value: '()))

(define (areas->edge-list (area-alist <list>))
  (let ((vx (make <vertex-index>))
        (l '()))
    ;;
    (values
     (map
      (lambda (area-assoc)
        (let ((name (car area-assoc))
              (area (cdr area-assoc)))
          (apply append
                 (map
                  (lambda ((subpath <area-subpath>))
                    (generate-subpath-edges 
                     (if (fixnum? name)
                         name
                         (list->label (list name)))
                     vx 
                     subpath))
                  (subpaths area)))))
      area-alist)
     vx)))

;;;

(define (point=? a b)
  (and (= (x a) (x b))
       (= (y a) (y b))))
  
(define (alloc-vertex (vx <vertex-index>) (p <point>))
  (let loop ((l (vertices vx)))
    (if (null? l)
        (let ((v (make-vertex (x p) (y p) 
                              (symbol-append 'p (length (vertices vx))))))
          (set-vertices! vx (cons v (vertices vx)))
          v)
        (if (point=? (position (car l)) p)
            (car l)
            (loop (cdr l))))))
;;;

(define (generate-subpath-edges inner-label vx (subpath <area-subpath>))
  (let* ((pp (list->vector (map (lambda (p)
                                  (alloc-vertex vx (position p)))
                                (path-points subpath))))
         (n (vector-length pp))
         (edges (make-vector n #f)))
    ;;
    (for-each
     (lambda (i)
       (vector-set! edges
                    i
                    (make-edge
                     (vector-ref pp i)
                     (vector-ref pp (modulo (+ i 1) n))
                     (symbol-append 'E (car (label->list inner-label)))
                     (negative-label inner-label)
                     inner-label))) ;(list inner-label)
     (range n))
    ;;
    (vector->list edges)))


;;;

(define-method intersect-edges ((a <edge>) (b <edge>))
  (bind ((p h t (line-intersect (position (from a))
                                (position (to a))
                                (position (from b))
                                (position (to b)))))
    (if p
        (values p t h)
        (values))))

(define (coincident-intersects (a <point>) (b <point>) (c <point>) (d <point>))
  (let* ((n (point- d c))
         (den (inner-product n (point- a b)))
         (num1 (inner-product n (point- a c)))
         (num2 (inner-product n (point- a d))))
    (values (/ num1 den)
            (/ num2 den))))

;;;

(define (coincident-edges* (a <edge>) (b <edge>) t0 t1)
  ;; returns 4 values, a0, a1, b0, b1, characterizing 
  ;; where `a' and `b' need to be split
  ;; in order to crack the coincident edges
  ;;
  ;;   a0 = a boundary of `b' that is the first new point on `a', or #f
  ;;   a1 = a boundary of `b' that is the second new point on `a', or #f
  ;;   b0 = a boundary of `a' that is the first new point on `b', or #f
  ;;   b1 = a boundary of `a' that is the second new point on `b', or #f

  #|
  (format #t "(Detected coincident edges: ~s ~s  at t0=~s and t1=~s)\n"
          a b t0 t1)
  |#
  (cond
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ((< t0 0)                            ; Cases 1-6
    (cond
     ((<= t1 0)                         ; Cases 1-3
      (values))
     ((< t1 1)                          ; Case 4
      (values (to b) #f (from a) #f))
     ((= t1 1)                          ; Case 5
      (values #f #f (from a) #f))
     (else                              ; Case 6
      (values #f #f (from a) (to a)))))
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ((= t0 0)                            ; Cases 7-11
    (cond
     ((<= t1 0)                         ; Cases 7,8
      (values))
     ((< t1 1)                          ; Case 9
      (values (to b) #f #f #f))
     ((= t1 1)                          ; Case 10
      (values))
     (else                              ; Case 11
      (values #f #f (to a) #f))))
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ((< t0 1)                            ; Cases 12-17
    (cond
     ((< t1 0)                          ; Case 12
      (values (from b) #f (from a) #f))
     ((= t1 0)                          ; Case 13
      (values (from b) #f #f #f))
     ((< t1 1)                          ; Case 14, 15
      (if (< t0 t1)             
          (values (from b) (to b) #f #f); Case 14
          (values (to b) (from b) #f #f))); Case 15
     ((= t1 1)                          ; Case 16
      (values (from b) #f #f #f))
     (else                              ; Case 17
      (values (from b) #f (to a) #f))))
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ((= t0 1)                            ; Cases 18-22
    (cond
     ((< t1 0)                          ; Case 18
      (values #f #f (from a) #f))
     ((= t1 0)                          ; Case 19
      (values))
     ((< t1 1)                          ; Case 20
      (values (to b) #f #f #f))
     (else                              ; Cases 21, 22
      (values))))
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   (else                                ; Cases 23-28
    (cond
     ((< t1 0)                          ; Case 23
      (values #f #f (to a) (from a)))
     ((= t1 0)                          ; Case 24
      (values #f #f (to a) #f))
     ((< t1 1)                          ; Case 25
      (values (to b) #f (to a) #f))
     (else                              ; Cases 26, 27, 28
      (values))))))

#|
  ;; `t0' is where on A it is that B.from sits
  ;; `t1' is where on A it is that B.to sits
  (cond                                 ; A     |--------------|
   ((and (= t0 0) (= t1 1))             ; B     *-------------->        
    (values #f #f #f #f))
   ((and (= t0 1) (= t1 0))             ; B     <--------------*
    (values #f #f #f #f))
   ;;
   ((and (= t0 0) (< t1 0))             ; B  <--*
    (values #f #f #f #f))
   ((and (= t0 1) (> t1 1))             ; B                    *-->
    (values #f #f #f #f))
   ((and (= t0 0) (< t1 1))             ; B     *-------->
    (values (to b) #f #f #f))
   ((and (= t0 0) (> t1 1))             ; B     *----------------->
    (values #f #f (to a) #f))
   ((and (= t0 1) (> t1 0))             ; B            <-------*
    (values (to b) #f #f #f))
                                        ;
   ((and (= t1 0) (> t0 1))             ; B     <----------------*
    (values #f #f (to a) #f))
   ((and (= t1 0) (< t1 1))             ; B     <-------*
    (values (from b) #f #f #f))
   ;;
   ((and (< t0 0) (> t1 1))             ; B   *------------------>
    (values #f #f (from a) (to a)))
   ((and (< t1 0) (> t0 1))             ; B   <------------------*
    (values #f #f (to a) (from a)))
   ((and (< t0 1) (> t1 1))             ; B             *-------->
    (values (from b) #f (to a) #f))
   ((and (< t1 1) (> t0 1))             ; B             <--------*
    (values (to b) #f (to a) #f))
   ((and (< t0 1) (< t1 1))             ; B         *------*
    (if (< t0 t1)                       
        (values (from b) (to b) #f #f)  ; B         *------>
        (values (to b) (from b) #f #f))); B         <------*
   ((and (< t0 0) (< t1 1))             ; B   *------->
    (values (to b) #f (from a) #f))
   ((and (< t1 0) (< t0 1))             ; B   <-------*
    (values (from b) #f (from a) #f))
   (else
    (values))))
|#

(define (coincident-edges (a <edge>) (b <edge>))
  ;; For case numberings, run `(test-jig coincidences)'
  (if (edges-coincident? a b)
      (bind ((t0 t1 (coincident-intersects (position (from a))
                                           (position (to a))
                                           (position (from b))
                                           (position (to b)))))
        (coincident-edges* a b t0 t1))
      (values)))
      
(define (crack-edges-when-coincident (a <edge>) a0 a1
                                     (b <edge>) b0 b1)
  (let ((a-0 (label (side-0 a)))
        (a-1 (label (side-1 a)))
        (b-0 (label (side-0 b)))
        (b-1 (label (side-1 b))))
    (cond
     ((and a0 a1)
      (unlink-edge! a)
      (values (make-edge (from a) a0     'CA1/3 a-0 a-1)
              (make-edge a0       a1     'CA2/3 a-0 a-1)
              (make-edge a1       (to a) 'CA3/3 a-0 a-1)
              b
              #f
              #f))
     ((and b0 b1)
      (unlink-edge! b)
      (values a
              #f
              #f
              (make-edge (from b) b0     'CB1/3 b-0 b-1)
              (make-edge b0       b1     'CB2/3 b-0 b-1)
              (make-edge b1       (to b) 'CB3/3 b-0 b-1)))
     ((and a0 b0)
      (unlink-edge! a)
      (unlink-edge! b)
      (values (make-edge (from a) a0     'CA1/2 a-0 a-1)
              (make-edge a0       (to a) 'CA2/2 a-0 a-1)
              #f
              (make-edge (from b) b0     'CB1/2 b-0 b-1)
              (make-edge b0       (to b) 'CB2/2 b-0 b-1)
              #f))
     (a0
      (assert (not b0))
      (unlink-edge! a)
      (values (make-edge (from a) a0     'CA1/2 a-0 a-1)
              (make-edge a0       (to a) 'CA2/2 a-0 a-1)
              #f
              b
              #f
              #f))
     (b0
      (assert (not a0))
      (unlink-edge! b)
      (values a
              #f
              #f
              (make-edge (from b) b0     'CB1/2 b-0 b-1)
              (make-edge b0       (to b) 'CB2/2 b-0 b-1)
              #f)))))
              
              
              
     

;;;  Given two lists of edges 
;;;  (each representing a distinct normalized polygon),
;;;  produce a single list of edges with new vertices
;;;  as necessary to account for intersections, and any
;;;  coincident edges deleted
;;;
;;;  The inputs are normalized, meaning they have
;;;  no self-intersections.

(define (unify-edges (a <list>) (b <list>) (vx <vertex-index>))
  #|
  (format #t "=========== UNIFY ===========\n")
  (for-each print a)
  (format #t ".............................\n")
  (for-each print b)
  (format #t "-----------------------------\n")
  |#
  ;;
  (let ((b (append b '())))     ; make a copy of the b list
    ;;
    (define (find-any-intersection (e <edge>) excluding)
      (let loop ((b b))
        (if (null? b)
            (values)
            (if (memq (car b) excluding)
                (loop (cdr b))
                (bind ((p t h (intersect-edges (car b) e)))
                  (if (and p (> t 0) (< t 1))
                      (values (alloc-vertex vx p) (car b))
                      (if (and p (= t 0))
                          (values (from (car b)) (car b))
                          (if (and p (= t 1))
                              (values (to (car b)) (car b))
                              (loop (cdr b))))))))))
    ;;;
    (define (find-any-coincident (e <edge>) excluding)
      (let loop ((b b))
        (if (null? b)
            (values)
            (if (memq (car b) excluding)
                (loop (cdr b))
                (bind ((e0 e1 b0 b1 (coincident-edges e (car b))))
                  (if (or e0 b0)
                      (values (car b) e0 e1 b0 b1)
                      (loop (cdr b))))))))
    ;;;
    (define (process-all-intersections (e <edge>) excluding)
      ;; handle coincident edges
      (bind ((bx e0 e1 b0 b1 (find-any-coincident e excluding)))
        ;; if bx,... are returned,
        ;;   bx = the <edge> from the `b' list that is coincident with another
        ;;   e0 = where `e' needs to be split (a boundary of bx), or #f
        ;;   e1 = another place where `e' needs to be split, or #f
        ;;   b0 = where `bx' needs to be split (a boundary of e), or #f
        ;;   b1 = another place where `bx' needs to be split, or #f
        #|
        (format #t "  found coincidents of ~s : ~s ~s ~s ~s ~s\n"
                e bx e0 e1 b0 b1)
        |#
        (if (or e0 b0)  ; if anybody needs splitting...
            (bind ((a0 a1 a2 b0 b1 b2 (crack-edges-when-coincident e e0 e1
                                                                   bx b0 b1))
                   (new-excludes (append (if b0 (list b0) '())
                                         (if b1 (list b1) '())
                                         (if b2 (list b2) '())
                                         excluding)))
              #|
              (format #t "Crack coincident\n")
              (format #t "     ~s ~s\n" e bx)
              (format #t " ==>\n")
              (format #t "     ~s ~s ~s\n" a0 a1 a2)
              (format #t "     ~s ~s ~s\n" b0 b1 b2)
              |#
              ;;
              (if (and b1 b2)
                  (set! b (cons* b0 b1 b2 (delq! bx b)))
                  (if b1
                      (set! b (cons* b0 b1 (delq! bx b)))))
              ;;
              (if a1
                  (if a2
                      (append
                       (process-all-intersections a0 new-excludes)
                       (process-all-intersections a1 new-excludes)
                       (process-all-intersections a2 new-excludes))
                      (append
                       (process-all-intersections a0 new-excludes)
                       (process-all-intersections a1 new-excludes)))
                  (process-all-intersections a0 new-excludes)))
            ;; no coincident edges; handle intersecting edges
            (bind ((p b-edge (find-any-intersection e excluding)))
              (if p
                  (bind ((a0 a1 b0 b1 (crack-edges-at-intersection e b-edge
                                                                   p)))
                    ;; (a1 or b1 may be #f, if the output edge is the same
                    ;; as the input edge)
                    (if (not b1) (assert (eq? b0 b-edge)))
                    (if (not a1) (assert (eq? a0 e)))
                    ;; remove the b-edge from the b list,
                    ;; replacing it with two more (b0 and b1)
                    (if b1
                        (set! b (cons* b0 b1 (delq! b-edge b))))
                    (let ((new-excludes (if b1
                                            (cons* b0 b1 excluding)
                                            (cons b0 excluding))))
                      ;;
                      (if a1
                          (append
                           (process-all-intersections a0 new-excludes)
                           (process-all-intersections a1 new-excludes))
                          (process-all-intersections a0 new-excludes))))
                  (list e))))))
    ;;
    ;; find intersections
    (let loop ((a a)
               (result '()))
      ;(format #t "checking ~s\n" a)
      (if (null? a)
          (eliminate-coincident-edges (append b result))
          (loop (cdr a)
                (append (process-all-intersections (car a) '())
                        result))))))

;;;

(define (eliminate-coincident-edges edgelist)
  ;(format #t "============= ELIMINATE COINCIDENT ============\n")
  ;(for-each print edgelist)
  ;(format #t "-----------------------------------------------\n")
  ;; inefficient for now
  (let loop ((e edgelist)
             (r '()))
    (if (null? e)
        (begin
          ;(format #t "\nRESULT:\n\n")
          ;(for-each print r)
          ;;
          r)
        (loop (cdr e) (merge-duplicate-edge (car e) r)))))

(define (merge-duplicate-edge (src <edge>) lst)
  (let loop ((l lst))
    (if (null? l)
        (cons src lst)          ; not a dup; include it
        (let* ((t (car l))
               (q (edge-cmp t src)))
          ;; merge their labels, but we have to figure 
          ;; out which side is which
          (case q
            ((1)        ; same sides
             (set-label! (side-0 t) (bitwise-or (label (side-0 t))
                                                (label (side-0 src))))
             (set-label! (side-1 t) (bitwise-or (label (side-1 t))
                                                (label (side-1 src))))
             (unlink-edge! src)
             lst)
            ((-1)       ; opposite sides
             (set-label! (side-0 t) (bitwise-or (label (side-0 t))
                                                (label (side-1 src))))
             (set-label! (side-1 t) (bitwise-or (label (side-1 t))
                                                (label (side-0 src))))
             (unlink-edge! src)
             lst)
            (else
             (loop (cdr l))))))))
             

        
(define (edge-cmp (a <edge>) (b <edge>))
  (cond
   ((and (eq? (from a) (from b)) (eq? (to a) (to b)))
    1)
   ((and (eq? (from a) (to b)) (eq? (to a) (from b)))
    -1)
   (else
    #f)))
      
  
;;;

(define (crack-edges-at-intersection (a <edge>) (b <edge>) (x <vertex>))
  (cond
   ((and (or (eq? x (from a)) (eq? x (to a)))
         (or (eq? x (from b)) (eq? x (to b))))
    ;;; (case 0) vertex-to-vertex intersection; the vertex x is
    ;;;          already an endpoint of both edges, so no changes
    ;;;          are necessary
    (values a #f b #f))
   ;;
   ((or (eq? x (from a)) (eq? x (to a)))
    ;;; (case 1) vertex `x' is already an endpoint of a; only need to split b
    (unlink-edge! b)
    (values a
            #f
            (make-edge (from b) x 'B1 (label (side-0 b)) (label (side-1 b)))
            (make-edge x (to b)   'B2 (label (side-0 b)) (label (side-1 b)))))
   ((or (eq? x (from b)) (eq? x (to b)))
    ;;; (case 2) vertex `x' is already an endpoint of b; only need to split a
    (unlink-edge! a)
    (values (make-edge (from a) x 'A1 (label (side-0 a)) (label (side-1 a)))
            (make-edge x (to a)   'A2 (label (side-0 a)) (label (side-1 a)))
            b
            #f))
   (else
    ;;; (case 5) transverse intersection; the vertex x is not
    ;;;          at the endpoint of either edge, so both need
    ;;;          to be split
    (unlink-edge! a)
    (unlink-edge! b)
    ;;
    (values
     (make-edge (from a) x 'A1 (label (side-0 a)) (label (side-1 a)))
     (make-edge x (to a)   'A2 (label (side-0 a)) (label (side-1 a)))
     (make-edge (from b) x 'B1 (label (side-0 b)) (label (side-1 b)))
     (make-edge x (to b)   'B2 (label (side-0 b)) (label (side-1 b)))))))

;;;

#|
   Debugging facilities for contour subsystem

(define-method render-contour ((self <path-bounded-area>) dev)
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

(define-method render-contour ((self <contour>) dev)
  (moveto dev (position (departure-vertex (entry-point self))))
  (let loop ((e (edges self)))
    (if (null? e)
        (closepath dev)
        (begin
          (lineto dev (position (arrival-vertex (car e))))
          (loop (cdr e))))))

(define (show-contour dev (self <contour>) color)
  (setcolor dev (device-color dev color))
  (render-contour self dev)
  (stroke dev)
  (flush dev))

(define (show-results dev cl)
  (let ((r (device-color dev dark-red))
        (b (device-color dev light-blue)))
    (for-each
     (lambda ((c <contour>))
       (if (ccw? c)
           (setcolor dev r)
           (setcolor dev b))
       (render-contour (inset-contour c 0.1) dev)
       (stroke dev))
     cl)
    (flush dev)))

|#

;;;

(define (label-accum inherited current)
  (bitwise-or
   ;; use the `inherited' bits if nothing is specified in the `current'
   (if (eq? 0 (bitwise-and current #b0011))
       (bitwise-and inherited #b0011)
       (bitwise-and current #b0011))
   (if (eq? 0 (bitwise-and current #b1100))
       (bitwise-and inherited #b1100)
       (bitwise-and current #b1100))))


;;; The `accept?' procedure works as follows:
;;;
;;;   The contours from both input paths are labeled
;;;      0001  ==> A
;;;      0100  ==> B
;;;
;;;   Then the "outside" edges are added, labeled as follows
;;;      0010  ==> not A
;;;      1000  ==> not B
;;;
;;;   Then, coincident edges are merged and their labels
;;;   bitwise-or'd together.  Hence, a label of 0101 means
;;;   the edge is part of both A and B, while 1001 means
;;;   it is part of A but not part of B.
;;;
;;;   The accept? procedure decides whether to accept an edge
;;;   based on it's label.

(define (binary-area-eval (a <path-bounded-area>) 
                          (b <path-bounded-area>)
                          accept?)
  (let ((sp '()))
    ;;
    (define (contribute-subpath (c <contour>))
      (let ((vl (map
                 (lambda ((e <edge-side>))
                   (make <area-subpath-vertex>
                     position: (position (departure-vertex e))))
                 (edges c))))
        (set! sp (cons (make <area-subpath> path-points: vl) sp))))
    ;;
    #|
    (define (d c inherit)
      (format #t "~s + ~s ==> ~s\n"
              c
              (label->list inherit)
              (accept? (label-accum inherit (label c)))))
    |#
    ;;
    ;; `inherit' is the contribution to our label from the fact
    ;; that we are completely contained in another contour
    (define (process-group (g <contour-group>) inherit)
      #|
      (format #t "----------------- group ~s ------------- inherited ~s\n"
              g (label->list inherit))
      (d (outside g) inherit)
      |#
      (if (accept? (label-accum inherit (label (outside g))))
          (contribute-subpath (outside g)))
      (for-each 
       (lambda (i)
         ;(d (car i) inherit)
         (let ((adj-label (label-accum inherit (label (car i)))))
           (if (accept? adj-label)
               (contribute-subpath (car i)))
           (for-each 
            (lambda (s)
              (process-group s adj-label))
            (cdr i))))
       (inside g)))
    ;;
    ;; top-level contour groups...
    ;;
    (for-each (lambda (g)
                (process-group g #b1010))
              (compute-two-areas a b))
    ;;
    (make <path-bounded-area>
          subpaths: (reverse! sp))))

(define (compute-two-areas (a <path-bounded-area>) (b <path-bounded-area>))
  (bind ((elist vx (areas->edge-list (list (cons #b0001 a)
                                           (cons #b0100 b))))
         (all-edges (unify-edges (car elist) (cadr elist) vx)))
    (for-each (lambda ((v <vertex>))
                (fix-up-participants! v))
              (vertices vx))
    ;(for-each print all-edges)
    (let* ((contours (crunch-contours all-edges))
           (cgroups (build-contour-groups contours))
           (top-level (organize-contour-groups cgroups)))
      top-level)))


;;;

#|

(define p3 (make-vertex 0 0 'p3))
(define p2 (make-vertex 1 0 'p2))
(define p5 (make-vertex 2 0 'p5))

(define A (make-edge p5 p2 'A '(A) '(~A)))
(define B (make-edge p2 p3 'B '(B) '(~B)))

(define p2 (make-vertex 7 1 'p2))
(define p3 (make-vertex 2 1 'p3))
(define p4 (make-vertex 0 1 'p4))
(define p5 (make-vertex 3 1 'p5))

(define A (make-edge p2 p3 'A2 '(A) '(~A)))
(define B (make-edge p4 p5 'B0 '(B) '(~B)))

(define (txor)
  (let ((p0 (make-vertex 1 1 'p0))
        (p1 (make-vertex 3 1 'p1))
        (p2 (make-vertex 3 3 'p2))
        (p3 (make-vertex 1 3 'p3))
        (p4 (make-vertex 3 2 'p4))
        (p5 (make-vertex 5 2 'p5))
        (p6 (make-vertex 5 4 'p6))
        (p7 (make-vertex 3 4 'p7)))
    (let ((a1 (make-edge p0 p1 'a1 '(~A) '(A)))
          (a2 (make-edge p1 p2 'a2 '(~A) '(A)))
          (a3 (make-edge p2 p3 'a3 '(~A) '(A)))
          (a4 (make-edge p3 p0 'a4 '(~A) '(A)))
          (b1 (make-edge p4 p5 'b1 '(~B) '(B)))
          (b2 (make-edge p5 p6 'b2 '(~B) '(B)))
          (b3 (make-edge p6 p7 'b3 '(~B) '(B)))
          (b4 (make-edge p7 p4 'b4 '(~B) '(B))))
      ;;
      (let* ((vx (make <vertex-index>
                       vertices: (list p0 p1 p2 p3 p4 p5 p6 p7)))
             (u (unify-edges (list a1 a2 a3 a4)
                             (list b1 b2 b3 b4)
                             vx)))
      (for-each fix-up-participants! (vertices vx))
      (print (vertices vx))
      u))))

(define p2 (make-vertex 6 3 'p2))
(define p3 (make-vertex 4 3 'p3))
(define p5 (make-vertex 1 3 'p5))
(define a3 (make-edge p2 p3 'a3 '(~A) '(A)))
(define b3 (make-edge p2 p5 'b3 '(~B) '(B)))


(define (txor2 #optional flip?)         ; (txor2)=>broken, (txor2 #t)=>works
  (let ((p0 (make-vertex 4 1 'p0))
        (p1 (make-vertex 6 1 'p1))
        (p2 (make-vertex 6 3 'p2))
        (p3 (make-vertex 4 3 'p3))
        (p4 (make-vertex 1 1 'p4))
        (p5 (make-vertex 1 3 'p5)))
    (let ((a1 (make-edge p0 p1 'a1 '(~A) '(A)))
          (a2 (make-edge p1 p2 'a2 '(~A) '(A)))
          (a3 (make-edge p2 p3 'a3 '(~A) '(A)))
          (a4 (make-edge p3 p0 'a4 '(~A) '(A)))
          (b1 (make-edge p4 p0 'b1 '(~B) '(B)))
          (b2 (make-edge p1 p2 'b2 '(~B) '(B)))
          (b3 (make-edge p2 p5 'b3 '(~B) '(B)))
          (b4 (make-edge p5 p4 'b4 '(~B) '(B))))
      ;;
      (let* ((vx (make <vertex-index>
                       vertices: (list p0 p1 p2 p3 p4 p5)))
             (u (if flip?
                    (unify-edges (list b1 b2 b3 b4) (list a1 a2 a3 a4) vx)
                    (unify-edges (list a1 a2 a3 a4) (list b1 b2 b3 b4) vx))))
      (for-each fix-up-participants! (vertices vx))
      (print (vertices vx))
      u))))

(define (t)
  (let ((p0 (make-vertex 0 0 'p0))
        (p1 (make-vertex 5 0 'p1))
        (p2 (make-vertex 5 3 'p2))
        (p3 (make-vertex 0 3 'p3))
        (q0 (make-vertex 4 2 'q0))
        (q1 (make-vertex 6 2 'q1))
        (q2 (make-vertex 6 4 'q2))
        (q3 (make-vertex 4 4 'q3))
        (x1 (make-vertex 5 2 'x1))
        (x2 (make-vertex 4 3 'x2)))
    (bind ((a (make-edge p0 p1 'a '(~A) '(A)))
           (b (make-edge p1 x1 'b '(~A) '(A)))
           (c (make-edge x1 p2 'c '(~A) '(A)))
           (d (make-edge p2 x2 'd '(~A) '(A)))
           (e (make-edge x2 p3 'e '(~A) '(A)))
           (f (make-edge p3 p0 'f '(~A) '(A)))
           ;
           (g (make-edge q0 x1 'g '(~B) '(B)))
           (h (make-edge x1 q1 'h '(~B) '(B)))
           (i (make-edge q1 q2 'i '(~B) '(B)))
           (j (make-edge q2 q3 'j '(~B) '(B)))
           (k (make-edge q3 x2 'k '(~B) '(B)))
           (l (make-edge x2 q0 'l '(~B) '(B))))
      ;
      (fix-up-participants! p0)
      (fix-up-participants! p1)
      (fix-up-participants! p2)
      (fix-up-participants! p3)
      (fix-up-participants! q0)
      (fix-up-participants! q1)
      (fix-up-participants! q2)
      (fix-up-participants! q3)
      (fix-up-participants! x1)
      (fix-up-participants! x2)
      ;
      (list a b c d e f g h i j k l))))

|#

;;;
