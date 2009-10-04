,(use tables
      rs.util.properties)

(load "heappic.scm")

(set! *heap-picture-size* (make-size 30 20))
(set! *heap-picture-gap* 0)
(set! *draw-obj-radius* 3)

(define (mini-heap-pic lgen lcol rgen rcol)
  (gsaved
   (scale 0.5 0.5)
   ;(translate (make-point -20 0))
   (let ((f (heap-frame 0))
         (l (obj (- 1 lgen) 2 2.5))
         (r (obj (- 1 rgen) 8 2.5)))
     (case lcol
       ((white) (l 'draw))
       ((gray) (l 'draw-gray)))
     (case rcol
       ((white) (r 'draw))
       ((gray) (r 'draw-gray)))
     ;;
     (edge l r)
     ;(rectstroke (heap-frame 0))
     (moveto (upper-left f))
     (lineto (upper-right f))
     (stroke))))

;;;

(define-class <table-cell> (<object>)
  (properties type: <vector> init-value: '#())
  (first-row type: <fixnum>)
  (first-column type: <fixnum>)
  (num-rows type: <fixnum> init-value: 1)
  (num-columns type: <fixnum> init-value: 1)
  (min-width type: <real> init-value: 0)
  (min-height type: <real> init-value: 0)
  (assigned-frame init-value: #f)
  (border-style type: <vector> init-value: '#((0 none) (0 none) (0 none) (0 none))))     ; T B L R

(define (for-each-width-view lst proc)
  (for-each
   (lambda ((c <table-cell>))
     (proc (first-column c)
           (num-columns c)
           (min-width c)))
   lst))

(define (for-each-height-view lst proc)
  (for-each
   (lambda ((c <table-cell>))
     (proc (first-row c)
           (num-rows c)
           (min-height c)))
   lst))

;;;

(define (compute-table-dimens (cells <list>))
  (let ((nr 0)
        (nc 0))
    (for-each
     (lambda ((c <table-cell>))
       (set! nr (max nr (+ (first-row c) (num-rows c))))
       (set! nc (max nc (+ (first-column c) (num-columns c)))))
     cells)
    (values nr nc)))

(define (vector-max! v k x)
  (vector-set! v k (max x (vector-ref v k))))

(define (vector-bump! v k dx)
  (vector-set! v k (+ dx (vector-ref v k))))

(define (index-seq i n)
  (let loop ((j (+ i n -1))
             (r '()))
    (if (< j i)
        r
        (loop (- j 1)
              (cons j r)))))
  
(define (vector-subsum v i n)
  (let loop ((i i)
             (n n)
             (sum 0))
    (if (= n 0)
        sum
        (loop (+ i 1)
              (- n 1)
              (+ sum (vector-ref v i))))))

(define (compose-border! space c j i d s)
  (if s
      (let* ((node (vector-ref (vector-ref (node-matrix space) j) i))
             (edge (case d
                     ((east) (adjacent-east node))
                     ((south) (adjacent-south node)))))
        ;;
        (format #t "compose border (~d,~d):~s at ~s add ~s\n" j i d node s)
        ;;
        (cond
         ((= (car s) (style-level edge))
          (if (not (memq (cadr s) (style-set edge)))
              (set-style-set! edge (cons (cadr s) (style-set edge)))))
         ((> (car s) (style-level edge))
          (set-style-level! edge (car s))
          (set-style-set! edge (cdr s)))))))

(define (compute-cell-border (c <table-cell>) space)
  (let ((top-j (first-row c))
        (bottom-j (+ (first-row c) (num-rows c)))
        (left-i (first-column c))
        (right-i (+ (first-column c) (num-columns c))))
    (format #t "--- (~d,~d) x (~d,~d)\n" (first-column c) (first-row c) (num-columns c) (num-rows c))
    (for-each
     (lambda (i)
       (compose-border! space c top-j i 'east (vector-ref (border-style c) 0))
       (compose-border! space c bottom-j i 'east (vector-ref (border-style c) 1)))
     (index-seq (first-column c) (num-columns c)))
    (for-each
     (lambda (j)
       (compose-border! space c j left-i 'south (vector-ref (border-style c) 2))
       (compose-border! space c j right-i 'south (vector-ref (border-style c) 3)))
     (index-seq (first-row c) (num-rows c)))))

(define-class <border-node> (<object>)
  name
  position
  (adjacent-north init-value: #f)
  (adjacent-south init-value: #f)
  (adjacent-east init-value: #f)
  (adjacent-west init-value: #f))

(define-class <border-edge> (<object>)
  name
  (style-level init-value: -1)
  (style-set init-value: '())
  (a-peer type: <border-node>)
  (b-peer type: <border-node>))

(define-method write-object ((self <border-edge>) port)
  (format port "#[<border-edge> ~a ~a]" (machine-bits->string self) (name self)))

(define-method write-object ((self <border-node>) port)
  (format port "#[<border-node> ~a ~a]" (machine-bits->string self) (name self)))

(define-class <border-space> (<object>)
  (node-matrix type: <vector>)
  (all-edges type: <vector>))

(define (bind-border-nodes (a <border-node>) (b <border-node>) dir)
  (let ((e (make <border-edge>
             name: (format #f "~a-~a" (name a) (name b))
             a-peer: a
             b-peer: b)))
    (case dir
      ((east)
       (set-adjacent-east! a e)
       (set-adjacent-west! b e))
      ((south)
       (set-adjacent-south! a e)
       (set-adjacent-north! b e)))
    e))

(define (matrix-ref mx i j)             ; i=column/x, j=row/y
  (vector-ref (vector-ref mx j) i))


(define (make-border-space numr numc #key (col-widths default: #f) (row-heights default: #f))
  ;;
  (define compute-node-x
    (if col-widths
        (lambda (i)
          (vector-subsum col-widths 0 i))
        (lambda (i)
          (* 20 i))))
  ;;
  (define compute-node-y
    (if row-heights
        (let ((th (vector-subsum row-heights 0 (vector-length row-heights))))
          (lambda (j)
            (- th (vector-subsum row-heights 0 j))))
        (lambda (j)
          (* -10 j))))
  ;;
  (let ((mx (list->vector
             (map (lambda (j)
                    (let ((y (compute-node-y j)))
                      (list->vector
                       (map (lambda (i)
                              (make <border-node>
                                    ;; note that we can have border nodes that
                                    ;; are not associated with any table-cell,
                                    ;; because table-cells represent semantic
                                    ;; cells.  What with spaning and all, there
                                    ;; may be no cell for a particular node.
                                    position: (make-point (compute-node-x i) y)
                                    name: (format #f "~d,~d" i j)))
                            (range (+ numc 1))))))
                  (range (+ numr 1)))))
        (edgeq (make-dequeue)))
    (for-each
     (lambda (j)
       (for-each
        (lambda (i n)
          (if (< i numc)
              (dequeue-push-back!
               edgeq
               (bind-border-nodes n (matrix-ref mx (+ i 1) j) 'east)))
          (if (< j numr)
              (dequeue-push-back!
               edgeq
               (bind-border-nodes n (matrix-ref mx i (+ j 1)) 'south)))
          (values))
        (index-seq 0 (+ numc 1))
        (vector->list (vector-ref mx j))))
     (index-seq 0 (+ numr 1)))
    ;;
    (make <border-space>
          node-matrix: mx
          all-edges: (dequeue-state edgeq))))

(define (compute-borders (cells <list>) #rest r)
  (bind ((numr numc (compute-table-dimens cells))
         (spc (apply make-border-space numr numc r)))
    (for-each
     (lambda ((c <table-cell>))
       (compute-cell-border c spc))
     cells)
    spc))

(define (node->edge (self <border-node>) dir)
  (case dir
    ((north) (adjacent-north self))
    ((south) (adjacent-south self))
    ((east) (adjacent-east self))
    ((west) (adjacent-west self))))

(define (peer-node (self <border-edge>) (me <border-node>))
  (cond
   ((eq? me (a-peer self))
    (b-peer self))
   ((eq? me (b-peer self))
    (a-peer self))
   (else
    (error "~s is not involved in ~s" me self))))

(define (try-next-direction dir k)
  (vector-ref '#(north east south west)
              (modulo (+ (vmemq dir '#(north east south west)) k) 4)))
         
(define (chase-border* (self <border-node>) dir style)
  ;(format #t "chasing ~s [~s] -- coming in from ~s\n" self style dir)
  (let loop ((rel 0))
    (if (= rel 4)
        ;; ran out of directions to check
        (list self)
        (let* ((d (try-next-direction dir rel))
               (edge (node->edge self d)))
          ;(format #t "  try dir ~s .. ~s\n" d (and edge (style-set edge)))
          (if (and edge (memq style (style-set edge)))
              ;; we found a way outta here
              (begin
                (set-style-set! edge (delq style (style-set edge)))
                (cons self (chase-border* (peer-node edge self) d style)))
              (loop (+ rel 1)))))))

;;;
;;;  Returns the direction required to get from the a-peer to the b-peer
;;;

(define (edge-orientation (self <border-edge>))
  (cond
   ((eq? (node->edge (a-peer self) 'north) self) 'north)
   ((eq? (node->edge (a-peer self) 'south) self) 'south)
   ((eq? (node->edge (a-peer self) 'east) self) 'east)
   ((eq? (node->edge (a-peer self) 'west) self) 'west)
   (else (error "no direction gets from A to B"))))

(define (found-border fwd rev style)
  (let* ((nlist (append (reverse (cdr rev)) fwd))
         (closed? (eq? (car nlist) (last nlist))))
    ;;
    (format #t "~s: ~a ~a\n"
            style
            (string-join " - " (map name nlist))
            (if closed?
                " (closed)"
                " (open)"))
    ;;
    (gsaved
     ;;
     (setlinewidth 0.5)
     (case style
       ((solid) #f)
       ((dash) (setdash '#(1 1) 0)))
     ;;
     (moveto (position (car nlist)))
     (for-each (lambda ((n <border-node>))
                 (lineto (position n)))
               (cdr nlist))
     (if closed?
         (closepath))
     (stroke))))

(define (chase-all-borders-from (self <border-edge>))
  (let loop ()
    (if (pair? (style-set self))
        (begin
          (format #t "========= Chasing style [~s] from ~s =========\n" (car (style-set self)) self)
          (let ((s (car (style-set self))))
            (set-style-set! self (cdr (style-set self)))
            (let ((o (edge-orientation self)))
              (case o
                ((south)
                 (found-border (chase-border* (b-peer self) 'south s)
                               (cons (b-peer self) (chase-border* (a-peer self) 'north s))
                               s))
                ((east)
                 (found-border (chase-border* (b-peer self) 'east s)
                               (cons (b-peer self) (chase-border* (a-peer self) 'west s))
                               s))
                (else
                 (error "surprising orientation (by construction)")))))
          (loop)))))

(define (chase-all-borders (self <border-space>))
  (vector-for-each
   (lambda ((e <border-edge>))
     (chase-all-borders-from e))
   (all-edges self)))
     

(define (layout-table frame (cells <list>))
  (bind ((numr numc (compute-table-dimens cells))
         (row-heights (make-vector numr 0))
         (col-widths (make-vector numc 0)))
    (values)
    ;;
    (define (process-dimen iterator vec)
      ;; two passes:
      ;; first, find the singleton values
      (iterator
       cells
       (lambda (i n size)
         (if (= n 1)
             (vector-max! vec i size))))
      ;; second, for multiindex values that are
      ;; not yet big enough, spread the excess around
      (iterator
       cells
       (lambda (i n size)
         (if (> n 1)
             (let ((sum (vector-subsum vec i n)))
               (format #t "span (~d,~d) sum ~s need ~s\n" i n sum size)
               (if (> size sum)
                   (let ((adj-per (/ (- size sum) n)))
                     (format #t "adj ~s on ~s\n" adj-per (index-seq i n))
                     (for-each (lambda (k)
                                 (vector-bump! vec k adj-per))
                               (index-seq i n)))))))))
    ;;
    (process-dimen for-each-width-view col-widths)
    (process-dimen for-each-height-view row-heights)
    ;;
    (let ((total-h (vector-subsum row-heights 0 numr))
          (total-w (vector-subsum col-widths 0 numc)))
      ;;
      (for-each
       (lambda ((c <table-cell>))
         (let ((rh (vector-subsum row-heights 
                                  (first-row c)
                                  (num-rows c))))
           (set-assigned-frame! c (make-rect (vector-subsum col-widths 0 (first-column c))
                                             (- total-h
                                                (vector-subsum row-heights 0 (first-row c))
                                                rh)
                                             (vector-subsum col-widths 
                                                            (first-column c)
                                                            (num-columns c))
                                             rh))))
       cells)
      ;;
      (values (make-size total-w total-h)
              col-widths
              row-heights))))

    

(define (t)
  (define (hspan j i n w h)
    (make <table-cell>
          properties: (vector 'content $blank)
          first-row: j
          first-column: i
          num-columns: n
          min-width: w
          min-height: h
          border-style: '#((2 solid) (2 solid) (2 solid) (2 solid))))
  (define (vspan j i n w h)
    (make <table-cell>
          properties: (vector 'content $blank)
          first-row: j
          first-column: i
          num-rows: n
          min-width: w
          min-height: h
          border-style: '#((2 solid) (2 solid) (2 solid) (2 solid))))
  (define (simple j i w h)
    (make <table-cell>
          properties: (vector 'content $blank)
          first-row: j
          first-column: i
          min-width: w
          min-height: h
          border-style: '#((1 dash) (1 dash) (1 dash) (1 dash))))
  ;;
  (list ;(simple 0 0 10 10)
        (simple 0 1 30 10)
        ;(simple 0 2 50 10)

        (simple 1 0 10 10)
        (simple 1 1 40 10)
        ;(simple 1 2 40 10)

        (hspan 2 0 2 70 10)
        (vspan 0 2 2 5 5)
        (simple 2 2 40 10)))
        
(define (tsim)
  (bind ((size cw rh (layout-table #f (t))))
    (format #t "         Size: ~s\n" size)
    (format #t "  Row heights: ~s\n" rh)
    (format #t "Column widths: ~s\n" cw)
    
    (let ((b (compute-borders (t))))
      (print (node-matrix b))
      ;;
      (for-each
       (lambda (j)
         (for-each
          (lambda (i)
            (format #t "(~d,~d):\n" i j)
            (print (vector-ref (vector-ref (node-matrix b) j) i)))
          (range 3)))
       (range 3))
      (chase-all-borders b)
      ;;
      )))

;;;;

;;;

(define-class <cell-content> (<object>) :abstract
  (align-bits init-value: #b000000))


(define-generic-function cell-content-min-size ((self <cell-content>)))
(define-generic-function cell-content-render ((self <cell-content>) (frame <rect>)))

;;;

(define-class <blank-cell-content> (<cell-content>))

(define $blank (make <blank-cell-content>))

(define-method cell-content-min-size ((self <blank-cell-content>))
  $zero-size)

(define-syntax (debug-frames . body)
  ;(begin . body)
  (values))
  

(define-method cell-content-render ((self <blank-cell-content>) frame)
  (debug-frames
   (gsaved (setcolor (device-color '(rgb 0.5 0.5 1))) (rectstroke (inset-rect frame 2 2))))
  ;; a NOP
  (values))

;;;
;;;  This is consistent with the notation used by CSS
;;;

(define-method padding-values ((self <real>)) ; return four values: T R B L
  (values self self self self))

(define-method padding-values ((self <vector>)) ; return four values: T R B L
  (case (vector-length self)
    ((0) (values 0 0 0 0)
    ((1) (values (vector-ref self 0)
                 (vector-ref self 0)
                 (vector-ref self 0)
                 (vector-ref self 0)))
    ((2) (values (vector-ref self 0)
                 (vector-ref self 1)
                 (vector-ref self 0)
                 (vector-ref self 1)))
    ((3) (values (vector-ref self 0)
                 (vector-ref self 1)
                 (vector-ref self 2)
                 (vector-ref self 1)))
    ((4) (values (vector-ref self 0)
                 (vector-ref self 1)
                 (vector-ref self 2)
                 (vector-ref self 3))))))

;;;

(define-class <graphic-cell-content> (<cell-content>)
  (bbox type: <rect>)
  (render-proc type: <function>))

(define-method cell-content-min-size ((self <graphic-cell-content>))
  (size (bbox self)))

(define-method cell-content-render ((self <graphic-cell-content>) frame)
  (debug-frames
   (gsaved (setcolor (device-color '(rgb 0.5 1 0.5))) (rectstroke (inset-rect frame 2 2))))
  (gsaved
   ((render-proc self) frame)))

;;

(define-class <text-cell-content> (<cell-content>)
  (tbox type: <rect>)
  (padding init-value: 0)
  (rotation init-value: 0)
  (has-brace? init-value: #f)
  (contents type: <string>)
  font)

(define (braced-text-cell (str <string>) (font <text-font>) orientation)
  (make <text-cell-content>
        tbox: (string-bbox (font-metrics font) str)
        rotation: (case orientation
                    ((vertical) 90)
                    ((horizontal) 0))
        has-brace?: #t
        contents: str
        font: font))
  
(define (text-cell (str <string>) (font <text-font>) #key (padding default: #f))
  (let ((c (make <text-cell-content>
                 tbox: (string-bbox (font-metrics font) str)
                 contents: str
                 font: font)))
    (if padding (set-padding! c padding))
    c))

(define-method cell-content-min-size ((self <text-cell-content>))
  (bind ((t r b l (padding-values (padding self))))
    (make-size (+ r l (string-width (font self) (contents self)))
               (+ t b (font-size (font self))))))

(define-method cell-content-render ((self <text-cell-content>) frame)
  (debug-frames
   (gsaved (setcolor (device-color '(rgb 1 0.5 0.5))) (rectstroke (inset-rect frame 2 2))))
  ;; center on rect baseline for now
  (setfont (font self))
  (if (has-brace? self)
      (if (= (rotation self) 90)
          (draw-long-brace base: (make-line (- (x (lower-right frame)) 5)
                                            (y (lower-right frame))
                                            (- (x (upper-right frame)) 5)
                                            (y (upper-right frame)))
                           ontip: (lambda ()
                                    (cshow 0 6 (contents self))
                                    ))
          (draw-long-brace base: (make-line (x (lower-left frame))
                                            (+ (y (lower-left frame)) 5)
                                            (x (lower-right frame))
                                            (+ (y (lower-right frame)) 5))
                           ontip: (lambda ()
                                    (cshow 0 6 (contents self))
                                    )))
      (cshow (center-x frame) 
             (+ 2 (- (center-y frame) (/ (font-size (font self)) 2)))
             (contents self))))

(define-method write-object ((self <text-cell-content>) port)
  (format port "#[<text> ~s]" (contents self)))
;;;

(define *label-font* (get-text-font "Times" "Italic" 9))
(define *content-font* (get-text-font "Helvetica" "Bold" 10))

(define (wb-table tbl)
  ;;
  (let ((accum (make-dequeue))
        (index (make-table))
        (depth 0))
    ;;
    (define (addcell con (self <table-cell>))
      (let ((s (cell-content-min-size con)))
        (set-property! self 'content con)
        (set-min-width! self (width s))
        (set-min-height! self (height s))
        (dequeue-push-back! accum self)
        (table-insert! index
                       (cons (first-column self) (first-row self))
                       self)))
    ;;
    (define (hspan j i n con #optional s)
      (debug-frames (set! s '(0 solid)))
      (addcell
       con
       (make <table-cell>
         first-row: j
         first-column: i
         num-columns: n
         border-style: (vector s s s s))))
    ;;
    (define (vspan j i n con #optional s)
      (debug-frames (set! s '(0 solid)))
      (addcell
       con
       (make <table-cell>
         first-row: j
         first-column: i
         num-rows: n
         border-style: (vector s s s s))))
    ;;
    (define (simple j i con #optional s)
      (debug-frames (set! s '(0 solid)))
      (addcell
       con
       (make <table-cell>
         first-row: j
         first-column: i
         border-style: (vector s s s s))))
    ;;
    (define (border-edge j i s e)
      (let ((c (table-lookup index (cons j i))))
        (if c
            (vector-set! (border-style c) e s))))
    ;;
    (define (box j i nc nr s)
      (set! depth (+ depth 1))
      (let ((s (list depth s)))
        (let ((i0 i)
              (i1 (+ i nr -1)))
          ;; top/bottom edges
          (for-each
           (lambda (j)
             (border-edge j i0 s 0)  ; top
             (border-edge j i1 s 1)) ; bottom
           (index-seq j nc)))
        (let ((j0 j)
              (j1 (+ j nc -1)))
          ;; left/right edges
          (for-each
           (lambda (i)
             (border-edge j0 i s 2)         ; left
             (border-edge j1 i s 3))        ; right
           (index-seq i nr)))))
    ;;
    (vspan 3 0 4 (braced-text-cell "lvalue" *label-font* 'vertical))
    (vspan 3 1 2 (text-cell "0(T)" *label-font*))
    (vspan 5 1 2 (text-cell "7(P)" *label-font*))
    (simple 3 2 (text-cell "w" *label-font* padding: 2))
    (simple 4 2 (text-cell "b" *label-font* padding: 2))
    (simple 5 2 (text-cell "w" *label-font* padding: 2))
    (simple 6 2 (text-cell "b" *label-font* padding: 2))
    ;;
    (hspan 0 3 4 (braced-text-cell "rvalue" *label-font* 'horizontal))
    (hspan 1 3 2 (text-cell "0(T)" *label-font*))
    (hspan 1 5 2 (text-cell "7(P)" *label-font*))
    (simple 2 3 (text-cell "w" *label-font* padding: 2))
    (simple 2 4 (text-cell "b" *label-font* padding: 2))
    (simple 2 5 (text-cell "w" *label-font* padding: 2))
    (simple 2 6 (text-cell "b" *label-font* padding: 2))
    ;;
    (for-each
     (lambda (j)
       (for-each
        (lambda (i)
          (simple (+ 3 j) (+ 3 i) (matrix-ref tbl i j)))
        (range 4)))
     (range 4))
    ;;
    (box 3 3 1 1 'dash)
    (box 4 3 1 1 'dash)
    (box 5 3 1 1 'dash)
    (box 6 3 1 1 'dash)
    ;;
    (box 3 4 1 1 'dash)
    (box 4 4 1 1 'dash)
    (box 5 4 1 1 'dash)
    (box 6 4 1 1 'dash)
    ;;
    (box 3 5 1 1 'dash)
    (box 4 5 1 1 'dash)
    (box 5 5 1 1 'dash)
    (box 6 5 1 1 'dash)
    ;;
    (box 3 6 1 1 'dash)
    (box 4 6 1 1 'dash)
    (box 5 6 1 1 'dash)
    (box 6 6 1 1 'dash)
    ;;
    (box 3 3 2 2 'solid)
    (box 5 3 2 2 'solid)
    (box 3 5 2 2 'solid)
    (box 5 5 2 2 'solid)
    ;;
    (vector->list (dequeue-state accum))))




(define (render-matrix lst)
  (bind ((size cw rh (layout-table #f lst))
         (bs (compute-borders lst 
                              col-widths: cw
                              row-heights: rh)))
    ;;
    (chase-all-borders bs)
    ;;
    (for-each
     (lambda ((c <table-cell>))
       (print c)
       (if (has-property? c 'content)
           (cell-content-render (get-property c 'content) (assigned-frame c))))
     lst)
    size))
