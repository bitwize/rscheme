(define-macro (debug-table . forms)
  '(values))

(define-class <table-row-list> (<vitem>)
  (content type: <flow-table>)
  (start-index init-value: 0)           ; row #
  (index-count init-value: 0)           ; number of rows
  (column-widths init-value: '())
  (row-heights init-value: '())
  (layouts init-value: '#()))

(define-method height ((self <table-row-list>))
  (reduce + 0 (row-heights self)))

(define-method width ((self <table-row-list>))
  (reduce + 0 (column-widths self)))

(define (compute-rule-coords a0 delta-list op)
  (let loop ((r (list a0))
             (w delta-list))
    (if (null? w)
        (reverse r)
        (loop (cons (op (car r) (car w)) r)
              (cdr w)))))

(define (list-split l n)
  (if (or (= n 0) (null? l))
      (values '() l)
      (bind ((a b (list-split (cdr l) (- n 1))))
        (values (cons (car l) a) b))))

(define (draw-table-rules (self <table-row-list>) x0 y0 dev)
  (let ((xs (compute-rule-coords x0 (column-widths self) +))
        (ys (compute-rule-coords y0 (row-heights self) -))
        (nh (num-header-rows (content self)))
        (draw-rules? #t))
    ;; double-rule after the header rows
    (if (> nh 0)
        (bind ((header-ys body-ys (list-split ys nh)))
          (set! ys (append header-ys
                           (list (car body-ys)
                                 (- (car body-ys) 2))
                           (cdr body-ys)))
          #|
          (set! ys (cons (car ys)
                         (cons* (cadr ys)
                                (- (cadr ys) 2)
                                (cddr ys))))
          |#
          
          ;; draw shadings
          (with-gstate-saved
           dev
           (lambda ()
             (setcolor dev (device-color dev '(gray 0.9)))
             (rectfill dev (make-rect (car xs)
                                      (car body-ys)
                                      (- (last xs) (car xs))
                                      (- (car header-ys) (car body-ys))))))))
    ;;
    ;(format #t "xs => ~s\n" xs)
    ;(format #t "ys => ~s\n" ys)
    (if draw-rules?
        (begin
          ;;
          ;; draw vrules
          (setlinewidth dev 0.5)
          (for-each (lambda (x)
                      (moveto dev (make-point x (car ys)))
                      (lineto dev (make-point x (last ys))))
                    (cdr (reverse (cdr xs))))
          ;; draw vrules
          (for-each (lambda (y)
                      (moveto dev (make-point (car xs) y))
                      (lineto dev (make-point (last xs) y)))
                    (cdr (reverse (cdr ys))))
          (stroke dev)
          ;; the box around the whole rowset
          (rectstroke dev (make-rect x0 y0
                                     (- (last xs) x0)
                                     (- (last ys) y0)))))
    ))

(define-method render ((self <table-row-list>) stream x0 y0 dev vlist)
  (let ((n (length (row-heights self)))
        (mx (layouts self))
        (sfr (subframe-rect (placement vlist))))
    ;;
    (with-gstate-saved
     dev
     (lambda ()
       ;;
       (translate 
        dev 
        (case (align (content self))
          ((center) (make-point (/ (- (width sfr) (width self)) 2) 0))
          ((left) $zero-point)
          ((right) (make-point (- (width sfr) (width self)) 0))
          (else (error "~s: unknown table alignment ~s" 
                       self 
                       (align (content self))))))
       ;;
       (draw-table-rules self x0 (+ y0 (height self)) dev)
       ;; fill in the content
       (let loop ((i (start-index self))
                  (y (+ y0 (height self))))
         (if (and (< i n)
                  (< (- i (start-index self)) (index-count self)))
             (let ((h (list-ref (row-heights self) i)))
               (render-table-row self i x0 (- y h) dev 
                                 (vector-ref mx i)
                                 (list-ref (row-heights self) i)
                                 (column-widths self))
               (loop (+ i 1)
                     (- y h)))))))))
                      

(define (render-table-row (self <table-row-list>)
                          (i <fixnum>)
                          x0 y0 dev 
                          (row <vector>)
                          row-height
                          col-widths)
  (let loop ((j 0)
             (cw col-widths)
             (x x0))
    (if (pair? cw)
        (begin
          (with-gstate-saved
           dev
           (lambda ()
             (let ((cell (vector-ref row j)))
               (debug-table
                (format #t "--- ~s ---  x ~d  y ~d\n" cell x y0))
               (translate dev (make-point x y0))
               (render (vector-ref row j) dev))))
          (loop (+ j 1)
                (cdr cw)
                (+ x (car cw)))))))
             
#|
  (rectstroke dev (make-rect x0 y0 
                             (reduce + 0 col-widths)
                             row-height)))
|#
                          
;;;
(define (gen-flow-table)
  (let ((ft (make <flow-table>
                  column-widths: '(1in 2in)
                  row-heights: '(20 20 20)
                  num-header-rows: 1
                  content: '())))
    ;;
    (define (tc str)
      (let ((tr (make <text-run>
                      style: 'body-char-style
                      content: str)))
        (make <table-cell>
              owner: ft
              content: (make <flow>
                             content: (list
                                       (make <para>
                                             style: 'body-para-style
                                             content: (list tr)))))))
    ;;
    (set-content! ft 
                  (list
                   (list (tc "A1") (tc "B1"))
                   (list (tc "A2") (tc "B2"))
                   (list (tc "A3") (tc "This is a test of the fun and exciting table layout algorithms."))))
    ft))

;;;
(define (make-simple-vstream (sf <placement-group>))
  ;; construct a placement stream (a la make-placement-stream)
  ;; which just returns successive lines
  (make-request-stream
   (lambda (rs)
     (let loop ((y 0))
       (bind ((type subtype (request-read rs)))
         (debug-table
          (format #t "(y=~d) simple vstream ~s ~s\n" y type subtype))
         (case type
           ((vskip)
            ;; XXX should we ignore space at the beginning of a cell?
            (request-return rs 'ok)
            (loop (+ y subtype)))
           ;;
           ((break)
            (request-return rs 'ok)
            (loop y))
           ;;
           (else
            ;; ignore subtype
            (request-return rs sf (+ y type))
            (loop (+ y type)))))))))

(define-class <cell-flow-layout-info> (<flow-layout-info>)
  flow-layout)

(define-method add-vl-to-flow-layout! ((self <cell-flow-layout-info>) 
                                       (vl <vlist>))
  (add-vl-to-flow-layout! (flow-layout self) vl))

(define (layout-flow-in-table-cell (self <flow>)
                                   (target <table-cell>)
                                   cell-width
                                   tgroup-style)
  (if (null? (content self)) 
      (values)          ; do nothing
      ;; otherwise, we'll need at least one page...
      (let* ((sf (make <placement-cell>
                       cell: target
                       height: 0
                       width: cell-width))
             (pstream (make-simple-vstream sf))
             (current-fli (make <cell-flow-layout-info>
                                flow-layout: (make <flow-layout>))))
        (let loop ((lst (content self)))
          (if (pair? lst)
              (begin
                (layout-flow-member (car lst)
                                    pstream
                                    current-fli)
                (loop (cdr lst)))
              (bind ((sf y (request pstream 0 'in-column)))
                (set-height! sf y)
                (values (flow-layout current-fli) 
                        (case tgroup-style
                          ((tight)
                           (- y 5))
                          (else
                           y)))))))))

(define (concrete-column-widths (self <flow-table>) 
                                (wrt <placement-subframe>))
  ;; Now is where the rubber is hitting the road; we have
  ;; to decide exactly how wide we want these columns
  (if (every? number? (column-widths self))
      ;; if the column widths are already fixed, we're done
      (column-widths self)
      ;; otherwise, we have some extra work to do
      (finalize-column-widths
       (size-width (subframe-rect wrt))  ; layout area width
       (column-widths self)
       (lambda (i)
         (compute-maximum-cell-width self i)))))

(define (compute-maximum-cell-width (self <flow-table>) col)
  ;(format #t "Compute maximum width for column ~s\n" col)
  (let loop ((m 0)
             (rows (content self)))
    (if (null? rows)
        m
        (bind ((c (list-ref (car rows) col))
               (fl h (layout-flow-in-table-cell (content c) c 1000000
                                                'normal))
               (w (flow-layout-natural-width fl))
               (w (+ w (* 2 (cell-margin c)))))
          (print (content fl))
          ;(format #t "/////////// ==> ~s\n" w)
          (loop (max m w) (cdr rows))))))

(define (flow-layout-natural-width (self <flow-layout>))
  (let ((w 0))
    (for-each 
     (lambda ((vl <vlist>))
       (for-each
        (lambda ((vi <vitem>))
          (set! w (+ w (width vi))))
        (content vl)))
     (content self))
    w))
  
(define (layout-table (self <flow-table>) (wrt <placement-subframe>))
  ;; the placement subframe drives how wide of space we target
  ;;
  ;; XXX for now, we don't know how to break a table into
  ;;     different <table-row-list>'s
  (let* ((n (length (content self)))
         (flowmx (list->vector
                  (map (lambda (row)
                         (make-vector (length row) #f))
                       (content self))))
         (trl (make <table-row-list>
                    content: self
                    start-index: 0
                    index-count: n
                    layouts: flowmx
                    row-heights: (row-heights self)
                    column-widths: (concrete-column-widths self wrt))))
    (format #t "-------------------(layout-table)-------------\n")
    (format #t "layout table ~s => ~s\n"
            (column-widths self)
            (column-widths trl))
    (print wrt)
    (print (frame wrt))
    (format #t "----------------------------------------------\n")
    (set-row-heights!
     trl
     (map
      (lambda (r row)
        (let ((q (map
                  (lambda (c col-w (cell <table-cell>))
                    (bind ((fl h (layout-flow-in-table-cell (content cell)
                                                            cell
                                                            col-w
                                                            (style self))))
                      (vector-set! (vector-ref flowmx r) c fl)
                      h))
                  (range (length row))
                  (column-widths trl)
                  row)))
          (if (null? q)
              10                        ; fallback row height
              (apply max q))))
      (range n)
      (content self)))
    trl))

#|
(define (ttable2)
  (let* ((t (gen-flow-table))
         (tc (caar (content t))))
    (layout-flow-in-table-cell (content tc)
                               tc
                               (car (column-widths t)))))

(define (ttrl)
  (let ((c (gen-flow-table)))
    (values (layout-table c) c)))
#|
    (make <table-row-list>
          content: c
          start-index: 0
          index-count: 3
          column-widths: (column-widths c)
          row-heights: (row-heights c))
|#
(define (ttable1)
  (let ((trl (ttrl)))
    (reset *dev*)
    (render trl #f 100 100 *dev*)
    (flush-output-port *dev*)
    trl))
|#

;;;

(define (aslist x)
  (if (or (pair? x)
          (null? x))
      x
      (list x)))

(define-method bridge-display-stuff ((self <TABLE>) emit)
  (let ((n (get-table-name self)))
    (emit (make-paragraph-from-inline
           (list "\t"
                 (make-anchor self <BOOK> (list 'table n))
                 (list :bold (format #f "Table ~a." n))
                 "\t"
                 (xpath self "title"))
           'table-title-style
           bridge-title-stuff))
    (bridge-table self emit)))

(define-method bridge-display-stuff ((self <INFORMALTABLE>) emit)
  (bridge-table self emit))

(define (parse-column-widths (self <TGROUP>))
  (let ((w (make-vector (string->number (get-attribute-value self "cols"))
                        ;; this is per the specification: 
                        ;;    an unspecified width shall be treated as though
                        ;;    it where colwidth="*"
                        '(p 1))))
    (let loop ((cs (xpath* self "colspec"))
               (k 0))
      (if (null? cs)
          (vector->list w)
          ;; XXX handle colnum=N instead of just going to the next
          (let* ((colspec (car cs))
                 (ws (get-attribute-value colspec "colwidth")))
            (if ws
                (vector-set! w k (parse-length ws)))
            (loop (cdr cs)
                  (+ k 1)))))))

(define (finalize-column-widths area-width widths fn)
  ;;  a WIDTH is one of:
  ;;      - a NUMBER, specifying a fixed width
  ;;      - a list (p N), specifying a width of the form N*
  ;;      - a list (p N F), specifying a width of the form N*+F
  ;;      - the symbol ?, indicating an unspecified value
  ;; XXX for now, this is rather crude:  For all the unspecified
  ;;     values, we just use the natural width.  This takes some
  ;;     care and planning on the part of the <table> user :-(
  (let* ((widths (map (lambda (i x)
                        (if (eq? x '?)
                            (fn i)
                            x))
                      (range (length widths))
                      widths))
         (star-coeff (reduce + 0 (map (lambda (p)
                                        (if (pair? p)
                                            (cadr p)
                                            0))
                                      widths)))
         (fixed-part (reduce + 0 (map (lambda (p)
                                        (if (and (pair? p)
                                                 (pair? (cddr p)))
                                            (caddr p)
                                            (if (number? p)
                                                p
                                                0)))
                                      widths)))
         (per (if (> star-coeff 0)
                  (/ (max 0 (- area-width fixed-part)) star-coeff)
                  0)))
    (values (map (lambda (w)
                   (cond
                    ((number? w) w)
                    ((null? (cddr w))
                     (* per (cadr w)))
                    (else
                     (+ (caddr w) (* per (cadr w))))))
                 widths)
            star-coeff fixed-part 
            per)))

#|
(define (finalize-column-widths area-width widths column-natural-width)
  (let* ((spare (- area-width (reduce + 0 (select number? widths))))
         (total (reduce + 0 (map cadr (select pair? widths))))
         (per (/ spare total)))
    (map (lambda (w)
           (if (pair? w)
               (max 0.25in (* (cadr w) per))
               w))
         widths)))
|#

(define (bridge-table self emit)
  ;;
  (let* ((h-rows (xpath* self "tgroup" "thead" "row"))
         (b-rows (xpath* self "tgroup" "tbody" "row"))
         (style (let ((g (xpath self "tgroup")))
                  (or (get-attribute-value g "tgroupstyle")
                      "normal")))
         (style-name (cond
                      ((string-ci=? style "normal")
                       'normal)
                      ((string-ci=? style "tight")
                       'tight)
                      (else
                       (error "Unknown tgroupstyle=~s" style))))
         (ft (make <flow-table>
                   column-widths: (parse-column-widths 
                                   (xpath self "tgroup"))
                   style: style-name
                   row-heights: (map (lambda (r) 20)
                                     (append h-rows b-rows))
                   num-header-rows: (length h-rows)
                   content: '())))
    (format #t "tgroup tgroupstyle=~s (~s)\n" style style-name)
    (set-content! ft (append
                      (parse-sgml-table-cells ft h-rows #t)
                      (parse-sgml-table-cells ft b-rows)))
    (emit ft)))

(define (parse-sgml-table-cells ft rows #optional header?)
  (map (lambda (r)
         (map (lambda (c)
                ;; XXX pernicious mixed content... could be a <PARA> itself
                (parse-sgml-table-cell ft c header?))
              (xpath* r "entry")))
       rows))

(define (parse-sgml-table-cell ft node #optional header?)
  (revisionflag-check 
   node
   (make <table-cell>
         owner: ft
         content: (make <flow>
                        content: (list
                                  (make-paragraph-from-inline 
                                   (content node)
                                   (if header?
                                       'table-header-style
                                       'body-para-style)
                                   bridge-inline-stuff))))))


(define-method layout-flow-member ((self <flow-table>)
                                   pstream
                                   (fli <flow-layout-info>))
  (bind ((sf0 y0 (request pstream 0 'in-column))
         (ft (layout-table self sf0))
         (sf y (request pstream (height ft) 'in-column)))
    (add-vl-to-flow-layout! 
     fli
     (make <vlist>
           placement: (make <placement>
                            subframe: sf
                            y: y)
           content: (list ft)))))

(define star-plus-width (reg-expr->proc '(seq (save (* digit)) 
                                              #\*
                                              #\+
                                              (save
                                               (seq (+ (or digit #\.))
                                                    (* alpha))))))

(define star-width (reg-expr->proc '(seq (save (+ digit)) #\*)))

(define (parse-length str)
  (if (string=? str "?")
      '?
      (if (string=? str "*")
          '(p 1)
          (bind ((s e p f (star-plus-width str)))
            (if s
                (list 'p 
                      (if (string=? p "")
                          1
                          (string->number p))
                      (or (parse-inches f)
                          (string->number f)
                          (error "Could not parse length spec: ~s" str)))
                (bind ((s e n (star-width str)))
                  (if s
                      (list 'p (string->number n))
                      (or (parse-inches str)
                          (string->number str)
                          (error "Could not parse length spec: ~s" str)))))))))

