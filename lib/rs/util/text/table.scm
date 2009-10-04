;;;
;;;  Usage:
;;;
#|
(display-table '("Name" "Age") '(("Alice" 26) ("Bob" 27)))
|#
;;;

;;;
;;; a table is an outer-boxed matrix
;;; with a title box, like:
;;;
;;;    +============+=============+=====+
;;;    | Name of    |             |     |
;;;    | Individual | SSN         | Age |
;;;    +============+=============+=====+
;;;    | Bob          123-45-6789   22  |
;;;    | Alice        234-56-7890   25  |
;;;    | Chris        345-67-8901   23  |
;;;    +================================+
;;;

(define *table-title-cell-fmt* (make <cell-format>
                                     cf-left-border-style: 1
                                     cf-right-border-style: 1
                                     cf-bottom-border-style: 2
                                     cf-top-border-style: 2
                                     cf-vert-align: ':bottom))

(define *table-null-bottom-cell* (list ""
                                       (make <cell-format>
                                             cf-top-border-style: 2)))

(define *table-left-cell-fmt* (make <cell-format>
                                    cf-left-border-style: 1))
(define *table-mid-cell-fmt* (make <cell-format>))
(define *table-right-cell-fmt* (make <cell-format>
                                    cf-right-border-style: 1))

(define (null-bottom c) *table-null-bottom-cell*)
(define (title-cell c) (list c *table-title-cell-fmt*))
(define (left-cell c) (list c *table-left-cell-fmt*))
(define (mid-cell c) (list c *table-mid-cell-fmt*))
(define (right-cell c) (list c *table-right-cell-fmt*))

(define (display-table/list titles data)
  ;
  (display-matrix
   (vector-append
    ;; title box
    (vector
     (list->vector (map title-cell titles)))
    ;; data rows
    (vector-map
     (lambda (row)
       (list->vector
        (append (list (left-cell (car row)))
                (map mid-cell
                     (reverse (cdr (reverse (cdr row)))))
                (list (right-cell (last row))))))
     (list->vector data))
    ;; null bottom row to get the bottom border on
    (vector
     (list->vector (map null-bottom titles))))))

(define (display-table/vector titles data)
  ;
  (display-matrix
   (vector-append
    ;; title box
    (vector
     (vector-map title-cell titles))
    ;; data rows
    (let* ((num-cols (vector-length (vector-ref data 0)))
           (row-template (vector-append (vector left-cell)
                                        (make-vector (- num-cols 2) mid-cell)
                                        (vector right-cell))))
      (vector-map
       (lambda (row)
         (vector-map (lambda (f c)
                       (f c))
                     row-template
                     row))
       data))
    ;; null bottom row to get the bottom border on
    (vector
     (vector-map null-bottom titles)))))


(define (display-table titles data)
  (cond
   ((list? titles)
    (display-table/list titles data))
   ((vector? titles)
    (display-table/vector titles data))
   (else
    (error "Don't know how to display sequence type ~s" 
           (object-class titles)))))
