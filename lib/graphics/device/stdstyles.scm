
;;;

(define-style-type <font-style>
  family
  size
  angle
  weight
  width
  variation
  stretch)

(define-style-type <stroke-style>
  paint
  linewidth
  linejoin
  linecap
  miterlimit)

(define-style-type <fill-style>
  paint)

(define-style-type <solid-paint-style>
  color)

(define-style-type <pattern-paint-style>
  pattern
  color)

;;;

(define-method get-text-font ((self <font-style>))
  (bind ((fam siz ang wei wid var (list->values
                                   (get-style-attributes self '(family
                                                                size
                                                                angle
                                                                weight
                                                                width
                                                                variation))))
         (lst (query-font-database family: fam
                                   angle: ang
                                   weight: wei
                                   variation: (if (eq? var 'none)
                                                  #f
                                                  var)
                                   width: (if (eq? wid 'normal)
                                              #f
                                              wid))))
    (case (length lst) 
      ((1)
       (get-text-font (font-family (car lst))
                      (font-style (car lst))
                      siz))
      ((0)
       (error "~s: No matching font found" self))
      (else
       (error "~s: ~d fonts found: ~s" self (length lst) lst)))))
  
(define-method style-compile ((self <font-style>))
  (let ((fnt (get-text-font self)))
    (lambda (dev)
      (setfont dev fnt))))

(define-method style-compile ((self <fill-style>))
  (bind ((p (list->values (get-style-attributes self '(paint))))
         (pc (style-compile p 'fill)))
    (lambda (dev)
      (pc dev))))

(define-method style-compile ((self <stroke-style>))
  (bind ((p lw lj lc ml (list->values (get-style-attributes self
                                                            '(paint
                                                              linewidth
                                                              linejoin
                                                              linecap
                                                              miterlimit))))
         (pc (style-compile p 'stroke)))
    (lambda (dev)
      (setlinewidth dev lw)
      (if lj (setlinejoin dev lj))
      (if lc (setlinecap dev lc))
      (if ml (setmiterlimit dev ml))
      (pc dev))))
    
;;;



(define-style-definer define-stroke-style <stroke-style>)
(define-style-definer define-fill-style <fill-style>)
(define-style-definer define-font-style <font-style>)
(define-style-definer define-solid-paint-style <solid-paint-style>)
(define-style-definer define-pattern-paint-style <pattern-paint-style>)

;;;

(define-method style-compile ((self <solid-paint-style>) #optional mode)
  (let ((c (get-style-attribute self 'color)))
    (lambda (dev)
      (setcolor dev (device-color dev c) mode))))

(define-method style-compile ((self <pattern-paint-style>) #optional mode)
  (let ((p (get-style-attribute self 'pattern))
        (c (get-style-attribute self 'color)))
    (if c
        (lambda (dev)
          (setcolor dev (device-color dev (list 'pattern p c)) mode))
        (lambda (dev)
          (setcolor dev (device-color dev (list 'pattern p)) mode)))))
    

(define-method setfillstyle ((self <graphics-device>) style)
  (style-apply self style))

(define-method setstrokestyle ((self <graphics-device>) style)
  (style-apply self style))

;;;

(define (apply-gstate-changes dev lst)
  (let loop ((a (keyword-list->assoc-list lst)))
    (if (null? a)
        (values)
        (begin
          (case (caar a)
            ((linewidth:) (setlinewidth dev (cdar a)))
            ((color:) (setcolor dev (device-color dev (cdar a))))
            ((dash:) (setdash dev (cdar a) 0))
            (else
             (error "Unknown gstate change keyword: ~s" (caar a))))
          (loop (cdr a))))))

(define (filler dev style)
  (cond
   ((eq? style #t)
    (lambda () (fill dev)))
   ((pair? style)
    (lambda ()
      (apply-gstate-changes dev style)
      (fill dev)))
   (else
    (lambda ()
      (setfillstyle dev style)
      (fill dev)))))

(define (stroker dev style)
  (cond
   ((eq? style #t)
    (lambda () 
      (stroke dev)))
   ((pair? style)
    (lambda ()
      (apply-gstate-changes dev style)
      (stroke dev)))
   (else
    (lambda ()
      (setstrokestyle dev style)
      (stroke dev)))))

;;;
;;;  `fill' and `stroke' are specifications of
;;;  how the given area should be filled or stroked.
;;;
;;;  Each value is one of:
;;;    - #f, meaning it should not be filled or stroked
;;;    - #t, meaning it should be filled or stroked using
;;;          the current graphics state
;;;    - a symbol, meaning the fill or stroke style should
;;;                applied
;;;    - a keyword-value list, meaning that some changes
;;;                            to the graphics state should be applied
;;;

(define-method areashow ((self <graphics-device>) area 
                         #key (fill default: #f) (stroke default: #f))
  (let ((f (and fill (filler self fill)))
        (s (and stroke (stroker self stroke))))
    (cond
     ((and f s)
      (areapath self area)
      (with-gstate-saved self f)
      (s))
     (f
      (areapath self area)
      (f))
     (s
      (areapath self area)
      (s)))))

