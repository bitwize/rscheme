(define-class <style> (<object>) :abstract
  (properties type: <vector> init-value: '#())
  (name init-value: #f)         ; or a symbol
  (basis init-value: #f))

(define (style? x)
  (instance? x <style>))

(define-method write-object ((self <style>) port)
  (if (name self)
      (format port "#[~a ~a]" (name (object-class self)) (name self))
      (next-method)))

(define-class <stroke-style> (<style>))
(define-class <fill-style> (<style>))
(define-class <color-style> (<style>))
(define-class <character-style> (<style>))
(define-class <font-style> (<style>))
(define-class <paragraph-style> (<style>))
(define-class <page-style> (<style>))

(define $page-style-attributes
  '(page-width
    page-height
    numbering-format
    numbering-group                     ; default: 'body
    page-start                          ; (any recto(=right) verso(=left))
    content
    orientation))                       ; (portrait landscape)
    
(define $paragraph-style-attributes
  '(
    default-char-style
    lines                               ; ISO/IEC 10179:1996(E) p.218
    ;;
    ;;  Basic horizontal spacing
    ;;
    left-margin-first                   ; relative to frame.x; first line
    left-margin                         ; relative to frame.x; subseq lines
    right-margin-last                   ; relative to frame.limit-x; last line
    right-margin                        ; rel. to frame.limit-x; previous lines
    tab-stops                           ; list of tab stops, each:
                                        ;    (position: ({l|r} distance)
                                        ;     align: {left|right|<char>}
                                        ;     [leading: "str"])
    align                               ; align non-tabbed content
                                        ;  {left|right|center|justify}
    ;;
    ;; Basic vertical spacing
    ;;
    space-before                        ; before the para
    space-after                         ; after the para
    line-spacing                        ; min. line spacing (baseline-baseline)
    line-spacing-fixed?                 ; fixed? else adjust to line content
    ;;
    ;;  pagination control
    ;;
    start                               ; (column page left-page right-page)
    keep-with-next?
    keep-with-previous?
    widow-control                       ; column have at least this many lines
    placement                           ; (in-column
                                        ;  sidebar/first-baseline
                                        ;  sidebar/top-edge
                                        ;  sidebar/last-baseline
                                        ;  across-columns
                                        ;  across-frame)
   ))

(define $font-style-attributes '(family
                                 size
                                 angle
                                 weight
                                 variation
                                 stretch))

(define $character-style-attributes '(font
                                      color
                                      spread  ; +/- percent ... 0 = normal
                                      underline
                                      overline
                                      strikethrough
                                      baseline-shift
                                      smallcaps
                                      annotation
                                      kerning?))

(define $fill-style-attributes '(color
                                 winding-rule))

(define $stroke-style-attributes '(line-width
                                   line-join
                                   line-cap
                                   miter-limit
                                   color))

(define (setstyle dev style)
  (dm "setstyle ~s" style)
  ((compile-style style) dev))

(define-method getstyle ((self <style>))
  self)

(define-method getstyle ((self <symbol>))
  (or (table-lookup *style-dictionary* self)
      (em "No such style: ~s" self)))

(define-method compile-style ((self <symbol>))
  (compile-style (getstyle self)))
        

(define-method query-style ((self <symbol>) query)
  (query-style (getstyle self) query))

(define-method query-style ((self <style>) what)
  (get-style-attributes self what))

(define-method compile-style ((self <stroke-style>))
  (bind ((line-width line-join line-cap miter-limit color
                       (get-style-attributes self
                                             'line-width
                                             'line-join
                                             'line-cap
                                             'miter-limit
                                             'color))
         (setcolor (compile-style color)))
    (lambda (dev)
      (setcolor dev)
      (setlinewidth dev line-width))))

(define-method compile-style ((self <fill-style>))
  (bind ((color winding-rule (get-style-attributes self
                                                   'color
                                                   'winding-rule))
         (setcolor (compile-style color)))
    (lambda (dev)
      (setcolor dev))))

(define $color-style-attributes '(color gray))

(define (spec->color-style spec)
  (case spec
    ;; these are well-known color-style names
    ((black white)
     spec)
    ;;
    ;; otherwise, construct a new color-style
    (else
     (if (pair? spec)
         (case (car spec)
           ((gray) (color-style gray: (cadr spec)))
           ((rgb) (color-style color: (make-color red: (cadr spec)
                                                  green: (caddr spec)
                                                  blue: (cadddr spec))))
           (else
            (em "Bad color spec: ~s" spec)))
         (em "Bad color spec (not a list): ~s" spec)))))

(define-method compile-style ((self <color-style>))
  (bind ((color gray (get-style-attributes self 'color 'gray))
         (pix #f)
         (pix-for-dev #f))
    (lambda (dev)
      (if (not (eq? dev pix-for-dev))
          (begin
            (set! pix-for-dev dev)
            (if color
                (bind ((r g b (color-rgb color)))
                  ;(dm "color style ~s => ~s" self (list r g b))
                  (set! pix (device-color dev (list 'rgb r g b))))
                (set! pix (device-color dev (list 'gray gray))))))
      (setcolor dev pix))))

(define-method get-style-attributes ((self <symbol>) #rest lst)
  (apply get-style-attributes (getstyle self) lst))

(define-method get-style-attributes ((self <style>) #rest lst)
  (let ((tbl (flatten-style-attributes self)))
    (list->values (map (lambda (k)
                         (table-lookup tbl k))
                       lst))))
    
(define-method flatten-style-attributes ((s <symbol>))
  (flatten-style-attributes (getstyle s)))

(define-method flatten-style-attributes ((s <style>))
  (let ((tbl (if (basis s)
                 (flatten-style-attributes (basis s))
                 (make-symbol-table)))
        ((p <vector>) (properties s))
        ((n <fixnum>) (vector-length (properties s))))
    (with-module
        usual-inlines
      (let loop (((i <fixnum>) 0))
        (if (eq? i n)
            tbl
            (begin
              (table-insert! tbl (vector-ref p i) (vector-ref p (add1 i)))
              (loop (fixnum+ i 2))))))))

(define *style-dictionary* (make-symbol-table))

(define (set-style-attribute! (self <style>) key value)
  ;;; XX need to check if it's allowable...
  (set-property! self (keyword->symbol key) value))

(define (set-style-attributes! (s <style>) allowable properties)
  (let loop ((p properties))
    (cond
     ((null? p) s)
     ((not (keyword? (car p)))
      (em "~s: not a keyword: ~s"
          (name (object-class s)) 
          (car p)))
     ((not (pair? (cdr p)))
      (em "~s: missing value after ~s" 
          (name (object-class s)) 
          (car p)))
     ((eq? (car p) 'basis:)
      (let ((b (cadr p)))
        (if (not (or (symbol? b)
                     (instance? b (object-class s))))
            (em "~s: incompatible basis ~s" (name (object-class s)) b))
        (set-basis! s b)
        (loop (cddr p))))
     ((not (memq (keyword->symbol (car p)) allowable))
      (em "~s: not an allowable keyword: ~s" 
          (name (object-class s)) 
          (car p)))
     (else
      (set-property! s (keyword->symbol (car p)) (cadr p))
      (loop (cddr p))))))

(define (color-style . properties)
  (set-style-attributes! (make <color-style>)
                         $color-style-attributes
                         properties))

(define (fill-style . properties)
  (set-style-attributes! (make <fill-style>)
                         $fill-style-attributes
                         properties))

(define (stroke-style . properties)
  (set-style-attributes! (make <stroke-style>)
                         $stroke-style-attributes
                         properties))

(define (override-style basis . overrides)
  (let ((b (getstyle basis)))
    (set-style-attributes! (make (object-class b))
                           (key-sequence (flatten-style-attributes b))
                           `(basis: ,basis ,@overrides))))
            


(define (paragraph-style . properties)
  (set-style-attributes! (make <paragraph-style>)
                         $paragraph-style-attributes
                         properties))

(define (font-style . properties)
  (set-style-attributes! (make <font-style>)
                         $font-style-attributes
                         properties))

(define (character-style . properties)
  (set-style-attributes! (make <character-style>)
                         $character-style-attributes
                         properties))

(define (page-style . properties)
  (set-style-attributes! (make <page-style>)
                         $page-style-attributes
                         properties))

(define (define-style (name <symbol>) s)
  (let ((s (clone s)))
    (set-name! s name)
    (table-insert! *style-dictionary* name s)))

(define-style 'default-fill (fill-style color: 'black))
(define-style 'default-line (stroke-style color: 'black
                                          line-width: 1
                                          line-join: 0
                                          line-cap: 0
                                          miter-limit: 1))
                             
(define-style 'black (color-style gray: 0))
(define-style 'white (color-style gray: 1))

(define-style 'black-line (stroke-style basis: 'default-line))
(define-style 'white-line (stroke-style basis: 'default-line
                                        color: 'white))
(define-style 'white-fill (fill-style basis: 'default-fill
                                      color: 'white))

(define (default-style-dictionary)
  (hash-table-copy *style-dictionary*))

(define-class <style-set> (<object>)
  style-set-stroke
  style-set-fill)
  
(define (default-style-set)
  (make <style-set>
        style-set-stroke: 'black-line
        style-set-fill: 'white-fill))
