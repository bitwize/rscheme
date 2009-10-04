(define (media-box page-style)
  (bind ((w h (get-style-attributes/m page-style '(page-width page-height))))
    (make-rect 0 0 w h)))


(define (style? x)
  (instance? x <style>))

(define (get-style-attributes/m style attr-list)
  (list->values (get-style-attributes style attr-list)))

(define-style-type <page-style>
  page-width
  page-height
  numbering-format
  numbering-group                     ; default: 'body
  page-start                          ; (any recto(=right) verso(=left))
  content
  orientation)

;;;

(define-style-type <paragraph-style>
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
   )

(define-style-type <character-style>
  font
  color
  spread  ; +/- percent ... 0 = normal
  underline
  overline
  strikethrough
  baseline-shift
  smallcaps
  annotation
  kerning?)


(define-style-definer define-character-style <character-style>)
(define-style-definer define-paragraph-style <paragraph-style>)
(define-style-definer define-page-style <page-style>)

;;;

(define (make-superscript self)
  (let* ((f (get-style-attribute self 'font))
         (s (get-style-attribute f 'size))
         (nf (override-style f size: (/ s 2))))
    (format #t "SUPERSCRIPT of ~s ==> ~s ~s\n" 
            f
            nf (/ s 2))
    (override-style self
                    baseline-shift: (/ s 2)
                    font: nf)))
    
(define-method make-bold ((self <font-style>))
  (override-style self
                  weight: 'bold))

(define-method make-bold ((self <symbol>))
  (make-bold (get-style self)))

(define-method make-bold ((self <character-style>))
  (override-style self
                  font: (make-bold (get-style (query-style self 'font)))))

(define *family-font-map*
  '((("Times" normal normal normal) ("Times" "Roman"))
    (("Times" bold normal normal) ("Times" "Bold"))
    (("Times" normal italic normal) ("Times" "Italic"))
    (("Times" bold italic normal) ("Times" "Bold Italic"))

    (("Palatino" normal normal normal) ("Palatino" "Roman"))
    (("Palatino" bold normal normal) ("Palatino" "Bold"))
    (("Palatino" normal italic normal) ("Palatino" "Italic"))
    (("Palatino" bold italic normal) ("Palatino" "Bold Italic"))

    (("Helvetica" normal normal normal) ("Helvetica" "Regular"))
    (("Helvetica" bold normal normal) ("Helvetica" "Bold"))
    (("Helvetica" normal italic normal) ("Helvetica" "Italic"))
    (("Helvetica" bold italic normal) ("Helvetica" "Bold Italic"))

    (("Symbol" normal normal normal) ("Symbol" "Regular"))

    (("ZapfDingbats" normal normal normal) ("ZapfDingbats" "Regular"))
    
    (("Courier" normal normal normal) ("Courier" "Regular"))
    (("Courier" bold normal normal) ("Courier" "Bold"))
    (("Courier" normal italic normal) ("Courier" "Italic"))
    (("Courier" bold italic normal) ("Courier" "Bold Italic"))

    (("Minion" normal normal condensed) ("Minion" "Condensed"))
    (("Minion" bold normal condensed) ("Minion" "Condensed Bold"))
    (("Minion" normal italic condensed) ("Minion" "Condensed Italic"))
    (("Minion" bold italic condensed) ("Minion" "Condensed Bold Italic"))

    (("Univers" normal normal condensed) ("Univers" "Condensed"))
    (("Univers" bold normal condensed) ("Univers" "Condensed Bold"))
    (("Univers" normal italic condensed) ("Univers" "Condensed Italic"))
    (("Univers" bold italic condensed) ("Univers" "Condensed Bold Italic"))

    (("BriemMono" normal normal condensed) ("BriemMono" "Condensed"))
    (("BriemMono" bold normal condensed) ("BriemMono" "Condensed Bold"))
    (("BriemMono" normal italic condensed) ("BriemMono" "Condensed Italic"))
    (("BriemMono" bold italic condensed) ("BriemMono" "Condensed Bold Italic"))

    ))

(define-method query-style ((self <style>) what)
  (get-style-attribute self what))

(define-method query-style ((self <paragraph-style>) what)
  (case what
    ((line-spacing)
     (bind ((ls (get-style-attribute self 'line-spacing)))
       (if (eq? ls 'using-font-size)
           (query-style
            (query-style
             (query-style self 'default-char-style)
             'font)
            'size)
           ls)))
    (else
     (get-style-attribute self what))))

(define-method query-style ((self <symbol>) what)
  (query-style (get-style self) what))

(define-method query-style ((self <font-style>) what)
  (case what
    ((afm)
     (font-metrics (font-shape (query-style self 'font))))
    ((font)
     (get-text-font self))
    (else
     (get-style-attribute self what))))

(define-method style-compile ((self <font-style>))
  (let ((font (query-style self 'font)))
    (lambda (dev)
      (setfont dev font))))

(define-method style-compile ((self <character-style>))
  (bind ((font color spread baseline-shift annot (get-style-attributes/m
                                                  self
                                                  '(font
                                                    color
                                                    spread
                                                    baseline-shift
                                                    annotation)))
         (apply-font (style-compile font))
         (apply-color (style-compile color))
         (the-font (query-style font 'font))
         (fm (font-metrics the-font))
         (dx (if (zero? spread)
                 0
                 (/ spread 100)))            ; spread is in percent (%)
         (dy (if (zero? baseline-shift)
                 0
                 (* baseline-shift 
                    (get-property fm 'CapHeight)
                    (/ 100)))))         ; baseline shift is in percent (%)
    (lambda (dev (text <string>) #optional x-widths)
      (apply-color dev)
      (apply-font dev)
      ;;
      (define (do-show)
        (if x-widths
            (xshow dev text x-widths)  ; they already took spread into account
            (if (not (zero? dx))
                (xshow dev
                       text
                       (map (lambda (cw)
                              (* cw dx))
                            (char-widths fm text)))
                (show dev text))))
      ;;
      (if (not (zero? baseline-shift))
          (begin
            (translate dev (make-point 0 dy))
            (do-show)
            (translate dev (make-point 0 (- dy))))
          (do-show))
      ;;
      (if annot
          (let ((xw (or x-widths
                        (map (lambda (cw)
                               (+ cw (* cw dx)))
                             (char-widths fm text)))))
            (annot dev 
                   (make-rect 0 (- dy 2) (reduce + 0 xw) 
                              (font-size the-font))))))))

                                     
                                     

;;;

(define (char-style-afm cf)
  (query-style (query-style cf 'font) 'afm))

(define (char-style-size cf)
  (query-style (query-style cf 'font) 'size))

(define (trim-rect r #key 
                   (left default: #f)
                   (top default: #f)
                   (bottom default: #f)
                   (right default: #f))
  (bind ((x y w h (rect->values r))
         (w (if right (- w right) w))
         (h (if top (- h top) h))
         (x w (if left (values (+ x left) (- w left)) (values x w)))
         (y h (if bottom (values (+ y bottom) (- h bottom)) (values y h))))
    (make-rect x y w h)))


(define (make-vspace h)
  (make <flow-vbox>
        height: h
        render-proc: (lambda (self width dev))))

(define (make-hrule h dy #key 
                    (linewidth default: 0.5)
                    (left default: 0)
                    (right default: 0))
  (make <flow-vbox>
        height: h
        render-proc: (lambda (self width dev)
                       (setlinewidth dev linewidth)
                       (moveto dev (make-point left dy))
                       (lineto dev (make-point (- width right) dy))
                       (stroke dev))))



;;;

(define (override-style base #rest over)
  (let ((b (get-style base)))
    (apply make-style
           (object-class (get-style base))
           basis: b
           over)))

(define (as-symbol-char-style cf)
  (override-style
   cf
   font: (override-style (query-style cf 'font)
                         family: "Symbol"
                         variation: 'normal)))

;;

(define-page-style letter-landscape ()
  page-width: 792
  page-height: 612
  orientation: 'landscape
  page-start: 'any
  content: '()
  numbering-group: 'body
  numbering-format: 'arabic)

(define-page-style letter ()
  page-width: 612
  page-height: 792
  page-start: 'any
  orientation: 'portrait
  content: '()
  numbering-group: 'body
  numbering-format: 'arabic)

(define-page-style letter-booklet ()
  page-width: 535
  page-height: 792
  page-start: 'any
  orientation: 'portrait
  content: '()
  numbering-group: 'body
  numbering-format: 'arabic)

;;;

(define (eval-dynamic-content items page)
  (string-join "" (map (lambda (item)
                         (cond
                          ((string? item)
                           item)
                          ((pair? item)
                           (case (car item)
                             ((page-number)
                              (page-label page))
                             ((ref)
                              (or (get-document-variable (in-document page)
                                                         (cadr item))
                                  "???"))
                             ((section-page-header)
                              (section-page-header page))
                             (else
                              "???")))
                          (else
                           "????")))
                       items)))
