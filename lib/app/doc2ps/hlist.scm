
;;

(define *show-outlines* #f)             ; turn on with '--show-outlines'

;;

(define-class <eps-graphic> (<object>)
  (properties init-value: '#())
  (bbox init-value: '#uninit)
  frame         ; a <point> or a <rect>
  content)      ; a filename

(define-method initialize ((self <eps-graphic>))
  (set-bbox! self (parse-eps (content self))))

(define-method render ((self <eps-graphic>) dev)
  (with-gstate-saved
   dev
   (lambda ()
     (translate dev (frame self))
     (render-eps dev #f (bbox self) (content self) (get-property self 'scale 1) #f))))

(define-class <text-box> (<object>)
  frame
  content
  style
  align)

(define-method render ((self <text-box>) dev)
  ;; build the format string -- XXX use generate-text-runs etc. for this
  (let* ((str (eval-dynamic-content (content self)
                                    (current-page dev)))
         (cf (style self))
         (font-size (char-style-size cf))
         (cw-list (map (lambda (cw)
                         (* cw font-size))
                       (char-widths (char-style-afm cf) str)))
         (sw (reduce + 0 cw-list)))
    ;;
    (render-layout dev
                   (list
                    (cons cf
                          (map (lambda (ch cw)
                                 (list ch cw 0))
                               (string->list str)
                               cw-list)))
                   (case (align self)
                     ((left) (origin (frame self)))
                     ((right) (point+ (lower-right (frame self))
                                      (make-size (- sw) 0)))
                     ((center) (point+ (origin (frame self))
                                       (make-size (/ (- (size-width (frame self))
                                                        sw)
                                                     2)
                                                  0)))
                     (else (error "bad textbox align ~s" (align self)))))))
                                    
;;;

(define-class <script-graphic> (<object>)
  frame
  script
  (argv init-value: '()))

(define-class <line-graphic> (<object>)
  line-start
  line-end
  (line-style init-value: 'default-stroke))

(define (render-rectangle dev frame #key stroke-color line-width fill-color)
  (if fill-color
      (begin
        (setcolor dev (device-color dev fill-color))
        (rectfill dev frame)))
  (if stroke-color
      (begin
        (setlinewidth dev line-width)
        (setcolor dev (device-color dev stroke-color))
        (rectstroke dev frame))))

(define *script-render-procs*
  (list (cons 'rectangle render-rectangle)))

(define (add-script-renderer! name proc)
  (let ((old (assq name *script-render-procs*)))
    (if old
        (set-cdr! old proc)
        (set! *script-render-procs* (cons (cons name proc) 
                                          *script-render-procs*)))
    (values)))

(define (script-proc (self <script-graphic>))
  (let ((s (script self)))
    (cond
     ((symbol? s)
      (let ((f (assq s *script-render-procs*)))
        (if f
            (cdr f)
            (error "unknown script: ~s" s))))
     ((procedure? s)
      s)
     (error
      (error "unknown script: ~s" s)))))
      
(define-method render ((self <script-graphic>) dev)
  (let ((f (script-proc self)))
    (with-gstate-saved
     dev
     (lambda ()
       (apply f dev (frame self) (argv self))))))

(define-method render ((self <line-graphic>) dev)
  (with-gstate-saved
   dev
   (lambda ()
     ((style-compile (line-style self)) dev)
     (moveto dev (line-start self))
     (lineto dev (line-end self))
     (stroke dev))))

;;;

(define-class <flow-layout> (<object>)
  (content init-value: '()))

(define-class <vlist> (<object>)
  (placement type: <placement>)
  content)

(define-method add-vl-to-flow-layout! ((self <flow-layout>) (v <vlist>))
  (set-content! self (append! (content self) (list v))))

(define-class <vitem> (<object>) :abstract)     ; appears in a <vlist>

(define-class <hline> (<vitem>)
  height
  content)              ; a sequence of <hlist>'s

(define-class <vbox> (<vitem>)
  height
  width
  x
  (content type: <flow-vbox>))

(define-class <vskip> (<vitem>)
  height)

  
(define-class <hlist> (<object>)
  para
  width
  start-index
  index-count
  x
  align
  stretch
  hyphenate?)

(define-method width ((self <hline>))
  (reduce + 0 (map (lambda ((h <hlist>))
                     (+ (width h) (x h)))
                   (content self))))

(define-method initialize ((self <hlist>))
  (assert (> (index-count self) 0)))

(define-method render ((self <flow-layout>) dev)
  (for-each
   (lambda ((v <vlist>))
     (render v dev))
   (content self)))


(define-method render ((self <vlist>) dev)
  (let ((r (subframe-rect (placement self))))
    ;(format #t "render vlist ~s  subframe => ~s\n" self r)
    ;(rectstroke dev r)
    (let loop ((y (- (limit-y r) (y (placement self))))
               (lines (content self)))
      ;(format #t "   vlist y= ~s\n" y)
      (if (null? lines)
          y
          (if (instance? (car lines) <vitem>)
              ;; the subframe-placement is already right for the first baseline
              (let ((y (if (eq? lines (content self))
                           y
                           (- y (height (car lines))))))
                ;(format #t "         == ~s\n" (car lines))
                (render (car lines) #f (origin-x r) y dev self)
                (loop y (cdr lines)))
              (loop (- y (car lines)) (cdr lines)))))))

(define-method render ((self <vskip>) stream x0 y0 dev vlist)
  (values))


(define *ligatures* '(("fi" #\o256)
                      ("fl" #\o257)))

(define-method render ((self <flow-vbox>) width dev)
  ((render-proc self) self width dev))

(define-method render ((self <vbox>) stream x0 y0 dev vlist)
  ;(format #t "    render vbox-script ~s -- ~d ~d\n" self x0 y0)
  (with-gstate-saved
   dev
   (lambda ()
     (translate dev (make-point x0 y0))
     (if *show-outlines*
         (render-outline self dev))
     (render (content self) (width self) dev))))

(define-method render ((self <hline>) stream x0 y0 dev vlist)
  ;(format #t "    render line ~s -- ~d ~d\n" self x0 y0)
  (for-each (lambda (hl)
              (render hl stream x0 y0 dev vlist))
            (content self)))

(define-method origin ((self <anchor>))
  (let ((sfr (subframe-rect (placement (owner self)))))
    (point+ (upper-left sfr)
            (make-size (relative-x self)
                       (- (relative-y self))))))

(define-method render ((self <anchor>) dev)
  (if *show-outlines*
      (with-gstate-saved
       dev
       (lambda ()
         (translate dev (origin self))
         (moveto dev (make-point -1 0))
         (lineto dev (make-point 0 1.5))
         (lineto dev (make-point 1 0))
         (lineto dev (make-point 0 -1.5))
         (closepath dev)
         (fill dev)))))

(define-method render ((self <hlist>) stream x0 y0 dev vlist)
  ;(format #t "        render hlist ~s -- (~d ~d)\n" self x0 y0)
  (bind ((tr new-stream anchors (generate-text-runs self stream)))
    (for-each (lambda ((a <anchor>))
                (render a dev))
              anchors)
    (render-layout dev 
                   (fold-over-struts
                    (if (zero? (stretch self))
                        (no-stretch tr)
                        (apply-stretch tr (stretch self))))
                   (make-point (+ x0 (x self)) y0))))

(define (ligature-check style str)
  (if (query-style style 'kerning?)
      (let ((k (assoc str *ligatures*)))
        ;(format #t "~s ~s ==> kerning on: ~s\n" style str k)
        k)
      (begin
        ;(format #t "~s ~s ==> kerning off\n" style str)
        #f)))

;; note that "text-runs" in the title of this procedure is
;; not the same thing as the contentmodel <text-run>...

(define-method generate-text-runs ((self <hlist>) stream)
  (let ((stream (or stream
                    (open-inline-stream (para self) (start-index self))))
        (need-hyph? (hyphenate? self)))
    ;;
    (define (build-run style accum)
      (let* ((fontsize (char-style-size style))
             (afm (char-style-afm style))
             (font (query-style (query-style style 'font) 'font))
             (spread (/ (query-style style 'spread) 100))
             (char-list (select char? accum))
             (str (list->string char-list))
             (char-width-list (char-widths (font-metrics font) str))
             (kern (map (lambda (cw ka)
                          (+ (* ka fontsize)
                             (* cw spread)))
                        char-width-list
                        (if (query-style style 'kerning?)
                            (append (string-x-deltas afm str) '(0))
                            (map (lambda (ch) 0) char-list)))))
        (let loop ((k kern)
                   (cw char-width-list)
                   (i accum)
                   (r '()))
          (if (null? i)
              (cons style (reverse! r))
              (if (char? (car i))
                  (loop (cdr k)
                        (cdr cw)
                        (cdr i)
                        (cons (list (car i) 
                                    (car cw)
                                    (car k))
                              r))
                  (loop k cw (cdr i) (cons (car i) r)))))))
    ;;
    (let loop ((i 0)
               (style #f)
               (accum '())
               (aruns '())
               (anchors '()))
      ;(format #t "/// ~s ~s ~s ~s\n" i style accum aruns)
      (if (>= i (index-count self))
          (values (reverse! 
                   (cons (build-run style 
                                    (reverse! 
                                     (if need-hyph?
                                         (cons #\- accum)
                                         accum)))
                         aruns))
                  stream
                  anchors)
          (bind ((type info detail (stream)))
            (format #t "IN HLIST[~d]: ~s ~s ~s\n" i type info detail)
            (case type
              ((-)
               (if (and need-hyph?
                        (= (+ i 1) (index-count self)))
                   (begin
                     (set! need-hyph? #f)
                     (loop (+ i 1) style (cons #\- accum) aruns anchors))
                   (loop (+ i 1) style accum aruns anchors)))
              ((/)
               (loop (+ i 1) style accum aruns anchors))
              ((anchor)
               (loop (+ i 1) style accum aruns (cons info anchors)))
              ((space)
               ;; go through some trouble to try to use a space
               ;; character to represent the space
               (bind ((new-style info)
                      (strut glue (parse-space info detail))
                      (space-width (style-char-width new-style #\space))
                      (accum aruns (if (or (not style)
                                           (eq? style info))
                                       (values accum aruns)
                                       (if (null? accum)
                                           (values '() aruns)
                                           (values '() (cons 
                                                        (build-run
                                                         style
                                                         (reverse! accum))
                                                        aruns)))))
                      (accum+glue (if (zero? glue)
                                      accum
                                      (cons (list 'stretch glue) accum))))
                 (loop (+ i 1)
                       new-style
                       (if (> strut (* space-width 0.75))
                           (cons* #\space
                                  (list 'strut (- strut space-width))
                                  accum+glue)
                           (cons* (list 'strut strut) accum+glue))
                         aruns
                         anchors)))
              ((char)
               (if (or (not style)
                       (eq? style info))
                   (loop (+ i 1) 
                         (or style info)
                         (if (null? accum)
                             (cons detail '())
                             (let ((lig (ligature-check 
                                         style
                                         (string (car accum) detail))))
                               (if lig
                                   (if (null? (cdr accum))
                                       (cons (cadr lig) '())
                                       (cons (cadr lig) (cdr accum)))
                                   (cons* detail
                                          (if (char=? detail #\space)
                                              $iws
                                              $ics)
                                          accum))))
                         aruns
                         anchors)
                   (loop (+ i 1) 
                         info 
                         (cons detail '()) 
                         (cons (build-run style (reverse! (cons $ics accum)))
                               aruns)
                         anchors)))
              ((#f)
               (error "hlist ~s EOF at ~d ; expected index-count ~d"
                      self
                      i
                      (index-count self)))
              (else
               (error "Unexpected ~s in hlist" type))))))))

(define $ics '(stretch 10))
(define $iws '(stretch 50))

