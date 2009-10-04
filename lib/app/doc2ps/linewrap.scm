,(use sort)

(define *debug-linewrap* #f)

(define-macro (dbegin . form)
  `(begin
     (if *debug-linewrap*
         (begin ,@form))
     (values)))

(define-macro (dformat msg . args)
  `(begin
     (if *debug-linewrap*
         (format #t ,msg ,@args))
     (values)))

(define (tab-position b0 b1 tab) 
  (let ((p (cadr (memq 'position: tab))))
    (if (number? p)
        (+ b0 p)
        (case (car p)
          ((l) (+ b0 (cadr p)))
          ((r) (- b1 (cadr p)))
          ((c) (+ (/ (+ b0 b1) 2) (cadr p)))
          (else (error "unknown tab position spec: ~s" p))))))
        

(define (tab-align tab) (cadr (memq 'align: tab)))


#|
;; pragma no-warn-class-redefinition
(begin  (gvec-set! (& define-class) 3 #f) (values))
;; what about pragma no-warn-redefinition

(define-class <para> (<object>) style content)
(define-class <inline-item> (<object>))
(define-class <text-run> (<inline-item>) style content)

(define-class <hline> (<object>)
  height
  content)              ; a sequence of <hlist>'s

(define-class <hlist> (<object>)
  x
  width
  start-index
  index-count
  stretch
  hyphenate?)                           ; did hyphenate?
|#

(define $inter-char-glue 1)
(define $inter-word-glue 3)
(define $inter-sentence-glue 7)

(define $shrink-badness 10000)          ; XXX never shrink....
;(define $shrink-badness 150/100)
(define $stretch-badness 1)
(define $hyphenation-badness 1.20)

(define (parse-space info detail)
  (let ((new-cf info))
    (case detail
      ((word) 
       (values (style-char-width new-cf #\space)
               $inter-word-glue))
      ((sentence)
       (values (* 2 (style-char-width new-cf #\space))
               $inter-sentence-glue))
      ((em)
       (values (style-char-width new-cf #\m) 0))
      (else
       (if (pair? detail)
           (values (car detail) (cadr detail))
           (values detail 0))))))

(define (insert-hyphenation para r breaks)
  (dformat "inserting hyphenation...\n")
  (dbegin (print (reverse r)))
  ;;
  (let* ((word (list->string (map caddr (reverse r))))
         (hyph (hyphenated para word)))
    (dformat "current word: ~s  hyphenated: ~s\n" word hyph)
    (if (eq? hyph word)
        breaks
        (let loop ((b breaks)
                   (i 0)
                   (cap (reverse r)))           ; captured data
          (if (< i (string-length hyph))
              (if (char=? (string-ref hyph i) #\-)
                  (loop (cons (list-ref (car cap) 4) b) 
                        (+ i 1) 
                        cap)
                  (loop b
                        (+ i 1) 
                        (cdr cap)))
              b)))))


(define (wrap2 para input index0 tab b0 x-initial b1 b1-break hyph-allowed? fill-mode?)
  (dformat "wrap2: b0 ~d  x-initial ~d  b1 ~d  b1' ~d\n"
           b0
           x-initial
           b1
           b1-break)
  (let ((index index0)
        (align (tab-align tab))
        (anchors '())
        (kern-enable? #t)              ; a function of char-style?
        (cf-cache #f)
        (fm-cache #f))
    ;;
    (define (out-of-bounds? x0 x1)
      (or (< x0 b0) (> x1 b1)))
    ;;
    (define (compute-break-badness x0 x1 lineposn)
      (let* ((use-b1 (if (eq? lineposn 'last)
                         b1
                         b1-break))
             ;; XXX this doesn't work right when we are at a tab;
             ;;     we should look at it relative to the an
             (gap (case (tab-align tab)
                    ((left) (- x1 use-b1))
                    ((right) (- b0 x0))
                    (else (error "?sn error; tab")))))
        ; gap<0  ==>  expansion (stretch) will be required
        ; gap>0  ==>  compression (shrink) will be required
        (if (< gap 0)
            (* $stretch-badness (- gap))
            (* $shrink-badness gap))))
    ;;

    (define (return-best-break r breaks)
      (let ((breaks (insert-hyphenation para r breaks)))
        (dformat "possible breaks:\n")
        (let ((poss (sort
                     (map (lambda (b)
                            (cons
                             (* (if (eq? (cadr b) '-)
                                    $hyphenation-badness
                                    1)
                                (compute-break-badness (caddr b)
                                                       (cadddr b)
                                                       (list-ref b 5)))
                             b))
                          breaks)
                     (lambda (p q)
                       (< (car p) (car q))))))
          (dbegin (print poss))
          (bind ((b (car poss))
                 (i (list-ref b 1))
                 (mode (list-ref b 2))
                 (x0 x1 (values (list-ref b 3) (list-ref b 4)))
                 (glue (list-ref b 5)))
            (values
             (and (> i index)
                  (make <hlist>
                        para: para
                        align: 'left
                        x: x0
                        width: (- x1 x0)
                        start-index: index
                        index-count: (- i index)
                        stretch: 0              ; XXX
                        hyphenate?: (eq? mode '-)))
             (- i index0)
             #t
             (select (lambda (a)
                       (<= (car a) i))
                     anchors))))))
    ;;
    (define (fill-cf-cache cf)
      (set! cf-cache cf)
      (set! fm-cache (font-metrics
                      (query-style 
                       (query-style
                        cf
                        'font)
                       'font))))
    ;;
    (letrec-syntax ((sync-cf-cache (syntax-form (cf)
                                     (if (not (eq? cf cf-cache))
                                         (fill-cf-cache cf))))
                    (style-char-width (syntax-form (cf ch)
                                        (sync-cf-cache cf)
                                        (car (char-widths fm-cache 
                                                          (cons ch '())))))
                    (pair-kern (syntax-form (cf a b)
                                 (sync-cf-cache cf)
                                 (car (string-x-deltas fm-cache
                                                       (cons* a b '()))))))
      ;; inline this to take advantage of cf-cache etc.
      (define (parse-space info detail)
        (let ((new-cf info))
          (case detail
            ((word) 
             (values (style-char-width new-cf #\space)
                     $inter-word-glue))
            ((sentence)
             (values (* 2 (style-char-width new-cf #\space))
                     $inter-sentence-glue))
            ((em)
             (values (style-char-width new-cf #\m) 0))
            (else
             (if (pair? detail)
                 (values (car detail) (cadr detail))
                 (values detail 0))))))
      ;;
      (let loop ((i index)
                 (x0 x-initial)
                 (x1 x-initial)
                 (glue 0)                 ; accumulated stretchiness
                 (cf #f)
                 (r '())                  ; list of (style char dx)
                 (b '()))                 ; list of (i - x0 x1 glue)
        (bind ((type info detail (input)))
          (case type
            ((#f)
             (if (and (out-of-bounds? x0 x1)
                      (pair? b))
                 (return-best-break r (cons (list i '/ x0 x1 glue 'last) b))
                 (values
                  (and (> i index)
                       (make <hlist>
                             para: para
                             align: 'left
                             x: x0
                             width: (- x1 x0)
                             start-index: index
                             index-count: (- i index)
                             stretch: 0
                             hyphenate?: #f))
                  (- i index0)
                  #t
                  anchors)))
            ((break)
             ;; XXX do we need to check (out-of-bounds?) here, as above?
             (values
              (and (> i index)
                   (make <hlist>
                         para: para
                         align: 'left
                         x: x0
                         width: (- x1 x0)
                         start-index: index
                         index-count: (- i index)
                         stretch: 0
                         hyphenate?: #f))
              (- i index0)
              #f
              anchors))
            ((-)
             ;; drop leading (explicit) hyphenation points
             (if (eq? i index)
                 (begin
                   (dformat "[~d] --- HYPH (drop leading)\n" i)
                   (set! index (+ index 1))
                   (loop (+ i 1) x0 x1 glue cf r b))
                 (if hyph-allowed?
                     (let ((b (cons (list i '- x0 x1 glue 'mid) b)))
                       (dformat "[~d] --- HYPH break point at (~d ~d)\n" i x0 x1)
                       (if (out-of-bounds? x0 x1)
                           (return-best-break r b)
                           (loop (+ i 1) x0 x1 glue cf r b)))
                     (loop (+ i 1) x0 x1 glue cf r b))))
            ((/)
             (if (eq? i index)
                 (begin
                   (dformat "[~d] --- WORD (drop leading)\n" i)
                   (set! index (+ index 1))
                   (loop (+ i 1) x0 x1 glue cf r b))
                 (let ((b (cons (list i '/ x0 x1 glue 'mid) b)))
                   (dformat "[~d] --- WORD break point at (~d ~d)\n" i x0 x1)
                   (if (out-of-bounds? x0 x1)
                       (return-best-break r b)
                       (loop (+ i 1) x0 x1 glue cf '() b)))))
            ;;
            ((anchor)
             (set! anchors (cons (list i x1 info) anchors))
             (loop (+ i 1) x0 x1 glue cf r b))
            ;;
            ((space)
             (bind ((new-cf info)
                    (width stretch (parse-space info detail)))
               (if (eq? i index)
                   ;; drop leading space
                   (begin
                     (dformat "[~d] --- space (drop leading)\n" i)
                     (set! index (+ index 1))
                     (loop (+ i 1) x0 x1 glue cf '() b))
                   ;;
                   (bind ((dx width)
                          (nx0 nx1 (case align
                                     ((left) (values x0 (+ x1 dx)))
                                     ((center) (values (- x0 (/ dx 2))
                                                       (+ x1 (/ dx 2))))
                                     (else
                                      ;; this handles both left & char alignment
                                      (values (- x0 dx) x1)))))
                     (dformat "[~d] space ~s => dx ~d    now (~d ~d)\n"
                              i
                              detail
                              dx
                              nx0 nx1)
                     (loop (+ i 1) nx0 nx1 (+ glue stretch) new-cf 
                           '()
                           b)))))
            ;;
            ((char)
             (let* ((new-cf info)
                    (new-ch detail)
                    (new-cw (style-char-width new-cf new-ch))
                    (kern-adj (+ (if (and kern-enable?
                                        ; only kern across 
                                        ; same char style
                                          (eq? new-cf cf)
                                          (pair? r)
                                        ; and don't kern across space nodes
                                          (eq? (caar r) 'char))
                                     (pair-kern cf (caddar r) new-ch)
                                     0)
                                 (* new-cw
                                    (/ (query-style new-cf 'spread) 100)))))
               (if (eq? new-ch align)
                   (set! align 'left))      ; flip to left justified
               (bind ((dx (+ new-cw kern-adj))
                      (nx0 nx1 (case align
                                 ((left) (values x0 (+ x1 dx)))
                                 ((center) (values (- x0 (/ dx 2))
                                                   (+ x1 (/ dx 2))))
                                 (else
                                  ;; this handles both left and char alignment
                                  (values (- x0 dx) x1)))))
                 (dformat "[~d] ~s => dx ~d    now (~d ~d)\n"
                          i
                          new-ch
                          dx
                          nx0 nx1)
                 (loop (+ i 1) nx0 nx1 (+ glue $inter-char-glue) new-cf 
                       ;; XXX currently, we'll try to hyphenate
                       ;; "/usr/local/bin" as "usrlocalbin", because
                       ;; we want to hyphenate "Finally." as "finally".
                       ;; a better state machine is called for here...
                       (if (memq (char-category new-ch) '(upper lower))
                           (cons (list 'char new-cf 
                                       new-ch 
                                       new-cw 
                                       (list i '- nx0 nx1 (+ glue $inter-char-glue)
                                             'mid))
                                 r)
                           r)
                       b))))
            (else
             (error "huh?"))))))))

#|
(define (string-width cf str)
  (reduce + 0 (map (lambda (ch)
                     (char-width cf ch))
                   (string->list str))))

(define (char-width cf ch)
  (if (eq? cf 'pro)
      (if (memq ch '(#\i #\1 #\l #\.))
          5
          10)
      12))

(define (pair-kern (cf <symbol>) (c1 <char>) (c2 <char>))
  (if (eq? cf 'pro)
      (cadr (or (assoc (string c1 c2)
                       '(("Ay" -1)
                         ("y/" -1)
                         ("/A" -1)))
                '(#f 0)))
      0))
|#

(define (pair-kern cf c1 c2)
  (kerning-shift cf c1 c2))

(define (list->iterator lst)
  (let ((p lst))
    (lambda ()
      (if (null? p)
          (values)
          (let ((c (car p)))
            (set! p (cdr p))
            (list->values c))))))

(define (char-category ch)
  (cond
   ((char-lower-case? ch) 'lower)
   ((eq? ch #\tab) 'tab)
   ((eq? ch #\newline) 'newline)
   ((eq? ch #\-) '-)
   ((char-whitespace? ch) 'whitespace)
   ((char-upper-case? ch) 'upper)
   ((char-numeric? ch) 'numeric)
   ((memq ch '(#\. #\! #\?)) 'eos)
   ((memq ch '(#\, #\; #\:)) 'punct)
   ((memq ch '(#\o210)) 'symbol)
   (else 'other)))

(define (temit . args)
  (dformat "  ~-10s : ~s\n" (car args) (cdr args)))

(define (paragraph-wrap (self <para>) get-next-baseline emit)
  ;;
  (define (initial-tab x0 x1)
    (let ((pa (query-style (style self) 'align)))
      (case pa
        ((left)
         `(initial: #t
           position: ,(query-style (style self) 'left-margin-first)
           align: left))
        ((right)
         `(initial: #t
           position: ,(- x1 (query-style (style self) 'right-margin))
           align: right))
        (else
         (error "bad para align type: ~s" pa)))))
  ;;
  (define (initial-subseq-tab x0 x1)
    (let ((pa (query-style (style self) 'align)))
      (case pa
        ((left)
         `(initial: #t
           position: ,(query-style (style self) 'left-margin)
           align: left))
        ((right)
         `(initial: #t
           position: ,(- x1 (query-style (style self) 'right-margin))
           align: right))
        (else
         (error "bad para align type: ~s" pa)))))
  ;;
  (define (next-tab-stop x0 x1 tab)
    (let ((tab-stops (query-style (style self) 'tab-stops)))
      (if (eq? (car tab) 'initial:)
          (car tab-stops)
          (let ((ts (memq tab tab-stops)))
            (if (and ts (pair? (cdr ts)))
                (cadr ts)
                ;; default -- advance to next 1/2-inch position
                `(position: ,(* 36 (+ (quotient 
                                       (tab-position x0 x1 tab) 
                                       36) 
                                      1))
                            align: left))))))
  ;;
  (define (flush-hline hlists anchors)
    (emit anchors (reverse! hlists)))                   ; L.HLCB
  ;;
  (define (left-first-margin) 0)
  (define (left-subseq-margin) 0)
  (define (right-last-margin) 0)
  (define (right-prev-margin) 0)
  ;;
  (let loop ((l (paragraph->item-list self))
             (i 0)
             (tab #f)
             (x0 #f)
             (x1 #f)
             (sf #f)
             (hlists '())
             (anchors '()))
    (if (null? l)
        (if x0
            (flush-hline hlists anchors))
        (bind ((x0 x1 (sf <placement-group>) (if x0
                                                 (values x0 x1 sf)
                                                 (get-next-baseline))))
          ;; to handle right-aligned paragraphs, we need to get the
          ;; initial "tab" position AFTER we get the baseline extent x0...x1
          (if (eq? tab #f)
              (set! tab (if (eq? i 0)
                            (initial-tab x0 x1)
                            (initial-subseq-tab x0 x1))))
          ;;
          (cond
           ((equal? (car l) '(break tab))
            (loop (cdr l)
                  (+ i 1)
                  (next-tab-stop x0 x1 tab)
                  x0 x1 sf
                  hlists
                  anchors))
           ((equal? (car l) '(break line))
            (flush-hline hlists anchors)
            (loop (cdr l)
                  (+ i 1)
                  #f #f #f #f
                  '()
                  '()))
           (else
            (bind ((hlist n last-in-line? nanchors (wrap2
                                                    self
                                                    (list->iterator l)
                                                    i
                                                    tab
                                                    (+ x0
                                                      (if (eq? i 0)
                                                          (left-first-margin)
                                                          (left-subseq-margin)))
                                                   (tab-position x0 x1 tab)
                                                   (- x1 (right-last-margin))
                                                   (- x1 (right-prev-margin))
                                                   #t             ; hyphenation?
                                                   #t))          ; fill?
                   (new-anchors (map (lambda (ai)
                                       (let (((a <anchor>) (caddr ai)))
                                         (set-relative-x! a (cadr ai))
                                         a))
                                     nanchors)))
              (dformat "/// ~s ~d ~s\n" hlist n last-in-line?)
              (if hlist
                  (dbegin (print hlist)))
              (if (pair? anchors)
                  (dformat "    anchors: ~s\n" nanchors))
              ;;
              (if last-in-line?
                  (begin
                    (flush-hline (if hlist
                                     (cons hlist hlists)
                                     hlists)
                                 (append new-anchors anchors))
                    (loop (list-tail l n)
                          (+ i n)
                          #f
                          #f #f #f
                          '() '()))
                  (loop (list-tail l n)
                        (+ i n)
                        tab
                        x0 x1 sf
                        (if hlist
                            (cons hlist hlists)
                            hlists)
                        (append new-anchors anchors))))))))))
                  

(define (paragraph->item-list (self <para>))
  (let ((r '())
        (mode (query-style (style self) 'lines)))
    ;;
    (define (emit . args)
      (set! r (cons args r)))
    ;;
    ;;  BUG: If two successive <text-run>'s have whitespace that
    ;;       adjoin, we will not collapse that space...
    (for-each
     (lambda ((item <inline-item>))
       (generate-item-list item emit self mode))
     (content self))
    (print (reverse r))))

(define-method generate-item-list ((self <text-run>) emit para line-mode)
  (let ((save *debug-linewrap*)
        (s (style self)))
    (format #t "GENERATE-ITEM-LIST OF ~s WITH STYLE ~s\n" self s)
    (if (get-property self 'revisionflag #f)
        (begin
          (format #t ">>> IS REVISED ~s\n" self)
          (set! s (make-bold s))))
    ;;
    (if (get-property self 'debug #f)
        (begin
          (format #t "--- start debug ~s\n" self)
          (set! *debug-linewrap* #t)))
    ;;
    (let ((r (process-text-stream s
                                  (content self)
                                  emit 
                                  line-mode)))
      (if (get-property self 'debug #f)
          (format #t "--- end debug ~s\n" self))
      (set! *debug-linewrap* save)
      r)))

(define-method generate-item-list ((self <anchor>) emit para line-mode)
  (emit 'anchor self))

(define-method generate-item-list ((self <computed-inline>) 
                                   emit para line-mode)
  (for-each (lambda ((ii <inline-item>))
              (generate-item-list ii emit para line-mode))
            ((inline-computer self) self
                                    para 
                                    (query-style (style para) 
                                                 'default-char-style))))

(define (process-text-stream cf str emit line-mode)
  (define (keep-nl?) 
    (memq line-mode '(asis asis-wrap)))
  ;;
  ;;  XXX what about literal-space-preserving mode for screen?
  ;;
  (define (keep-spaces?)
    (memq line-mode '(asis asis-wrap)))         ;;XXX for now

  (define (preserve-dashes?)
    (not (query-style cf 'kerning?)))   ;;XXX for now
  ;;
  (define (discretionary-break) 
    (if (memq line-mode '(wrap asis-wrap))
        (emit '/)))
  ;;
  (letrec ((start (lambda (i l eos-mode)
                    ;(dformat "~d S ~s ~s\n" i (if (pair? l) (car l) '-) eos-mode)
                    (if (null? l)
                        i
                        (case (char-category (car l))
                          ((upper) 
                           (emit 'char cf (car l))
                           (start (+ i 1) (cdr l) 0))
                          ((-)
                           (saw-hyphen (+ i 1) (cdr l)))
                          ((lower numeric other punct)
                           (emit 'char cf (car l))
                           (start (+ i 1) (cdr l) 1))
                          ((symbol)
                           (emit 'char 
                                 (as-symbol-char-style cf)
                                 (case (car l)
                                   ((#\o210) #\o256)
                                   (else 
                                    (error "Unknown symbol char ~s" (car l)))))
                           (start (+ i 1) (cdr l) 1))
                          ((whitespace)
                           (if (keep-spaces?)
                               (begin
                                 (emit 'char cf #\space)
                                 (start (+ i 1) (cdr l) 0))
                               (transition-to-whitespace i l eos-mode)))
                          ((eos)
                           (emit 'char cf (car l))
                           (start (+ i 1) (cdr l) (if (>= eos-mode 1)
                                                      2
                                                      0)))
                          ((newline)
                           (if (keep-nl?)
                               (begin
                                 (discretionary-break)
                                 (emit 'break 'line)
                                 (start (+ i 1) (cdr l) 0))
                               (transition-to-whitespace i l eos-mode)))
                          ((tab)
                           (discretionary-break)
                           (emit 'break 'tab)
                           (start (+ i 1) (cdr l) 0))
                          (else (error "start state: ~s?" (car l)))))))
           ;;
           (saw-hyphen (lambda (i l)
                         (if (null? l)
                             (begin
                               (emit 'char cf #\-)
                               (discretionary-break)
                               i)
                             (let ((cat (char-category (car l))))
                               (if (or (not (eq? cat '-))
                                       (preserve-dashes?))
                                   (begin
                                     (emit 'char cf #\-)
                                     (discretionary-break)
                                     (start i l 0))
                                   (begin
                                     (discretionary-break)
                                     (emit 'char cf #\o261)   ; endash
                                     (start (+ i 1) (cdr l) 0)))))))
           ;;
           (transition-to-whitespace (lambda (i l eos-mode)
                                       (discretionary-break)
                                       (if (eq? eos-mode 2)
                                           (emit 'space cf 'sentence)
                                           (emit 'space cf 'word))
                                       (saw-space (+ i 1) (cdr l))))
                                     ;;
           (saw-space (lambda (i l)
                        ;(dformat "~d SPC ~s\n" i (if (pair? l) (car l) '-))
                        (if (null? l)
                            i
                            (case (char-category (car l))
                              ((whitespace)
                               ;; eat multiple whitespace characters
                               (saw-space (+ i 1) (cdr l)))
                              (else
                               (start i l 0)))))))
    (start 0 (string->list str) 0)))

          
(define (/-separate strs)
  (cdr 
   (apply append
          (map (lambda (s)
                 (cons '(/)
                       (map (lambda (ch)
                              (case ch
                                ((#\space)
                                 '(space pro word))
                                ((#\-)
                                 '(-))
                                (else
                                 (list 'char 'pro ch))))
                            (string->list s))))
               strs))))

(define (simple . strs)
  (list->iterator (/-separate strs)))

(define-method equal? ((self <hlist>) b)
  (and (instance? b <hlist>)
       (gvec-equal? self b)))

(define (check-equal a b)
  (if (equal? a b)
      a
      (let ((ap (with-output-to-string (lambda () (print a))))
            (bp (with-output-to-string (lambda () (print b)))))
        (dformat "~35a : ~a\n"
                "Actual value"
                "Expected value")
        (for-each (lambda (a b)
                    (dformat "~35a ~a ~a\n" 
                            a
                            (if (string=? a b) " " "|")
                            b))
                  (cdr (string-split ap #\newline))
                  (cdr (string-split bp #\newline)))
        (error "check-equal: failed"))))


(define (t) 
  (test-wrap2-word-wrap))

(define (skip-iter iter n)
  (let loop ((i n))
    (if (= i 0)
        iter
        (begin 
          (iter) 
          (loop (- i 1))))))
;;

(define (test-wrap2-alignment)
  (let ((tab0 '(position: 0 align: left))
        (tab1 '(position: 50 align: center))
        (tab2 '(position: 75 align: #\.))
        (tab3 '(position: 100 align: right)))
    ;;
    (check-equal
     (wrap2 #f (simple "Aya") 0 tab0 0 0 100 100 #t #f)
     (make <hlist>
           x: 0
           width: 29
           start-index: 0
           index-count: 3
           stretch: 0
           hyphenate?: #f))
    ;;
    (check-equal
     (wrap2 #f (simple "Aya") 0 tab1 0 (tab-position 0 100 tab1) 100 100 #t #f)
     (make <hlist>
           x: (/ (- 100 29) 2)
           width: 29
           start-index: 0
           index-count: 3
           stretch: 0
           hyphenate?: #f))
     ;;
    (check-equal
     (wrap2 #f (simple "3.141") 0 tab2 0 (tab-position 0 100 tab2) 100 100 #t #f)
     (make <hlist>
           x: (- 75 10)
           width: (+ 10 5 5 10 5)
           start-index: 0
           index-count: 5
           stretch: 0
           hyphenate?: #f))
   ;;
    (check-equal
     (wrap2 #f (simple "Aya") 0 tab3 0 (tab-position 0 100 tab3) 100 100 #t #f)
     (make <hlist>
           x: (- 100 29)
           width: 29
           start-index: 0
           index-count: 3
           stretch: 0
           hyphenate?: #f))
    ;;
    ))

(define (make-accumulator)
  (let ((f '()))
    (values (lambda args (set! f (cons args f)))
            (lambda () (reverse f)))))

(define (test-wrap2-word-wrap)
  ;;
  (let ((tab0 '(position: 0 align: left))
        (tab1 '(position: 50 align: center))
        (tab2 '(position: 75 align: #\.))
        (tab3 '(position: 100 align: right)))
    ;;
    (define (ptt str mode)
      (let ((f '()))
        (process-text-stream 'x 
                             str 
                             (lambda args (set! f (cons args f)))
                             mode)
        (reverse f)))
    ;;
    (define (st)
      (simple "This" " isnt" " a" " test" " of" " word" " breaking."))
    ;;
    (check-equal
     (wrap2 #f (st) 0 tab0 0 (tab-position 0 100 tab0) 100 100 #f #f)
     (make <hlist>
           x: 0
           width: 100
           start-index: 0
           index-count: 13
           stretch: 0
           hyphenate?: #f))
    ;;
    (check-equal
     (wrap2 #f (simple "" " word" " breaking.")
            0 tab0
            0 (tab-position 0 100 tab0) 100 100 #f #f)
     (make <hlist>
           x: 0
           width: 130
           start-index: 2
           index-count: 15
           stretch: 0
           hyphenate?: #f))
    ;;
    (check-equal
     (ptt "Bob. I.B.M. is cool." 'wrap)
     '((char x #\B) (char x #\o) (char x #\b) (char x #\.) (/) 
                    (space x sentence) (char x #\I) (char x #\.) 
                    (char x #\B) (char x #\.) (char x #\M) (char x #\.) 
                    (/) (space x word) (char x #\i) (char x #\s) (/) 
                    (space x word) (char x #\c) (char x #\o) (char x #\o)
                    (char x #\l) (char x #\.)))
    ;;
    (check-equal
     (ptt "Bob. I.B.M. is cool." 'asis)
     '((char x #\B) (char x #\o) (char x #\b) (char x #\.)  
                    (space x sentence) (char x #\I) (char x #\.) 
                    (char x #\B) (char x #\.) (char x #\M) (char x #\.) 
                     (space x word) (char x #\i) (char x #\s)  
                    (space x word) (char x #\c) (char x #\o) (char x #\o)
                    (char x #\l) (char x #\.)))
    ;;
    (wrap2 #f (st) 0 tab0 0 (tab-position 0 100 tab0) 100 100 #f #f)
    ))

(define p0
  (make <para>
        style: 'pp
        content: (list
                  (make <text-run>
                        style: 'x
                        content: "Foo ")
                  (make <text-run>
                        style: 'y
                        content: "bar.  Sam's your uncle?!  O.K. then."))))

(define (insert-line-breaks2 (self <para>) rstream)
  (paragraph-wrap self
                  (lambda ()
                    (request-read rstream))
                  (lambda (anchors hline)       ; called from L.HLCB
                    (request-return 
                     rstream
                     (make <hline>
                           content: hline
                           height: (query-style (style self)
                                                'line-spacing))
                     anchors))))
                             
(define (t-paragraph-wrap)
  (paragraph-wrap p0 
                  (lambda ()
                    (values 0 100))
                  (lambda (hline)
                    (dformat "*** ~-10s: ~s\n"
                            (car hline)
                            (map (lambda ((hl <hlist>))
                                   (list (x hl)
                                         (width hl)
                                         (list (start-index hl)
                                               (+ (start-index hl)
                                                  (index-count hl)))))
                                 (cdr hline))))))
                            
