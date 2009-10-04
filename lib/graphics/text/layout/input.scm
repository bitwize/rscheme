(define-class <text-style> (<object>)
  (font-style init-value: 'default-font)
  (font init-value: #f)
  (space-glue init-value: #f)
  (explicit-space-glue init-value: #f)
  (sentence-glue init-value: #f)
  (kern-pair-rev init-value: #f))

(define (scaled-char-dimens (self <text-style>) ch)
  (let ((m (car (get-char-metrics (font-metrics (font self)) (list ch)))))
    (values (qty->scaled (char-width m))
            (qty->scaled (char-height m))
            (qty->scaled (char-depth m)))))

(define-class <hlist-accumulator> (<object>)
  hlist
  (current-style init-value: #f)
  (kern/lig-table init-value: #f)
  (ligature-stack init-value: '())
  (skip-spaces? init-value: #f)
  (space-factor init-value: 1000))

(define (clear-ligature-stack! (self <hlist-accumulator>))
  (set-ligature-stack! self '()))

(define (push-ligature-stack! (self <hlist-accumulator>) (ch <char>))
  (set-ligature-stack! self (cons ch (ligature-stack self))))

(define (rplc-ligature-stack! (self <hlist-accumulator>) (ch <char>))
  (set-car! (ligature-stack self) ch)
  (values))

(define (get-kern/lig-table (self <hlist-accumulator>))
  (or (kern/lig-table self)
      (let ((t (times-roman-lig/kern-table (current-style self))))
        (set-kern/lig-table! self t)
        t)))

(define (append-skip (self <hlist-accumulator>) (glue <glue>))
  ;; [dmk] I just made this up... TODO: check how TeX does it
  (let ((g (make <glue-node>
                 content: glue)))
    (dequeue-push-back! (hlist self) g)
    g))

(define (append-penalty (self <hlist-accumulator>) (p <penalty-node>))
  ;; [dmk] I just made this up... TODO: check how TeX does it
  (dequeue-push-back! (hlist self) p)
  p)

(define (append-char (self <hlist-accumulator>) sty ch)
  ;; TeX#1033...?
  ;;
  (if (not (eq? (current-style self) sty))
      (begin
        (set-current-style! self sty)
        (set-kern/lig-table! self #f)))
  ;;
  ;;  Adjust space factor (TeX#1034)
  (let ((sf (or (table-lookup *space-factor* ch) 1000)))
    (if (not (eq? sf 0))
        (if (<= sf 1000)
            ;; setting it to a value <= 1000 is always OK
            (set-space-factor! self sf)
            ;; otherwise, if it was previously <1000, then set it to only 1000
            (if (< (space-factor self) 1000)
                (set-space-factor! self 1000)
                (set-space-factor! self sf)))))
  ;;
  (debug
   (format #t "insert ~s  (space factor ~s)\n" ch (space-factor self)))
  ;;
  (let ((kern/lig (get-kern/lig-table self)))
    (cond
     ((table-lookup kern/lig ch)
      => (lambda (f)
           (f self)))
     (else
      (let ((f (lazy-kern/lig-proc (current-style self) ch)))
        (table-insert! kern/lig ch f)
        (f self))))))
                                   
    
(define (init-style-glue (self <text-style>))
  (let* ((fnt (get-text-font (get-style (font-style self))))
         (spc (get-char-metrics (font-metrics fnt) '(#\space)))
         (spcw (char-width (car spc))))
    ;;
    ;;  PostScript fonts don't have a notion of stretch() and shrink()
    ;;  as a character metric.  Rats.  We'll assume 10% for now
    ;;
    ;;  Computer Modern Roman seems to set "normal" space to 6u#+2letter_fit#,
    ;;  while "extra" space is set to 2u#.  "normal_stretch" is 3u#, and
    ;;  "normal_shrink" is 2u#
    ;;
    ;;  Hmmm.  In cmr9, u# is set to "18.5/36pt", and letter_fit# is set to
    ;;         0pt (in cmr5, u# is 12.5/36pt and letter_fit# is 5/36pt), 
    ;;         which implies that normal space in cmr9 is ~3.08pt
    ;;
    ;;         In any case, we note that, since letter_fit=0, stretch is 
    ;;         1/2 of normal, and shrink is 1/3 of normal
    ;;
    (set-font! self fnt)
    (set-space-glue! 
     self 
     (make <glue-node>
           content: (make <glue>
                          natural: (qty->scaled spcw)
                          stretch: (qty->scaled (/ spcw 2))
                          shrink: (qty->scaled (/ spcw 3)))))
    (set-explicit-space-glue!
     self 
     (make <explicit-kern-node>
           width: (qty->scaled spcw)))
    ;;
    (set-sentence-glue! 
     self 
     (make <glue-node>
           content: (make <glue>
                          natural: (qty->scaled (* spcw 3/2))
                          stretch: (qty->scaled (/ (* spcw 3/2) 2))
                          shrink: (qty->scaled (/ (* spcw 3/2) 3)))))
    ;;
    ;; initialize the kern-pair (in reverse) table
    (let ((kpr (make-char-table)))
      ;;
      (vector-for-each
       (lambda (c1)
         (for-each
          (lambda (kp)
            (let ((c2 (car kp))
                  (dx (cdr kp)))
              (if (not (zero? dx))
                  (table-insert! 
                   kpr
                   c2
                   (cons (cons c1 (qty->scaled dx))
                         (or (table-lookup kpr c2) '()))))))
          (kerning-pairs-after (font-metrics fnt) c1)))
       (font-characters fnt))
      ;;
      (set-kern-pair-rev! self kpr))
    ))

(define (text->hlist sxml-node-list init-style)
  (let ((accum (make <hlist-accumulator>
                     hlist: (make-dequeue))))
    ;;
    (text-list->hlist* sxml-node-list accum init-style)
    (dequeue-state (hlist accum))))

(define (text-list->hlist* sxml-node-list accum style)
  (for-each
   (lambda (n)
     (text->hlist* n accum style))
   sxml-node-list))

(define (text->hlist* sxml-node accum style)
  (define (app ch)
    (append-char accum style ch))
  ;;
  (cond
   ((sxml:text? sxml-node)
    (for-each app (string->list sxml-node)))
   ((sxml:element? sxml-node)
    (case (car sxml-node)
      ((quote)
       (app #\")
       (text-list->hlist* (sxml:children sxml-node) accum style)
       (app #\"))
      ((font)
       (text-list->hlist* (sxml:children sxml-node)
                          accum
                          (apply-style-change style sxml-node)))
      ((span)
       (text-list->hlist* (sxml:children sxml-node) accum style))
      ((skip)
       (append-skip accum (sxml->glue sxml-node)))
      ((penalty)
       (append-penalty accum (sxml->penalty sxml-node)))
      (else
       (error "don't understand element ~s" (car sxml-node)))))
   (else
    (error "don't understand sxml node ~s" sxml-node))))

(define (sxml->penalty node)
  (let ((p (xpath-str node "@penalty")))
    (make <penalty-node>
          penalty: (if (string=? p "infinite")
                       $inf-penalty
                       (string->number p)))))

(define (apply-style-change style node)
  (let* ((angle (xpath-str node "@angle"))
         (id (xpath-str node "@id"))
         (opts (append
                (cond
                 ((string=? angle "")
                  '())
                 ((string=? angle "italic")
                  '(angle: italic))
                 ((string=? angle "slant")
                  '(angle: slant))
                 ((string=? angle "normal")
                  '(angle: normal)))))
         (new-style (make <text-style>
                          font-style: (if (string=? id "")
                                          (apply make-style <font-style> 
                                                 basis: (font-style style)
                                                 opts)
                                          (get-style (string->symbol id))))))
    (init-style-glue new-style)
    new-style))

;; sf(default) = 1000
;; sf(UPPERCASE-LETTER) = 999

;; from plain.tex,
;;
;;   [this apparently takes advantage of the fact that space_factor is left
;;    unchanged when sf(chr) = 0, so that the effect of punctuation
;;    before a closing delimiter is preserved, e.g., in "(This is a sentence.)"]
;;
;; sf(#\)) = 0
;; sf(#\') = 0
;; sf(#\]) = 0
;;
;; and, in \frenchspacing,
;;      sf(#\.) = 1000
;;      sf(#\?) = 1000
;;      sf(#\!) = 1000
;;      sf(#\:) = 1000
;;      sf(#\;) = 1000
;;      sf(#\,) = 1000
;; and, in \nonfrenchspacing,
;;      sf(#\.) = 3000
;;      sf(#\?) = 3000
;;      sf(#\!) = 3000
;;      sf(#\:) = 2000
;;      sf(#\;) = 1500
;;      sf(#\,) = 1250
;;
;; then, if xpsace-skip<>0, we use xspace whenever space_factor >= 2000

(define (times-roman-lig/kern-table style)
  (let ((tbl (make-char-table))
        (fnt (font style)))
    ;;
    (define (space ch #optional explicit?)
      (table-insert! 
       tbl
       ch
       (if explicit?
           (lambda (accum)
             (dequeue-push-back! (hlist accum) (explicit-space-glue style))
             (values))
           (lambda (accum)
             (if (skip-spaces? accum)
                 (values)
                 (let ((q (hlist accum))
                       (sf (space-factor accum)))
                   (if (= sf 1000)
                       (dequeue-push-back! q (space-glue style))
                       (dequeue-push-back! q (factored-space-glue-node style sf)))
                   ;; clear the ligature stack and skip any subsequent spaces
                   (set-skip-spaces?! accum #t)
                   (clear-ligature-stack! accum)))))))
    ;;
    (define (ligature lig c1 c2 exp)
      (bind ((cw ch cd (scaled-char-dimens style lig))
             (cw-m ch-m cd-m  (scaled-char-dimens style c2))
             (miss (make <char-node>
                         width: cw-m
                         height: ch-m
                         depth: cd-m
                         content: c2
                         font: fnt))
             (hit (make <ligature-node>
                        font: fnt
                        height: ch
                        depth: cd
                        width: cw
                        content: lig
                        expanded: exp)))
        (table-insert!
         tbl
         c2
         (lambda (accum)
           (set-skip-spaces?! accum #f)
           (let ((n (length (ligature-stack accum)))
                 (stack (ligature-stack accum))
                 (q (hlist accum)))
             (if (and (>= n 1) (eq? (car stack) c1))
                 (begin
                   (dequeue-set! q (sub1 (dequeue-count q)) hit)
                   (rplc-ligature-stack! accum lig))
                 (begin
                   (dequeue-push-back! q miss)
                   (push-ligature-stack! accum c2))))))))
    ;;
    (ligature #\o256 #\f #\i "fi")
    (ligature #\o257 #\f #\l "fl")
    ;;
    (space #\space)
    (space #\newline)
    (space #\cr)
    (space #\tab)
    (space #\xA0 #t)
    ;;
    tbl))

(define (lazy-kern/lig-proc style ch)
  (bind ((cw cht cd (scaled-char-dimens style ch))
         (cnode (make <char-node>
                      width: cw
                      height: cht
                      depth: cd
                      content: ch
                      font: (font style)))
         (kpb (list->vector
               (apply append
                      (map (lambda (kp)
                             (list (car kp)
                                   (make <kern-node>
                                         width: (cdr kp))))
                           (or (table-lookup (kern-pair-rev style) ch) '()))))))
    (if (zero? (vector-length kpb))
        (lambda (accum)
          (set-skip-spaces?! accum #f)
          (let ((n (length (ligature-stack accum)))
                (stack (ligature-stack accum))
                (q (hlist accum)))
            (dequeue-push-back! q cnode)
            (push-ligature-stack! accum ch)))
        (begin
          ;(debug (format #t "Kerning Table for ~s\n" ch) (print kpb))
          (lambda (accum)
            (set-skip-spaces?! accum #f)
            (let ((n (length (ligature-stack accum)))
                  (stack (ligature-stack accum))
                  (q (hlist accum)))
              (cond
               ((and (pair? stack)
                     (vassq (car stack) kpb))
                => (lambda (i)
                     (dequeue-push-back! q (vector-ref kpb i)))))
              (dequeue-push-back! q cnode)
              (push-ligature-stack! accum ch)))))))

(define (factored-space-glue-node style sf)  
  ;; Handle spaces when space_factor != 1000; essentially TeX#1043
  (if (>= sf 2000)
      (sentence-glue style)
      (let (((p <glue>)  (content (space-glue style))))
        (make <glue-node>
              content: (make <glue>
                             natural: (round (* (/ sf 1000) (natural p)))
                             stretch: (round (* (/ sf 1000) (stretch p)))
                             stretch-order: (stretch-order p)
                             shrink: (round (* (/ sf 1000) (shrink p)))
                             shrink-order: (shrink-order p))))))
  
(define *space-factor* (make-char-table))

;; Giving these a <1000 space factor means that
;; a sequence of the form (#\A #\.) will leave
;; space-factor at 1000 instead of 3000, and hence
;; implements the behavior of dotted initials preceding
;; a space, such as "Sue Kolbly B.A. is cute"

(for-each (lambda (ch)
            (table-insert! *space-factor* ch 999))
          (string->list "ABCDEFGHIJKLMNOPQRSTUVWXYZ"))

(table-insert! *space-factor* #\) 0)
(table-insert! *space-factor* #\" 0)
(table-insert! *space-factor* #\' 0)
(table-insert! *space-factor* #\] 0)
(table-insert! *space-factor* #\space 0)
(table-insert! *space-factor* #\tab 0)
(table-insert! *space-factor* #\cr 0)
(table-insert! *space-factor* #\lf 0)

(table-insert! *space-factor* #\. 3000)
(table-insert! *space-factor* #\? 3000)
(table-insert! *space-factor* #\! 3000)
(table-insert! *space-factor* #\: 2000)
(table-insert! *space-factor* #\; 1500)
(table-insert! *space-factor* #\, 1250)


;;;

(define (make-text-style)
  (let ((s (make <text-style>)))
    (init-style-glue s)
    s))

