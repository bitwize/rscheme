
;;;
;;;  Flow a paragraph
;;;

(define $inter-char-gap-stretch 10)
(define $inter-word-gap-stretch 50)

(define (strip-trailing-whitespace runs)
  (let ((x (strip-trailing-whitespace1 (cdr (last runs)))))
    (if (null? x)
        (strip-trailing-whitespace (reverse (cdr (reverse runs))))
        (reverse (cons (cons (car (last runs)) x)
                       (cdr (reverse runs)))))))

(define (strip-trailing-whitespace1 lst)
  (let ((l (last lst)))
    (if (or (and (pair? l) (eq? (car l) 'strut))
            (and (pair? l) (eq? (car l) 'stretch))
            (and (pair? l) (eq? (car l) #\space)))
        (strip-trailing-whitespace1 (reverse (cdr (reverse lst))))
        lst)))



(define (line-width runs)
  (reduce 
   + 
   0
   (map
    (lambda (run)
      (let ((widths (map (lambda (ent)
                           (if (pair? ent)
                               (cond
                                ((char? (car ent))
                                 (+ (cadr ent) (caddr ent)))
                                ((eq? (car ent) 'strut)
                                 (cadr ent))
                                (else 0))
                               0))
                         (cdr run))))
        (reduce + 0 widths)))
    runs)))


(define (style-char-width cf ch)
  (string-width (cf-afm cf)
                (cf-size cf)
                (string ch)))


(define (kerning-shift cf c1 c2)
  (* (car (string-x-deltas (cf-afm cf) (string c1 c2)))
     (cf-size cf)))


(define (fold-over-struts runs)
  (map (lambda (run)
         (cons (car run) (fold-over-struts1 (cdr run))))
       runs))

(define (fold-over-struts1 items)
  ;; take all the (strut DX) nodes that follow
  ;; a character node and roll the DX into the character
  ;; node's width
  (let loop ((s items)
             (d '()))
    (if (null? s)
        (reverse! d)
        (if (and (eq? (caar s) 'strut)
                 (pair? d)
                 (char? (caar d)))
            (loop (cdr s)
                  (cons (list (caar d)
                              (cadar d)
                              (+ (caddar d) (cadar s)))
                        (cdr d)))
            (loop (cdr s)
                  (cons (car s) d))))))

(define (no-stretch runs)
  (map (lambda (run)
         (cons
          (car run)
          (select (lambda (item)
                    (not (and (pair? item)
                              (eq? (car item) 'stretch))))
                  (cdr run))))
       runs))

(define (apply-stretch runs dw)
  (let ((remaining-stretch (sum-stretch runs))
        (remaining-delta dw))
    (map (lambda (run)
           (cons 
            (car run)
            (map (lambda (item)
                   (if (and (pair? item)
                            (eq? (car item) 'stretch))
                       (let ((spc (* remaining-delta 
                                     (/ (cadr item) 
                                        remaining-stretch))))
                         (set! remaining-stretch (- remaining-stretch (cadr item)))
                         (set! remaining-delta (- remaining-delta spc))
                         (list 'strut spc))
                       item))
                 (cdr run))))
         runs)))
    
(define (sum-stretch runs)
  (let loop ((stretch 0)
             (runs runs))
    (if (null? runs)
        stretch
        (let iloop ((items (car runs))
                    (stretch stretch))
          (if (null? items)
              (loop stretch (cdr runs))
              (if (and (pair? (car items))
                       (eq? (caar items) 'stretch))
                  (iloop (cdr items) (+ (cadar items) stretch))
                  (iloop (cdr items) stretch)))))))

#|
(define (layout-line items cf-start)
  ;; output consists four kinds of nodes,
  ;;   - character nodes        (#\F 18 0)      (<char> <char-width> <kern>)
  ;;   - fixed space nodes      (strut 18)      (strut <width>)
  ;;   - stretchy space nodes   (stretch 10)    (strech <portion>)
  ;;   - discretionary break    -
  ;; arranged into a list of runs, each of which is
  ;;   (<charformat> <item> ...)
  ;;
  (let ((cf-cache #f)
        (fm-cache #f))
    ;;
    (let-syntax ((style-char-width (syntax-form (cf ch)
                                     (error "rats ~s" ch))))
    ;;
      (let loop ((l items)
                 (x 0)
                 (cf cf-start)
                 (run '())
                 (runs '()))
        (if (null? l)
            (values (reverse! (if (pair? run)
                                  (cons (cons cf (reverse! run)) runs)
                                  runs))
                    x)
            (let ((i (car l)))
              (cond
               ((string? i)
                (loop (append (string->list i) (cdr l)) x cf run runs))
               ((eq? i #\space)
                (let ((cw (style-char-width cf #\space)))
                  (loop (cdr l)
                        (+ x cw)
                        cf
                        (cons '-
                              (cons (list 'stretch $inter-word-gap-stretch)
                                    (cons (list i cw 0) run)))
                        runs)))
               ((and (char? i)
                     (pair? (cdr l))
                     (char? (cadr l)))
                (let ((cw (style-char-width cf i))
                      (dx (kerning-shift cf i (cadr l))))
                  (loop (cdr l)
                        (+ x cw dx)
                        cf
                        (cons (list 'stretch $inter-char-gap-stretch)
                              (cons (list i cw dx) run))
                        runs)))
               ((char? i)
                (let ((cw (style-char-width cf i)))
                  (loop (cdr l)
                        (+ x cw)
                        cf
                        (cons (list 'stretch 10)
                              (cons (list i cw 0) run))
                        runs)))
               ((and (pair? i) (eq? (car i) 'space))
                (if (zero? (caddr i))
                    (loop (cdr l)
                          (+ x (cadr i))
                          cf
                          (cons (list 'strut (cadr i)) run)
                          runs)
                    (if (zero? (cadr i))
                        (loop (cdr l)
                              x
                              cf
                              (cons (list 'stretch (caddr i)) run)
                              runs)
                        (loop (cdr l)
                              (+ x (cadr i))
                              cf
                              (cons (list 'stretch (caddr i))
                                    (cons (list 'strut (cadr i))
                                          run))
                              runs))))
               ((and (pair? i) (eq? (car i) 'font))
                (loop (cdr l)
                      x
                      (cadr i)
                      '()
                      (if (pair? run)
                          (cons (cons cf (reverse! run)) runs)
                          runs)))
               (else
                (error "what? ~s" i)))))))))
|#

;;; How we weight an overrun vs. an underrun
;;;  1.0 => overruns are as ugly as underruns
;;;  2.0 => An overrun of 2X is as bad as an underrun of X
(define $overflow-weight 15)

#|
(define (insert-line-breaks para port)
  ;; We are the producer for the `port' channel; requests
  ;; consist of horizontal bounds on the line to be broken;
  ;; the response is an <hline> object.
  ;;
  ;; We are the consumer of item-stream; it's a normal iterator
  ;; over the content of a <para>
  (let ((x 0)
        (x0 0)
        (last-brk-x #f)
        (pending '())
        (pending-i 0)
        (current-i 0)
        (current '())
        (cf #f)
        (was-hyph? #f)
        (line-width #f)
        (istream (open-inline-stream para 0)))
    ;;
    (define (get-next-baseline)
      (bind ((line-x0 line-x1 (request-read port)))
        (set! line-width (- line-x1 line-x0))
        (set! x0 line-x0)))
    ;;
    (define (local-char-width ch)
      (* (char-style-size cf)
         (car (char-widths (char-style-afm cf) (string ch)))))
    ;;
    (define (emit-current i h?)
      (format #t "Emit C ~s ~d ... ~d\n" 
              (list->string
               (select char? (map cadr (select pair? (reverse current)))))
              current-i
              i)
      (request-return
       port
       (if (> i current-i)
           (make <hline>
                 height: (query-style (style para) 'line-spacing)
                 content: (list (make <hlist>
                                      para: para
                                      start-index: current-i
                                      index-count: (- i current-i)
                                      x: x0
                                      align: 'left
                                      stretch: 0
                                      hyphenate?: h?)))
           #f))
      (get-next-baseline)
      (set! current-i i))
    ;;
    (define (collapse-leading-space)
      (let loop ()
        (if (pair? current)
            (let ((i (last current)))
              (if (or (and (pair? i) (eq? (cadr i) #\space))
                      (memq i '(- /)))
                  (begin
                    (set! current (reverse (cdr (reverse current))))
                    (set! current-i (+ current-i 1))
                    (loop)))))))

    ;;
    (define (emit-item i item)
      
      #|
      (format #t "[~d] x ~2d lbx ~2a C ~20s P ~20s + ~s\n" 
              i
              x
              last-brk-x
              (list->string (select char? (map cadr (select pair? (reverse current)))))
              (list->string (select char? (map cadr (select pair? (reverse pending)))))
              item)
      |#
      ;;
      (if (or (eq? item '-)
              (eq? item '/))
          (if (> x line-width)
              ;; make a choice... which is better, this break or the last one?
              (begin
                (collapse-leading-space)
                (if (or last-brk-x              ; never overflow lines...XXX
                        (and last-brk-x
                             (< (- line-width last-brk-x)
                                (* (- x line-width) $overflow-weight))))
                    ;; the last one is preferable
                    (begin
                      (emit-current pending-i was-hyph?)
                      (set! current pending)
                      (set! x (max 0 (- x last-brk-x))))
                    ;; the current one is preferable
                    (begin
                      (set! current (append! pending current))
                      (emit-current i (eq? item '-))
                      (set! current (list item))
                      (set! x 0)))
                (set! pending '())
                (set! last-brk-x #f)
                (set! pending-i i))
              ;; remember this breakpoint for later
              (begin
                (set! was-hyph? (eq? item '-))
                (set! last-brk-x x)
                (set! current (append! pending current))
                (set! pending (list item))
                (set! pending-i i)))
          (let ((w (cond
                    ((char? (car item))
                     (+ (cadr item) (caddr item)))
                    ((eq? (car item) 'strut)
                     (cadr item))
                    (else
                     0))))
            (set! x (+ x w))
            (set! pending (cons (cons cf item) pending)))))
    ;;
    (get-next-baseline)
    ;;
    (let loop ((k 0))
      (bind ((type info detail (istream)))
        (case type
          ((#f)
           (emit-item k '/)
           (set! current (append! pending current))
           (emit-current k #f))
          ((/) (emit-item k '/)
               (loop (+ k 1)))
          ((-) (emit-item k '-)
               (loop (+ k 1)))
          ((breaking-space)
           (emit-item k 'breaking-space)
           (loop (+ k 1)))
          ((char) 
           (set! cf info)
           (emit-item k (list detail (local-char-width detail) 0))
           (loop (+ k 1)))
          ((break)
           (case info
             ((line)
              (emit-item k '/)
              ;; XXX this doesn't handle blank lines properly;
              ;; our consumer thinks we're all done emitting stuff!
              (set! current (append! pending current))
              (emit-current k #f)
              (set! pending-i (+ k 1))
              (set! current-i (+ k 1))
              (set! current '())
              (set! pending '())
              (set! last-brk-x #f)
              (loop (+ k 1)))
             ((tab)
              (emit-item k '/)
              (set! current (append! pending current))
              (if (pair? current)
                  (emit-current k #f))
              (set! x0 (+ x0 (* 8 (local-char-width #\space))))
              ;; XXX NOTE: this is wrong; we really need to be building
              ;;     multiple <hlist>'s within the same <hline>
              (set! pending-i (+ k 1))
              (set! current-i (+ k 1))
              (set! current '())
              (set! pending '())
              (set! last-brk-x #f)
             (loop (+ k 1)))
             (else
              (error "funky break type <~s>" info))))
          (else
           (error "funky para item <~s ~s ~s>" type info detail)))))))
|#

#|
(define (ilb-test p)
  (make-request-stream
   (lambda (rs)
     (insert-line-breaks p rs))))
|#
