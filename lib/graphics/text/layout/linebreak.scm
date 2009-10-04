
(define (line-break hlist)
  (let ((h (append hlist (list 
                          (make <penalty-node> penalty: $inf-penalty)
                          (make <glue-node> content: (parfillskip))))))
    (insert-breaks
     (or (and (>= *pretolerance* 0)
              (begin
                (debug 
                 (format #t "************** PASS 1 **********\n"))
                (line-break* h #f *pretolerance*)))
         (begin
           (debug
            (format #t "************** PASS 2 **********\n"))
           (line-break* h #t *tolerance*))
         (error "Could not break line"))
     h)))

(define (insert-breaks (best <active-node>) hlist)
  (let ((q (make-dequeue)))
    (let loop ((s hlist)
               (brk (reverse-break-list best))
               (lineno 0))
      (bind ((u (car brk))
             (line (strip-leading-glue (list-until s u)))
             (line u (setup-line-suffix line u))
             (lw eoc (compute-line-width-and-end-of-class lineno)))
        (dequeue-push-back! q (hpack lineno line lw 'exactly))
        #|
        (format #t "s ~s brk=~s\n" s u)
        (format #t "Line: ~s\n" (map (lambda (n)
                                       (cond
                                        ((instance? n <char-node>)
                                         (content n))
                                        ((instance? n <glue-node>)
                                         " ")
                                        (else
                                         n)))
                                     line))
        |#
        (if (pair? u)
            (loop u (cdr brk) (+ lineno 0))
            (dequeue-state q))))))

(define (hpack lineno line line-width mode)
  (assert (memq mode '(exactly          ; box is specified a priori
                       additional)))    ; box is increased from the natural
  (bind ((total-w max-h max-d (node-list-size line)))
    ;; TeX#657
    (let* ((w (if (eq? mode 'additional)
                  ;; in additional mode, the given line-width is how much
                  ;; to add to the natural width
                  (+ (natural total-w) line-width)
                  line-width))
           (excess (- w (natural total-w))))
      ;;
      (make <packed-line>
            line: (list->vector line)
            line-number: lineno
            height: max-h
            depth: max-d
            glue-set: (set-glue (pair? line) total-w excess)))))

(define (set-glue has-any? total-w excess)
  (cond
   ((= excess 0)
    $zero-glue-set)
   ((> excess 0)
    ;; need to stretch
    (stretching-glue-set has-any? total-w excess))
   (else
    ;; need to shrink
    (shrinking-glue-set has-any? total-w excess))))

(define (stretching-glue-set has-any? total-w excess)
  ;; TeX#658
  (bind ((o amt (space-stretch total-w)))
    (if (= amt 0)
        $zero-glue-set
        (begin
          (if (and (< *hbadness* $infinitely-bad)
                   (= o 0)
                   has-any?)
              (let ((b (badness excess amt)))
                (if (> b *hbadness*)
                    (format (current-error-port) "~a hbox (badness ~d)\n"
                            (if (> b 100) "Underfull" "Loose")
                            b))))
          (make <glue-set>
                glue-set-sign: 1
                glue-set-order: o
                glue-set-ratio: (exact->inexact (/ excess amt)))))))

(define (shrinking-glue-set has-any? total-w excess)
  ;; TeX#664
  (bind ((o amt (space-shrink total-w)))
    (if (= amt 0)
        $zero-glue-set
        (let ((ratio (exact->inexact (/ (- excess) amt))))
          ;;
          (if (and (= o 0) has-any?)
              (cond
               ((< (- excess) amt)
                (set! ratio 1)
                ;; Report an underfull hbox TeX#666 (666... Ha!)
                ;; except we're not going to add a rule to mark it...
                (if (or (> (- (- excess) amt) *hfuzz*)
                        (< *hbadness* 100))
                    (format (current-error-port) "Overfull hbox (~d too wide)\n"
                            (scaled->qty (- (- excess) amt)))))
               ((< *hbadness* 100)
                ;; Report a tight hbox if needed
                (let ((b (badness (- excess) amt)))
                  (if (> b *hbadness*)
                      (format (current-error-port) "Tight hbox (badness ~d)\n" 
                              b))))))
          ;;
          (make <glue-set>
                glue-set-sign: -1
                glue-set-order: o
                glue-set-ratio: ratio)))))


;;;  Returns width (a <space>), height, depth

(define (node-list-size nodes)
  ;; TeX#650 through TeX#655
  (let ((total-width (make <space>))
        ((max-height <fixnum>) 0)
        ((max-depth <fixnum>) 0))
    ;;
    (let-syntax ((height-is (syntax-form (n)
                              (let (((temp <fixnum>) n))
                                (if (fixnum>? temp max-height)
                                    (set! max-height temp)))))
                 (depth-is (syntax-form (n)
                             (let (((temp <fixnum>) n))
                               (if (fixnum>? temp max-depth)
                                   (set! max-depth temp))))))
      (for-each
       (lambda (n)
         (cond
          ((instance? n <char-node>)
           (space+natural! total-width (width n))
           (height-is (height n))
           (depth-is (depth n)))
          ((instance? n <glue-node>)
           (space+! total-width (glue->space (content n))))
          ((instance? n <kern-node>)
           (space+natural! total-width (width n)))
          ((instance? n <penalty-node>)
           (values))
          (else
           (error "node-list-size: Don't know how to handle ~s" n))))
       nodes)
      (values total-width max-height max-depth))))

(define (setup-line-suffix q follow)
  ;; TeX#881: modify end of line to reflect nature of break
  (if (pair? follow)
      (cond
       ((instance? (car follow) <glue-node>)
        (values (append (leftskip-list) q (rightskip-list))
                follow))
       ((instance? (car follow) <disc-node>)
        (values (append (leftskip-list)
                        q
                        (pre-break (car follow))
                        (rightskip-list))
                (append (post-break (car follow))
                        (cdr follow))))
       ((instance? (car follow) <math-node>)
        (error "~s not implemented (TeX#881)" (car follow)))
       ((instance? (car follow) <kern-node>)
        (error "~s not implemented (TeX#881)" (car follow)))
       (else
        (values (append (leftskip-list)
                        q 
                        (rightskip-list)) 
                follow)))
      (values (append (leftskip-list)
                      q
                      (rightskip-list)) 
              follow)))
        
  
;;;  TeX#886: the \rightskip glue that comes at the end of a line
(define (rightskip-list)
  (list (make <glue-node>
              content: (right-skip #f))))

(define (leftskip-list)
  (list (make <glue-node>
              content: (left-skip #f))))

(define (non-discardable? x)
  ;; happens to be the same as can-precede-break?... see TeX#148
  (can-precede-break? x))

(define (strip-leading-glue h)
  (let loop ((h h))
    (if (or (instance? (car h) <char-node>)
            (non-discardable? (car h)))
        h
        (loop (cdr h)))))

(define (list-until lst until)
  (let loop ((lst lst)
             (r '()))
    (if (eq? lst until)
        (reverse! r)
        (loop (cdr lst) (cons (car lst) r)))))

(define (reverse-break-list (best <active-node>))
  (let loop ((p (passive best))
             (r '()))
    (if p
        (loop (prev-break p) (cons (break p) r))
        r)))

(define (line-break* hlist second-pass? threshold)
  (let ((ctx (make-para-context hlist: hlist
                                second-pass?: second-pass?
                                threshold: threshold)))
    (find-optimal-breakpoints ctx)))

(define (find-optimal-breakpoints (ctx <para-context>))
  ;; the bulk of TeX#863
  (let ((auto-breaking? #t))    ; gets cleared by inline math mode
    ;;
    (let loop ((prev #f)
               (p (hlist ctx)))
      (cond
       ((null? p)
        ;; TeX#873: Try the final line break at the end of the paragraph
        (try-break ctx '() $eject-penalty 'hyphenated)
        (debug
         (format #t "Finally, left with\n")
         (print ctx))
        (if (null? (active ctx))
            #f  ; nothing works
            (let ((b (find-best-active-node ctx)))
              (debug
               (format #t "Best active node: ~s\n" b))
              b)))
       (else
        (debug
         (format #t "=== (find-optimal-breakpoints) =============================\n")
         (format #t "Skipping: "))
        ;; TeX#866...
        (bind ((p w prev (skip-char-sequence p prev)))
          (debug
           (format #t "  (width ~d)\n" w))
          (set-active-width! ctx (space+ (active-width ctx) w))
          (let ((n (car p)))
            (cond
             ((instance? n <glue-node>)
              ;; TeX#868
              (if (and prev
                       (if auto-breaking?
                           (instance? (car prev) <char-node>) 
                           (can-precede-break? (car prev))))
                  (try-break ctx p 0 'unhyphenated))
              ;; TeX#869
              (set-active-width! ctx (space+ (active-width ctx) (content n)))
              ;;
              (if (and (second-pass? ctx) auto-breaking?)
                  (loop p (try-to-hyphenate-following-word (cdr p)))
                  (loop p (cdr p))))
             ((instance? n <penalty-node>)
              (try-break ctx p (penalty n) 'unhyphenated)
              (loop p (cdr p)))
             ((instance? n <kern-node>)
              (if (and (instance? (cadr p) <glue-node>)
                       auto-breaking?)
                  (try-break ctx p 0 'unhyphenated))
              (set-active-width! ctx (space+ (active-width ctx) (width (car p))))
              (loop p (cdr p)))
             (else
              (error "need to implement other cases, such as: ~s" n))))))))))

(define (try-to-hyphenate-following-word p)
  (debug
   (format #t "Trying to hyphenate...\n"))
  p)



(define (skip-char-sequence p prev)
  ;; TeX#867
  ;; Elsewhere, we ensure that there is a non-character-node at the end
  ;; of the list, so we don't need to check for null?
  (let loop ((p p)
             (w 0)
             (prev prev))
    (if (instance? (car p) <char-node>)
        (begin
          (debug 
           (format #t " ~s" (content (car p))))
          (loop (cdr p) (+ w (width (car p))) p))
        (values p w prev))))

;;;

(define (find-best-active-node (ctx <para-context>))
  (let loop ((l (active ctx))
             (min-dem $awful-bad)
             (best #f))
    (if (null? l)
        best
        (if (and (not (delta-node? (car l)))
                 (< (total-demerits (car l)) min-dem))
            (loop (cdr l) (total-demerits (car l)) (car l))
            (loop (cdr l) min-dem best)))))

;;;

;;; TeX#148
;;;  TeX's node type order:
;;;
;;;    hlist vlist rule ins mark adjust ligature disc whatsit 
;;;    math glue kern penalty

(define-method can-precede-break? ((self <hlist-node>))         #t)
(define-method can-precede-break? ((self <vlist-node>))         #t)
(define-method can-precede-break? ((self <rule-node>))          #t)
(define-method can-precede-break? ((self <ins-node>))           #t)
(define-method can-precede-break? ((self <mark-node>))          #t)
(define-method can-precede-break? ((self <adjust-node>))        #t)
(define-method can-precede-break? ((self <ligature-node>))      #t)
(define-method can-precede-break? ((self <disc-node>))          #t)
(define-method can-precede-break? ((self <whatsit-node>))       #t)

(define-method can-precede-break? ((self <math-node>))          #f)
(define-method can-precede-break? ((self <glue-node>))          #f)
(define-method can-precede-break? ((self <kern-node>))          #f)
(define-method can-precede-break? ((self <penalty-node>))       #f)

;;;

#|
(define (tlb) 
  (with-output-to-file
      "/tmp/t1"
    (lambda ()
      (with-line-width 16 (lambda () (line-break *htest*))))))


|#
