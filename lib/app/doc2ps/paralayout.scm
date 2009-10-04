
#|
(define-macro (debug-para-layout . forms)
  '(values))
|#

(define-macro (debug-para-layout . forms)
  `(begin ,@forms))

;;;

(define (layout-paragraph (self <para>)
                          (para-line-stream <request-stream>)
                          (placement-stream <request-stream>))
  (if (memq (query-style (style self) 'placement)
            '(sidebar/first-baseline*))
      (let* ((save (request placement-stream 'ypush))
             (l (layout-paragraph* self para-line-stream placement-stream)))
        (request placement-stream 'ypop save)
        l)
      (layout-paragraph* self para-line-stream placement-stream)))
  ;;
  

(define (layout-paragraph* (self <para>)
                           (para-line-stream <request-stream>)
                           (placement-stream <request-stream>))
  ;;
  ;;  We are the consumer of `placement-stream', 
  ;;   which supplies <placement-group>s and baselines within the
  ;;         placement subframe
  ;;
  ;;  We are also the consumer of `para-line-stream', which
  ;;    supplies us with <hline>'s as a function of (x0,x1) bounds
  ;;
  ;;
  ;;  Just to make this code tractable with my limited layout skills,
  ;;  we are going to limit the badness to two BASELINE approximations.
  ;;
  ;;    (1) instead of looking at the line's bounding box and figuring
  ;;        out where it intersects any wraparounds, we will just measure
  ;;        along the baseline and go from there, and
  ;;
  ;;    (2) instead of adjusting the baseline boundaries as the baseline
  ;;        slides down due to excessive risers (assuming non-fixed line
  ;;        heights), we will stick with the boundaries known at the start
  ;;        of the line.
  ;;    
  ;;
  (define (get-next-line x0 x1 (sf <placement-group>))
    ;; (x0,x1) are expressed relative to the targetted subframe; hence,
    ;; occupying the entire subframe means going from 0 to WIDTH
    (request para-line-stream x0 x1 sf))
#|
             (+ x0 (query-style (style self) 'left-margin))
             (- x1 (query-style (style self) 'right-margin))
|#
  ;;
  (define (line-spacing)
    (query-style (style self) 'line-spacing))
  ;;
  (define (place-line dy)
    (request placement-stream
             dy
             (query-style (style self) 'placement)))
  ;;
  (define (get-next-baseline)
    (place-line (line-spacing)))
  ;;
  (define (extend-vlists vlists sf lst anchors)
    (if (null? lst)
        vlists
        (let ((vl (make <vlist>
                        placement: sf
                        content: (reverse! lst))))
          (debug-para-layout
           (format #t "### extended vlist: ~s\n   with: ~s\n"
                   sf
                   (content vl)))
          (for-each (lambda ((a <anchor>))
                      (set-owner! a vl))
                    anchors)
          (cons vl vlists))))
  ;;
  (placement-vskip placement-stream
                   (query-style (style self) 'space-before))
  ;;
  (let loop ((vlists '())
             (current-sfp #f)
             (current-vlist '())
             (anchors '()))
    (bind ((new-sf y (get-next-baseline))
           (sfr (subframe-rect (or new-sf
                                   (error "No room to place paragraph ~s" self))))
           (hl new-anchors (get-next-line 0 (size-width sfr) new-sf)))
      (debug-para-layout
       (format #t "### get-next-line returned: + ~s\n" anchors)
       (print hl))
      ;;
      (if hl
          ;; get an adjusted baseline
          (bind ((new-sf y (if (= (height hl) (line-spacing))
                               (values new-sf y)
                               (place-line (- (height hl) (line-spacing))))))
            (debug-para-layout
             (format #t "### adjusted baseline: ~s ~s\n" new-sf y))
            (for-each (lambda ((a <anchor>))
                        (set-relative-y! a y))
                      new-anchors)
            (if (or (not current-sfp)
                    (eq? new-sf (subframe current-sfp)))
                (loop vlists
                      (if current-sfp
                          current-sfp
                          (make <placement>
                                subframe: new-sf
                                y: y))
                      (cons hl current-vlist)
                      (append new-anchors anchors))
                (loop (extend-vlists vlists current-sfp current-vlist anchors)
                      (make <placement>
                            subframe: new-sf
                            y: y)
                      (list hl)
                      new-anchors)))
          ;; end of the line, buddy...  give back the vspace we took
          (begin
            (placement-vskip placement-stream
                             (query-style (style self) 'space-after))
            ;;
            (place-line (- (line-spacing)))
            (reverse! (extend-vlists vlists 
                                     current-sfp 
                                     current-vlist
                                     anchors)))))))

;;;

#|
(define (break-para-lines (self <para>) item-stream port)
  ;; We are the producer for the `port' channel; requests
  ;; consist of horizontal bounds on the line to be broken;
  ;; the response is an <hline> object.
  ;;
  ;; We are the consumer of item-stream; it's a normal iterator
  ;; over the content of a <para>
  ;;
  ;; outer loop processes lines
  (let oloop ((i0 0))
    ;; inner loop builds an hlist
    (bind ((x0 x1 (request-read port)))
      ;;
      (define (make-hline i)
        (if (> i i0)            ; don't build empty hlists
            (make <hline>
                  height: (query-style (style self) 'line-spacing)
                  content: (list (make <hlist>
                                       para: self
                                       start-index: i0
                                       index-count: (- i i0)
                                       x: x0
                                       align: 'left
                                       stretch: 0
                                       hyphenate?: #f)))
            #f))
      ;;
      (let build-hlist-loop ((i i0)
                             (x x0))
        ;(format #t " [~d]  x0 ~d < x ~d < x1 ~d\n" i x0 x x1)
        (if (< x x1)
            (bind ((type info detail (item-stream)))
              (case type
                ;;
                ((#f) ;; ran out of input
                 (request-return port (make-hline i)))
                ;;
                ((/ - -- tab)
                 ;; ignore it
                 (build-hlist-loop (+ i 1) x))
                ;;
                ((char)
                 (let* ((afm (char-style-afm info))
                        ((ch <char>) detail)
                        (cw (+ 0.0 (* (char-style-size info)
                                      (car (char-widths afm (string ch)))))))
                   ;(format #t "~s delta ~d ... x = ~d\n" ch cw (+ x cw))
                   (build-hlist-loop (+ i 1) (+ x cw))))
                ;;
                (else
                 (error "wierd stuff in content horz-item stream at [~d]: ~s ~s ~s"
                        i
                        type info detail))))
            (begin
              ;; ran out of space
              (request-return port (make-hline i))
              (oloop i)))))))
|#

#|

(define (break-para-lines style item-stream port)
  ;; We are the producer for the `port' channel; requests
  ;; consist of horizontal bounds on the line to be broken;
  ;; the response is an <hline> object.
  ;;
  ;; We are a consumer on item-stream (which is not a request-stream;
  ;; it's just a normal iteration closure)
  ;;
  ;;
  ;; our coordinate system is that of the subframe
  ;;
  (let loop ((i 0)
             (hlists '())
             (hlist '())
             (x0 #f)
             (x1 #f)
             (lh (query-style style 'line-spacing))
             ;; pending is stuff we will put on the hlist if it fits
             (pending '())
             (tabs (cons '(position: (lm 0)  ; relative to current left margin
                           align: left)
                         (query-style style 'tab-stops)))
             (saved-break-contn #f)                ; try again w/mid-r-margin
             (breaks (make-breakpoint-history)))
    ;;
    (bind ((type info detail (item-stream)))
      ;;
      (case type
        ;;
        ;;  we have finally run out of input...
        ;;
        ((#f)
         (if x0                         ; we are processing a request
             (request-return port (make <hline>
                                        height: lh
                                        content: (reverse! 
                                        
         (emit-hlist (reverse! (append! pending hlist))))
        ;;
        ;;  there is a discretionary break point
        ;;
        ((/ - --)
         (let ((b (compute-badness posn)))
           (if pending-badness
               ;; if there is at least some evil at the previous
               ;; possible breakpoint, then decide which is less
               ;; evil and break there
               (if (evil<? pending-badness b)
                   ;; the old break point was better...
                   (begin
                     (emit-hlist (reverse! hlist))
                     (loop pending (new-line-posn) '() #f #f))
                   ;; this new break point is better
                   (begin
                     (emit-hlist (reverse! (append! pending hlist)))
                     (loop '() (new-line-posn) '() #f #f)))
               ;; there was no evil before; flush pending and keep going...
               (loop (append! pending hlist) posn '() b tab))))
        ;;
        ;;  a tab
        ;;
        ((tab)
         ;; completely ignore this if there are no tab stops left
         (let ((new-tab (if tab (+ tab 1) 0))
               (tab-list (query-style style 'tabstops)))
           (if (< new-tab (length tab-list))
               (begin
                 ;; flush the current hlist, complete with anything
                 ;; pending (note that, normally, a tab is preceded by
                 ;; a break point (/))
                 (if (not (and (null? pending) (null? hlist)))
                     (emit-hlist (reverse! (append! pending hlist))))
                 ;; start a new hlist
                 (loop '()
                       (cons (car posn)
                             (make-rect (compute-tab-x new-tab)
                                        (y (cdr posn))
                                        0
                                        (query-style style 'line-spacing)))
                       '()
                       #f
                       (list-ref tab-list new-tab)))
               ;; complete ignore it...
               (loop hlist posn pending pending-badness tab))))
        ;;
        ;;  a manual line break
        ;;
        ((break)
         ...)
        ;;
        ;;  some horizontal whitespace
        ;;
        ((space)
         ...)
        ;;
        ;;  huh?  an actual character?
        ;;
        ((char)
         (let* ((bb (compute-char-bbox ...))
                (bbo (offset-rect bb x (+ y baseline-shift))
                     ...))))
        ;;
        ;; something wierd in the inline item stream
        ;;
        (else
         (error "unknown inline item: ~s" type))))))

;; return #t if the first is the lesser of the two evils

(define (evil<? evil1 evil2)
  ...)
|#

#|
(define (tpbrk)
  (make-request-stream
   (lambda (rs)
     (break-para-lines *test-para*
                       (open-inline-stream *test-para* 0)
                       rs))))
|#
