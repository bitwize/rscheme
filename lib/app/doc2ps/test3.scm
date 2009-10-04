(define (tgs)
  (reset *dev*)
  (test3 *dev*)
  (flush-output-port *dev*))

(define (tpr)
  (let ((p (open-ps-device "/tmp/out.ps")))
    (test3 p)
    (close-graphics-device p)))

(define (test3 dev)
  (let ((frames (chapter-start-page)))
    (for-each (lambda (f)
                (render-outline f dev))
              frames)
    ;;
    (render (gen-test-flow-layout) dev)
    #|
    (for-each
     (lambda (y hl)
       (render hl #f 36 y dev))
     '(212 200)
     *test-hlist*)
    |#
    ))

(define (style-strip s)
  (define (scheck chk)
    (and (> (string-length s) 3)
         (string=? (substring s 0 3) chk)))
  ;;
  (cond
   ((scheck ".I ")
    (values (substring s 3) 'emphasis-char-style))
   ((scheck ".B ")
    (values (substring s 3) 'bold-char-style))
   (else
    (values s 'body-char-style))))

(define (markup-para . strings)
  (make <para>
        style: 'body-para-style
        content: (map (lambda (s)
                        (bind ((s style (style-strip s)))
                          (make <text-run>
                                style: style
                                content: (list->string
                                          (map (lambda (ch)
                                                 (case ch
                                                   ((#\/) #\nul)
                                                   (else ch)))
                                               (string->list s))))))
                      strings)))


#|
(define *test-flow*
  (let ((p0 (markup-para
             "Aya aya, this is a test of the "
             ".I emergency"
             " finance broadcast sys/tem."))
        (p1 (markup-para
             "Aya aya, this is yet another test of the "
             ".I flashy pant"
             " and "
             ".I dif/fi/cult"
             " broadcast sys/tem."))
        (pageflow (make-request-stream
                   (lambda (rs)
                     (iterate-over-flow (chapter-start-page) rs)))))
    ;;
    (define (subframe)
      (print (request pageflow 18)))
    ;;
    (define (hlist p from to #optional hyph?)
      (make <hlist>
            para: p
            start-index: from
            index-count: (- to from)
            x: 0
            align: 'left
            stretch: 0
            hyphenate?: hyph?))
    ;;
    (define (lines . l)
      (map (lambda (h)
             (if (number? h)
                 h
                 (make <hline>
                       height: 18
                       content: (list h))))
           l))

    (make <flow-layout>
          content: (list
                    (make <vlist>
                          subframe: (subframe)
                          content: (lines (hlist p0 0 41)))
                    (make <vlist>
                          subframe: (subframe)
                          content: (lines (hlist p0 41 63 #t)
                                          (hlist p0 63 67)
                                          12
                                          (hlist p1 0 9)))
                    (make <vlist>
                          subframe: (subframe)
                          content: (lines (hlist p1 9 34)
                                          (hlist p1 34 61 #t)
                                          (hlist p1 61 87)))))))

|#

(define (layout-lines vitems rs)
  (define (extend-vlists vlists sf lst)
    (if (null? lst)
        vlists
        (cons (make <vlist>
                    placement: sf
                    content: (reverse! lst))
              vlists)))
  ;;
  (let loop ((vi vitems)
             (vlists '())
             (current-sfp #f)
             (current-vlist '()))
    (if (null? vi)
        (reverse! (extend-vlists vlists 
                                 current-sfp 
                                 current-vlist))
        (cond
         ;;
         ((and (pair? (car vi)) 
               (eq? (caar vi) 'break))
          (request rs 'break (cadar vi))
          (loop (cdr vi)
                (extend-vlists vlists 
                               current-sfp 
                               current-vlist) 
                #f
                '()))
         ;;
         ((and (pair? (car vi))
               (eq? (caar vi) 'space))
          ;;
          (bind ((size (cadar vi))
                 (hline (make <hline>
                              height: size
                              content: '()))
                 (new-sf y (request rs size 'in-column)))
            (if (or (not current-sfp)
                    (eq? new-sf (subframe current-sfp)))
                (loop (cdr vi)
                      vlists
                      (if current-sfp
                          current-sfp
                          (make <placement>
                                subframe: new-sf
                                y: y))
                      (cons hline current-vlist))
                ;; if it causes a break, then go ahead and keep the
                ;; break, but drop the space itself (i.e., because it
                ;; appears at the end of a break section)
                (loop (cdr vi)
                      (extend-vlists vlists current-sfp current-vlist)
                      #f
                      '()))))
         ;;
         (else
          (bind (((l <hline>) (car vi))
                 (new-sf y (request rs 
                                    (height l)
                                    (query-style 
                                     (style (para (car (content l))))
                                     'placement))))
            (if (or (not current-sfp)
                    (eq? new-sf (subframe current-sfp)))
                ;; XXX what about when the returned y represents
                ;; a different height than requested
                (loop (cdr vi)
                      vlists
                      (if current-sfp
                          current-sfp
                          (make <placement>
                                subframe: new-sf
                                y: y))
                      (cons (car vi) current-vlist))
                (loop (cdr vi)
                      (extend-vlists vlists current-sfp current-vlist)
                      (make <placement>
                            subframe: new-sf
                            y: y)
                      (list (car vi))))))))))
                        
            
(define (gen-test-flow-layout)
  (let ((pt (markup-para ".B The Thing"))
        (p0 (markup-para
             "Aya aya, this is a test of the "
             ".I emergency"
             " finance broadcast sys/tem."))
        (p1 (markup-para
             "Aya aya, this is yet another test of the "
             ".I flashy pant"
             " and "
             ".I dif/fi/cult"
             " broadcast sys/tem.")))
    ;;
    (define (hlist p from to #optional hyph?)
      (make <hlist>
            para: p
            start-index: from
            index-count: (- to from)
            x: 0
            align: 'left
            stretch: 0
            hyphenate?: hyph?))
    ;;
    (define (lines . l)
      (map (lambda (h)
             (cond
              ((number? h)
               (list 'space h))
              ((symbol? h)
               (list 'break h))
              (else
               (make <hline>
                     height: 18
                     content: (list h)))))
           l))
    ;;
    (make <flow-layout>
          content: (layout-lines
                     (lines (hlist pt 0 9)
                            'column
                            (hlist p0 0 19)
                            (hlist p0 19 41)
                            (hlist p0 41 63 #t)
                            (hlist p0 63 67)
                            12
                            (hlist p1 0 9)
                            'column
                            (hlist p1 9 34)
                            (hlist p1 34 61 #t)
                            (hlist p1 61 87))
                     (make-request-stream
                      (lambda (rs)
                        (iterate-over-flow (chapter-start-page) 
                                           rs)))))))


(define (test-page-style)
  (list
   (make <text-frame>
         flow: 'a
         frame: (make-rect 1in 8.5in 6.75in 1.5in))
   (make <text-frame>
         flow: 'a
         frame: (make-rect 1in 1in 6.75in 7in)
         num-columns: 2
         primary-sidebar: 'left
         primary-sidebar-width: 1.5in
         sidebar-gap: 0.25in)
   (make <line-graphic>
         line-start: (make-point 1in 8.25in)
         line-end: (make-point 7.75in 8.25in))
   (make <line-graphic>
         line-start: (make-point 1in 0.75in)
         line-end: (make-point 7.75in 0.75in))
   (make <text-box>
         frame: (make-rect 7in 0.5in 0.75in 0.25in)
         content: '("- " (page-number) " -")
         style: 'emphasis-char-style
         align: 'right)))

(define *test-page-sequence*
  (make <simple-page-sequence>
        initial-page-styles: '()
        repeat-page-styles: '(test-page-style)
        blank-page-style: 'intentionally-blank-page))

(define (test4)
  (layout-flow-in-page-sequence 
   (gen-test-flow)
   *test-page-sequence*))


