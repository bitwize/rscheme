

(define (makeflow tag . content)
  (make <flow>
        flow-tag: tag
        content: content))

(define (gen-toc-test-flow-layout)
  (define (page-ref page)
    (make <computed-inline>
          inline-computer: (lambda (self pp cf)
                             (list
                              (make <text-run>
                                    style: cf
                                    content: (format #f "~a" page))))))
  (define (h1 label title page)
    (make <para>
          style: 'toc-level1-para
          content: (list
                    (make <text-run>
                          style: 'toc-level1-char-style
                          content: (format #f "~a\t~a\t" label title))
                    (page-ref page))))
  (define (h2 label title page)
    (make <para>
          style: 'toc-level2-para
          content: (list
                    (make <text-run>
                          style: 'toc-level2-char-style
                          content: (format #f "~a\t~a\t" label title))
                    (page-ref page))))
  (define (h3 label title page)
    (make <para>
          style: 'toc-level3-para
          content: (list
                    (make <text-run>
                          style: 'toc-level3-char-style
                          content: (format #f "~a\t~a -- " label title))
                    (page-ref page))))
  ;;
  (makeflow
   'toc
   (make <para>
         style: 'toc-metatitle-para
         content: (list
                   (make <text-run>
                         style: 'toc-metatitle-char-style
                         content: "Table of Contents")))
   (make-hrule 0.25in 0.2in 
               linewidth: 2
               left: 0in
               right: 3in)
   (h1 "1" "Introduction" 1)
   (h2 "1.1" "Stuff" 1)
   (h3 "1.1.1" "Nitty" 2)
   (h3 "1.1.2" "Gritty" 2)
   (h3 "1.1.3" "Details" 3)
   (h2 "1.2" "Continuing" 3)
   (h2 "1.3" "Conclusion" 4)
   (h1 "2" "Requirements" 6)
   (h2 "2.1" "Functional Requirements" 6)
   (h2 "2.2" "Performance Requirements" 9)
   (h2 "2.3" "Non-stated" 11)
   (h1 "3" "Design" 13)
   (h1 "A" "References" 16)))

(define (testtoc)
  (reset-pages)
  (layout-flow-in-page-sequence
   (gen-toc-test-flow-layout)
   (make <simple-page-sequence>
         initial-page-styles: '()
         repeat-page-styles: '(toc-right-page toc-left-page)
         blank-page-style: 'intentionally-blank-page-preface)
   #f))

(define (generate-toc-pages toc)
  (layout-flow-in-page-sequence
   (iteration->flow 'toc
                    (lambda (emit)
                      (generate-toc-title emit)
                      (generate-toc-pages* toc emit)))
   (make <simple-page-sequence>
         initial-page-styles: '()
         repeat-page-styles: '(toc-right-page toc-left-page)
         blank-page-style: 'intentionally-blank-page-preface)
   #f))

(define (generate-toc-title emit)
  (emit (make <para>
              style: 'toc-metatitle-para
              content: (list
                        (make <text-run>
                              style: 'toc-metatitle-char-style
                              content: "Table of Contents"))))
  (emit (make-hrule 0.25in 0.2in 
                    linewidth: 2
                    left: 0in
                    right: 3in)))

(define-method page-label ((self <anchor>))
  (if (owner self)
      (page-label (page (subframe (placement (owner self)))))
      "??"))

(define-method page-ordinal ((self <anchor>))
  (if (owner self)
      (+ 1 (page-ordinal-in-document
            (page (subframe (placement (owner self))))))
      0))

;;; pdf-annotate-link:
;;;    `rect' is relative to current-point

(define (pdf-annotate-link dev rect (anchor <anchor>))
  (let* ((p0 (inverse-transform (currentpoint dev)
                                (currentmatrix dev)))
         (r0 (offset-rect rect (x p0) (y p0)))
         (r (transform r0 (currentmatrix dev))))
    ;;
    (pdfmark-annotation dev
                        Rect: r0
                                        ;rect 
                                        ;r
                        Subtype: 'Link
                        Page: (page-ordinal anchor))))

;;; generates a text run that contains something like "13",
;;; referencing the page associated with the given key of
;;; the given node

(define (make-element-ref-page (e <sgml-node>) key)
  ;;
  (define (element-referrer self pp cf)
    (let (((anchor <anchor>) (get-property e key)))
      ;;
      (list
       (make <annotated-text-run>
             style: (override-style 
                     cf
                     annotation: (lambda (dev rect)
                                   (pdf-annotate-link dev rect anchor)))
             content: (page-label anchor)))))
   ;;
  (make <computed-inline>
        inline-computer: element-referrer))

;;; generates a text run that contains something like "13"

(define (make-anchor-ref-page (a <anchor>) #optional char-style)
  (assert (memq (car (name a)) '(section indexterm)))
  (make <computed-inline>
        inline-computer: (lambda (self pp cf)
                           ;;
                           (define (ann dev rect)
                             (pdf-annotate-link dev rect a))
                           ;;
                           (list
                            (make <annotated-text-run>
                                  style: (override-style 
                                          (or char-style cf)
                                          annotation: ann)
                                  content: (page-label a))))))

;;; generates a text run that contains something like "2.3"

(define-method make-section-ref-label ((tn <toc-node>) cf)
  (make <computed-inline>
        inline-computer: (lambda (self pp cf)
                           (list
                            (make <text-run>
                                  style: cf
                                  content: (name tn))))))

(define-method make-section-ref-label ((tn <special-toc-node>) cf)
  (make <text-run>
        style: cf
        content: ""))
        

;;; generates a text run that contains something like "Object System"

(define-method make-section-ref-title ((tn <toc-node>) cf)
  (make <computed-inline>
        inline-computer: (lambda (self pp cf)
                           (iteration->list
                            (lambda (emit)
                              (bridge-title-stuff
                               (xpath (element tn) "title")
                               emit
                               cf))))))

(define-method make-section-ref-title ((tn <special-toc-node>) cf)
  (make <text-run>
        style: cf
        content: (title tn)))
        

(define-method toc-anchor ((self <toc-node>))
  (get-property (element self) 'title-anchor))

(define-method toc-anchor ((self <special-toc-node>))
  (anchor self))

(define (generate-toc-pages* (self <toc-node>) emit)
  (format #t "----------- generate-toc-pages* --------------\n")
  ;(print self)
  ;;
  (let* ((depth (length (label self)))
         (para-style (case depth
                       ((1) 'toc-level1-para)
                       ((2) 'toc-level2-para)
                       ((3) 'toc-level3-para)
                       (else #f))))
    (if para-style
        (let ((cf (query-style para-style 'default-char-style))
              (a (toc-anchor self)))
          (emit
           (make <para>
                 style: para-style
                 content: (if (eq? para-style 'toc-level3-para)
                              (list
                               (make-section-ref-label self cf)
                               (make <text-run> style: cf content: " ")
                               (make-section-ref-title self cf)
                               (make <text-run> style: cf content: " -- ")
                               (make-anchor-ref-page a))
                              (list
                               (make-section-ref-label self cf)
                               (make <text-run> style: cf content: "\t")
                               (make-section-ref-title self cf)
                               (make <text-run> style: cf content: "\t")
                               (make-anchor-ref-page a)))))))
    (if (pair? (child-nodes self))
        (begin
          (if (< depth 3)
              (emit (make-vspace 6)))
          (for-each
           (lambda (sub)
             (generate-toc-pages* sub emit))
           (child-nodes self))
          (if (< depth 3) 
              (emit (make-vspace 6)))))))
          
