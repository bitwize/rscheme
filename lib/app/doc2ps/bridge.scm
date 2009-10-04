;;;
;;;  Bridge the SGML world to the LAYOUT world
;;;

;;;**********************************************************************


(define-method bridge-display-stuff ((self <list>) emit)
  (for-each (lambda (item)
              (bridge-display-stuff item emit))
            self))

(define-method bridge-display-stuff ((self <BEGINPAGE>) emit)
  ;; XXX! Note that this is not really supposed to *cause* a page break...
  (emit (make <vbreak> section: 'page)))

(define-method bridge-display-stuff ((self <TITLE>) emit)
  ;; skip this; it's handled elsewhere
  (values))

(define-method bridge-display-stuff ((self <SUBTITLE>) emit)
  ;; skip this; it's handled elsewhere
  (values))

(define-method bridge-display-stuff ((self <DOCINFO>) emit)
  ;; skip this; it's handled elsewhere
  (values))

(define (emit-conformance self emit)
  (let ((conformance (get-attribute-value self "conformance")))
    (if conformance
        (emit (make-paragraph-from-inline
               (string-append "(" conformance ")")
               'conformance-para-style
               bridge-inline-stuff)))))
  
(define-method bridge-display-stuff ((self <PARA>) emit)
  (emit-conformance self emit)
  (emit (bridge-paragraph self 'body-para-style)))

(define-method bridge-display-stuff ((self <SIMPARA>) emit)
  (emit (bridge-paragraph self 'body-para-style)))

(define-method bridge-display-stuff ((self <FORMALPARA>) emit)
  (let ((p0 (make-paragraph-from-inline (xpath self "title")
                                        'paratitle-para-style
                                        bridge-title-stuff))
        (p1 (make-paragraph-from-inline (content (xpath self "para"))
                                        'body-para-style
                                        bridge-inline-stuff))
        (my-anchor (make-anchor self <BOOK> (list 'para self))))
    (set-property! self 'title-anchor my-anchor)
    (emit-conformance self emit)
    (emit (make <para>
                style: 'body-para-style
                content: (append (list my-anchor)
                                 (content p0)
                                 (list
                                  (make <text-run>
                                        style: 'paratitle-char-style
                                        content: ". "))
                                 (content p1))))))


(define (make-bent-rule #key edge left-margin right-margin height offset)
  (let* ((radius 3)
         (dy (if (eq? edge 'top)
                 (- radius)
                 radius)))
    ;;
    (define (render-bent-rule dev width)
      (let ((p0 (make-point (+ left-margin radius) (+ offset dy)))
            (p1 (make-point (- width right-margin radius) (+ offset dy))))
        ((style-compile 'screen-stroke) dev)
        (moveto dev (make-point left-margin (+ offset dy)))
        (if (eq? edge 'top)
            (arcn dev p0 radius 180 90)
            (arc dev p0 radius 180 270))
        (if (eq? edge 'top)
            (arcn dev p1 radius 90 0)
            (arc dev p1 radius 270 360))
        (stroke dev)))
    ;;
    (make <flow-vbox>
          height: height
          render-proc: (lambda (self width dev)
                         (render-bent-rule dev width)))))

(define-method bridge-display-stuff ((self <BLOCKQUOTE>) emit)
  (let ((p0 (make-paragraph-from-inline (xpath self "attribution")
                                        'attribution-para-style
                                        bridge-inline-stuff))
        (px (map
             (lambda (item)
               (make-paragraph-from-inline (content item)
                                           'blockquote-para-style
                                           bridge-inline-stuff))
             (select (lambda (f)
                       (not (instance? f <ATTRIBUTION>)))
                     (content self)))))
    (for-each emit px)
    (emit p0)))
  
(define-method bridge-display-stuff ((self <INFORMALFIGURE>) emit); DocBook 3.1
  (bridge-figure-content (content self) emit))

(define-method bridge-display-stuff ((self <FIGURE>) emit)
  (let ((my-anchor (make-anchor self <BOOK> (list 'figure self))))
    (set-property! self 'title-anchor my-anchor)
    (emit (make-paragraph-from-inline 
           (list
            "\t"
            (list :bold
                  (format #f "Figure ~a." (get-property self 'figure-name)))
            my-anchor
            "\t"
            (xpath self "title"))
           'table-title-style
           bridge-title-stuff))
    (bridge-figure-content (content self) emit)))

(define-method bridge-figure-content ((self <list>) emit)
  (for-each (lambda (c) (bridge-figure-content c emit)) self))

(define-method bridge-figure-content ((self <TITLE>) emit)
  (values))                             ; handled in <FIGURE>

(define-method bridge-figure-content ((self <SCREEN>) emit)
  (bridge-display-stuff self emit))

(define (bridge-section (self <sgml-element>) level emit)
  (let ((toc-node (get-property self 'toc-node #f))
        (my-anchor (make-anchor self <BOOK> (list 'section self))))
    (set-property! self 'title-anchor my-anchor)
    (if (= level 1)
        (emit (make-hrule 10 3 left: -1in right: 1in)))
    (emit (make-paragraph-from-inline
           (if toc-node
               (if (= level 1)
                   (list "\t"
                         (toc-section-number toc-node)
                         ".\t"
                         my-anchor
                         (xpath self "TITLE"))
                   (list (toc-section-number toc-node)
                         ". "
                         my-anchor
                         (xpath self "TITLE")))
               (xpath self "TITLE"))
           (case level
             ((1) 'title1-para-style)
             ((2) 'title2-para-style)
             ((3 4) 'title3-para-style)
             (else (error "level too deep at ~s" self)))
           bridge-title-stuff))
    ;(bridge-paragraph (xpath self "TITLE") 'title1-para-style)
    (bridge-display-stuff (content self) emit)))

(define-method bridge-display-stuff ((self <SECT1>) emit)
  (bridge-section self 1 emit))

(define-method bridge-display-stuff ((self <SECT2>) emit)
  (bridge-section self 2 emit))

(define-method bridge-display-stuff ((self <SECT3>) emit)
  (bridge-section self 3 emit))

(define-method bridge-display-stuff ((self <SECT4>) emit)
  (bridge-section self 4 emit))

(define-method bridge-display-stuff ((self <SCREEN>) emit)
  (let ((role (string->symbol 
               (string-downcase (or (get-attribute-value self "role") 
                                    "default")))))
  ;; XXX this needs to be much better...
  ;;  (1) handle callouts
  ;;  (2) handle line wrapping "properly"
    (if (memq role '(default))
        (emit (make-bent-rule edge: 'top
                              left-margin: 18
                              right-margin: 18
                              height: 10
                              offset: 3)))
    ;;
    (let ((sco (init-callout-scope! self)))
      (emit (make-paragraph-from-inline (content self)
                                        'screen-style
                                        bridge-screen-stuff))
      (if (memq role '(default))
          (emit (make-bent-rule edge: 'bottom
                                left-margin: 18
                                right-margin: 18 
                                height: 10
                                offset: 1))))))
  
         

(define-method bridge-display-stuff ((self <PROGRAMLISTING>) emit)
  ;; XXX this needs to be much better...
  ;;  (1) handle callouts
  ;;  (2) handle line wrapping "properly"
  (emit (make-bent-rule edge: 'top
                        left-margin: 18
                        right-margin: 18 
                        height: 10 
                        offset: 3))
  (emit (make-paragraph-from-inline
         (content self)
         'screen-style
         bridge-screen-stuff))
  (emit (make-bent-rule edge: 'bottom
                        left-margin: 18
                        right-margin: 18 
                        height: 10
                        offset: 1)))

(define $bullet-text-runs
  (list (make <text-run>
              style: 'body-char-style
              content: "\t\267\t")))
  
(define $list-indent0 0.3in)
(define $list-indent1 0.35in)

(define-method bridge-display-stuff ((self <CALLOUTLIST>) emit)
  (bridge-display-list self
                       emit
                       (lambda (i n l)
                         (let ((refs (find-elements-by-id 
                                      self
                                      (get-attribute-value l "arearefs"))))
                           (list
                            (make <text-run>
                                  style: 'dingbat-char-style
                                  content: (string-append
                                            "\t"
                                            (string-join
                                            ""
                                            (map (lambda (ref)
                                                   (list-ref '("\312"
                                                               "\313"
                                                               "\314"
                                                               "\315"
                                                               "\316"
                                                               "\317"
                                                               "\320"
                                                               "\321"
                                                               "\322"
                                                               "\323")
                                                             (get-property 
                                                              ref
                                                              'callout-tag)))
                                                 refs))
                                            "\t")))))))

(define-method bridge-display-stuff ((self <ITEMIZEDLIST>) emit)
  (bridge-display-list self
                       emit
                       (lambda (i n l)
                         $bullet-text-runs)))

(define-method bridge-display-stuff ((self <ORDEREDLIST>) emit)
  (let* ((num (get-attribute-value self "numeration"))
         (numstyle (cond
                    ((not (string? num)) 'arabic)
                    ((string-ci=? num "arabic") 'arabic)
                    ((string-ci=? num "loweralpha") 'alpha)
                    ((string-ci=? num "upperalpha") 'Alpha)
                    ((string-ci=? num "lowerroman") 'roman)
                    ((string-ci=? num "upperroman") 'Roman)
                    (else (error "Unknown numeration ~s" num)))))
    (bridge-display-list 
     self
     emit
     (lambda (i n l)
       (list
        (make <text-run>
              style: 'paratitle-char-style
              content: (string-append
                        "\t"
                        (format-number (+ i 1) numstyle)
                        ".\t")))))))

(define-method get-itemlist ((self <sgml-element>))
  (xpath* self "listitem"))

(define-method get-itemlist ((self <CALLOUTLIST>))
  (xpath* self "callout"))

(define (indented-list-style s first?)
  (if first?
      (let* ((t (query-style s 'tab-stops))
             (new-tabs (cons*
                        `(position: (l ,$list-indent0)
                                    align: right %type: list)
                        `(position: (l ,$list-indent1) 
                                    align: left)
                        t)))
        (override-style 
         s
         tab-stops: new-tabs
         left-margin: (+ $list-indent1 (query-style s 'left-margin))))
      (override-style 
       s
       tab-stops: (map (lambda (t)
                         (adjust-left-tab-stop t $list-indent1))
                       (query-style s 'tab-stops))
       left-margin-first: (+ $list-indent1
                             (query-style s 'left-margin-first))
       left-margin: (+ $list-indent1
                       (query-style s 'left-margin)))))

(define (adjust-left-tab-stop stop adj)
  (define (adjp s)
    (if (null? s)
        '()
        (if (eq? (car s) 'position:)
            (if (eq? (caadr s) 'l)
                (cons* (car s)
                       (list 'l (+ adj (cadadr s)))
                       (cddr s))
                s)
            (cons* (car s) (cadr s) (adjp (cddr s))))))
  ;;
  (adjp stop))

(define (bridge-display-list (self <sgml-element>)
                             emit
                             item-label-proc)
  ;;
  (define (style-adjust s first?)
    (indented-list-style s first?))
  ;;
  (let* ((itemlist (get-itemlist self))
         (n (length itemlist)))
    (for-each
     (lambda (l (i <fixnum>))
       (let ((first? #t))
         (bridge-display-stuff 
          (content l)
          (lambda (p)
            ;; This may be something of a hack, but is designed to
            ;; handle the interaction between "conformance" attribute
            ;; processing in <[FORMAL]PARA> and <ITEMIZEDLIST> processing.
            ;; The idea is that the sidebar generated by a conformance
            ;; attribute should neither be subject to nor influence the
            ;; paragraph adjustment to handle itemized list processing.
            (if (memq (query-style (style p) 'placement)
                      '(sidebar/first-baseline
                        sidebar/first-baseline*
                        sidebar/top-edge
                        sidebar/last-baseline))
                (emit p)
                (begin
                  (emit
                   (make <para>
                         style: (style-adjust (style p) first?)
                         content: (if first?
                                      (append
                                       (item-label-proc i n l)
                                       (content p))
                                      (content p))))
                  (set! first? #f)))))))
     itemlist
     (range n))))

(define-method bridge-display-stuff ((self <WARNING>) emit)
  (bridge-admonition self emit 'warning))
  
(define-method bridge-display-stuff ((self <NOTE>) emit)
  (bridge-admonition self emit 'note))

(define (bridge-admonition self emit type)
  ;;
  (let* ((indent0 0.25in)
         (label (string-append (note-role-label self type) ":"))
         (indent1 (+ indent0
                     (string-width (query-style 'paratitle-font 'font)
                                   label)
                     0.1in)))
    (define (style-adjust s first?)
      (if first?
          (override-style 
           s
           tab-stops: (cons
                       `(position: (l ,indent1) align: left)
                       (query-style s 'tab-stops))
           left-margin-first: (+ indent0
                                 (query-style s 'left-margin-first))
           left-margin: (+ indent1 (query-style s 'left-margin)))
          (override-style 
           s
           left-margin-first: (+ indent1
                                 (query-style s 'left-margin-first))
           left-margin: (+ indent1
                           (query-style s 'left-margin)))))
    ;;
    (let ((first? #t))
      (bridge-display-stuff 
       (content self)
       (lambda (p)
         (emit
          (make <para>
                style: (style-adjust (style p) first?)
                content: (if first?
                             (cons
                              (make <text-run>
                                    style: 'paratitle-char-style
                                    content: (string-append label "\t"))
                              (content p))
                             (content p))))
         (set! first? #f))))))

(define-method note-role-label ((self <WARNING>) type)
  "WARNING")

(define-method note-role-label ((self <NOTE>) type)
  (let ((role (string->symbol 
               (string-downcase (or (get-attribute-value self "role") 
                                    "unknown")))))
    (case role
      ((rationale) "RATIONALE")
      ((issue) "ISSUE")
      (else "NOTE"))))

(define-method bridge-screen-stuff ((self <list>) emit style)
  (for-each (lambda (item)
              (bridge-screen-stuff item emit style))
            self))
  
(define-method bridge-screen-stuff ((self <sgml-text>) emit style)
  (emit (make <text-run>
              style: style
              content: (content self))))

(define-method bridge-screen-stuff ((self <sgml-entity-ref>) emit style)
  (emit (make <text-run>
              style: style
              content: (file->string (entity-os-path (subject self))))))

(define-class <callout-scope> (<object>)
  (callouts init-value: '()))

(define (init-callout-scope! elem)
  (let ((sco (make <callout-scope>)))
    (set-property! elem 'callout-scope sco)
    sco))

(define (find-callout-scope n)
  (or (get-property n 'callout-scope #f)
      (find-callout-scope (parent n))))

(define-method bridge-screen-stuff ((self <CO>) emit style)
  (let* ((sco (find-callout-scope self))
         (n (length (callouts sco))))
    ;;
    (set-callouts! sco (append (callouts sco) (list self)))
    (set-property! self 'callout-tag n)
    ;;
    (emit (make <text-run>
                style: 'dingbat-char-style
                content: (list-ref '("\312"
                                     "\313"
                                     "\314"
                                     "\315"
                                     "\316"
                                     "\317"
                                     "\320"
                                     "\321"
                                     "\322"
                                     "\323")
                                   n)))))

(define-method bridge-screen-stuff ((self <PROMPT>) emit style)
  (bridge-screen-stuff (content self) emit 'prompt-char-style))

(define-method bridge-screen-stuff ((self <USERINPUT>) emit style)
  (bridge-screen-stuff (content self) emit 'prototype-char-style))

(define-method bridge-screen-stuff ((self <REPLACEABLE>) emit style)
  (bridge-screen-stuff (content self) emit 'argument-char-style))

(define-method bridge-screen-stuff ((self <PARAMETER>) emit style)
  (bridge-screen-stuff (content self) emit 'argument-char-style))

(define-method bridge-screen-stuff ((self <COMPUTEROUTPUT>) emit style)
  (bridge-screen-stuff (content self) emit 'literal-char-style))

(define-method bridge-screen-stuff ((self <KEYCOMBO>) emit style)
  (bridge-screen-stuff (content self) emit 'prototype-char-style))

(define-method bridge-screen-stuff ((self <KEYCAP>) emit style)
  (bridge-screen-stuff (content self) emit 'prototype-char-style))

(define-method bridge-screen-stuff ((self <OPTIONAL>) emit style)
  (emit (make <text-run> style: 'body-char-style content: "["))
  (bridge-screen-stuff (content self) emit style)
  (emit (make <text-run> style: 'body-char-style content: "]")))



(define (iteration->flow tag proc)
  (let ((next (proc->iterator proc)))
    (let loop ((r '()))
      (let ((p (next)))
        (if p
            (loop (cons p r))
            (make <flow>
                  flow-tag: tag
                  content: (reverse! r)))))))

(define (chapter-page-sequence)
  (let ((f (or (getenv "PAGE_FORMAT") "default")))
    (or (table-lookup *page-format-definitions* f)
        (error "PAGE_FORMAT=~s not available" f))))
      

(define (bridge-chapters chapter-list)
  ;; Go through and bridge everything.  `chapter-list'
  ;; may contain <CHAPTER> as well as other chapter-level
  ;; content such as <APPENDIX>, <COLOPHON>, <PREFACE>,
  ;; <DEDICATION>, <GLOSSARY>, etc.
  ;;
  (progress " Format")
  (let ((flows (map (lambda (chap)
                      (assert (memq (object-class chap)
                                    (list <CHAPTER>
                                          <APPENDIX>
                                          <PREFACE>
                                          <GLOSSARY>)))
                      (let ((toc-node (get-property chap 'toc-node)))
                        (progress " [~a]" (toc-section-number toc-node))
                        (bridge-chapter chap)))
                    chapter-list)))
    (progress "\n")
    ;;
    (for-each
     (lambda (chap flow)
       (let* ((toc-node (get-property chap 'toc-node))
              (sn (toc-section-number toc-node)))
         (progress " Layout chapter [~a].." sn)
         (if (or (not *chapter-list*) (member sn *chapter-list*))
             (layout-flow-in-page-sequence flow
                                           (chapter-page-sequence)
                                           toc-node)
             (progress " -- skipping: not in ~s" *chapter-list*))
         (progress "\n")))
     chapter-list
     flows)))

(define (bridge-part (self <PART>))
  (let ((toc-node (get-property self 'toc-node))
        (my-anchor (make-anchor self
                                <BOOK> 
                                (list 'section self))))
    (progress " Layout part [~a]..." (toc-section-number toc-node))
    (set-property! self 'title-anchor my-anchor)
    ;;
    (layout-flow-in-page-sequence
     (iteration->flow
      'a
      (lambda (emit)
        (emit (make-paragraph-from-inline
               (list my-anchor (xpath self "title"))
               'part-title-para-style
               bridge-title-stuff))))
     *part-page-sequence*
     toc-node))
     ;;
  (progress "\n")
  (bridge-chapters (collect-chapter-elements self)))
  
;;;
;;;  find the chapter-like elements with `node' as a parent
;;;  front-matter material (preface and dedication) are handled
;;;  separately

(define (collect-chapter-elements node)
  (append ;(xpath* node "preface")
          (xpath* node "chapter")
          (xpath* node "appendix")
          (xpath* node "glossary")
          (xpath* node "colophon")))

(define (bridge-chapter chapter)
  (let ((my-anchor (make-anchor chapter
                                <BOOK> 
                                (list 'section chapter))))
    (set-property! chapter 'title-anchor my-anchor)
    (iteration->flow
     'a
     (lambda (emit)
       ;; if we have an actual TOC label (i.e., this chapter is 
       ;; not the top level object), then emit a chapter label
       (if (not (null? (label (get-property chapter 'toc-node #f))))
           (emit (make-chapter-label)))
       ;;
       (emit (make-paragraph-from-inline
              (list "\t"
                    my-anchor
                    (xpath chapter "title"))
              'chapter-title-para-style
              bridge-title-stuff))
       (emit (make <vbreak> section: 'frame))
       (if (xpath? chapter "docinfo" "releaseinfo")
           (emit (make-paragraph-from-inline
                  (list "\t" (xpath chapter "docinfo" "releaseinfo" 'cdata))
                  'chapter-releaseinfo-para-style
                  bridge-inline-stuff)))
       (emit (make <vbreak> section: 'frame))
       (bridge-display-stuff (content chapter) emit)))))

;;;**********************************************************************

(define (make-paragraph-from-inline content 
                                    para-style
                                    inliner)
  (make <para>
        style: para-style
        content: (iteration->list
                  (lambda (emit)
                    (inliner content 
                             emit 
                             (query-style para-style 'default-char-style))))))

#|
  (let ((next (proc->iterator
               (lambda (emit)
                 (inliner content 
                          emit 
                          (query-style para-style 'default-char-style))))))
    (let loop ((runs '()))
      (let ((tr (next)))
        (if tr
            (loop (cons tr runs))
            (make <para>
                  style: para-style
                  content: (reverse! runs))))))
|#
  
(define (bridge-paragraph para style)
  (make-paragraph-from-inline (content para)
                              style
                              bridge-inline-stuff))
#|
  (let ((next (proc->iterator
               (lambda (emit)
                 (bridge-inline-stuff (content para) 
                                      emit
                                      (query-style style 'default-char-style))))))
    (let loop ((runs '()))
      (let ((tr (next)))
        (if tr
            (loop (cons tr runs))
            (make <para>
                  style: style
                  content: (reverse! runs)))))))
|#

(define (literal-paragraph style text)
  (make <para>
        style: style
        content: (list (make <text-run>
                             style: (query-style style 'default-char-style)
                             content: text))))

(define-method bridge-inline-stuff ((self <ATTRIBUTION>) emit style)
  (emit (make <text-run>
              style: style
              content: "-- "))
  (bridge-inline-stuff (content self) emit style))

  
(define-method bridge-inline-stuff ((self <string>) emit style)
  (emit (make <text-run>
              style: style
              content: self)))

(define (bridge-inline-list (self <list>) emit style gf)
  (if (and (pair? self)
           (or (symbol? (car self))
               (flag? (car self))
               (style? (car self))))
      (if (flag? (car self))
          (gf (cdr self) emit
              (case (car self)
                ((:bold) (make-bold (get-style style)))
                (else
                 (error "unknown incremental style modifier ~s"
                        (car self)))))
          (gf (cdr self) emit (car self)))
      (for-each (lambda (item)
                  (gf item emit style))
                self)))

(define-method bridge-title-stuff ((self <anchor>) emit style)
  (emit self))

(define-method bridge-title-stuff ((self <list>) emit style)
  (bridge-inline-list self emit style bridge-title-stuff))

(define-method bridge-title-stuff ((self <TITLE>) emit style)
  (bridge-title-stuff (content self) emit style))

(define-method bridge-title-stuff ((self <SUBTITLE>) emit style)
  (bridge-title-stuff (content self) emit style))

(define-method bridge-title-stuff ((self <PHRASE>) emit style)
  ;;; XXX segues into bridge-inline-stuff.  Hope that doesn't hurt
  (bridge-inline-stuff self emit style))

(define-method bridge-title-stuff ((self <LITERAL>) emit style)
  ;;; XXX segues into bridge-inline-stuff.  Hope that doesn't hurt
  (bridge-inline-stuff self emit style))

(define-method bridge-title-stuff ((self <USERINPUT>) emit style)
  ;;; XXX segues into bridge-inline-stuff.  Hope that doesn't hurt
  (bridge-inline-stuff self emit style))

(define-method bridge-title-stuff ((self <SUPERSCRIPT>) emit style)
  (bridge-title-stuff (content self) 
                      emit 
                      (make-superscript style)))

(define-method bridge-title-stuff ((self <REPLACEABLE>) emit style)
  ;;; XXX segues into bridge-inline-stuff.  Hope that doesn't hurt
  (bridge-inline-stuff self emit style))

(define (tab-check text src)
  (if (string-search text #\tab)
      (begin
        (if src
            (format #t "~s: " src))
        (format #t "***WARNING*** ~s contains a #\\TAB character\n" text))))
  
(define-method bridge-title-stuff ((self <string>) emit style)
  (emit (make <text-run>
              style: style
              content: self)))

(define-method bridge-title-stuff ((self <sgml-text>) emit style)
  (tab-check (content self) self)
  (emit (make <text-run>
              style: style
              content: (content self))))

;;;----------------------------------------

(define-method bridge-inline-stuff ((self <list>) emit style)
  (bridge-inline-list self emit style bridge-inline-stuff))

(define-method bridge-inline-stuff ((self <sgml-text>) emit style)
  (tab-check (content self) self)
  (emit (make <text-run>
              style: style
              content: (content self))))

(define (make-anchor node ancestor name)
  ;; we are going to ignore the ancestor argument for
  ;; now, because in supporting the build of a chapter-at-a-time,
  ;; it isn't clear how the scoping should work.  Hard coding
  ;; to <BOOK> doesn't make sense ... it should be something
  ;; like *GLOBAL*
  (assert (eq? ancestor <BOOK>))
  ;;  instead, we'll use find-anchor-scope, which searches
  ;;  up the ancestor chain for an anchor-scope property.  This
  ;;  isn't really right either...
  ;;  XXX (get-property (find-ancestor node ancestor) 'anchor-scope)
  (let ((a (make <anchor>
                 scope: (find-anchor-scope node)
                 name: name)))
    (table-insert! (index (scope a)) name a)
    a))

(define (find-anchor-scope n)
  (or (get-property n 'anchor-scope #f)
      (find-anchor-scope (parent n))))

(define-method bridge-inline-stuff ((self <CITETITLE>) emit style)
  ;; XXX do this
  (bridge-inline-stuff (content self) emit style))

(define-method bridge-inline-stuff ((self <INDEXTERM>) emit style)
  (process-index-term
   self
   (lambda ()
     (let ((a (make-anchor
               self 
               <BOOK>
               (list 'indexterm self))))
       (emit a)
       a))))


(define-method bridge-inline-stuff ((self <FIRSTTERM>) emit style)
  (emit (make-anchor self <BOOK> 
                     (list 'glossary
                           (get-attribute-value self "role")
                           (plain-content self))))
  (bridge-inline-stuff (content self) emit 'emphasis-char-style))

(define-method generate-long-xref ((self <FIGURE>))
  (format #f "Figure ~a" (get-figure-name self)))

(define-method generate-long-xref ((self <TABLE>))
  (format #f "Table ~a" (get-table-name self)))

(define-method generate-long-xref ((self <CHAPTER>))
  (format #f "Chapter ~a" (name (get-property self 'toc-node))))

(define-method generate-long-xref ((self <SECT1>))
  (format #f "Section ~a" (name (get-property self 'toc-node))))

(define-method generate-long-xref ((self <SECT2>))
  (format #f "Section ~a" (name (get-property self 'toc-node))))

(define-method generate-long-xref ((self <SECT3>))
  (format #f "Section ~a" (name (get-property self 'toc-node))))
  
(define-method bridge-inline-stuff ((self <XREF>) emit style)
  (emit-xref (find-element-by-id 
              self 
              (get-attribute-value self "linkend"))
             self 
             emit
             style))

;;; special behavior for a reference entry (specifically, the REFNAME
;;; inside it) is to name the referenced name (in TT style) and give
;;; the page number in parentheses, e.g., 
;;;
;;;    See <xref linkend="f.cons"> for building pairs.
;;;
;;; becomes:
;;;
;;;    See cons (p.13) for building pairs
;;;

(define-method emit-xref ((target <REFNAME>) (self <XREF>) emit style)
  (bridge-inline-stuff (content target) emit 'prototype-char-style)
  (emit (make <text-run>
              style: style
              content: " (p."))
  (emit (make-element-ref-page target 'id-anchor))
  (emit (make <text-run>
              style: style
              content: ")")))
   
(define-method emit-xref ((target <sgml-node>) (self <XREF>) emit style)
  ;; default behavior for the content of an XREF is to refer
  ;; to the lowest ancestor of the target, e.g., "section 2.3"
  (let* ((in-section (or (find-ancestor target <FIGURE>)
                         (find-ancestor target <SECT3>)
                         (find-ancestor target <SECT2>)
                         (find-ancestor target <SECT1>)
                         (find-ancestor target <CHAPTER>)))
         (role (or (get-attribute-value self "role") "plain"))
         (long? (or (string-ci=? role "long")
                    (string-ci=? role "title")))
         ;; XXX note; anchor to the SECT if the linkend is a PARA :-/
         (anchor-target (if (instance? target <FORMALPARA>)
                            target
                            in-section))
         ;;
         (ann (lambda (dev rect)
                (pdf-annotate-link 
                 dev 
                 rect 
                 (get-property anchor-target 'title-anchor))))
         ;;
         ;; title-part could vary by (object-class target)...
         (title-part (if (or (instance? target <FORMALPARA>)
                             (string-ci=? role "title"))
                         (iteration->list
                          (lambda (emit)
                            (bridge-inline-stuff (content
                                                  (xpath target "title"))
                                                 emit
                                                 (override-style
                                                  'paratitle-char-style
                                                  annotation: ann))))
                         '())))
    ;;
    (format #t "xref in-section: ~s role: ~s\n" in-section role)
    ;;
    (if long?
        (let ((cfa (override-style style annotation: ann)))
          (emit (make <text-run>
                      style: cfa
                      content: (generate-long-xref target)))
          (if (pair? title-part)
              (begin
                (emit (make <text-run>
                            style: cfa
                            content: ", "))
                (for-each emit title-part))))
        ;; XXX does this really need to be computed?
        (emit (make <computed-inline>
                    inline-computer: (lambda (self para cf)
                                       (xref-inline-content self 
                                                            para
                                                            cf
                                                            ann
                                                            in-section
                                                            title-part)))))))

(define (xref-inline-content self para cf ann in-section title-part)
  ;;
  (define (is-figure?)
    (has-property? in-section 'figure-name))
  ;;
  (define (xref-target-name)
    (if (is-figure?)
        (get-property in-section 'figure-name)
        (name (get-property in-section 'toc-node))))
  ;;
  (let ((cfa (override-style cf
                             annotation: ann)))
    (values
     (append
      (list
       (make <text-run>
             style: cfa
             content: (if (is-figure?)
                          "\247Figure "
                          "\247")) ; \247 => the "c.f." symbol
       (make <text-run>
             style: cfa
             content: (xref-target-name)))
      (if (null? title-part)
          '()
          (cons (make <text-run>
                      style: cfa
                      content: ", ")
                title-part)))
     cf)))

(define-method bridge-inline-stuff ((self <INLINEEQUATION>) emit style)
  (bridge-inline-stuff (xpath self 'alt) emit 'emphasis-char-style))

(define-method bridge-inline-stuff ((self <EMPHASIS>) emit style)
  (bridge-inline-stuff (content self) emit 'emphasis-char-style))

(define-method bridge-inline-stuff ((self <GLOSSTERM>) emit style)
  (bridge-inline-stuff (content self) emit 'emphasis-char-style))

(define-method bridge-inline-stuff ((self <SUPERSCRIPT>) emit style)
  (bridge-inline-stuff (content self) 
                       emit 
                       (make-superscript style)))

(define-method bridge-inline-stuff ((self <FOREIGNPHRASE>) emit style)
  (bridge-inline-stuff (content self) emit 'emphasis-char-style))

(define-method bridge-inline-stuff ((self <PARAMETER>) emit style)
  (bridge-inline-stuff (content self) emit 'emphasis-char-style))

(define-method bridge-inline-stuff ((self <OPTIONAL>) emit style)
  (emit (make <text-run> style: 'body-char-style content: "["))
  (bridge-inline-stuff (content self) emit style)
  (emit (make <text-run> style: 'body-char-style content: "]")))
  

(define-method bridge-inline-stuff ((self <CLASSNAME>) emit style)
  (bridge-inline-stuff (content self) emit 'literal-char-style))

(define-method bridge-inline-stuff ((self <APPLICATION>) emit style)
  (bridge-inline-stuff (content self) emit 'bold-char-style))

(define-method bridge-inline-stuff ((self <FOOTNOTE>) emit style)
  (emit (make <text-run>
              style: style
              content: "[1]")))


(define-method bridge-inline-stuff ((self <COMMENT>) emit style)
  (emit (make <text-run>
              style: 'comment-char-style
              content: "["))
  (bridge-inline-stuff (content self) emit 'comment-char-style)
  (emit (make <text-run>
              style: 'comment-char-style
              content: "]")))

(define-method bridge-inline-stuff ((self <FILENAME>) emit style)
  (bridge-inline-stuff (content self) 
                       emit
                       'prototype-char-style))

(define-method bridge-inline-stuff ((self <KEYCOMBO>) emit style)
  (bridge-inline-stuff (content self) 
                       emit
                       'prototype-char-style))

(define-method bridge-inline-stuff ((self <KEYCAP>) emit style)
  (bridge-inline-stuff (content self) 
                       emit
                       'prototype-char-style))

(define-method bridge-inline-stuff ((self <FUNCTION>) emit style)
  (bridge-inline-stuff (content self) 
                       emit
                       'prototype-char-style))

(define-method bridge-inline-stuff ((self <SYMBOL>) emit style)
  (bridge-inline-stuff (content self) 
                       emit
                       'prototype-char-style))

(define-method bridge-inline-stuff ((self <ENVAR>) emit style)
  (bridge-inline-stuff (content self) 
                       emit
                       'prototype-char-style))

(define-method bridge-inline-stuff ((self <LITERAL>) emit style)
  (bridge-inline-stuff (content self) 
                       emit
                       'literal-char-style))

(define-method bridge-inline-stuff ((self <CITATION>) emit style)
  (bridge-inline-stuff (content self)
                       emit
                       'emphasis-char-style))

(define-method bridge-inline-stuff ((self <QUOTE>) emit style)
  (emit (make <text-run>
              style: style
              content: "\252"))
  (bridge-inline-stuff (content self) emit style)
  (emit (make <text-run>
              style: style
              content: "\272")))

(define-method bridge-inline-stuff ((self <sgml-pi>) emit style)
  (processing-instruction (content self)))

(define-method bridge-display-stuff ((self <sgml-pi>) emit)
  (processing-instruction (content self)))

(define (processing-instruction pi)
  (format #t "PI: ~s\n" pi)
  (let ((x (string-search pi #\space)))
    (cond
     ((and x (string=? (substring pi 0 x) "watermark:"))
      (set! *document-watermark* (substring pi (+ x 1))))
     ((string=? pi "debugon")
      (set! *debug-pi* #t))
     ((string=? pi "debugoff")
      (set! *debug-pi* #f)))))


(define-method bridge-inline-stuff ((self <REPLACEABLE>) emit style)
  (bridge-inline-stuff (content self)
                       emit
                       'argument-char-style))

(define-method bridge-inline-stuff ((self <TOKEN>) emit style)
  (emit (make <text-run> style: style content: "\140"))    ; quoteleft
  (bridge-inline-stuff (content self)
                       emit
                       'prototype-char-style)
  (emit (make <text-run> style: style content: "\047")))   ; quoteright


(define-method bridge-inline-stuff ((self <PHRASE>) emit style)
  (revisionflag-check
   self
   (let ((role (string->symbol 
                (string-downcase (or (get-attribute-value self "role") 
                                     "unknown")))))
     (case role
       ((unknown)
        ;; the <PHRASE> structure may be here for some other reason,
        ;; for example with a REVISIONFLAG attribute
        (bridge-inline-stuff (content self) emit style))
       ((procedurename)
        (bridge-inline-stuff (content self)
                             emit
                             'prototype-char-style))
       ((var tokenclass)
        (bridge-inline-stuff (content self)
                             emit
                             'prototype-char-style))
       ((eqn)
        (bridge-inline-stuff (content self)
                             emit
                             'emphasis-char-style))
       ((token)
        (emit (make <text-run>
                    style: style
                    content: "\140"))    ; quoteleft
        (bridge-inline-stuff (content self)
                             emit
                             'prototype-char-style)
        (emit (make <text-run>
                    style: style
                    content: "\047")))   ; quoteright
       (else
        (bridge-inline-stuff (content self)
                             emit
                             'argument-char-style))))))
   

(define-method bridge-inline-stuff ((self <USERINPUT>) emit style)
  (bridge-inline-stuff (content self) 
                       emit
                       'literal-char-style))

(define-method bridge-inline-stuff ((self <COMPUTEROUTPUT>) emit style)
  (bridge-inline-stuff (content self) emit 'literal-char-style))

(define-method bridge-inline-stuff ((self <COMMAND>) emit style)
  (bridge-inline-stuff (content self) emit 'literal-char-style))

(define-method bridge-inline-stuff ((self <OPTION>) emit style)
  (bridge-inline-stuff (content self) emit 'prototype-char-style))

(define-method bridge-inline-stuff ((self <SCREEN>) emit style)
  (bridge-inline-stuff (content self) 
                       emit
                       'literal-char-style))

(define-method bridge-inline-stuff ((self <PROMPT>) emit style)
  (bridge-inline-stuff (content self) 
                       emit
                       'literal-char-style))

(define-method bridge-inline-stuff ((self <ULINK>) emit style)
  (emit (make <text-run>
              style: style
              content: "["))
  (bridge-inline-stuff (content self)
                       emit
                       style)
  (emit (make <text-run>
              style: style
              content: "]")))

;;;

(define (tbridge)
  (layout-flow-in-page-sequence 
   (make <flow>
         content: (list (bridge-paragraph (force *test-book*)
                                          'body-para-style)))
   *chapter-page-sequence*))

(define (tbridge2 #optional sect)
  (force *test-book*)
  (set-property! (element tt) 
                 'anchor-scope
                 (make <anchor-scope>))
  (let ((chap (toc-ref tt (or sect '(1 15)))))
    (layout-flow-in-page-sequence
     (bridge-chapter (element chap))
     *chapter-page-sequence*
     chap)))

#|
(define (reflow)
  (reset-pages)
  (tbridge2 '(1 14))
  (tgsp 0))
|#

(define (page->chapter p)
  (page-owner p))

(define (top-level-generate-chapter dd)
  (let ((tt (build-toc-structure dd))
        (anchors (make <anchor-scope>)))
    ;;
    (init-figure-names dd)
    (init-table-names dd)
    (init-example-names dd)
    ;;
    (set-property! (element tt) 'anchor-scope anchors)
    (init-version (xpath* dd "chapter" "docinfo"))
    ;;
    (bridge-chapters (list (xpath dd "chapter")))
    ;; make sure blank pages are inserted, so the TOC is formatted
    ;; with the right page numbers (XXX i.e., since we can't
    ;; reflow yet...)
    tt))

;;;
;;;  We want to support stuff like:
;;;    <chapter>
;;;      <docinfo>
;;;        <pubsnumber>A00</pubsnumber>
;;;        <revhistory>
;;;          <revision><revnumber>1.0</revnumber></revision>
;;;        </revhistory>
;;;      </docinfo>
;;;    ...

(define (init-version info-nodelist)
  (if (pair? info-nodelist)
      (set-property! *the-document*
                     'version
                     (info->version (car info-nodelist)))))

(define (info->version n)
  (let ((pubs (xpath* n "pubsnumber"))
        (revh (if (pair? (xpath* n "revhistory"))
                  (xpath* n "revhistory" "revision")
                  '())))
    (cond
     ((and (pair? pubs)
           (pair? revh))
      (format #f "~a Rev. ~a"
              (xpath (car pubs) 'cdata)
              (xpath (car revh) "revnumber" 'cdata)))
     ((pair? pubs)
      (xpath (car pubs) 'cdata))
     ((pair? revh)
      (format #f "Rev. ~a" (xpath (car revh) "revnumber" 'cdata)))
     (else
      ""))))

(define (generate-front-matter doc)
  ;;; handle: title dedication toc preface
  ;;; (although the toc is filled in later)
  (generate-title-pages doc))

#|
          
          (xpath (car (xpath* n
                              "revhistory"
                              "revision"))
                 "revnumber"
                 'cdata)))
|#

(define-method process-pi ((self <sgml-node>))
  (values))

(define-method process-pi ((self <sgml-pi>))
  (processing-instruction (content self)))

(define-method process-pi ((self <sgml-element>))
  (for-each
   (lambda (x)
     (if (instance? x <sgml-pi>)
         (processing-instruction (content x))))
   (content self)))

(define (top-level-generate-book dd)
  (let ((tt (build-toc-structure dd))
        (anchors (make <anchor-scope>)))
    ;;
    (set-property! (element tt) 'anchor-scope anchors)
    ;;
    (init-version (xpath* dd "book" "bookinfo"))
    ;;
    (init-figure-names dd)
    (init-table-names dd)
    (init-example-names dd)
    (init-index tt)
    ;;
    ;;
    (progress " Front matter...")
    (generate-front-matter dd)
    (progress "\n")
    ;;
    (process-pi (xpath dd "book"))
    (if (xpath? dd "book" "part")
        (for-each bridge-part (xpath* dd "book" "part"))
        (bridge-chapters (collect-chapter-elements (xpath dd "book"))))
    ;; make sure blank pages are inserted, so the TOC is formatted
    ;; with the right page numbers (XXX i.e., since we can't
    ;; reflow yet...)
    ;(progress " Adding blank pages...")
    ;(reorder-pages! '(preface body))
    ;;
    (if (needs-index? tt)
        (begin
          (add-index-toc-entry tt)
          (progress " Generating Index...")
          (generate-index-pages tt)
          (progress "\n")))
    ;;
    (reorder-pages! '(preface body index))
    ;;
    (progress "\n Generating TOC...")
    (generate-toc-pages tt)
    (progress "\n")
    ;;
    (reorder-pages! '(preface body index))
    tt))

(define (files->ps . files)
  (reset-pages)
  (progress " Reading SGML...\n")
  (let* ((dd (apply read-sgml-files files)))
    (set-property! *the-document*
                   'date 
                   (with-module syscalls
                     (time->string (time) "%Y-%m-%d %H:%M")))
    (let ((top-type (object-class (car (content dd)))))
      (progress " top level ~s\n" (name top-type))
      (let ((tt (cond
                 ((eq? top-type <BOOK>)
                  (top-level-generate-book dd))
                 ((eq? top-type <CHAPTER>)
                  (top-level-generate-chapter dd))
                 (else
                  (error "Don't know how to top-level generate a ~a" 
                         top-type)))))
        (let ((psf (change-extension (last files) "ps"))
              (psft (change-extension (last files) "ps#")))
          (progress " Generating PostScript: ~a" psf)
          (tprp psft)
          ;;
          (cond
           ((getenv "DOCUMENT_ID")
            => (lambda (w)
                 (progress "\nCopying to /tmp/doc-~a.ps..." w)
                 (with-module
                     rs.sys.threads.manager
                   (check-exit-status
                    (run "/bin/cp" "-p" psft (~ "/tmp/doc-~a.ps" w)))
                   (progress "compressing...")
                   (check-exit-status
                    (run "/bin/gzip" (~ "/tmp/doc-~a.ps" w)))))))
          ;;
          (rename psft psf))
        (progress "\ndone!\n")
        (list dd tt)))))

(define (change-extension old-path new-extn)
  (pathname->os-path
   (extension-related-path
    (string->file old-path)
    new-extn)))

(define (progress fmt . args)
  (apply format (current-error-port) fmt args)
  (flush-output-port (current-error-port)))

