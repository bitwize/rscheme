
;;;
;;;   A place to accumulate index listing entries
;;;

(define-class <index-listing> (<object>)
  anchor                ; the anchor for the index listing itself
  (entries init-value: '()))

(define-class <index-listing-entry> (<object>)
  (properties type: <vector> init-value: '#())
  (term type: <list>)
  (element type: <sgml-element>)
  (locations type: <list>))

(define (index-entry-section (self <index-listing-entry>))
  (string (char-upcase (string-ref (car (term self)) 0))))

(define (index-entry<? (a <index-listing-entry>) (b <index-listing-entry>))
  (<= (index-entry-cmp a b) 0))

(define (index-entry-cmp (a <index-listing-entry>) (b <index-listing-entry>))
  (let loop ((at (term a))
             (bt (term b)))
    (cond
     ((null? at)
      (if (null? bt)
          0
          1))
     ((null? bt)
      -1)
     (else
      (let ((d (string-ci-compare (car at) (car bt))))
        (if (= d 0)
            (loop (cdr at) (cdr bt))
            d))))))

(define (add-index-entry! (self <index-listing>) (item <index-listing-entry>))
  (set-entries! self (cons item (entries self))))

(define (init-index toc-tree)
  (set-property! (element toc-tree) 
                 'index
                 (make <index-listing>
                       anchor: (make-anchor (element toc-tree)
                                            <BOOK>
                                            (list 'section 'index)))))

(define (find-index node)
  (or (get-property node 'index #f)
      (find-index (parent node))))

;;;

(define (test)
  (make <index-listing>
        entries: (list
                  (make <index-listing-entry>
                        terms: '("Transformation")
                        secondary-occurences: '((201 - 226))
                        notes: '(see-also: ("Matrix Algebra")))

                  (make <index-listing-entry>
                        terms: '("Transformation" "affine")
                        secondary-occurences: '(207 598 1106))

                  (make <index-listing-entry>
                        terms: '("Transformation" "in object modeling")
                        secondary-occurences: '(291 (304 - 308) 315 336))

                  (make <index-listing-entry>
                        terms: '("Transformation" "graphics")
                        notes: '(see: ("Scan conversion" 
                                       "thick primitives"))))))

;;;


(define (gen-index-flow ix)
  (let ((root (organize-index (entries ix)))
        (first? #t))
    (format #t "============= INDEX =============\n")
    (print-index-org root)
    #|
    (for-each (lambda ((entry <index-listing-entry>))
                (format #t "<~a> *** term ~s at ~s\n" 
                        (index-entry-section entry)
                        (term entry)
                        (element entry)))
            (entries ix))
    |#
    (format #t "============= END OF INDEX =============\n")
    ;;
    (let ((g (lambda (return)
               ;;
               (define (rec jump tpair depth)
                 (case depth
                   ((0)
                    (return (make-index-section-title 
                             jump 
                             (if first?
                                 (lambda 'gen-anchor1 ()
                                   (list (anchor ix)))
                                 (lambda 'gen-anchor0 ()
                                   '()))))
                    (set! first? #f))
                   ((1)
                    (return
                     (make <para>
                           style: 'index-level1-para
                           content: (append
                                     (list
                                      (make <text-run>
                                            style: 'index-content-char-style
                                            content: jump))
                                     (index-referents (cdr tpair))))))
                   ((2)
                    (return
                     (make <para>
                           style: 'index-level2-para
                           content: (append
                                     (list
                                      (make <text-run>
                                            style: 'index-content-char-style
                                            content: jump))
                                     (index-referents (cdr tpair))))))
                   (else
                    (error "indexing too deep")))
                 (for-each
                  (lambda (sub)
                    (rec sub (table-lookup (car tpair) sub) (+ depth 1)))
                  (sort (key-sequence (car tpair)) string-ci<?)))
               ;;
               (for-each
                (lambda (sub)
                  (rec sub (table-lookup (car root) sub) 0))
                (sort (key-sequence (car root)) string-ci<?)))))
      ;;
      (make <flow>
            flow-tag: 'index
            content: (iteration->list g)))))


(define (make-index-section-title sectn gen-anchor)
  (make <para>
        style: 'index-section-para
        content: (cons
                  (make <text-run>
                        style: 'index-section-char-style
                        content: sectn)
                  (gen-anchor))))

(define (gen-index-test-flow)
  (eval-structure
   `(flow
     (flow-tag index)
     (content
      (list
       ;;
       (para
        (style index-section-para)
        (content
         (list
          (text-run
           (style index-section-char-style)
           (content "A")))))
       ;;
       (para
        (style index-level1-para)
        (content
         (list
          (text-run
           (style index-content-char-style)
           (content "Alpha, "))
          (text-run
           (style index-primary-ref-char-style)
           (content "302"))
          (text-run
           (style index-content-char-style)
           (content ", 175-178, 404")))))
       ;;
       (para
        (style index-level1-para)
        (content
         (list
          (text-run
           (style literal-char-style)
           (content "and"))
          (text-run
           (style index-content-char-style)
           (content ", "))
          (text-run
           (style index-primary-ref-char-style)
           (content "303")))))
       (para
        (style index-level2-para)
        (content
         (list
          (text-run
           (style index-content-char-style)
           (content "as operator in POSIX regular expressions, "))
          (text-run
           (style index-primary-ref-char-style)
           (content "707")))))
       ;;
       (para
        (style index-level1-para)
        (content
         (list
          (text-run
           (style index-content-char-style)
           (content "Antimony, "))
          (text-run
           (style index-primary-ref-char-style)
           (content "140"))
          (text-run
           (style index-content-char-style)
           (content ", "))
          (text-run
           (style index-seealso-char-style)
           (content "see also"))
          (text-run
           (style index-content-char-style)
           (content " Elements, Periodic Table "))
          (text-run
           (style index-seealso-char-style)
           (content "and"))
          (text-run
           (style index-content-char-style)
           (content " Alchemy in the 20th Century")))))
       ;;
       (para
        (style index-level2-para)
        (content
         (list
          (text-run
           (style index-content-char-style)
           (content "ore, "))
          (text-run
           (style index-primary-ref-char-style)
           (content "807")))))
       (para
        (style index-level2-para)
        (content
         (list
          (text-run
           (style index-content-char-style)
           (content "preparation, "))
          (text-run
           (style index-primary-ref-char-style)
           (content "1043")))))
       ;;
       (para
        (style index-section-para)
        (content
         (list
          (text-run
           (style index-section-char-style)
           (content "B")))))
       ;;
       (para
        (style index-level1-para)
        (content
         (list
          (text-run
           (style literal-char-style)
           (content "bitwise-and"))
          (text-run
           (style index-content-char-style)
           (content ", "))
          (text-run
           (style index-primary-ref-char-style)
           (content "310")))))
       (para
        (style index-level1-para)
        (content
         (list
          (text-run
           (style literal-char-style)
           (content "bitwise-or"))
          (text-run
           (style index-content-char-style)
           (content ", "))
          (text-run
           (style index-primary-ref-char-style)
           (content "310")))))

       (para
        (style index-section-para)
        (content
         (list
          (text-run
           (style index-section-char-style)
           (content "1")))))
       ,@(map (lambda (k)
                `(para
                  (style index-level1-para)
                  (content
                   (list
                    (text-run
                     (style literal-char-style)
                     (content ,(number->string (+ 100 k))))
                    (text-run
                     (style index-content-char-style)
                     (content ", "))
                    (text-run
                     (style index-primary-ref-char-style)
                     (content "310"))))))
              (range 36))
       (para
        (style index-level1-para)
        (content
         (list
          (text-run
           (style index-content-char-style)
           (content "1001")))))
       (para
        (style index-level2-para)
        (content
         (list
          (text-run
           (style index-content-char-style)
           (content "arabian nights, "))
          (text-run
           (style index-primary-ref-char-style)
           (content "473")))))
       (para
        (style index-level2-para)
        (content
         (list
          (text-run
           (style index-content-char-style)
           (content "engineering jokes, "))
          (text-run
           (style index-primary-ref-char-style)
           (content "439")))))
       (para
        (style index-level2-para)
        (content
         (list
          (text-run
           (style index-content-char-style)
           (content "things to do with a dead cat, "))
          (text-run
           (style index-primary-ref-char-style)
           (content "437")))))
       (para
        (style index-level2-para)
        (content
         (list
          (text-run
           (style index-content-char-style)
           (content "xylophones, "))
          (text-run
           (style index-seealso-char-style)
           (content "see"))
          (text-run
           (style index-content-char-style)
           (content " Xylophone, catalog")))))
      )))))


(define (eval-structure p)
  (with-module
      objsys
    (if (pair? p)
        (case (car p)
          ((list)
           (map eval-structure (cdr p)))
          ((flow para text-run)
           (apply
            make-instance 
            (case (car p)
              ((flow) <flow>)
              ((para) <para>)
              ((text-run) <text-run>)
              (else (error "Unknown tag: ~s" (car p))))
            (apply append
                   (map (lambda (s)
                          (list
                           (symbol->keyword (car s))
                           (eval-structure (cadr s))))
                        (cdr p))))))
        p)))
        
                              
               


;;;

(define (needs-index? toc-tree)
  (let ((ix (get-property (element toc-tree) 'index)))
    (pair? (entries ix))))
  
(define (add-index-toc-entry toc-tree)
  (let ((ix (get-property (element toc-tree) 'index)))
    (add-toc-node! toc-tree
                   (make <special-toc-node>
                         label: '(index)
                         type: 'index
                         element: #f
                         parent-node: #f
                         child-nodes: '()
                         title: "Index"
                         anchor: (anchor ix)))))

(define (generate-index-pages toc-tree)
  (let ((ix (get-property (element toc-tree) 'index)))
    (layout-flow-in-page-sequence
     (gen-index-flow ix)
     (make <simple-page-sequence>
           initial-page-styles: '(index-first-page)
           repeat-page-styles: '(index-right-page index-left-page)
           blank-page-style: 'intentionally-blank-page)
     #f)))
     

(define (process-index-endofrange (self <INDEXTERM>) startref gen-anchor)
  ;; this is the one case where we don't create a new
  ;; index-listing-entry
  (let* ((start-elem (find-element-by-id self startref))
         (e (get-property start-elem 'index-listing-entry)))
    ;;
    (if (or (not e)
            (not (= (length (locations e)) 1))
            (cadar (locations e)))
        (error "~s: StartRef of ~s is malformed" self start-elem))
    ;;
    (set-car! (cdar (locations e)) (gen-anchor))
    e))

(define (process-index-term (self <INDEXTERM>) gen-anchor)
  (let ((startref (get-attribute-value self "startref"))
        (sig (get-attribute-value self "significance")))
    (if startref ; [Case 2] EndOfRange
        (process-index-endofrange self startref gen-anchor)
        (let* ((t1 (xpath* self "primary"))
               (t2 (xpath* self "secondary"))
               (t3 (xpath* self "tertiary"))
               (e (make <index-listing-entry>
                        term: (map collect-text
                                   (if (pair? t3)
                                       (list (car t1) (car t2) (car t3))
                                       (if (pair? t2)
                                           (list (car t1) (car t2))
                                           (list (car t1)))))
                        locations: '()
                        element: self)))
          ;;
          (define (loc lst)
            (set-locations! e lst))
          ;;
          (if (string-ci=? sig "preferred")
              (set-property! e 'preferred #t))
          (set-property! self 'index-listing-entry e)
          ;;
          (let ((zone (get-attribute-value self "zone")))
            (if zone
                ;; [Case 3] the indexed content is elsewhere...
                (let ((targets (find-elements-by-id self zone)))
                  (loc (map (lambda (t)
                              `((element start ,t)
                                (element end ,t)))
                            targets)))
                (let ((class (get-attribute-value self "class")))
                  (cond
                   ((or (not class)
                        (string-ci=? class "singular"))
                    ;; [Case 1] simple reference
                    (loc (list (gen-anchor))))
                   ((string-ci=? class "endofrange")
                    (error "~s: EndOfRange without StartRef" self))
                   ((string-ci=? class "startofrange")
                    (loc (list (list (gen-anchor) #f))))
                   (else
                    (error "~s: Bad attribute Class=~s" class))))))
          ;;
          ;;
          (let ((ref (xpath* self "see")))
            (if (pair? ref)
                (set-property! self 'see (car ref))))
          ;;
          (let ((ref (xpath* self "seealso")))
            (if (pair? ref)
                (set-property! self 'seealso ref)))
          ;;
          (add-index-entry! (find-index self) e)
          e))))
           
                        

#|


Case 1, marking a particular point in the text...

   A foo
   <IndexTerm [class="Singular"]><primary>Foo</primary></IndexTerm>
   is similar to a bar, but different.
   ...
   Some foo<IndexTerm><primary>Foo</primary></IndexTerm> are not like bar.


Case 2, marking the start and end of the range explicitly...

   <IndexTerm id="x66" class="StartOfRange"><primary>Foo</primary></IndexTerm>
   blah
   blah
   blah...
   <IndexTerm [class="EndOfRange"] startref="x66"/>

   
Case 3, referencing a (set of) elements by id...

   <IndexTerm zone="p12 p75"><primary>Foo</primary></IndexTerm>
   ...
   <para id="p12">A foo is similar to a bar, but different.
   ...
   <para id="p75">Some foo are not like bar.


|#


(define (organize-index entries)
  ;;
  (define (insert tpair terms entry)
    (if (pair? terms)
        (let ((sub (table-lookup (car tpair) (car terms))))
          (if sub
              (insert sub (cdr terms) entry)
              (let ((sub (cons (make-string-ci-table) '())))
                (table-insert! (car tpair) (car terms) sub)
                (insert sub (cdr terms) entry))))
        (set-cdr! tpair (cons entry (cdr tpair)))))
  ;;
  (let ((root (cons (make-string-ci-table) '())))
    (for-each
     (lambda (e)
       (insert root (cons (index-entry-section e) (term e)) e))
     entries)
    ;;
    root))

;;

(define (print-index-org root)
  (define (rec jump tpair depth)
    (format #t "~a~a" 
            (make-string (* depth 4) #\space)
            jump)
    (if (pair? (cdr tpair))
        (format #t " [#~d]" (length (cdr tpair))))
    (newline)
    (for-each
     (lambda (sub)
       (rec sub (table-lookup (car tpair) sub) (+ depth 1)))
     (sort (key-sequence (car tpair)) string-ci<?)))
  ;;
  (for-each
   (lambda (sub)
     (rec sub (table-lookup (car root) sub) 0))
   (sort (key-sequence (car root)) string-ci<?)))

;;;
;;;  given a list of index entries,
;;;  build a list of <text-run>'s that contain the
;;;  referenced page numbers
;;;

(define (index-referents lst)
  (apply
   append
   (map
    (lambda ((e <index-listing-entry>))
      (let ((cf (if (get-property e 'preferred #f)
                    'index-primary-ref-char-style
                    'index-content-char-style))
            (ll (locations e)))
        (format #t "index-referents...\n")
        (print e)
        ;;
        ;;
        (apply append
               (map
                (lambda (loc)
                  (cons
                   (make <text-run>
                         style: cf
                         content: ", ")
                   (index-referent1 loc cf)))
                ll))))
    lst)))

(define-method index-referent1 ((self <anchor>) cf)
  (list
   (make-anchor-ref-page self cf)))

(define-method index-referent1 ((self <pair>) cf)
  (list
   (make-anchor-ref-page (car self) cf)
   (make <text-run>
         style: cf
         content: "-")
   (make-anchor-ref-page (cadr self) cf)))

