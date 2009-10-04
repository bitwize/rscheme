
(define (gen-title-test-flow-layout)
  (make <flow>
        flow-tag: 't
        content: (list
                  (make-hrule 0.5in 0.4in 
                              linewidth: 2
                              left: 2.5in
                              right: 2.5in)
                  (make <para>
                        style: 'book-title-para
                        content: (list
                                  (make <text-run>
                                        style: 'book-title-char-style
                                        content: "\tCustom Shield Writer")))
                  (make-hrule 1in 0.5in
                              linewidth: 0.5
                              left: 1.2in
                              right: 1.2in)
                  (make <para>
                        style: 'book-subtitle-para
                        content: (list
                                  (make <text-run>
                                        style: 'book-subtitle-char-style
                                        content: "\tHigh Level Design")))
                  (make-hrule 1in 0.5in
                              linewidth: 2
                              left: 2.5in
                              right: 2.5in)
                  (make <para>
                        style: 'book-author-para
                        content: (list
                                  (make <text-run>
                                        style: 'book-author-char-style
                                        content: "\tDonovan Kolbly")))
                  (make <para>
                        style: 'book-author-para
                        content: (list
                                  (make <text-run>
                                        style: 'book-author-char-style
                                        content: "\tJay Rolette")))
                  (make <vbreak>
                        section: 'frame)
                  (make <para>
                        style: 'revision-para
                        content: (list
                                  (make <text-run>
                                        style: 'revision-char-style
                                        content: "TECHD-0000000666\tversion 1.3\t2003-03-30"))))))

                                  

(define (testt)
  (reset-pages)
  (layout-flow-in-page-sequence
   (gen-title-test-flow-layout)
   (make <simple-page-sequence>
         initial-page-styles: '(title-front-page)
         repeat-page-styles: '()
         blank-page-style: #f)
   #f))

(define (generate-title-recto-page doc emit)
  ;;
  (emit (make-hrule 0.5in 0.4in
                    linewidth: 2
                    left: 2.5in
                    right: 2.5in))
  ;;
  (let ((p (make-paragraph-from-inline
            (xpath doc "book" "bookinfo" "title")
            'book-title-para
            bridge-title-stuff)))
    (set-content! p (cons (make <text-run>
                                style: 'book-title-char-style
                                content: "\t")
                          (content p)))
    (emit p))
  #|
  (emit (make <para>
              style: 'book-title-para
              content: (list
                        (make <text-run>
                              style: 'book-title-char-style
                              content: "\t")
                        (make <text-run>
                              style: 'book-title-char-style
                              content: (xpath doc 
                                              "book"
                                              "bookinfo"
                                              "title"
                                              'cdata)))))
  |#
  ;;
  (if (pair? (xpath* doc "book" "bookinfo" "subtitle"))
      (begin
        (emit (make-hrule 1in 0.5in
                          linewidth: 0.5
                          left: 1.2in
                          right: 1.2in))
        (emit (make <para>
                    style: 'book-subtitle-para
                    content: (list
                              (make <text-run>
                                    style: 'book-subtitle-char-style
                                    content: "\t")
                              (make <text-run>
                                    style: 'book-subtitle-char-style
                                    content: (xpath doc 
                                                    "book"
                                                    "bookinfo"
                                                    "subtitle"
                                                    'cdata)))))))
  ;;
  (emit (make-hrule 1in 0.5in
                    linewidth: 2
                    left: 2.5in
                    right: 2.5in))
  ;;
  (for-each
   (lambda ((author <AUTHOR>))
     (emit (make <para>
                 style: 'book-author-para
                 content: (list
                           (make <text-run>
                                 style: 'book-author-char-style
                                 content: "\t")
                           (make <text-run>
                                 style: 'book-author-char-style
                                 content: (xpath author "firstname" 'cdata))
                           (make <text-run>
                                 style: 'book-author-char-style
                                 content: " ")
                           (make <text-run>
                                 style: 'book-author-char-style
                                 content: (xpath author "surname" 'cdata))))))
   (xpath* doc
           "book"
           "bookinfo"
           "authorgroup"
           "author"))
  ;;
  ;;
  (if (pair? (xpath* doc "book" "bookinfo" "revhistory"))
      (let ((revs (xpath* doc "book" "bookinfo" "revhistory" "revision")))
        (emit (make <vbreak> section: 'frame))
        (emit 
         (make <para>
               style: 'revision-para
               content: (list
                         (make <text-run>
                               style: 'revision-char-style
                               content: (xpath doc 
                                               "book"
                                               "bookinfo"
                                               "pubsnumber"
                                               'cdata))
                         (make <text-run>
                               style: 'revision-char-style
                               content: "\tRev. ")
                         (make <text-run>
                               style: 'revision-char-style
                               content: (xpath (car revs)
                                               "revnumber"
                                               'cdata))
                         (make <text-run>
                               style: 'revision-char-style
                               content: "\t")
                         (make <text-run>
                               style: 'revision-char-style
                               content: (xpath (car revs)
                                               "date"
                                               'cdata))))))))


(define (generate-title-verso-page doc emit)
  ;;
  (emit (make <para>
              style: 'bookinfo-para
              content: (list
                        (make <text-run>
                              style: 'bold-char-style
                              content: (xpath doc 
                                              "book"
                                              "bookinfo"
                                              "title"
                                              'cdata)))))
  (if (pair? (xpath* doc "book" "bookinfo" "subtitle"))
      (emit (make <para>
                  style: 'bookinfo-para
                  content: (list
                            (make <text-run>
                                  style: 'emphasis-char-style
                                  content: (xpath doc 
                                                  "book"
                                                  "bookinfo"
                                                  "subtitle"
                                                  'cdata))))))
  ;;
  (emit (make-hrule 0.25in 0.125in left: 0 right: 4in))
  ;;
  (let ((authors (xpath* doc
                         "book"
                         "bookinfo"
                         "authorgroup"
                         "author")))
    (for-each
     (lambda (k (author <AUTHOR>))
       (emit
        (make <para>
              style: (override-style 
                      'body-para-style
                      tab-stops: '((position: (l 0.25in) align: left)))
              content: (list
                        (make <text-run>
                              style: 'emphasis-char-style
                              content: (if (= k 0)
                                           "by\t"
                                           "\t"))
                        (make <text-run>
                              style: 'body-char-style
                              content: (xpath author "firstname" 'cdata))
                        (make <text-run>
                              style: 'body-char-style
                              content: " ")
                        (make <text-run>
                              style: 'body-char-style
                              content: (xpath author "surname" 'cdata))))))
     (range (length authors))
     authors))
  ;;
  (emit (make-vspace 0.25in))
  (emit (make <para>
              style: 'body-para-style
              content: (list
                        (make <text-run>
                              style: 'body-char-style
                              content: "Publication Number: ")
                        (make <text-run>
                              style: 'body-char-style
                              content: (xpath doc 
                                              "book"
                                              "bookinfo"
                                              "pubsnumber"
                                              'cdata)))))
  ;;
  (if (pair? (xpath* doc "book" "bookinfo" "legalnotice"))
      (begin
        (emit (make-hrule 0.25in 0.125in left: 0 right: 4in))
        (bridge-display-stuff
         (content (xpath doc "book" "bookinfo" "legalnotice"))
         emit)
        (emit (make-vspace 0.25in))))
  ;;
  (let ((revs (xpath* doc "book" "bookinfo" "revhistory" "revision")))
    (if (pair? revs)
        (let ((ft (make <flow-table>
                        align: 'left
                        column-widths: '(0.5in 0.75in 1in 3in)
                        row-heights: (map (lambda (r) 12)
                                          (range (+ 1 (length revs))))
                        num-header-rows: 1
                        content: '())))
          (set-content! ft
                        (append
                         (list
                          (make-table-row ft
                                          '("\tRev"
                                            "\tDate"
                                            "\tAuthor(s)"
                                            "\tRemarks")
                                          'revtable-header-para))
                         (map
                          (lambda (r)
                            (make-table-row
                             ft
                             (list (xpath r "revnumber" 'cdata)
                                   (xpath r "date" 'cdata)
                                   (string-join
                                    ", "
                                    (map (lambda (a)
                                           (xpath a 'cdata))
                                         (xpath* r "authorinitials")))
                                   (xpath r "revremark" 'cdata))
                             'revtable-entry-para))
                          revs)))
          (emit ft)))))

(define (make-table-row tbl content styles)
  (map
   (lambda (cell style)
     (make <table-cell>
           owner: tbl
           content: (make <flow>
                          content: (list
                                    (make-paragraph-from-inline
                                     cell
                                     style
                                     bridge-inline-stuff)))))
   content
   (if (symbol? styles)
       (map (lambda (k) styles) content)
       styles)))
                                          


(define (generate-title-pages doc)
  (layout-flow-in-page-sequence
   (make <flow>
         flow-tag: 't
         content: (iteration->list
                   (lambda (emit)
                     (generate-title-recto-page doc emit)
                     (emit (make <vbreak>
                                 section: 'page))
                     (generate-title-verso-page doc emit))))
   (make <simple-page-sequence>
         initial-page-styles: '(title-front-page title-back-page)
         repeat-page-styles: '()
         blank-page-style: 'intentionally-blank-page-preface)
   #f))
