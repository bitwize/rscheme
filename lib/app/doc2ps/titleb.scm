(define-style 'revision-char-style
  (character-style basis: 'title3-char-style
                   font: (override-style 'title3-font
                                         size: 12)))

(define-style 'book-title-char-style
  (character-style basis: 'body-char-style
                   font: (override-style 'body-font size: 36)))

(define-style 'book-subtitle-char-style
  (character-style basis: 'body-char-style
                   font: (override-style 'body-font size: 24)))

(define-style 'book-author-char-style
  (character-style basis: 'body-char-style
                   font: (override-style 'body-font size: 14)))

(define-style 'book-title-para
  (paragraph-style basis: '$default-para-style
                   default-char-style: 'book-title-char-style
                   line-spacing: 'using-font-size
                   tab-stops: '((position: (l 3.25in) align: center))))

(define-style 'book-subtitle-para
  (paragraph-style basis: 'book-title-para
                   default-char-style: 'book-subtitle-char-style))

(define-style 'book-author-para
  (paragraph-style basis: 'book-title-para
                   default-char-style: 'book-author-char-style))

(define-style 'revision-para
  (paragraph-style basis: 'book-title-para
                   default-char-style: 'revision-char-style
                   left-margin-first: 6
                   right-margin: 6
                   line-spacing: 14
                   right-margin-last: 6
                   tab-stops: '((position: (l 3in) align: center)
                                (position: (r 6) align: right))))

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
         blank-page-style: 'intentionally-blank-page)
   #f))

(define (generate-title-recto-page doc emit)
  ;;
  (emit (make-hrule 0.5in 0.4in
                    linewidth: 2
                    left: 2.5in
                    right: 2.5in))
  ;;
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
  ;;
  (emit (make-hrule 1in 0.5in
                    linewidth: 0.5
                    left: 1.2in
                    right: 1.2in))
  (emit (make <para>
              style: 'book-subtitle-para
              content: (if (pair? (xpath* doc "book" "bookinfo" "subtitle"))
                           (list
                            (make <text-run>
                                  style: 'book-subtitle-char-style
                                  content: "\t")
                            (make <text-run>
                                  style: 'book-subtitle-char-style
                                  content: (xpath doc 
                                                  "book"
                                                  "bookinfo"
                                                  "subtitle"
                                                  'cdata)))
                           '())))
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
  (emit (make <vbreak> section: 'frame))
  ;;
  (let ((revs (xpath* doc "book" "bookinfo" "revhistory" "revision")))
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
                                           'cdata)))))))

(define (generate-title-pages doc)
  (layout-flow-in-page-sequence
   (make <flow>
         flow-tag: 't
         content: (iteration->list
                   (lambda (emit)
                     (generate-title-recto-page doc emit))))
   (make <simple-page-sequence>
         initial-page-styles: '(title-front-page)
         repeat-page-styles: '()
         blank-page-style: 'intentionally-blank-page)
   #f))
