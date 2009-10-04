
(define-style 'revtable-header-para
  (paragraph-style basis: 'body-para-style
                   tab-stops: '((position: (c 0) align: center))
                   default-char-style: 'bold-char-style))

(define-style 'revtable-entry-para
  (paragraph-style basis: 'body-para-style))

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
  (emit (make <para>
              style: 'bookinfo-para
              content: (list
                        (make <text-run>
                              style: 'bold-char-style
                              content: (xpath doc 
                                              "book"
                                              "bookinfo"
                                              "subtitle"
                                              'cdata)))))
  ;;
  (emit (make-hrule 1in 0.5in left: 0 right: 4in))
  ;;
  (for-each
   (lambda ((author <AUTHOR>))
     (emit (make <para>
                 style: 'body-para-style
                 content: (list
                           (make <text-run>
                                 style: 'body-char-style
                                 content: (xpath author "firstname" 'cdata))
                           (make <text-run>
                                 style: 'body-char-style
                                 content: " ")
                           (make <text-run>
                                 style: 'body-char-style
                                 content: (xpath author "surname" 'cdata))))))
   (xpath* doc
           "book"
           "bookinfo"
           "authorgroup"
           "author"))
  ;;
  (emit (make-vspace 1in))
  ;;
  (if (pair? (xpath* doc "book" "bookinfo" "revhistory"))
      (let* ((revs (xpath* doc "book" "bookinfo" "revhistory" "revision"))
             (ft (make <flow-table>
                       column-widths: '(1in 1in 3in)
                       row-heights: (map (lambda (r) 14)
                                         (+ 1 (range (length revs))))
                       num-header-rows: 1
                       content: '())))
        (set-content! ft
                      (append
                       (list
                        (make-table-row
                         '("\tRev"
                           "\tDate"
                           "\tAuthor(s)"
                           "\tRemarks")
                         'revtable-header-para))
                       (map
                        (lambda (r)
                          (make-table-row (list (xpath r "revnumber" 'cdata)
                                                (xpath r "date" 'cdata)
                                                (string-join
                                                 ", "
                                                 (map (lambda (a)
                                                        (xpath a 'cdata))
                                                      (xpath* r "authorinitials")))
                                                (xpath r "revremark" 'cdata))
                                          'revtable-entry-para))
                        revs)))
        (emit ft))))

(define (make-table-row content styles)
  (map
   (lambda (cell style)
     (make <table-cell>
           owner: ft
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
                                     
                                          

