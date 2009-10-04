(define (bibliography-style:biblioentry n)
  (bind ((article journal phd
                  (list->values
                   (map (lambda (rel)
                          (xpath ((r rel)) n "biblioset[@relation=$r]"))
                        '("article"
                          "journal"
                          "phd")))))
    ;;
    (cond
     ((and (= (length article) 1)
           (= (length journal) 1)
           (= (length phd) 0))
      (article-biblioentry n (car article) (car journal)))
     ;;
     ((and (= (length article) 0)
           (= (length journal) 0)
           (= (length phd) 1))
      (phd-biblioentry n (car phd)))
     ;;
     (else
      (error "Don't know how to format biblioentry ~s" n)))))

(define (phd-biblioentry n phd-info)
  (let ((uri (xpath-str-unique phd-info "bibliosource/link/uri")))
    ;;
     (if (not (string-search uri #\:))
        (make-artifact <linked-file>
                       name: uri
                       file: (append-path (current-absolute-directory)
                                          (string->file uri))))
     ;;
   `((li
       ,@(english-join-html
          (map xpath:node->string
               (xpath () phd-info "authorgroup/author")))
       ".  "
       (b (a (@ (class "text")
                (href ,uri))
             ,(xpath-str-unique phd-info "title")))
       ".  Ph.D. dissertation, "
       ,(xpath-str-unique phd-info "address/otheraddr")
       ", "
       ,(xpath-str-unique phd-info "date")
       ;;
       (blockquote
        ,@(in-mode 
           'block 
           (style-children
            (xpath-unique () phd-info "abstract"))))))))

(define (article-biblioentry n article-info journal-info)
  (let* ((uri (xpath-str-unique article-info "bibliosource/link/uri")))
    ;;
    (if (not (string-search uri #\:))
        (make-artifact <linked-file>
                       name: uri
                       file: (append-path (current-absolute-directory)
                                          (string->file uri))))
    ;;
    `((li
       ,@(english-join-html
          (map xpath:node->string
               (xpath () article-info "authorgroup/author")))
       ". "
       (b (a (@ (class "text")
                (href ,uri))
             ,(xpath-str-unique article-info "title"))
          ".")
       "  In "
       (i ,(xpath-str-unique journal-info "title"))
       ", pages "
       ,(xpath-str-unique article-info "artpagenums")
       ", "
       ,(xpath-str-unique journal-info "address/city")
       ", "
       ,(xpath-str-unique journal-info "address/country")
       ", "
       ,(xpath-str-unique journal-info "date")
       "."
       ;;
       (blockquote
        ,@(in-mode 
           'block 
           (style-children
            (xpath-unique () article-info "abstract"))))))))

(define (block-style:bibliography n)
  `((ol
     ,@(in-mode 'bibliography (style-children n)))))

