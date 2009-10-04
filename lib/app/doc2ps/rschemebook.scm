;;;
;;;  This is an SGML application for rendering the 
;;;  RScheme documentation (which is nominally in
;;;  DocBook format)
;;;

#|
    Master Pages
    ------------
    book-cover
    title-page
    front-matter
    toc-left
    toc-right
    part-cover
    chapter-start
    body-left
    body-right
    index-start
    index-left
    index-right
    glossary-start
    glossary-left
    glossary-right

    Paragraph Formats
    -----------------
    chapter-title-pp
    chapter-subtitle-pp
    sect1-title-pp
    sect2-title-pp
    body-pp
    prototype-pp
    condition-pp
    note-pp
    bullet-pp

    Font Formats
    ------------
    body-font                   ; the body text
    literal-font                ; other kinds of literals, like 42 and #f
    prototype-font              ; formal structure
    argument-font               ; formal arguments
    identifier-font             ; for proper RScheme names (i.e., object identifiers)

|#

(define-class <toc-node> (<object>)
  (label type: <list>)
  (type type: <symbol>)
  element
  parent-node
  child-nodes)

(define-class <special-toc-node> (<toc-node>)
  title
  anchor)

(define (add-toc-node! (parent <toc-node>) (child <toc-node>))
  (set-child-nodes! parent (append (child-nodes parent)
                                   (list child)))
  (set-parent-node! child parent))

(define-method name ((self <toc-node>))
  ;; we want the name of a chapter to be just it's number,
  ;; regardless of whether or not there is a PART or a SET
  ;; that wraps around it
  (let ((l (label self)))
    (if (null? l)
        #f
        (case (type self)
          ((glossary preface)
           "")
          ((appendix)
           (list-ref $appendix-names (car l)))
          ((chapter sect1 sect2 sect3)
           (string-join #\. (map number->string 
                                 (list-tail
                                  (reverse l)
                                  (max 0
                                       (- (length l)
                                          (case (type self)
                                            ((chapter) 1)
                                            ((sect1) 2)
                                            ((sect2) 3)
                                            ((sect3) 4))))))))
          ((part)
           (format-number (car (label self)) 'Roman))
          (else (format #f "[~s ~j]" (type self) (reverse l)))))))

(define-method write-object ((self <toc-node>) port)
  (format port "#[TOC ~a ~s]"
          (toc-section-number self)
          (element self)))

(define (build-toc-structure doc)
  ;; The TOC structure comes in two parts:
  ;;   (1) a set of nested <toc-node>'s which describe the tree structure
  ;;       of the document
  ;;
  ;;   (2) a hash table mapping nodes to their corresponding <toc-node>
  ;;
  (let* ((tbl (make-table))
         (book-node (car (content doc)))
          ;; XXX (xpath doc "BOOK")
         (o (overall-toc-hierarchy book-node))
         (root (build-toc* book-node tbl #f '() o)))
    ;;
    ;; assign the proper appendix labels
    ;;
    (fix-appendix-labels! (child-nodes root))
    ;;
    (values root tbl)))

(define $appendix-names '("A" "B" "C" "D" "E" "F" "G"))

(define (fix-appendix-labels! lst)
  (let loop ((i 0)
             (l lst))
    (if (null? l)
        (values)
        (if (eq? (type (car l)) 'appendix)
            (begin
              (set-label! (car l) (cons i (cdr (label (car l)))))
              (loop (+ i 1) (cdr l)))
            (loop i (cdr l))))))
                                                      
        
;;; NOTE: We don't handle <SET>'s yet

(define (book-has-parts? (self <BOOK>))
  (any? (lambda (child)
          (instance? child <PART>))
        (content self)))

(define-method overall-toc-hierarchy ((self <CHAPTER>))
  '((chapter "SECT1")
    (sect1 "SECT2")
    (sect2 "SECT3")))

(define-method overall-toc-hierarchy ((self <BOOK>))
  (if (book-has-parts? self)
      '((book "PART")
        (part "PREFACE" "CHAPTER" "APPENDIX" "GLOSSARY")
        (chapter "SECT1")
        (appendix "SECT1")
        (chapter "SECT1")
        (sect1 "SECT2")
        (sect2 "SECT3"))
      '((book "PREFACE" "CHAPTER" "APPENDIX" "GLOSSARY")
        (appendix "SECT1")
        (chapter "SECT1")
        (sect1 "SECT2")
        (sect2 "SECT3"))))

(define (build-toc* (self <sgml-element>) tbl parent label hier)
  (let* ((k (string->symbol (string-downcase (name self))))
         (next-lower (assq k hier))
         (this (make <toc-node>
                     label: label
                     element: self
                     type: k
                     parent-node: parent
                     child-nodes: '())))
    (table-insert! tbl self this)
    (set-property! self 'toc-node this)
    ;;
    (if next-lower
        (let ((subs (apply append 
                           (map 
                            (lambda (sub-elem)
                              (force-list (xpath* self sub-elem)))
                            (cdr next-lower)))))
          (set-child-nodes!
           this
           (map (lambda (i sub)
                  (build-toc* sub tbl this 
                              (cons (+ i 1) label)
                              hier))
                (range (length subs))
                subs))))
    this))

(define (force-list x)
  (if (list? x)
      x
      (list x)))

(define (string-downcase str)
  (list->string (map char-downcase (string->list str))))

(define (toc-section-number (self <toc-node>))
  (if (null? (label self))
      "top"
      (if (eq? (type self) 'appendix)
          (list-ref $appendix-names (car (label self)))
          (string-join #\. (map number->string (reverse (label self)))))))

(define (print-toc node #optional (path '()))
  (format #t "~a~a: ~s\n"
          (make-string (* (length path) 2) #\space)
          (string-join #\. (map number->string (reverse path)))
          (or (title node) node))
  (let loop ((i 1)
             (s (child-nodes node)))
    (if (pair? s)
        (begin
          (print-toc (car s) (cons i path))
          (loop (+ i 1) (cdr s))))))

(define (toc-ref node path)
  (if (null? path)
      node
      (toc-ref (list-ref (child-nodes node) (- (car path) 1)) (cdr path))))

(define-method hash-code ((self <sgml-element>))
  (transient->hash self))

(define-method title ((self <toc-node>))
  ;;
  ;; XXX note: this doesn't handle things like
  ;;              <title><phrase role="var">stat</phrase> Functions</title>
  ;;           as currently occurs in chaps/posix.sgml
  ;;
  (handler-case
   (xpath (element self) "title" 'cdata)
   ((<condition>)
    #f)))

;;;

(define dd #f)
(define tt #f)

(define (test-book)
  (set! dd (call-with-input-file "/tmp/a" read-sgml-element))
  (set! tt (build-toc-structure dd))
  (init-figure-names dd)
  (init-table-names dd)
  (init-example-names dd)
  dd)


(define *test-book* (delay (test-book)))

(define *the-fake-document*
  `((version "RScheme 0.7.3.3")
    (date ,(with-module syscalls
             (time->string (time) "%Y-%m-%d %H:%M:%S")))))

(define (section-page-header page)
  (let* (((chap <toc-node>) (page->chapter page))
         (n (name chap)))
    (if n
        (format #f "Chapter ~a. ~a" n (title chap))
        (title chap))))

(define (get-table-name (self <TABLE>))
  (get-property self 'table-name))

(define (get-figure-name (self <FIGURE>))
  (get-property self 'figure-name))

(define (get-example-name (self <EXAMPLE>))
  (get-property self 'example-name))

(define (init-table-names root)
  (let loop ((i 1)
             (tables (xpath* root '* "table")))
    (if (pair? tables)
        (begin
          (set-property! (car tables) 'table-name (number->string i))
          (loop (+ i 1) (cdr tables))))))

(define (init-figure-names root)
  (let loop ((i 1)
             (lst (xpath* root '* "figure")))
    (if (pair? lst)
        (begin
          (set-property! (car lst) 'figure-name (number->string i))
          (loop (+ i 1) (cdr lst))))))

(define (init-example-names root)
  (let loop ((i 1)
             (tables (xpath* root '* "example")))
    (if (pair? tables)
        (begin
          (set-property! (car tables) 'example-name (number->string i))
          (loop (+ i 1) (cdr tables))))))
