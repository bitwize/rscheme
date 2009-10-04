(define (top-style:chapter n)
  (style-children-except n 'title 'info))

(define (top-style:toc n)
  `((h2 "Table of Contents")
    (ol
     ,@(map (lambda (sect1)
              (bind ((uri sxml (href (ID sect1))))
                `(li (a (@ (href ,uri))
                        ,(xpath-str sxml "title")))))
            (xpath () (document *current-article*) "sect1")))))

(define (top-style:para n)
  (in-mode 'block (style-dispatch n)))

(define (my-anchor n)
  (cond
   ((ID n)
    => (lambda (id)
         (let (((target <vector>) (table-lookup (id-index *current-article*) 
                                                id)))
           `((a (@ (name ,(~ "L~d" (vector-ref target 2)))))))))
   (else
    '())))

(define (top-style:sect1 n)
  `((h2 ,@(my-anchor n)
        ,(ARABIC-NUM n)
        ". "
        ,@(in-mode 'title (style-children (xpath-unique () n "title"))))
    ,@(in-mode 'block (style-children-except n 'title 'info))))

(define (block-style:sect2 n)
  `((h3 ,@(my-anchor n)
        ,(ARABIC-NUM n)
        ". "
        ,@(in-mode 'title (style-children (xpath-unique () n "title"))))
    ,@(style-children-except n 'title)))

(define (block-style:sect3 n)
  `((h4 ,@(my-anchor n)
        ,(ARABIC-NUM n)
        ". "
        ,@(in-mode 'title (style-children (xpath-unique () n "title"))))
    ,@(style-children-except n 'title)))

(define (block-style:note n)
  `((table
     (tr
      (td (@ (valign "top")) (img (@ (src "/s/note.png"))))
      (td ,@(in-mode 'block (style-children n)))))))

(define (all-style:TEXT n)
  (list n))

(define (block-style:caution n)
  (format #t "caution: ~s\n" n)
  (format #t "first-child: ~s\n" (car (sxml:children n)))
  `((blockquote
     (p (img (@ (src "/s/caution.png")))
        ,@(in-mode 'note (style-children-first n)))
     ,@(in-mode 'block (style-children-rest n)))))

(define (note-style:para n)
  (in-mode 'inline (style-children n)))

(define (block-style:orderedlist n)
  `((ol
     ,@(in-mode 'list (style-node-list (xpath () n "listitem"))))))

(define (block-style:itemizedlist n)
  `((ul
     ,@(in-mode 'list (style-node-list (xpath () n "listitem"))))))

(define (list-style:listitem n)
  `((li
     ,@(in-mode 'block (style-children n)))))

(define (block-style:formalpara n)
  `((p
     (b ,@(in-mode 'inline (style-children (xpath-unique () n "title")))
        ".")
     "  "
     ,@(in-mode 'inline (style-children (xpath-unique () n "para"))))))


(define (block-style:figure n)
  (let* ((img (xpath-str-unique n "mediaobject/imageobject/imagedata/@fileref"))
         (alt (xpath-str-unique n "mediaobject/textobject/phrase"))
         (scm (string-append img ".scm"))
         (eps (append-path (current-absolute-directory)
                           (string->file (string-append img ".eps"))))
         (png (string-append img ".png"))
         (box (generate-graphic (append-path (current-directory) 
                                             (string->file scm)))))
    ;;
    (make-artifact <image-file>
                   name: img
                   eps-file: eps
                   png-file: (string->file png))
    ;;
    `((blockquote
       (table
        (tr
         (td ,@(my-anchor n)
             (b "Figure " ,(ARABIC-NUM n) ". ")
             ,@(in-mode 'title 
                        (style-children (xpath-unique () n "title")))))
        (tr (td (img (@ (src ,png)
                        (width ,(to-string (size-width box)))
                        (height ,(to-string (size-height box)))
                        (alt ,alt))))))))))
     


;;; Find the callout points in a subtree and assign
;;; them sequential numbers

(define *callout-index* (make-string-table))

(define (co-scope n)
  (let ((i 0))
    (for-each
     (lambda (co)
       (let ((n (+ i 1)))
         (set! i (+ i 1))
         (format #t "assign CO ~a {~a} => [~d]\n" 
                 (xpath-str co "@id")
                 (ID co)
                 n)
         (table-insert! *callout-index* ;; XXX accumulates memory!
                        (xpath-str co "@id")
                        n)))
     (xpath () n "//co"))))

(define *callout-chars* '#(#\x2460         ; (1)
                           #\x2461         ; (2)
                           #\x2462         ; (3)
                           #\x2463         ; (4)
                           #\x2464         ; (5)
                           #\x2465         ; (6)
                           #\x2466         ; (7)
                           #\x2467         ; (8)
                           #\x2468))       ; (9)

(define (unic (ch <char>))
  (map (lambda (ch)
         `(*ENTITY* ,(~ "#x~x" (char->integer ch))))
       (string->list (unicode-char->utf-string ch))))

(define (pre-style:co n)
  (let ((k (table-lookup *callout-index* (xpath-str n "@id"))))
    `((span (@ (class "co")) 
            ,@(unic (vector-ref *callout-chars* (- k 1)))))))
                                                     
(define (inline-style:coref n)
  (let ((k (or (table-lookup *callout-index* (xpath-str n "@label"))
               (error "coref: no callout named ~s"
                      (xpath-str n "@label")))))
    (unic (vector-ref *callout-chars* (- k 1)))))

(define (block-style:screen n)
  (co-scope n)
  `((pre ,@(in-mode 'pre (style-children n)))))

(define (block-style:programlisting n)
  (co-scope n)
  `((pre ,@(in-mode 'pre (style-children n)))))

(define (block-style:variablelist n)
  `((dl
     ,@(apply 
        append
        (map (lambda (varlistentry)
               `((dt ,@(in-mode 
                        'inline
                        (style-children
                         (xpath-unique () varlistentry "term"))))
                 (dd ,@(in-mode
                        'block
                        (style-children
                         (xpath-unique () varlistentry "listitem"))))))
             (xpath () n "varlistentry"))))))

(define (block-style:para n)
  `((p ,@(in-mode 'inline (style-children n)))))

(define (inline-style:package n)
  `((b (code ,@(style-children n)))))

(define (inline-style:variable n)
  `((code ,@(style-children n))))

(define (inline-style:function n)
  `((code (b ,@(style-children n)))))

(define (inline-style:replaceable n)
  `((em ,@(style-children n))))

(define (inline-style:code n)
  `((code ,@(style-children n))))

(define (inline-style:structname n)
  `((b (code ,@(style-children n)))))

(define (inline-style:structfield n)
  `((b (code ,@(style-children n)))))

(define (href id)
  (let* ((target (or (table-lookup (id-index *current-article*) id)
                     (error "href target id ~s not defined" id)))
         ((in-node <article-node>) (vector-ref target 0))
         ((sxml <list>) (vector-ref target 1))
         ((idn <fixnum>) (vector-ref target 2)))
    ;;
    (values (if (eq? in-node *current-article-node*)
                (~ "#L~d" idn)
                (~ "~a#L~d" (name in-node) idn))
            sxml)))

(define (inline-style:xref n)
  (bind ((uri sxml (href (xpath-str n "@linkend"))))
    ;;
    (case (car sxml)
      ((figure)
       `((a (@ (href ,uri)
               (title ,(xpath-str sxml "title")))
            "Figure " ,(to-string (ARABIC-NUM sxml)))))
      ((sect2)
       `((a (@ (href ,uri)
               (title ,(xpath-str sxml "title")))
            "Section " ,(to-string (ARABIC-NUM sxml))))))))
       
        

(define (inline-style:filename n)
  `((b ,@(style-children n))))

(define (inline-style:foreignphrase n)
  `((i ,@(style-children n))))

(define (inline-style:glossterm n)
  `((i ,@(style-children n))))

(define (inline-style:citetitle n)
  `((u ,@(style-children n))))

(define (inline-style:emphasis n)
  `((em ,@(style-children n))))

(define (inline-style:procedure n)
  `((code ,@(style-children n))))

(define (all-style:literal n)
  `((b (code ,@(style-children n)))))
