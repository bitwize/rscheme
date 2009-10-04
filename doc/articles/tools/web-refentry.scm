;,(use util.xml util.xpath)

(define (tre)
  (block-style:refentry 
   (sxml:root-element (call-with-input-file "ref.xml" port->sxml))))

(define (comma-separate lst)
  (if (or (null? lst) (null? (cdr lst)))
      lst
      (cons* (car lst) ", " (comma-separate (cdr lst)))))

(define (block-style:refentry refentry)
  ;;
  (define (divhead title)
    `(tr (td (@ (colspan "3"))
             (span (@ (class "refentrydiv")) ,title))))
  ;;
  (define (divbody content)
    `(tr
      (td (@ (width "10")) " ")
      (td (@ (colspan "2"))
          ,@content)))
  ;;
  `((hr (@ (class "refentry")))
    (table
     (@ (width "100%"))
     ,(divhead "Name")
     ,@(map
        (lambda (refnamediv)
          `(tr
            (td (@ (width "10")) " ")
            (td 
             ;;
             ;;  Allow stuff like:
             ;;
             ;;    <refnamediv>
             ;;      <refname>+</refname>
             ;;      <refname>-</refname>
             ;;      <refname>/</refname>
             ;;      <refname>*</refname>
             ;;      <refpurpose>usual arithemtic operations</refpurpose>
             ;;    </refnamediv>
             ;;
             (table
              (tr
               (td (@ (align "baseline"))
                   ,@(comma-separate
                      (map (lambda (n)
                             `(code ,(xpath:node->string n)))
                           (xpath () refnamediv "refname")))
                   " - ")
               (td (@ (align "baseline"))
                   ,(xpath-str refnamediv "refpurpose"))))
             ;;
             (td (@ (align "right"))
                 (i (b ,(xpath-str refnamediv "refclass[@role='type']")))))))
        
        (xpath () refentry "refnamediv"))
     ;;
     ,(divhead "Synopsis")
     ,(divbody `((pre
                  (@ (class "funcsynopsis"))
                  ,@(map 
                     (lambda (funcsynopsis)
                       `(span
                         "("
                         ,(xpath-str funcsynopsis
                                     "funcprototype/funcdef/function")
                         ,@(map 
                            (lambda (paramdef)
                              `(span
                                (*ENTITY* "#xA0")
                                (i ,(xpath-str paramdef "parameter"))))
                            (xpath () funcsynopsis "funcprototype/paramdef"))
                         ")"
                         "\n"))
                     (xpath () refentry "refsynopsisdiv/funcsynopsis")))))
     ;;
     ,@(apply 
        append
        (map (lambda (refsect1)
               `(,(divhead (xpath-str refsect1 "title"))
                 ,(divbody (in-mode 
                            'block
                            (style-children-except refsect1 'title)))))
             (xpath () refentry "refsect1"))))))
     
  
   

