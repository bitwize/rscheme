
(define-method bridge-display-stuff ((self <EXAMPLE>) emit)
  (emit (make-hrule 10 3 left: 1in right: 1in))
  (emit (make-paragraph-from-inline
         (append
          (list
           "\t"
           (list :bold (format #f "Example ~a." (get-example-name self)))
           (make-anchor self <BOOK> (list 'example 
                                          (get-example-name self)))
           "\t"
           (xpath self "title")))
         'table-title-style
         bridge-title-stuff))
  (bridge-examples (content self) emit)
  (emit (make-hrule 10 3 left: 1in right: 1in)))

(define-method bridge-display-stuff ((self <INFORMALEXAMPLE>) emit)
  (bridge-examples (content self) emit))
  
(define-method bridge-examples ((self <list>) emit)
  (for-each (lambda (m)
              (bridge-examples m emit))
            self))

(define-method bridge-examples ((self <TITLE>) emit)
  (values))                             ; handled in <EXAMPLE>

(define-method bridge-examples ((self <PARA>) emit)
  (emit (make-paragraph-from-inline
         (content self)
         'example-para-style
         bridge-examples-inline)))

(define-method bridge-examples ((self <SCREEN>) emit)
  (bridge-display-stuff self emit))


(define-method bridge-examples-inline ((self <sgml-text>) emit style)
  (emit (make <text-run>
              style: style
              content: (content self))))

(define-method bridge-examples-inline ((self <list>) emit style)
  (bridge-inline-list self emit style bridge-examples-inline))

(define-method bridge-examples-inline ((self <LITERAL>) emit style)
  (bridge-examples-inline (content self) 
                          emit
                          'literal-char-style))


(define-method bridge-examples-inline ((self <FILENAME>) emit style)
  (bridge-examples-inline (content self) 
                          emit
                          'prototype-char-style))

(define-method bridge-examples-inline ((self <PHRASE>) emit style)
  (let ((role (or (get-attribute-value self "role")
                  "default")))
    (case (string->symbol (string-downcase role))
      ((default)
       (bridge-examples-inline (content self) emit 'emphasis-char-style))
      ((toeval)
       (bridge-examples-inline (content self) emit 'emphasis-char-style))
      ((noprintform)
       (emit (make <text-run>
                   style: 'emphasis-char-style
                   content: "#{"))
       (bridge-examples-inline (content self) emit 'emphasis-char-style)
       (emit (make <text-run>
                   style: 'emphasis-char-style
                   content: "}")))
      (else
       (error "unknown Phrase role=~s" role)))))

(define-method bridge-examples-inline ((self <USERINPUT>) emit style)
  (let ((role (or (get-attribute-value self "role")
                  "default")))
    (case (string->symbol (string-downcase role))
      ((toeval)
       (bridge-examples-inline (content self) emit 'literal-char-style))
      ((evalsto)
       (emit (make <text-run>
                   style: 'symbol-char-style
                   content: "\t\336\t"))
       (bridge-examples-inline (content self) emit 'literal-char-style))
      ((default)
       (bridge-examples-inline (content self) emit 'literal-char-style))
      (else
       (error "unknown UserInput role=~s" role)))))

(define-method bridge-examples-inline ((self <COMPUTEROUTPUT>) emit style)
  (let ((role (or (get-attribute-value self "role")
                  "default")))
    (case (string->symbol (string-downcase role))
      ((toeval)
       (bridge-examples-inline (content self) emit 'literal-char-style))
      ((evalsto)
       (emit (make <text-run>
                   style: 'symbol-char-style
                   content: "\t\336\t"))
       (bridge-examples-inline (content self) emit 'literal-char-style))
      ((default)
       (bridge-examples-inline (content self) emit 'literal-char-style))
      (else
       (error "unknown UserInput role=~s" role)))))
