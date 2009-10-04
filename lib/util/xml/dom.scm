
(define-class <xml-node> (<object>) :abstract
  (value :sealed)
  (parent :sealed))

(define-class <xml-document> (<xml-node>))
(define-class <xml-element> (<xml-node>))
(define-class <xml-comment> (<xml-node>))
(define-class <xml-pi> (<xml-node>))
(define-class <xml-attribute> (<xml-node>))
(define-class <xml-text> (<xml-node>))
(define-class <xml-namespace> (<xml-node>))

(define (dom:document? n) (instance? n <xml-document>))
(define (dom:element? n) (instance? n <xml-element>))
(define (dom:comment? n) (instance? n <xml-comment>))
(define (dom:pi? n) (instance? n <xml-pi>))
(define (dom:attribute? n) (instance? n <xml-attribute>))
(define (dom:text? n) (instance? n <xml-text>))
(define (dom:namespace? n) (instance? n <xml-namespace>))

(define-method name ((self <xml-node>)) #f)
(define-method name ((self <xml-element>)) (car (value self)))
(define-method name ((self <xml-namespace>)) (car (value self)))
(define-method name ((self <xml-attribute>)) (car (value self)))

(define-method write-object ((self <xml-element>) port)
  (format port "#[<xml-element> ~a]" (name self)))

(define-method write-object ((self <xml-attribute>) port)
  (format port "#[<xml-attribute> ~s]" (name self)))

(define-method write-object ((self <xml-namespace>) port)
  (format port "#[<xml-namespace> ~s]" (name self)))

(define-method write-object ((self <xml-text>) port)
  (format port "#[<xml-text> ~#@*20s]" (value self)))

;;;

(define (sxml->dom n)
  (make <xml-document>
        value: n
        parent: #f))

;;; 

(define (crack-attrs (node <pair>))
  (if (pair? (cdr node))
      (if (pair? (cadr node))
          (if (eq? (caadr node) '@)
              (values '() (cdadr node) (cddr node))
              (if (eq? (caadr node) '@@)
                  (if (and (pair? (caddr node))
                           (eq? (caaddr node) '@))
                      (values (cdadr node) (cdaddr node) (cdddr node))
                      (values (cdadr node) '() (cddr node)))
                  (values '() '() (cdr node))))
          (values '() '() (cdr node)))
      (values '() '() (cdr node))))

(define-syntax (make-child child self)
  (cond
   ((pair? child)
    (make <xml-element>
          value: child
          parent: self))
   ((string? child)
    (make <xml-text>
          value: child
          parent: self))
   (else
    (error "bad child type: ~s" child))))

(define-method dom-axis:child ((self <xml-document>))
  (bind ((n a b (crack-attrs (value self))))
    (map (lambda (child)
           (make-child child self))
         b)))
  
(define-method dom-axis:child ((self <xml-element>))
  (bind ((n a b (crack-attrs (value self))))
    (map (lambda (child)
           (make-child child self))
         b)))

(define-method dom-axis:parent ((self <xml-document>))
  (list self))

(define-method dom-axis:parent ((self <xml-node>))
  (list (parent self)))

(define-method dom-axis:parent1 ((self <xml-node>))
  (parent self))

(define-method dom-axis:self ((self <xml-node>))
  (list self))

(define-method dom-axis:attribute ((self <xml-node>))
  '())

(define-method dom-axis:attribute ((self <xml-element>))
  (bind ((n a b (crack-attrs self)))
    (map (lambda (attr)
           (make <xml-attribute>
                 value: attr
                 parent: self))
         a)))

(define-method dom-axis:ancestor-or-self ((self <xml-document>))
  (list self))

(define-method dom-axis:ancestor-or-self ((self <xml-node>))
  (cons self (dom-axis:ancestor-or-self (parent self))))

(define-method dom-axis:ancestor ((self <xml-node>))
  (cdr (dom-axis:ancestor-or-self self)))

(define-method dom-axis:preceding-sibling ((self <xml-attribute>))
  '())

(define-method dom-axis:preceding-sibling ((self <xml-namespace>))
  '())

(define-method dom-axis:preceding-sibling ((self <xml-document>))
  '())

(define-method dom-axis:preceding-sibling ((self <xml-node>))
  (bind ((n a b (crack-attrs (value (parent self))))
         (rest (memq (reverse (value self)) b)))
    (map (lambda (child)
           (make-child child self))
         rest)))


(define-method dom-axis:following-sibling ((self <xml-attribute>))
  '())

(define-method dom-axis:following-sibling ((self <xml-namespace>))
  '())

(define-method dom-axis:following-sibling ((self <xml-document>))
  '())

(define-method dom-axis:following-sibling ((self <xml-node>))
  (bind ((n a b (crack-attrs (value (parent self))))
         (rest (memq (value self) b)))
    (map (lambda (child)
           (make-child child self))
         rest)))

;;;

(define-method dom-axis:namespace ((self <xml-node>))
  '())

(define-method dom-axis:namespace ((self <xml-document>))
  ;; in SSAX, currently only the root (*TOP*) has namespace decls
  (bind ((n a b (crack-attrs (value self)))
         (nss (assq '*NAMESPACES* n)))
    (if nss
        (map (lambda (ns)
               (make <xml-namespace>
                     value: ns
                     parent: self))
             (cdr nss))
        '())))

;;;

(define-method dom-axis:descendant ((self <xml-text>))
  '())

(define-method dom-axis:descendant ((self <xml-node>))
  (reduce append
          '()
          (map dom-axis:descendant-or-self (dom-axis:child self))))

(define-method dom-axis:descendant-or-self ((self <xml-node>))
  (cons self (dom-axis:descendant self)))

;;;

(define-method dom-axis:preceding ((self <xml-node>))
  (reduce append
          '()
          (map (lambda (prec)
                 (reverse (dom-axis:descendant prec)))
               (dom-axis:preceding-sibling self))))


(define-method dom-axis:following ((self <xml-node>))
  (reduce append '() (map dom-axis:descendant 
                          (dom-axis:following-sibling self))))

;;;

(define (simple-dom sxml)
  (car (dom-axis:child (sxml->dom (list '*TOP* sxml)))))



