(define (sxml:root-element sxml)
  (if (and (pair? sxml)
           (eq? (car sxml) '*TOP*))
      (let loop ((c (sxml:children sxml)))
        (if (null? c)
            sxml
            (if (sxml:element? (car c))
                (car c)
                (loop (cdr c)))))
      sxml))

(define (sxml:text? node)
  (string? node))

(define (sxml:pi? node)
  (and (pair? node)
       (eq? (car node) '*PI*)))

(define (sxml:comment? node)
  (and (pair? node)
       (eq? (car node) '*COMMENT*)))

(define (sxml:document? x)
  (and (pair? x) (eq? (car x) '*TOP*)))

(define (sxml:entityref? node)
  (and (pair? node)
       (eq? (car node) '*ENTITY*)))

(define (sxml:element? node)
  (and (pair? node)
       (not (memq (car node) '(@ 
                               *ENTITY*
                               &
                               *PI*
                               *DECL*
                               *COMMENT*
                               *XML*
                               *NAMESPACES*)))))

(define (sxml:attributes node)
  (if (pair? node)
      (if (and (pair? (cdr node))
               (pair? (cadr node))
               (eq? (caadr node) '@))
          (cdadr node)
          '())
      '()))

(define (sxml:children node)
  (if (pair? node)
      (if (and (pair? (cdr node))
               (pair? (cadr node)))
          (let ((t (caadr node)))
            (if (or (eq? t '@)
                    (eq? t '@@))
                (sxml:children (cdr node))
                (cdr node)))
          (cdr node))
      '()))

#|
(define-class <xpath-context-bindings> (<object>)
  context-variables
  context-procedures
  context-namespaces)
  
(define-class <xpath-context> (<object>)
  context-node
  context-position
  context-size
  (context-bindings type: <xpath-context-bindings>))
|#

;;;

(define (xpath:child node name)
  (select (lambda (sub)
            (and (pair? sub) (eq? (car sub) name)))
          (cdr node)))

(define (xpath:eval node expr)
  (let loop ((node-set (list node))
             (e expr))
    (if (null? e)
        node-set
        (case (caar e)
          ((literal)
           (loop (cadar e) (cdr e)))
          ((attribute)
           (if (and (pair? node-set)
                    (pair? (car node-set))
                    (pair? (cdar node-set))
                    (pair? (cadar node-set))
                    (eq? (caadar node-set) '@))
               (let* ((key (cadar e))
                      (a (assq key (cdadar node-set))))
                 (if a
                     (loop (cadr a) (cdr e))
                     (loop '() (cdr e))))
               (loop '() (cdr e))))
          ((child)
           (loop (append-map
                  (lambda (n)
                    (xpath:child n (cadar e)))
                  node-set)
                 (cdr e)))
          ((predicate)
           (let ((test (cadar e)))
             (loop (select (lambda (n)
                             (xpath:eval n test))
                           node-set)
                   (cdr e))))
          ((=)
           (let ((a (xpath:eval (car node-set) (cadar e)))
                 (b (xpath:eval (car node-set) (caddar e))))
             (if (and (string? a) (not (string? b)))
                 (set! b (xpath:node-set->string b)))
             (if (and (string? b) (not (string? a)))
                 (set! a (xpath:node-set->string a)))
             (loop (equal? a b) (cdr e))))
          (else
           (error "bad expr: ~s" e))))))

(define (append-map fn lst)
  (apply append (map fn lst)))

(define (xpath:node-set->string ns)
  (apply string-append (map xpath:node->string ns)))

(define (xpath:node->string n)
  (if (sxml:text? n)
      n
      (xpath:node-set->string (sxml:children n))))


