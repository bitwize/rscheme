;;;
;;;  Break up the input document into article nodes
;;;

(define (break-pi? i)
  (and (pair? i)
       (eq? (car i) '*PI*)
       (eq? (cadr i) 'rs:webnode)))

(define (break-pi-name i)
  (caddr i))

(define (next-title lst)
  (let loop ((i lst))
    (if (null? i)
        (error "No title found in: ~s" lst)
        (if (sxml:element? (car i))
            (let ((t (xpath () (car i) "//title")))
              (if (pair? t)
                  (xpath:node->string (car t))
                  (loop (cdr i))))
            (loop (cdr i))))))

(define (breakup-article-seq lst)
  (cond
   ((null? lst)
    '())
   ((break-pi? (car lst))
    (let ((n (make <article-node>
                   container: (container *current-article-node*)
                   name: (break-pi-name (car lst))
                   subtitle: ""
                   content: '())))
      (add-artifact (container n) n)
      (thread-let ((*current-article-node* n))
        (set-content! n (breakup-article-seq (cdr lst))))
      (set-subtitle! n (next-title (content n)))
      (list n)))
   (else
    (cons (breakup-article (car lst))
          (breakup-article-seq (cdr lst))))))
      
  
(define (breakup-article src)
  (cond
   ((and (pair? src) (memq (car src) '(@ *XML* *DECL* *PI*)))
    src)
   ;;
   ((sxml:element? src)
    (let* ((attr (sxml:attributes src))
           (idx (id-index (container *current-article-node*)))
           (fixup '())
           ;; note that we are making sure both the '*ID*
           ;; entry and the 'id entry, if any, have the same
           ;; node identifer.  For example, in <xref> processing
           ;; the source links to the 'id value, but in <sect2>
           ;; processing the anchor established is that of the
           ;; '*ID* value
           (nid (delay (table-size idx))))
      ;;
      (define (record-id key)
        (if (table-key-present? idx key)
            (error "Duplicate element id: ~s on ~s" key src))
        ;;
        (set! fixup (cons (vector *current-article-node* 
                                  '*NEED-FIXUP*
                                  (force nid))
                          fixup))
        (table-insert! idx key (car fixup)))
      ;;
      (cond ((ID src) => record-id))
      ;;
      (cond
       ((assq 'id attr)
        => (lambda (id)
             (record-id (cadr id)))))
      ;;
      (let* ((rest (breakup-article-seq (sxml:children src)))
             (out (if (null? attr)
                      (cons (car src) rest)
                      (cons* (car src) (cons '@ attr) rest))))
        (for-each
         (lambda (fx)
           (vector-set! fx 1 out))
         fixup)
        out)))
   ;;
   ((sxml:text? src)
    src)
   ;;
   ((sxml:comment? src)
    src)
   ;;
   (else
    (format #t "not breaking up: ~s\n" src)
    src)))

(define (build-article (self <article>) src)
  (let ((a (make <article-node>
                 container: self
                 name: "root"
                 subtitle: (title self)
                 content: '())))
    (add-artifact self a)
    (thread-let ((*current-article-node* a))
      (set-content! a (list (breakup-article src))))
    a))
