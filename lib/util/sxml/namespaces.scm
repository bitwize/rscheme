(define-class <nsdict> (<object>)
  (default init-value: #f)
  (mappings type: <list> init-value: '())
  (abbrev type: <symbol-table>))

(define (make-namespace-dict)
  (make <nsdict>
        abbrev: (make-symbol-table)))
        

;;; 
;;;  Create an nsdict (producing a new instance if needed)
;;;  that incorporates the changes from the list of attributes.
;;;
;;;  At the same time, translate the attributes into SXML
;;;  notation, returning two lists (in addition to the new dict).
;;;
;;;  In all, three values are returned
;;;
;;;     (1) the new namespace dictionary
;;;     (2) the list of regular attributes (@, in SXML)
;;;     (3) the list of namespace attributes (@@, in SXML),
;;;         although see the SXML spec updated on March 12, 2004

(define (update-nsdict (dict <nsdict>) attrs)
  (if (null? attrs)
      (values dict '() '())
      (let ((d dict))
        (let loop ((regular '())
                   (special '())
                   (attrs attrs))
          (if (null? attrs)
              (values d (fixup-regulars regular d) special)
              (let ((a (car attrs)))
                (cond
                 ;;
                 ((eq? (name a) 'xmlns)         ; setting the default
                  (if (eq? d dict)
                      (set! d (clone d)))
                  (let ((uri (intern-attr-value a)))
                    (set-default! d uri)
                    (loop regular
                          (cons (list '*DEFAULT* uri) special)
                          (cdr attrs))))
                 ;;
                 ((eq? (namespace a) 'xmlns)    ; binding a prefix
                  (if (eq? d dict)
                      (set! d (clone d)))
                  (let* ((uri (intern-attr-value a))
                         (setting (list (name a) uri)))
                    (set-mappings! d (cons setting (mappings d)))
                    (loop regular
                          (cons setting special)
                          (cdr attrs))))
                 ;;
                 (else
                  (loop (cons a regular)
                        special
                        (cdr attrs))))))))))

(define (intern-attr-value (a <xml-attr>))
  (string->symbol (expand-text (items a))))

(define (fixup-regulars lst dict)
  (map (lambda ((a <xml-attr>))
         (list (xform-attr-using-nsdict (name a)
                                        (namespace a)
                                        dict)
               (expand-text (items a))))
       lst))

;;;
;;;  Translate a name/prefix tuple (prefix may be #f,
;;;  indicating that the identifier did not have a prefix)
;;;  into the canonical representation given the current
;;;  namespace dictionary, which can specify the default,
;;;  the known prefix mappings, and any defined abbreviations
;;;

(define (xform-attr-using-nsdict name prefix (dict <nsdict>))
  ;; for attribute names, don't apply the default prefix
  (if prefix
      (let ((n (assq prefix (mappings dict))))
        (if n
            (cond
             ((table-lookup (abbrev dict) (cadr n))
              => (lambda (a)
                   (symbol-append a ":" name)))
             (else
              (symbol-append (cadr n) ":" name)))
            (error "Prefix prefix \"~a\" not declared" prefix)))
      name))

(define (xform-gi-using-nsdict name prefix (dict <nsdict>))
  (if prefix
      (let ((n (assq prefix (mappings dict))))
        (if n
            (cond
             ((table-lookup (abbrev dict) (cadr n))
              => (lambda (a)
                   (symbol-append a ":" name)))
             (else
              (symbol-append (cadr n) ":" name)))
            (error "Prefix prefix \"~a\" not declared" prefix)))
      (if (default dict)
          (cond
           ((table-lookup (abbrev dict) (default dict))
            => (lambda (a)
                 (symbol-append a ":" name)))
           (else
            (symbol-append (default dict) ":" name)))
          name)))

