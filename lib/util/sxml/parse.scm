
;;

(define *xml-entities* (make-symbol-table))

(table-insert! *xml-entities* 'quot "\"")
(table-insert! *xml-entities* 'apos "'")
(table-insert! *xml-entities* 'lt "<")
(table-insert! *xml-entities* 'gt ">")
(table-insert! *xml-entities* 'amp "&")

;;


(define (sxml-parse (port <input-port>) (ns0 <list>))
  (let ((state (make <xml-scanner-state>))
        ((buf <string>) "")
        (end? #f)
        (i 0))
    ;;
    (define (initial-dict)
      (let ((atab (make-symbol-table)))
        ;;
        ;; Set up the abbreviations table, which maps FQ prefixes
        ;; like 'http://schemas.xmlsoap.org/soap/envelope/' to
        ;; abbreviations that the application wants to use like 'SOAP-ENV'
        ;;
        (for-each
         (lambda (n)
           (let (((fq <string>) (cadr n))
                 ((abbrev <symbol>) (car n)))
             (table-insert! atab (string->symbol fq) abbrev)))
         ns0)
        ;;
        (make <nsdict>
              abbrev: atab
              mappings:
              ;; Compute the initial namespace, which is used to parse
              ;; XML text that doesn't have the appropriate xmlns declarations.
              ;; Note that it is impossible to use the initial-ns feature
              ;; without the abbreviation feature
              (map (lambda (n)
                     (list (car n) (string->symbol (cadr n))))
                   ns0))))
    ;;
    (define (token)
      (bind ((tc t j (xml-scan state buf i (string-length buf) end?)))
        (if (eq? tc 0)
            (let ((more (input-port-read-max port $port-chunk-size)))
              (sxml:debug-print "more: ~s[~d:] ~s\n" buf i more)
              (if (eof-object? more)
                  (begin
                    (set! end? #t)
                    (token))
                  (begin
                    (set! buf (if (< i (string-length buf))
                                  (string-append (if (eq? i 0)
                                                     buf
                                                     (substring buf i))
                                                 more)
                                  more))
                    (set! i 0)
                    (token))))
            (values tc t j))))
    ;;
    (define (elem-header t dict)
      (reverse (elem-header-rev t dict)))
    ;;
    (define (elem-header-rev t dict)
      (bind ((new-dict reglist nlist (update-nsdict dict (attributes t)))
             (g (xform-gi-using-nsdict (gi t)
                                       (namespace t)
                                       new-dict)))
        (if (null? reglist)
            (values (list g) new-dict)
            (values (list (cons '@ reglist) g) new-dict))))
    ;;
    (letrec ((prolog (lambda (tc t j)
                       (sxml:debug-print "(prolog ~s ~s ~s)\n" tc t j)
                       (case tc
                         ((6)                 ; PI
                          (set! i j)
                          (let ((tgt (target t))
                                (pi (expand-text (list (pi t)))))
                            (if (eq? tgt 'xml)
                                (bind ((tc t j (token)))
                                  (prolog1 tc t j `((*XML* ,pi))))
                                (prolog1 tc t j `((*PI* ,tgt ,pi))))))
                         ;;
                         ((9) ; TC_WHITESPACE
                          (set! i j)
                          (bind ((tc t j (token)))
                            (prolog1 tc t j '())))
                         ;;
                         ((7) ; TC_DOCTYPE_DECL
                          (prolog2 tc t j '()))
                         ;;
                         (else
                          (prolog1 tc t j '())))))
             ;;
             (prolog1 (lambda (tc t j preamble)
                       (sxml:debug-print "(prolog1 ~s ~s ~s)\n" tc t j)
                        (case tc
                          ((6)          ; PI
                           (set! i j)
                           (if (eq? (target t) 'xml)
                               (error "[22] Misplaced XMLDecl"))
                           (bind ((tc t j (token)))
                             (prolog1 tc t j `((*PI* ,(target t) 
                                                     ,(expand-text (list (pi t))))
                                               ,@preamble))))
                          ((9)          ; TC_WHITESPACE
                           (set! i j)
                           (bind ((tc t j (token)))
                             (prolog1 tc t j preamble)))
                          ((8)          ; TC_COMMENT
                           (set! i j)
                           (bind ((p `((*COMMENT* ,(expand-text
                                                    (list (item t))))
                                       ,@preamble))
                                  (tc t j (token)))
                             (prolog1 tc t j p)))
                          (else
                           (prolog2 tc t j preamble)))))
             ;;
             (prolog2 (lambda (tc t j preamble)
                        (sxml:debug-print "(prolog2 ~s ~s ~s)\n" tc t j)
                        (case tc
                          ((7)  ; TC_DOCTYPE_DECL
                           (set! i j)
                           (bind ((p (if (pubid t)
                                         `((*DECL* DOCTYPE
                                                   ,(name t)
                                                   PUBLIC
                                                   ,(pubid t)
                                                   ,(sysid t))
                                           ,@preamble)
                                         (if (sysid t)
                                             `((*DECL* DOCTYPE
                                                       ,(name t)
                                                       SYSTEM
                                                       ,(sysid t))
                                               ,@preamble)
                                             `((*DECL* DOCTYPE
                                                       ,(name t))
                                               ,@preamble))))
                                  (tc t j (token)))
                             (prolog3 tc t j p)))
                          (else
                           (rootelem tc t j preamble)))))
             ;;
             (prolog3 (lambda (tc t j preamble)
                        (sxml:debug-print "(prolog3 ~s ~s ~s)\n" tc t j)
                        (case tc
                          ((6)          ; PI
                           (set! i j)
                           (if (eq? (target t) 'xml)
                               (error "[22] Misplaced XMLDecl"))
                           (bind ((pre `((*PI* ,(target t) 
                                               ,(expand-text (list (pi t))))
                                         ,@preamble))
                                  (tc t j (token)))
                             (prolog3 tc t j pre)))
                          ((9)          ; TC_WHITESPACE
                           (set! i j)
                           (bind ((tc t j (token)))
                             (prolog3 tc t j preamble)))
                          ((8)          ; TC_COMMENT
                           (set! i j)
                           (bind ((pre `((*COMMENT* ,(expand-text
                                                      (list (item t))))
                                         ,@preamble))
                                  (tc t j (token)))
                             (prolog3 tc t j pre)))
                          (else
                           (rootelem tc t j preamble)))))
             (rootelem (lambda (tc t j preamble)
                         (let ((pre (if (null? preamble)
                                        '()
                                        (reverse preamble))))
                           (sxml:debug-print "(rootelem ~s ~s ~s)\n" tc t j)
                           (case tc
                             ((3)       ; TC_START_ELEMENT
                              (set! i j)
                              (bind ((p d (elem-header-rev t (initial-dict))))
                                (bind ((tc t j (token)))
                                  (body tc t j p (list pre) 1 (list d)))))
                             ((2)       ; TC_EMPTY_ELEMENT
                              (set! i j)
                              (let ((p `(,(elem-header t (initial-dict)) ,@pre)))
                                (bind ((tc t j (token)))
                                  (epilog tc t j p))))
                             (else (error "Expected start-element or empty-element at document root"))))))
             ;;
             (body (lambda (tc t j accum stack depth dstack)
                     (sxml:debug-print "(body ~s ~s ~s)\n" tc t j)
                     (case tc
                       ((4)             ; TC_END_ELEMENT
                        (set! i j)
                        (let ((g (xform-gi-using-nsdict (gi t)
                                                        (namespace t)
                                                        (car dstack))))
                          (if (not (eq? (last accum) g))
                              (error "Mismatch ~s <> ~s" (last accum) g)))
                        (let ((up (cons (reverse accum) (car stack))))
                          (if (= depth 1)
                              (bind ((tc t j (token)))
                                (epilog tc t j up))
                              (bind ((tc t j (token)))
                                (body tc t j up 
                                      (cdr stack) (- depth 1) (cdr dstack))))))
                       ((3)     ; TC_START_ELEMENT
                        (set! i j)
                        (bind ((p d (elem-header-rev t (car dstack))))
                          (bind ((tc t j (token)))
                            (body tc t j p
                                  (cons accum stack) 
                                  (+ depth 1) 
                                  (cons d dstack)))))
                       ((2)     ; TC_EMPTY_ELEMENT
                        (set! i j)
                        (let ((p (cons (elem-header t (car dstack)) accum)))
                          (bind ((tc t j (token)))
                            (body tc t j p stack depth dstack))))
                       ((5)
                        (set! i j)
                        (bind ((accum+ (append (expand-text-parsed (items t))
                                               accum))
                               (tc t j (token)))
                          (body tc t j accum+ stack depth dstack)))
                       ((1)
                        (error "Unexpected EOF; <~s> still open" 
                               (last accum)))
                       ((6)
                        (set! i j)
                        (let ((p (cons `(*PI* ,(target t) 
                                              ,(expand-text (list (pi t))))
                                       accum)))
                          (bind ((tc t j (token)))
                            (body tc t j p stack depth dstack))))
                       ((7)
                        (error "Unexpected <!DOCTYPE>"))
                       ((8 9) ; TC_COMMENT, TC_WHITESPACE
                        (set! i j)
                        ;; ignore them
                        (bind ((tc t j (token)))
                          (body tc t j accum stack depth dstack)))
                       (else
                        (error "Unexpected token type ~s" tc)))))
             ;;
             (epilog (lambda (tc t j main)
                       (sxml:debug-print "(epilog ~s ~s ~s)\n" tc t j)
                       (case tc
                         ((6 8 9)         
                          ;; ignore PI's, COMMENT's, and SPACE in the epilog
                          (set! i j)
                          (bind ((tc t j (token)))
                            (epilog tc t j main)))
                         (else
                          (values (cons '*TOP* (reverse main))
                                  buf
                                  i)))))
             )
      (bind ((tc t j (token)))
        (prolog tc t j)))))
        

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

;; Returns two values,
;;   (1) the normal attributes as an SXML alist
;;   (2) the namespace attributes

(define (get-attributes (self <xml-element>))
  (let ((alist (attributes self)))
    (if (null? alist)
        '()
        (reverse!
         (map (lambda ((attr <xml-attr>))
                (list (name attr) 
                      (expand-text (items attr))))
              alist)))))


;;;  Flatten out entity refs, etc., but can't handle parsed
;;;  entities.  This is used, for example, in attribute value
;;;  processing

(define (expand-text lst)
  (string-join ""
               (map (lambda (i)
                      (cond
                       ((vector? i)
                        (substring (vector-ref i 0)
                                   (vector-ref i 1)
                                   (vector-ref i 2)))
                       ((char? i)
                        (string i))
                       ((symbol? i)
                        (or (table-lookup *xml-entities* i)
                            (let ((f *xml-application-entity-ref*))
                              (if f
                                  (let ((x (f i)))
                                    (if x
                                        (if (string? x)
                                            x
                                            (error "Entity '&~s;' not valid here"
                                                   i))
                                        (error "Unknown entity '&~s;'" i)))
                                  (error "Unknown entity '&~s;'" i)))))
                       (else
                        (error "Unexpected ~s in textlist" i))))
                    lst)))

;;;
