,(use regex
      paths
      tables)

(define-class <sgml-node> (<object>)
  (properties init-value: '#())
  parent)

(define-class <sgml-entity-decl> (<sgml-node>)
  origin
  name
  type
  subtype
  f-record
  s-record)


(define entity-f-record (reg-expr->proc
                         '(seq "<OSFILE SOIBASE='"
                               (save (* (not #\')))
                               "'>"
                               (save (* any)))))

(define-method entity-os-path ((self <sgml-entity-decl>))
  (bind ((s e soibase name (entity-f-record (f-record self))))
    (pathname->os-path
     (append-path (file-directory
                   (append-path
                    (current-absolute-directory)
                    (string->file soibase)))
                  (string->file name)))))

(define-class <sgml-entity-ref> (<sgml-node>)
  name
  (subject type: <sgml-entity-decl>))

(define-class <sgml-text> (<sgml-node>)
  origin
  content)

(define-class <sgml-pi> (<sgml-node>)
  origin
  content)

(define-class <sgml-element> (<sgml-node>)
  origin
  name
  attributes
  content)

(define *element-subclass-table* (make-string-ci-table))

(define-macro (define-sgml-element name)
  (let ((class-name (symbol-append "<" name ">")))
    `(begin
       (define-class ,class-name (<sgml-element>))
       (table-insert! *element-subclass-table* 
                      ,(symbol->string name)
                      ,class-name))))
                    
       
(define-class <sgml-document> (<sgml-node>)
  origin
  attributes
  content
  entities
  (id-index init-function: make-string-table))

(define-class <sgml-attribute> (<sgml-node>)
  origin
  name
  type
  value)

(define-method origin-system-file ((self <sgml-element>))
  (cadr (origin self)))

(define-method write-object ((self <sgml-text>) port)
  (format port "#[<sgml-text> ~a:~d (~d chars)]"
          (cadr (origin self))
          (car (origin self))
          (string-length (content self))))

(define-method write-object ((self <sgml-element>) port)
  (format port "#[<~a> ~a:~d]"
          (name self)
          (cadr (origin self))
          (car (origin self))))

(define internal-sdata (reg-expr->proc 
                        '(seq #\\
                              #\|
                              #\[
                              (save (+ (not space)))
                              (* space)
                              #\]
                              #\\
                              #\|)))

(define internal-sdata-char (reg-expr->proc 
                             '(seq #\\
                                   #\|
                                   (save any)
                                   #\\
                                   #\|)))

(define (parse-sgml-value str)
  (let loop ((i 0)
             (prev '()))
    (let ((k (string-search str #\\ i)))
      (if k
          (let ((f (string-ref str (+ k 1))))
            (case f
              ((#\n)
               (loop (+ k 2) (cons* "\n" (substring str i k) prev)))
              ((#\\)
               (loop (+ k 2) (cons* "\\" (substring str i k) prev)))
              ((#\|)
               (bind ((s e label (internal-sdata str k)))
                 (if s
                     (loop e
                           (cons*
                            (cond
                             ((string=? label "amp") "&")
                             ((string=? label "lt") "<")
                             ((string=? label "gt") ">")
                             ((string=? label "quot") "\"")
                             ;; Unfortunately, these are PostScript
                             ;; font encoding values and not unicodes...
                             ((string=? label "rArr") "\210")
                             ((string=? label "ndash") "\261")
                             ((string=? label "mdash") "\320")
                             ((string=? label "trade") "\231")
                             (else
                              (error "unknown internal SDATA entity &~a;" label)))
                            (substring str i k) 
                            prev))
                     (bind ((s e ch (internal-sdata-char str k)))
                       (if s
                           (loop e (cons* ch (substring str i k) prev))
                           (error "wierd sdata-stuff: ~#@*40s"
                                  (substring str k)))))))
              ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7)
               (loop (+ k 4)
                     (cons* (string (integer->char
                                     (string->number
                                      (substring str (+ k 1) (+ k 4))
                                      8)))
                            (substring str i k)
                            prev)))
              ((#\# #\%)
               (error "unhandled escape sequence at ~s" (substring str k)))
              (else
               (error "unknown escape sequence at ~s" (substring str k)))))
          (if (null? prev)
              str
              (string-join "" (reverse! (cons (substring str i) prev))))))))

(define *sgmls-command-line* "onsgmls")

(define (read-sgml-files . files)
  (if (not (every? string? files))
      (error "read-sgml-files: Expected one or more filename (<string>) argument,\ngot ~s" files))
  (let* ((p (open-input-process (format #f "~a -oline ~j" 
                                        *sgmls-command-line*
                                        files)))
         (doc (read-sgml-element p)))
    (close-input-port p)
    doc))
  
(define (read-sgml-element port)
  (let ((entity-dict (make-string-table))
        (finfo #f)
        (sinfo #f)
        (apending '())
        (id-ix (make-string-table)))
    ;;
    (let loop ((stack (list (make <sgml-document>
                                  parent: #f
                                  origin: #f
                                  entities: entity-dict
                                  attributes: '()
                                  id-index: id-ix
                                  content: '())))
               (location #f))
      (let ((l (read-line port)))
        (if (string? l)
            (case (string-ref l 0)
              ((#\A) 
               (if (not (implied-attribute l))
                   (set! apending (cons l apending)))
               (loop stack location))
              ((#\() (let* ((n (substring l 1))
                            (c (or (table-lookup *element-subclass-table* n)
                                   <sgml-element>))
                            (e (make c
                                     parent: (car stack)
                                     origin: location
                                     name: (substring l 1)
                                     attributes: '()
                                     content: '())))
                       (set-attributes! 
                        e
                        (map (lambda (a)
                               (parse-attribute e location a id-ix))
                             apending))
                       (set! apending '())
                       (loop (cons e stack) location)))
              ((#\)) (assert (string=? (name (car stack)) (substring l 1)))
                     (set-content! (car stack) (reverse! (content (car stack))))
                     (add-content! (cadr stack) (car stack))
                     (loop (cdr stack) location))
              ((#\L) (let ((x (string-split (substring l 1) #\space)))
                       (if (pair? (cdr x))
                           (loop stack (list (string->number (car x)) (cadr x)))
                           (loop stack (cons (string->number (car x)) (cdr location))))))
              ((#\-) (add-content! (car stack)
                                   (make <sgml-text>
                                         parent: (car stack)
                                         origin: location
                                         content: (parse-sgml-value
                                                   (substring l 1))))
                     (loop stack location))
              ((#\?)
               (add-content! 
                (car stack)
                (make <sgml-pi>
                      parent: (car stack)
                      origin: location
                      content: (let ((n (string-length l)))
                                 ;; in XML, a PI ends with "?>"
                                 (if (char=? #\? (string-ref l (- n 1)))
                                     (substring l 1 (- n 1))
                                     (substring l 1)))))
               (loop stack location))
              ((#\N #\p)          ; these have something to do with notation decls
               (loop stack location))
              ((#\f)
               (set! finfo (substring l 1))
               (loop stack location))
              ((#\s)
               (set! sinfo (substring l 1))
               (loop stack location))
              ((#\E)
               (bind ((name type subtype (list->values (string-split
                                                        (substring l 1)
                                                        #\space))))
                 (table-insert! entity-dict
                                name
                                (make <sgml-entity-decl>
                                      parent: #f
                                      origin: location
                                      name: name
                                      type: type
                                      subtype: subtype
                                      f-record: finfo
                                      s-record: sinfo))
                 (set! sinfo #f)
                 (set! finfo #f)
                 (loop stack location)))
              ((#\C)
               (assert (null? (cdr stack)))
               (car stack))
              ((#\&)
               (let* ((n (substring l 1))
                      (e (table-lookup entity-dict n)))
                 (add-content! (car stack) (make <sgml-entity-ref>
                                                 parent: (car stack)
                                                 name: n
                                                 subject: e))
                 (loop stack location)))
              (else
               (error "What? ~s" l)))
            (if (pair? (cdr stack))
                (error "Too much left on stack: ~s" stack)
                (car stack)))))))

(define (t)
  (call-with-input-file "/tmp/a" read-sgml-element))

(define (add-content! recipient item)
  (set-content! recipient (cons item (content recipient))))

(define (add-attribute! recipient attr)
  (if attr
      (set-attributes! recipient (append! (attributes recipient) (list attr)))))

(define-method get-attribute ((self <sgml-element>) attr-name)
  (let loop ((a (attributes self)))
    (if (null? a)
        #f
        (if (string-ci=? (name (car a)) attr-name)
            (car a)
            (loop (cdr a))))))

(define-method get-attribute-value ((self <sgml-element>) attr-name)
  (let ((a (get-attribute self attr-name)))
    (if a
        (case (type a)
          ((entity)
           (table-lookup (entities (get-document-node self))
                         (value a)))
          (else (value a)))
        #f)))

(define (get-document-node n)
  (let loop ((n n))
    (if (instance? n <sgml-document>)
        n
        (loop (parent n)))))

(define (find-element-by-id from id)
  (let ((doc (get-document-node from)))
    (or (table-lookup (id-index doc) id)
        (error "~s: No element with id ~s" from id))))

(define (find-elements-by-id from ids)          ; `ids' is an IDREFS attribute
  (if (string=? ids "")
      '()
      (map (lambda (id)
             (find-element-by-id from id))
           (string-split ids #\space))))

(define implied-attribute (reg-expr->proc '(seq #\A
                                                (save (+ alpha))
                                                #\space
                                                "IMPLIED")))

(define specified-attribute (reg-expr->proc '(seq #\A
                                                  (save (+ alpha))
                                                  #\space
                                                  (save (or "TOKEN"
                                                            "CDATA"
                                                            "NOTATION"
                                                            "ENTITY"))
                                                  #\space
                                                  (save (* any)))))

(define (parse-attribute parent-node o l id-ix)
  (bind ((s e name (implied-attribute l)))
    (if s
        #f
        (bind ((s e name mode value (specified-attribute l)))
          (if s
              (let ((atype (cond
                            ((string=? mode "TOKEN") 'token)
                            ((string=? mode "CDATA") 'cdata)
                            ((string=? mode "NOTATION") 'notation)
                            ((string=? mode "ENTITY") 'entity)
                            (else (error "unknown mode ~s" mode)))))
                (if (and (eq? atype 'token) (string-ci=? name "id"))
                    (begin
                      (if (table-lookup id-ix value)
                          (error "~s: Duplicate id ~s" parent-node value))
                      (table-insert! id-ix value parent-node)))
                (make <sgml-attribute>
                      parent: parent-node
                      origin: o
                      name: name
                      type: atype
                      value: value))
              (error "cannot parse attribute line ~s" l))))))

(define-method plain-content ((self <list>))
  (string-join "" (map plain-content self)))

(define-method plain-content ((self <sgml-text>))
  (content self))

(define-method plain-content ((self <sgml-element>))
  (plain-content (content self)))
  
(define (xpath base . path)
  (let ((l (apply xpath* base path)))
    (case (length l)
      ((0) (error "~s: No ~s" base path))
      ((1) (car l))
      (else (error "~s: Multiple ~s" base path)))))

(define (xpath? base . path)
  (handler-case
   (pair? (apply xpath* base path))
   ((<condition>)
    #f)))

(define (xpath* base . path)
  (let loop ((n base)
             (p path))
    (if (null? p)
        n
        (cond
         ((eq? (car p) 'cdata)
          (if (null? (cdr p))
              (list (collect-text n))
              (error "~s: cdata not at end in ~s" base path)))
         ((eq? (car p) '*)
          (exhaustive-search-xpath n (cdr p)))
         (else
          (let ((sub (select (lambda (candidate)
                               (and (instance? candidate <sgml-element>)
                                    (string-ci=? (name candidate) (car p))))
                             (content n))))
            (if (null? (cdr p))
                sub
                (if (null? sub)
                    '();(error "~s: No ~s at ~s" base path (car p))
                    (if (pair? (cdr sub))
                        (if (number? (cadr p))
                            (loop (list-ref sub (cadr p)) (cddr p))
                            (error "~s: Too many ~s" base (car p)))
                        (loop (car sub) (cdr p)))))))))))

(define-method collect-text ((self <sgml-text>))
  (content self))

(define-method collect-text ((self <sgml-element>))
  (string-join "" (map collect-text (content self))))

(define (exhaustive-search-xpath node path)
  (if (null? path)
      '()
      (let ((here (apply xpath* node path))
            (below (apply
                    append
                    (map (lambda (c)
                           (if (instance? c <sgml-element>)
                               (begin
                                 ;(format #t "recursively look in ~s\n" c)
                                 (exhaustive-search-xpath c path))
                               '()))
                         (content node)))))
        (append here below))))

(define (find-ancestor node type)
  (if (instance? node type)
      node
      (if (parent node)
          (find-ancestor (parent node) type)
          #f)))

