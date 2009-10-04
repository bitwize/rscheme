(define-class <xml-token-stream> (<object>)
  (properties type: <vector> init-value: '#())
  (name type: <string>)
  (state type: <xml-scanner-state>)
  (port type: <input-port>)
  (buffer type: <string>)
  (offset type: <fixnum> init-value: 0)
  (at-end? type: <boolean> init-value: #f)
  (saved-token-class init-value: #f)
  (saved-token-data init-value: #f)
  (namespaces type: <nsdict>)
  (general-entities type: <symbol-table>)
  (buffer-rollback init-value: #f)
  (parameter-entities type: <symbol-table>))

(define (with-token-substream (self <xml-token-stream>) 
                              (source <input-port>)
                              thunk)
  (let ((save-port (port self))
        (save-buffer (buffer self))
        (save-offset (offset self))
        (save-at-end? (at-end? self)))
    ;;
    (set-port! self source)
    (set-buffer! self "")
    (set-offset! self 0)
    (set-at-end?! self #f)
    ;;
    (bind ((#rest r (thunk)))
      (set-port! self save-port)
      (set-buffer! self save-buffer)
      (set-offset! self save-offset)
      (set-at-end?! self save-at-end?)
      (set-saved-token-class! self #f)  ; popping an entity clears EOF
      (list->values r))))


(define (xml-save-position (self <xml-token-stream>))
  (let ((q (make-dequeue)))
    (set-buffer-rollback! self q)
    (vector (buffer self) (offset self) q)))

(define (xml-restore-position (self <xml-token-stream>) p)
  ;; This only works in the very narrow context in which it
  ;; is currently used -- i.e., to undo at the PORt level the
  ;; reading of the token after the end of the epilog
  (set-buffer-rollback! self #f)
  (set-buffer! self (string-append (substring (vector-ref p 0)
                                              (vector-ref p 1))
                                   (string-join
                                    ""
                                    (vector->list
                                     (dequeue-state (vector-ref p 2))))))
  (set-offset! self 0))

(define-constant xml-token-name 11)
(define-constant xml-token-entity-value 12)
(define-constant xml-token-external-id 13)
(define-constant xml-token-comment 8)
(define-constant xml-token-pi 6)

(define (xml-peek (s <xml-token-stream>) #optional (g default: 0))
  (if (saved-token-class s)
      (values (saved-token-class s)
              (saved-token-data s))
      (bind ((tc t j (xml-scan+ (state s)
                                (buffer s)
                                (offset s)
                                (string-length (buffer s))
                                (at-end? s)
                                g)))
        (case tc
          ((0)
           (refill-buffer s)
           (xml-peek s g))
          ((#f)
           (set-saved-token-class! s 1) ; TC_EOF
           (set-saved-token-data! s #f)
           (values 1))
          (else
           (set-saved-token-class! s tc)
           (set-saved-token-data! s t)
           (set-offset! s j)
           (values tc t))))))

(define (xml-read (s <xml-token-stream>) #optional (g default: 0))
  (bind ((tc td (xml-peek s g)))
    (if (not (eq? tc 1))        ; TC_EOF doesn't go away when read
        (set-saved-token-class! s #f))
    (values tc td)))

(define (xml-skip (s <xml-token-stream>))
  (set-saved-token-class! s #f)
  (values))

(define (make-xml-general-entities)
  (let ((t (make-symbol-table)))
    (table-insert! t 'lt '(entity lt cdata "<"))
    (table-insert! t 'gt '(entity gt cdata ">"))
    (table-insert! t 'amp '(entity gt cdata "&"))
    (table-insert! t 'apos '(entity gt cdata "'"))
    (table-insert! t 'quot '(entity gt cdata "\""))
    t))
    

(define-method open-xml-token-string ((self <string>) #optional base)
  (make <xml-token-stream>
        general-entities: (if base
                              (general-entities base)
                              (make-xml-general-entities))
        parameter-entities: (if base
                                (parameter-entities base)
                                (make-symbol-table))
        namespaces: (if base
                        (namespaces base)
                        (make-namespace-dict))
        name: "literal"
        state: (make <xml-scanner-state>)
        buffer: ""
        port: (open-input-string self)
        at-end?: #f))

(define-method open-xml-token-string ((self <input-port>) #optional nm)
  (make <xml-token-stream>
        general-entities: (make-xml-general-entities)
        parameter-entities: (make-symbol-table)
        namespaces: (make-namespace-dict)
        name: (or nm (name self))
        state: (make <xml-scanner-state>)
        buffer: ""
        port: self
        at-end?: #f))


(define (buffjoin (a <string>) ak b)
  (if (eof-object? b)
      (values a ak)
      (if (eq? ak 0)
          (values (string-append a b) 0)
          (if (eq? ak (string-length a))
              (values b 0)
              (values (string-append (substring a ak) b) 0)))))
  
(define (refill-buffer (self <xml-token-stream>))
  (if (not (at-end? self))
      (bind ((b0 (buffer self))
             (chunk (input-port-read-max (port self) $port-chunk-size))
             (b bj (buffjoin b0 (offset self) chunk)))
        (if (eq? b b0)
            (set-at-end?! self #t))
        (if (and (string? chunk) (buffer-rollback self))
            (dequeue-push-back! (buffer-rollback self) chunk))
        (set-buffer! self b)
        (set-offset! self bj)
        (values))))

(define (xml-readchar (self <xml-token-stream>))
  (let ((i (offset self)))
    (if (< i (string-length (buffer self)))
        (begin
          (set-offset! self (+ 1 i))
          (string-ref (buffer self) i))
        (if (at-end? self)
            $eof-object
            (begin
              (refill-buffer self)
              (xml-peekchar self))))))

(define (xml-peekchar (self <xml-token-stream>))
  (let ((i (offset self)))
    (if (< i (string-length (buffer self)))
        (string-ref (buffer self) i)
        (if (at-end? self)
            $eof-object
            (begin
              (refill-buffer self)
              (xml-peekchar self))))))

(define (match-read-buffer-prefix (self <xml-token-stream>) (str <string>))
  (let* (((n <fixnum>) (string-length str))
         ((i <fixnum>) (offset self))
         ((j <fixnum>) (+ i n))
         ((b <string>) (buffer self)))
    ;;
    (if (<= j (string-length b))
        (if (string=? str (substring b i j))
            (begin
              (set-offset! self j)
              #t)
            #f)
        (if (at-end? self)
            #f
            (begin
              (refill-buffer self)
              (match-read-buffer-prefix self str))))))

(define (match-peek-buffer-prefix (self <xml-token-stream>) (str <string>))
  (let* (((n <fixnum>) (string-length str))
         ((i <fixnum>) (offset self))
         ((j <fixnum>) (+ i n))
         ((b <string>) (buffer self)))
    ;;
    (if (<= j (string-length b))
        (string=? str (substring b i j))
        (if (at-end? self)
            #f
            (begin
              (refill-buffer self)
              (match-peek-buffer-prefix self str))))))

(define (match-read-whitespace (self <xml-token-stream>))
  (let loop ((i 0))
    (if (memq (xml-peekchar self) '(#\space #\tab #\cr #\lf))
        (begin
          (xml-readchar self)
          (loop (+ i 1)))
        (if (eq? i 0)
            #f
            i))))

(define (required-whitespace (self <xml-token-stream>) msg)
  (or (match-read-whitespace self)
      (error "XML Error: ~a" msg)))

(define (required-token (self <xml-token-stream>) goal msg)
  (bind ((tc t (xml-read self goal)))
    (if (not (eq? tc goal))
        (error "XML Error: ~a" msg))
    t))

(define (parse-ge-decl (self <xml-token-stream>))
  (bind ((t (required-token self xml-token-name
                            "[71] Required Name after '<!ENTITY' S")))
    (required-whitespace self "[71] Required S after Name")
    ;;
    (cond
     ((or (match-peek-buffer-prefix self "SYSTEM")
          (match-peek-buffer-prefix self "PUBLIC"))
      (let ((t2 (required-token 
                 self 
                 xml-token-external-id
                 "[75] Expected valid ExternalID following SYSTEM|PUBLIC")))
        (end-decl self 71 "GEDecl")
        ;(format #t "GE ~s X=> ~s\n" t t2)
        (let ((def `(entity ,t external ,(vector-ref t2 0)
                            ,@(if (vector-ref t2 1)
                                  (list (vector-ref t2 1))
                                  '()))))
          (table-insert! (general-entities self) t def)
          (cons def (parse-markupdecl self)))))
     (else
      (let ((t2 (required-token 
                 self 
                 xml-token-entity-value
                 "[73] Required EntityValue or ExternalID for EntityDef")))
        (end-decl self 71 "GEDecl")
        ;(format #t "GE ~s => ~s\n" t (expand-entity-value self t2))
        (let ((def `(entity ,t internal ,(expand-entity-value self t2))))
          (table-insert! (general-entities self) t def)
          (cons def (parse-markupdecl self))))))))

(define (parse-pe-decl (self <xml-token-stream>))
  (required-whitespace self "[72] Required S after '%'")
  (bind ((t (required-token self xml-token-name
                            "[72] Required Name after '<!ENTITY' S '%'")))
    (required-whitespace self "[72] Required S after Name")
    ;;
    (cond
     ((or (match-peek-buffer-prefix self "SYSTEM")
          (match-peek-buffer-prefix self "PUBLIC"))
      (let ((t2 (required-token 
                 self 
                 xml-token-external-id
                 "[75] Expected valid ExternalID following SYSTEM|PUBLIC")))
        (end-decl self 72 "PEDecl")
        ;(format #t "PE ~s X=> ~s\n" t t2)
        (bind-parameter-entity/external self t t2)
        (parse-markupdecl self)))
     (else
      (let ((t2 (required-token 
                 self 
                 xml-token-entity-value
                 "[74] Required EntityValue or ExternalID for PEDef")))
        (end-decl self 72 "PEDecl")
        ;(format #t "PE ~s => ~s\n" t (expand-entity-value self t2))
        (bind-parameter-entity/literal self t t2)
        (parse-markupdecl self))))))

(define (end-decl (self <xml-token-stream>) k what)
  (match-read-whitespace self)
  (if (not (match-read-buffer-prefix self ">"))
      (error "XML Error: ~a Expected '>' at end of ~a" k what)))
  

(define (bind-parameter-entity/literal (self <xml-token-stream>)
                                       (name <symbol>)
                                       value)
  (let ((str (expand-entity-value self value)))
    (table-insert!
     (parameter-entities self)
     name
     (lambda ()
       str))))

(define (bind-parameter-entity/external (self <xml-token-stream>)
                                        (name <symbol>)
                                        (value <vector>))
  (let ((ref (append-path (current-directory)
                          (string->file (vector-ref value 0)))))
    (table-insert!
     (parameter-entities self)
     name
     (lambda ()
       (file->string ref)))))

(define (parse-entitydecl (self <xml-token-stream>))
  (required-whitespace self "[71][72] Required S after '<!ENTITY'")
  (if (match-read-buffer-prefix self "%")
      (parse-pe-decl self)
      (parse-ge-decl self)))

(define (parse-subset-comment self)
  (xml-read self xml-token-comment)
  (parse-markupdecl self))

(define (parse-pe-reference self)
  (bind ((tcname name (xml-read self xml-token-name)))
    (if (not (char=? (xml-readchar self) #\;))
        (error "XMLError [69] PEReference not terminated by ';'"))
    ;(format #t "PEReference[~s]...\n" name)
    (append 
     (parse-markupdecl 
      (let ((f (or (table-lookup (parameter-entities self) name)
                   (error "XMLError: Parameter entity '%~a;' is unknown" 
                          name))))
        (open-xml-token-string (f) self)))
     (parse-markupdecl self))))

(define (parse-elementdecl self)
  (xml-error self 45 "<!ELEMENT...> not yet supported"))

(define (parse-attlistdecl self)
  (xml-error self 52 "<!ATTLIST...> not yet supported"))
  
(define (parse-notationdecl self)
  (xml-error self 82 "<!NOTATION...> not yet supported"))

(define (parse-subset-pi self)
  (xml-error self 29 "<?...?> inside document type subset not yet supported"))
  

(define (parse-markupdecl (self <xml-token-stream>))
  (cond
   ((match-read-buffer-prefix self "<!ELEMENT")
    (parse-elementdecl self))
   ((match-read-buffer-prefix self "<!ATTLIST")
    (parse-attlistdecl self))
   ((match-read-buffer-prefix self "<!ENTITY")
    (parse-entitydecl self))
   ((match-read-buffer-prefix self "<!NOTATION")
    (parse-notationdecl self))
   ((match-peek-buffer-prefix self "<?")
    (parse-subset-pi self))
   ((match-peek-buffer-prefix self "<!--")
    (parse-subset-comment self))
   ((match-peek-buffer-prefix self "%")
    (parse-pe-reference self))
   ((match-read-buffer-prefix self "]")
    (match-read-whitespace self)
    (if (match-read-buffer-prefix self ">")
        '()
        (xml-error self 28 "Expected '>' after ']'")))
   ((match-read-whitespace self)
    (parse-markupdecl self))))

(define (expand-entity-value (self <xml-token-stream>) lst)
  (string-join
   ""
   (map (lambda (i)
          (cond
           ((vector? i)
            (substring (vector-ref i 0)
                       (vector-ref i 1)
                       (vector-ref i 2)))
           ((char? i)       ; nb, `i' may be a unicode-char
            (string i))
           ((symbol? i)
            (string-append "&" (symbol->string i) ";"))
           ((pair? i)
            (assert (eq? (car i) '%))
            (cond
             ((table-lookup (parameter-entities self) (cdr i))
              => (lambda (f)
                   (f)))
             (else
              (error "Unknown parameter entity '%~s;'" (cdr i)))))
           (else
            (error "strange ~s in entity-value" i))))
        lst)))
  


(define (t)
  (define (z s)
    (parse-markupdecl (open-xml-token-string s)))
  ;;
  (z "<!ENTITY foo 'bar'>")
  (z "<!ENTITY foo SYSTEM 'foo.xml'>")
  (z "<!ENTITY % foo 'hahaha'>")
  (z "<!ENTITY % foo SYSTEM \"more's-the-core.dtd\">")
  (z "<!ENTITY ie \"<i>i.e.</i>\">")
  ; Examples from XML-1.0 section 4.5
  (z "<!ENTITY % pub \"&#xc9;ditions Gallimard\" >")
  (z "<!ENTITY  book \"La Pesta: Albert Camus, &#xA9; 1947 %pub;. &rights;\" >"))

(define (sxml-parse (self <xml-token-stream>))
  ;;
  (define (err re fmt . args)
    (apply xml-error self re fmt args))
  ;;
  (define (debug-parse msg . args)
    (sxml:debug
     (bind ((tc t (xml-peek self)))
       (format #t "SXML PARSE ")
       (apply format #t msg args)
       (format #t " (NEXT [~s] ~s)\n" tc t))))
  ;;
  (define entity-resolver
    (if (get-property self 'expand-entities? #t)
        (lambda (ent)
          (cond
           ((table-lookup (general-entities self) ent)
            => (lambda (def)
                 (case (caddr def)
                   ((external)
                    (let ((xid (cadddr def)))
                      (call-with-input-file xid 
                        (lambda (port)
                          (expanded-entity port ent)))))
                   ((internal)
                    (expanded-entity 
                     (open-input-string (cadddr def))
                     ent))
                   ;; note that there is no way to declare CDATA entities
                   ;; in XML, but we use them as a faster way to do the
                   ;; standard entities like &lt;
                   ((cdata)
                    (cadddr def)))))
           ((*xml-application-entity-ref* ent)
            => (lambda (def)
                 ;; the application is expected to supply 
                 ;; pre-parsed (SXML) data
                 def))
           ;;
           (else
            (format #t "WARNING: Could not resolve '&~a;'\n" ent)
            `((*ENTITY* ,ent)))))
        resolve-no-entities))
  ;;
  (define (sxml:xml t)
    (let ((pi (expand-text (list (pi t)))))
      `(*XML* ,pi)))
  ;;
  (define (sxml:pi t)
    (let ((tgt (target t))
          (pi (expand-text (list (pi t)))))
      `(*PI* ,tgt ,pi)))
  ;;
  (define (sxml:empty t)
    (bind ((gi attrs d2 (namespace-transformation t)))
      (if (null? attrs)
          (list gi)
          `(,gi (@ ,@attrs)))))

  (define (sxml:comment t)
    `(*COMMENT* ,(expand-text (list (item t)))))
  ;;
  (define (sxml:doctype t)
    (let ((sub (if (subtype t)
                   (cons 'SUBTYPE (parse-markupdecl self))
                   '())))
      (cond
       ((pubid t)
        `(*DECL* DOCTYPE
                 ,(name t)
                 PUBLIC
                 ,(pubid t)
                 ,(sysid t)
                 ,@sub))
       ((sysid t)
        `(*DECL* DOCTYPE
                 ,(name t)
                 SYSTEM
                 ,(sysid t)
                 ,@sub))
       (else
        `(*DECL* DOCTYPE
                 ,(name t)
                 ,@sub)))))
  ;;
  (define (prolog)
    (debug-parse "prolog")
    (bind ((tc t (xml-peek self)))
      (case tc
        ((6)                 ; PI
         (xml-skip self)
         (prolog1 (list (sxml:pi t))))
        ;;
        ((14)                   ; XML
         (xml-skip self)
         (prolog1 (list (sxml:xml t))))
        ;;
        ((9) ; TC_WHITESPACE
         (xml-skip self)        ; discard it
         (prolog1 '()))         ; an <?xml ...> is no longer acceptable
        ;;
        ((7) ; TC_DOCTYPE_DECL
         (prolog2 '()))
        ;;
        (else
         (prolog1 '())))))
  ;;
  (define (prolog1 preamble)
    ;; We're in between the <?xml...> and any <!DOCTYPE...> or later stuff
    (debug-parse "prolog1")
    ;;
    (bind ((tc t (xml-peek self)))
      (case tc
        ((14)           ; TC_XML
         (err 22 "Misplaced XMLDecl"))
        ((6)            ; TC_PI
         (xml-skip self)
         (prolog1 (cons (sxml:pi t) preamble)))
        ((9)            ; TC_WHITESPACE
         (xml-skip self)
         (prolog1 preamble))
        ((8)            ; TC_COMMENT
         (xml-skip self)
         (prolog1 (cons (sxml:comment t) preamble)))
        ((1)    ; TC_EOF
         ;; an EOF anywhere other than after some whitespace
         ;; constitutes an error
         (if (pair? preamble)
             (err 0 "Misplaced EOF after preamble content")
             $eof-object))
        ;;
        (else
         (prolog2 preamble)))))
  ;;
  (define (prolog2 preamble)
    (debug-parse "prolog2")
    ;;
    (bind ((tc t (xml-peek self)))
      (case tc
        ((7)
         (xml-skip self)
         (prolog3 (cons (sxml:doctype t) preamble)))
        (else
         (rootelem preamble)))))
  ;;
  (define (prolog3 preamble)
    (debug-parse "prolog3")
    ;;
    (bind ((tc t (xml-peek self)))
    (case tc
      ((6)              ; TC_PI
       (xml-skip self)
       (prolog3 (cons (sxml:pi t) preamble)))
      ((9)              ; TC_WHITESPACE
       (xml-skip self)
       (prolog3 preamble))
      ((8)              ; TC_COMMENT
       (xml-skip self)
       (prolog3 (cons (sxml:comment t) preamble)))
      (else
       (rootelem preamble)))))
  ;;
  (define (rootelem preamble)
    (let ((ontop (if (null? preamble)
                     (lambda (root)
                       `(*TOP* ,root))
                     (lambda (root)
                       `(*TOP* ,@(reverse preamble) ,root)))))
      (debug-parse "rootelem")
      ;;
      (bind ((tc t (xml-peek self)))
        (case tc
          ((3)          ; TC_START_ELEMENT
           (let ((root (element)))
             (epilog)   ; discard the epilog
             (ontop root)))
          ((2)          ; TC_EMPTY_ELEMENT
           (xml-skip self)
           (epilog)
           (ontop (sxml:empty t)))
          (else
           (err 1 "Required element after prolog"))))))
  ;;
  (define (namespace-transformation (t <xml-element>))
    (bind ((new-dict reglist nlist (update-nsdict (namespaces self)
                                                  (attributes t)))
           (g (xform-gi-using-nsdict (gi t)
                                     (namespace t)
                                     new-dict)))
      (values g reglist new-dict)))
  ;;
  (define (namespace-transformation-end (t <xml-end-element>))
    (xform-gi-using-nsdict (gi t) (namespace t) (namespaces self)))
  ;;
  (define (expanded-entity src ent)
    (with-token-substream
     self
     src
     (lambda ()
       (bind ((children tc t (children-sequence* (~ "&~a;" ent))))
         ;(format #t "~s ==> ~s\n" ent children)
         (case tc
           ((4) ; TC_END_ELEMENT
            (err 0 "Unmatched </~a> in entity" 
                 (namespace-transformation-end t)))
           ((1) ; TC_EOF
            children))))))
  ;;
  (define (children-sequence msg d2)
    (let ((save-namespaces (namespaces self)))
      (set-namespaces! self d2)
      (bind ((ch tc t endgi (children-sequence* msg)))
        (set-namespaces! self save-namespaces)
        (values ch tc t endgi))))
  ;;
  (define (children-sequence* msg)
    (let loop ((children '()))
        (debug-parse "element child of ~a" msg)
        (bind ((tc t (xml-peek self)))
          (case tc
            ((3)        ; TC_START_ELEMENT
             (loop (cons (element) children)))
            ((2)        ; TC_EMPTY_ELEMENT
             (xml-skip self)
             (loop (cons (sxml:empty t) children)))
            ((5)        ; TC_TEXT
             (xml-skip self)
             (loop (append (expand-text-parsed (items t)
                                               entity-resolver)
                           children)))
            ((1 #f)        ; TC_END_ELEMENT, TC_EOF
             (values (reverse! children) tc t #f))
            ((4)        ; TC_END_ELEMENT, TC_EOF
             (values (reverse! children) tc t
                     (namespace-transformation-end t)))
            ((6)        ; TC_PI
             (xml-skip self)
             (loop (cons (sxml:pi t) children)))
            ((7)        ; TC_DOCTYPE_DECL
             (err 0 "Misplaced <!DOCTYPE>"))
            ((8)        ; TC_COMMENT
             (xml-skip self)
             (loop (cons (sxml:comment t) children)))
            ((9)        ; TC_WHITESPACE
             ;; c.f. 2.10 "White Space Handling"...
             ;; we must pass it on to the application
             (xml-skip self)
             (loop (append (expand-text-parsed (items t)) children)))
            (else
             (err 0 "Unexpected token type <~s>" tc))))))
  ;;
  (define (element)
    (debug-parse "element")
    (bind ((tc t (xml-read self))
           (_ (assert (eq? tc 3)))
           (gi0 a0 d2 (namespace-transformation t))
           (children tc t gi2 (children-sequence (~ "gi <~a>" gi0) d2)))
      ;;
      (case tc
        ((4)        ; TC_END_ELEMENT
         (xml-skip self)
         (if (eq? gi2 gi0)
             (if (null? a0)
                 `(,gi0 ,@children)
                 `(,gi0 (@ ,@a0) ,@children))
             (err 0 "GI mismatch ~s at end not equal ~s" gi2 gi0)))
        ((1)        ; TC_EOF
         (err 0 "Unexpected EOF; <~s> still open" gi0)))))
  ;;
  (define (epilog)
    (debug-parse "epilog")
    (bind ((ptr (xml-save-position self))
           (tc t (xml-peek self)))
      (case tc
        ((6 8 9) ; ignore PI's, COMMENT's, and SPACE in the epilog
         (xml-skip self)
         (epilog))
        (else
         (xml-restore-position self ptr)
         (values)))))
  ;;
  (prolog))

#|
(define (parse-doc s)
  (print (cadr (values->list (xml-read s))))         ; PI <?xml ...>
  (print (cadr (values->list (xml-read s))))         ; TEXT "\n\n"
  (print (cadr (values->list (xml-read s))))         ; DOCTYPE <!DOCTYPE ... [
  (parse-markupdecl s))
|#

(define (tf)
  (call-with-input-file
      "test-02.xml"
    (lambda (port)
      (sxml-parse (open-xml-token-string port)))))

