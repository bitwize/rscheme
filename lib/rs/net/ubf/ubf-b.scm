
(define-safe-glue (fullhash (str <string>))
{
  UINT_32 x = crc_hash( string_text( str ), string_length( str ), 0 );
  REG0 = uint_32_compact( x );
  RETURN1();
})


(define (scan port)
  (let ((ch (peek-char port)))
    (if (eof-object? ch)
        #f
        (case ch
          ((#\")
           (read port))
          ((#\- #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)
           (read port))
          ((#\[) (read-char port) '%open-bracket)
          ((#\]) (read-char port) '%close-bracket)
          ((#\() (read-char port) '%open-paren)
          ((#\)) (read-char port) '%close-paren)
          ((#\{) (read-char port) '%open-brace)
          ((#\}) (read-char port) '%close-brace)
          ((#\.) (read-char port) '%dot)
          ((#\,) (read-char port) '%comma)
          ((#\+) (read-char port) '%plus)
          ((#\=) (read-char port) '%equals)
          ((#\|) (read-char port) '%vertical-bar)
          ((#\;) (read-char port) '%semicolon)
          ((#\%) (read-line port) (scan port))
          ((#\space #\tab #\cr #\lf)
           (read-char port)
           (scan port))
          (else
           (if (char-alphabetic? ch)
               (read port)
               (error "Bad UBF(B) scan character: ~s" ch)))))))


(define (match-ident x)
  (and (symbol? x)
       (not (char=? (string-ref (symbol->string x) 0) #\%))
       x))

(define (match-string x)
  (and (string? x) x))

(define-macro (ubf-grammar . p)
  `(grammar 
    (terminal ident
              string
              "TYPE"
              "NAME"
              "VSN"
              "TYPES"
              "IMPORTS"
              "EXPORTS"
              ("+" %plus)
              ("." %dot)
              ("," %comma)
              (";" %semicolon)
              ("(" %open-paren)
              (")" %close-paren)
              ("=" %equals)
              ("[" %open-bracket)
              ("]" %close-bracket)
              ("{" %open-brace)
              ("}" %close-brace)
              ("|" %vertical-bar))
    ,@p))

(define *UBF-B*
  (ubf-grammar
   ;;
   (start ((* TOP-LEVEL)))
   ;;
   (TOP-LEVEL ("+" "TYPE" x:TYPE-DECL ".") (list 'types x))
   (TOP-LEVEL ("+" "TYPES" x:TYPES-SECTION ".") (cons 'types x))
   (TOP-LEVEL ("+" "NAME" "(" s:string ")" ".") `(meta (name ,s)))
   (TOP-LEVEL ("+" "EXPORTS" x:EXPORTS-SECTION ".") (cons 'exports x))
   (TOP-LEVEL ("+" "IMPORTS" x:IMPORTS-SECTION ".") (cons 'imports x))
   ;;
   (TYPES-SECTION (h:TYPE-DECL) (list h))
   (TYPES-SECTION (h:TYPE-DECL ";" t:types-ne) (cons h t))
   (TYPES-SECTION () '())
   ;;
   (types-ne (h:TYPE-DECL) (list h))
   (types-ne (h:TYPE-DECL ";" t:types-ne) (cons h t))
   ;;
   ;;----------------------------------------
   ;;
   (EXPORTS-SECTION (h:EXPORT-DECL) (list h))
   (EXPORTS-SECTION (h:EXPORT-DECL ";" t:exports-ne) (cons h t))
   (EXPORTS-SECTION () '())
   ;;
   (exports-ne (h:EXPORT-DECL) (list h))
   (exports-ne (h:EXPORT-DECL ";" t:exports-ne) (cons h t))
   ;;
   (EXPORT-DECL (n:ident "(" ")") n)
   ;;----------------------------------------
   ;;
   (IMPORTS-SECTION (h:IMPORT-DECL) (list h))
   (IMPORTS-SECTION (h:IMPORT-DECL ";" t:imports-ne) (cons h t))
   (IMPORTS-SECTION () '())
   (imports-ne (h:IMPORT-DECL) (list h))
   (imports-ne (h:IMPORT-DECL ";" t:imports-ne) (cons h t))
   ;;
   (IMPORT-DECL (s:string) s)
   ;;

   (TYPE-DECL (n:ident "(" ")" "=" t:TYPE)
              (list 'deftype n t))
   (TYPE ("[" t:TYPE "]")
         (list 'list t))
   (TYPE (t1:TYPE "|" t2:TYPE)
         (list 'or t1 t2))
   (TYPE (n:ident)
         (list 'quote n))
   (TYPE (n:ident "(" ")")
         (case n
           ((int) 'primitive:integer)
           ((string) 'primitive:string)
           ((constant) 'primitive:constant)
           ((bin) 'primitive:binary)
           (else n)))
   ;;
   (TYPE ("{" body:typelist "}")
         (cons 'struct body))
   (typelist () '())
   (typelist (h:TYPE "," t:typelistne) (cons h t))
   (typelist (h:TYPE) (list h))
   (typelistne (h:TYPE "," t:typelistne) (cons h t))
   (typelistne (h:TYPE) (list h))
))

(define (base-ubf-grammar)
  *UBF-B*)


(define (parse-ubf-b port #optional (grammar default: *UBF-B*))
  (parse-using-grammar (lambda () 
                         (let* ((l (input-port-line-number port))
                                (token (scan port)))
                           ;(format #t "~d: ~s\n" l token)
                           token))
                       grammar))

 
            
(define (pu str)
  (let ((decl (parse-ubf-b (open-input-string str))))
    (pp (car decl))
    decl))

;;;

(define-class <ubf-b-spec> (<object>)
  (properties init-value: '#())
  (static-symtab init-function: make-symbol-table)
  (type-table init-function: make-symbol-table)
  (exported-types init-value: '())
  (imported-modules init-value: '()))

(define-thread-var *ubf-context* #f)

(define-method emit-c-checker ((type <pair>) expr name fail incr)
  (case (car type)
    ((quote)
     (format #t "  if (~a != ~a) { ~a }\n" expr (symname (cadr type)) fail))
    ((list)
     (format #t "  { /* check list */\n")
     (format #t "    UBFObject *p = ~a;\n" expr)
     (format #t "    while (UBF_PAIR_Q( p )) {\n")
     (format #t "      UBFObject *e = UBF_FIRST( p );\n")
     (emit-c-checker (cadr type) "e" name fail incr)
     (format #t "      p = UBF_REST( p );\n")
     (format #t "    }\n")
     (format #t "    if (!UBF_NULL_Q( p )) { ~a }\n" fail)
     (format #t "  }\n"))
    ((or)
     (let ((L0 (~ "L~d" (incr)))
           (L1 (~ "L~d" (incr))))
       ;;
       (emit-c-checker (cadr type) expr name (~ "goto ~a;" L1) incr)
       (format #t "    goto ~a;\n" L0)
       (format #t "  ~a:\n" L1)
       (emit-c-checker (caddr type) expr name fail incr)
       (format #t "  ~a: ;\n" L0)))
    ((struct)
     (format #t "  if (!UBF_STRUCT_Q( ~a )) { ~a }\n" expr fail)
     (format #t "  if (UBF_STRUCT_LEN( ~a ) != ~d) { ~a }\n" 
             expr
             (length (cdr type))
             fail)
     (if (pair? (cdr type))
         (let ((mvar (~ "m~d" (incr))))
           (format #t "  { /* check structure elements */\n")
           (format #t "    UBFObject *~a;\n" mvar)
           (for-each (lambda (i subtype)
                       (format #t "    ~a = UBF_STRUCT_REF( ~a, ~d );\n" 
                               mvar expr i)
                       (emit-c-checker subtype mvar name fail incr))
                     (range (length (cdr type)))
                     (cdr type))
           (format #t "  }\n"))))
    (else
     (error "bad compound: ~s" type))))

(define-method emit-c-checker ((type <symbol>) expr name fail incr)
  (case type
    ((primitive:integer)
     (format #t "  if (!UBF_INTEGER_Q( ~a )) { ~a }\n" expr fail))
    ((primitive:string)
     (format #t "  if (!UBF_STRING_Q( ~a )) { ~a }\n" expr fail))
    ((primitive:constant)
     (format #t "  if (!UBF_CONSTANT_Q( ~a )) { ~a }\n" expr fail))
    ((primitive:binary)
     (format #t "  if (!UBF_BINARY_Q( ~a )) { ~a }\n" expr fail))
    (else
     (format #t "  if (!check_~a( ~a )) { ~a }\n" type expr fail))))

(define (symname (s <symbol>))
  (let ((v (format #f "SYM_~a" s)))
    (table-insert! (static-symtab *ubf-context*) s v)
    v))

(define (with-output-to-c-file file thunk)
  (format #t "generating: ~s" file)
  (flush-output-port (current-output-port))
  ;;
  (with-output-to-file file thunk)
  (format #t " [ok]\n"))

(define (with-output-to-h-file file tag thunk)
  (with-output-to-c-file
   file
   (lambda ()
     (format #t "#ifndef _H_~a\n" tag)
     (format #t "#define _H_~a\n" tag)
     (thunk)
     (format #t "#endif /* _H_~a */\n" tag))))

;(define $symtab-capacity 1667)
(define $symtab-capacity 64)

#|
(define (checkwrap type predicate)
  predicate)
|#

(define (checkwrap type predicate)
  (lambda (actual)
    (if (predicate actual)
        #t
        (begin
          (format #t "~s is not of type: ~s\n" actual type)
          #f))))

(define-method gen-scheme-checker ((type <pair>) tbl)
  (case (car type)
    ((quote)
     (checkwrap type (lambda (x)
                       (eq? x (cadr type)))))
    ((list)
     (let ((f (gen-scheme-checker (cadr type) tbl)))
       (checkwrap type (lambda (x)
                         (and (list? x)
                              (every? f x))))))
    ((or)
     (let ((a (gen-scheme-checker (cadr type) tbl))
           (b (gen-scheme-checker (caddr type) tbl)))
       (checkwrap type (lambda (x)
                         (or (a x) (b x))))))
    ((struct)
     (let ((m (list->vector (map (lambda (s)
                                   (gen-scheme-checker s tbl))
                                 (cdr type)))))
       (checkwrap type
                  (lambda (x)
                    (scheme-struct-check m x)))))
    (else
     (let ((lup (table-lookup *scheme-compound-check-generator* (car type))))
       (if lup
           (lup type tbl)
           (error "bad compound: ~s" type))))))

(define *scheme-compound-check-generator* (make-symbol-table))
(define *scheme-leaf-check-generator* (make-symbol-table))

(define (scheme-struct-check (proto <vector>) given)
  (and (vector? given)
       (= (vector-length given) (vector-length proto))
       (let loop ((i 0))
         (if (eq? i (vector-length proto))
             #t
             (if ((vector-ref proto i) (vector-ref given i))
                 (loop (+ i 1))
                 #f)))))

(define-method gen-scheme-checker ((type <symbol>) tbl)
  (case type
    ((primitive:integer)
     (checkwrap type integer?))
    ((primitive:string)
     (checkwrap type string?))
    ((primitive:constant)
     (checkwrap type symbol?))
    ((primitive:binary)
     (checkwrap type (lambda (x)
                       (instance? x <byte-vector>))))
    (else
     (let ((lup (table-lookup *scheme-leaf-check-generator* type)))
       (if lup
           (lup type tbl)
           (if (table-lookup tbl type)
               ;; checker already generated...
               (table-lookup tbl type)
               ;; insert a recursion-avoidance stub, until we can build
               ;; the real thing...
               (begin
                 (table-insert! tbl type (lambda (x)
                                           ((table-lookup tbl type) x)))
                 (let ((f (gen-scheme-checker
                           (caddr
                            (or (table-lookup (type-table *ubf-context*) type)
                                (error "UBF(B) type `~s' is not known" type)))
                           tbl)))
                   (table-insert! tbl type f)
                   f))))))))

(define (generate-ubf-checker/scheme (self <ubf-b-spec>) type)
  (thread-let ((*ubf-context* self))
    (let ((tbl (make-symbol-table)))
      (gen-scheme-checker type tbl))))


(define (generate-ubf-checkers/c (self <ubf-b-spec>))
  (thread-let ((*ubf-context* self))
    ;;
    (with-output-to-h-file 
     "ubfchk.h"
     "UBFCHK"
     (lambda ()
       ;;
       (format #t "#include \"ubf.h\"\n")
       ;;
       (table-for-each
        (type-table self)
        (lambda (h k v)
          (if (memq k (exported-types self))
              (format #t "int check_~a( UBFObject *u );\n" k))))))
    ;;
    (with-output-to-c-file
     "ubfchk.c"
     (lambda ()
       ;;
       ;; tell "ubf.h" not to include UBF type checkers
       (format #t "#define UBF_NOCHECK\n")
       ;;
       (format #t "#include <stdio.h>\n")
       (format #t "#include \"ubfchk.h\"\n")
       (format #t "#include \"ubfsym.h\"\n")
       ;;
       (format #t "#ifdef DEBUG_UBF_TYPES\n")
       (format #t "#define FAIL(t) do { fprintf( stderr, \"%s:%d: %s failed\\n\", __FILE__, __LINE__, t ); return 0; } while (0)\n")
       (format #t "#else\n")
       (format #t "#define FAIL(t) return 0\n")
       (format #t "#endif\n")
       ;;
       (table-for-each
        (type-table self)
        (lambda (h k v)
          (if (not (memq k (exported-types self)))
              (begin
                (format #t "static ")
                (format #t "int check_~a( UBFObject *u );\n" k)))))
       ;;
       (table-for-each
        (type-table self)
        (lambda (h k v)
          (if (not (memq k (exported-types self)))
              (format #t "static "))
          (format #t "int check_~a( UBFObject *u )\n" k)
          (format #t "{\n")
          (emit-c-checker (caddr v) "u" (cadr v) (~ "FAIL(\"~s\");" k)
                          (let ((i -1))
                            (lambda ()
                              (set! i (+ i 1))
                              i)))
          (format #t "  return 1;\n")
          (format #t "}\n")))))
    ;;
    (with-output-to-h-file
     "ubfsym.h"
     "UBFSYM"
     (lambda ()
       (format #t "#include \"ubf.h\"\n")
       ;;
       (for-each
        (lambda (s)
          (format #t "extern UBFObject *~a;\n" s))
        (value-sequence (static-symtab self)))))
    ;;
    (with-output-to-c-file
     "ubfsym.c"
     (lambda ()
       (format #t "#include <stdio.h>\n")
       (format #t "#include <stdlib.h>\n")
       (format #t "#include <string.h>\n")
       (format #t "#include \"ubfsymtab.h\"\n")
       (format #t "#include \"ubfsym.h\"\n")
       (format #t "#include \"hash.h\"\n")
       (format #t "#define SYMTAB_SIZE (~d)\n" $symtab-capacity)
       (format #t "unsigned symtab_max = SYMTAB_SIZE;\n")
       (format #t "#define SYMBOL(str,hash) UBF_CONSTANT, 0, data: { ubf_constant: { str, hash } } \n")
       ;;
       ;;
       (let ((ubfsymtab (make-vector $symtab-capacity))
             (ix (make-symbol-table)))
         ;;
         (table-for-each
          (static-symtab self)
          (lambda (h k v)
            (table-insert! ix k (ubfsym-hash-insert ubfsymtab k))))
         ;;
         (format #t "unsigned symtab_count = ~d;\n" (table-size ix))
         (format #t "UBFObject symtab[SYMTAB_SIZE] = {\n")
         (vector-for-each
          (lambda (e)
            (if e
                (format #t " { SYMBOL( ~s, 0x~08xUL ) },\n"
                        (symbol->string e)
                        (ubfhash e))
                (format #t " { UBF_NULL },\n")))
          ubfsymtab)
         (format #t "};\n")
         ;;
         (table-for-each
          (static-symtab self)
          (lambda (h k v)
            (format #t "UBFObject *~a = &symtab[~d];\n" 
                    v (table-lookup ix k))))
         ;;
         (newline))))
    ;;
    (values)))


(define (ubfsym-hash-insert (v <vector>) (k <symbol>))
  (let ((h (ubfhash k)))
    (let loop ((i (modulo h $symtab-capacity)))
      (if (vector-ref v i)
          (loop (modulo (+ i 1) $symtab-capacity))
          (begin
            (vector-set! v i k)
            i)))))

(define (ubfhash k)
  (fullhash (symbol->string k)))

(define (load-ubf-b-spec src-file #optional grammar)
  (let ((s (make <ubf-b-spec>)))
    (for-each
     (lambda (entry)
       ;(format #t "~s\n" entry)
       (case (car entry)
         ((types)
          (for-each
           (lambda (deft)
             (table-insert! (type-table s)
                            (cadr deft) 
                            deft))
           (cdr entry)))
         ((meta)
          (for-each
           (lambda (decl)
             (set-property! s (car decl) (cadr decl)))
           (cdr entry)))
         ((exports)
          (set-exported-types! s (append (exported-types s)
                                         (cdr entry))))
         ((imports)
          (set-imported-modules! s (append (imported-modules s)
                                           (cdr entry))))
         (else
          (error "Don't know how to handle ~s" (car entry)))))
     (car (if grammar
              (call-with-input-file src-file (lambda (p)
                                               (parse-ubf-b p grammar)))
              (call-with-input-file src-file parse-ubf-b))))
    ;;
    s))


;;;

(define (compile-protocol-file src)
  (generate-ubf-checkers/c (load-ubf-b-spec src)))

