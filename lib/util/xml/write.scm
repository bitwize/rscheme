
;;; SGML delimiters

(define $STAGO "<")
(define $ETAGO "</")
(define $TAGC ">")
(define $PIO "<?")
(define $PIC ">")
(define $ETAGC "/>")    ; XML; not SGML

(define (set-rscheme-sgml-mode!)
  (set! $STAGO "{")
  (set! $ETAGO "{/")
  (set! $TAGC "}")
  (set! $PIO "{?")
  (set! $PIC "}")
  (set! $ETAGC #f))

(define (sxml->string sx)
  (call-with-output-string
   (lambda (port)
     (write-sxml-rec sx port))))

(define (write-sxml node #optional (port default: (current-output-port)))
  (write-sxml-rec node port))

(define (write-sxml-top self port)
  (write-sxml-rec-list
   (if (and (pair? (cdr self))
            (pair? (cadr self))
            (eq? (caadr self) '@@))
       (cddr self)
       (cdr self))
   port))

  
(define (write-sxml-xmldecl decl port)
  (format port "<?xml ~a?>\n" (cadr decl)))

(define (write-sxml-pi decl port)
  (format port "<?~a ~a?>\n" (cadr decl) (caddr decl)))

(define (write-sxml-comment decl port)
  (format port "<!-- ~a -->\n" (cadr decl)))

(define (write-sxml-decl decl port)
  (format port "<!~a" (cadr decl))
  (let loop ((l (cddr decl)))
    (if (null? l)
        (format port ">\n")
        (let ((a (car l)))
          (cond
           ((string? a) (format port " \"~a\"" 
                                (sxml-escape-str a *xml-dirty+quot*)))
           (else (format port " ~a" a)))
          (loop (cdr l))))))

(define-method write-sxml-rec ((self <object>) port)
  (error "Invalid item type in SXML: ~s" self))

(define-method write-sxml-rec ((self <pair>) port)
  (if (not (list? self))
      (error "SXML element-like item is not a list: ~#@*50s" self))
  ;;
  (if (not (symbol? (car self)))
      (error "SXML element tag is not a symbol in: ~#@*50s" self))
  ;;
  (cond
   ((eq? (car self) '*TOP*)
    (write-sxml-top self port))
   ((eq? (car self) '*DECL*)
    (write-sxml-decl self port))
   ((eq? (car self) '*PI*)
    (write-sxml-pi self port))
   ((eq? (car self) '*XML*)
    (write-sxml-xmldecl self port))
   ((eq? (car self) '*COMMENT*)
    (write-sxml-comment self port))
   ((memq (car self) '(& *ENTITY*))
    (write-string port (string-append "&" 
                                      (to-string (cadr self))
                                      ";")))
   ((symbol? (car self))
    (write-string port
                  (string-append $STAGO (symbol->string (car self))))
    (let* ((r (if (and (pair? (cdr self))
                       (pair? (cadr self))
                       (eq? (caadr self) '@))
                  (let ((attr-list (cdadr self)))
                    (if (not (list? attr-list))
                        (error "SXML attributes are not a list in ~#*@50s"
                               self))
                    (for-each
                     (lambda (attr)
                       (let ((n (symbol->string (car attr))))
                         (cond 
                          ((null? (cdr attr))
                           (write-string port (string-append " " n)))
                          ((and (pair? (cdr attr))
                                (string? (cadr attr)))
                           (write-string port
                                         (sxml-escaped-attr-binding
                                          n
                                          (cadr attr))))
                          (else
                           (error "SXML: Bad attribute ~s" attr)))))
                     attr-list)
                    (cddr self))
                  (cdr self)))
           (body (skip-aux-list r)))
      (if (or (pair? body) (not $ETAGC))
          (begin
            (write-string port $TAGC)
            (write-sxml-rec-list (skip-aux-list r) port)
            (write-string port (string-append 
                                $ETAGO
                                (symbol->string (car self))
                                $TAGC)))
          (write-string port $ETAGC))))
   (else
    (write-sxml-rec-list self port))))

(define (skip-aux-list lst)
  (if (and (pair? lst)
           (pair? (car lst))
           (eq? (caar lst) '@aux))              ;; XXX  '@@ in True SXML
      (cdr lst)
      lst))
      
(define (write-sxml-rec-list lst port)
  (for-each (lambda (item)
              (write-sxml-rec item port))
            lst))

;;;

(define *xml-dirty*      (reg-expr->proc '(or #\< #\> #\&    )))
(define *xml-dirty+apos* (reg-expr->proc '(or #\< #\> #\& #\')))
(define *xml-dirty+quot* (reg-expr->proc '(or #\< #\> #\& #\")))

(define (sxml-write-escaped str s search port)
  (let loop ((p 0)
             (s s))
    (if (and s (> s p))
        (write-string port (substring str p s)))
    (if s
        (begin
          (case (string-ref str s)
            ((#\<) (write-string port "&lt;"))
            ((#\>) (write-string port "&gt;"))
            ((#\&) (write-string port "&amp;"))
            ((#\') (write-string port "&apos;"))
            ((#\") (write-string port "&quot;")))
          (bind ((n e (search str (+ s 1))))
            (loop (+ s 1) n)))
        (write-string port (substring str p)))))

(define-method write-sxml-rec ((self <string>) port)
  (bind ((s e (*xml-dirty* self)))
    (if s
        (sxml-write-escaped self s *xml-dirty* port)
        (write-string port self))))

(define (sxml-escape-str str search)
  (bind ((s e (search str)))
    (if s
        (call-with-output-string
         (lambda (port)
           (sxml-write-escaped str s search port)))
        str)))

(define (sxml-escaped-attr-binding name str)
  (if (string-search str #\")
      (string-append " " name "='" 
                     (sxml-escape-str str *xml-dirty+apos*) 
                     "'")
      (string-append " " name "=\"" 
                     (sxml-escape-str str *xml-dirty+quot*)
                     "\"")))
