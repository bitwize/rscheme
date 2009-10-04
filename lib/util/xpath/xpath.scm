
#|
(define (t str)
  (scan-xpath (open-input-string str)))
|#

(define (scan-xpath port)
  ;;
  (define (scan-decimal-part)
    (let dloop ((accum (list (read-char port)))
                (scale 10))
      (let ((ch (peek-char port)))
        (if (char-numeric? ch)
            (dloop (cons (read-char port) accum)
                   (* scale 10))
            (/ (string->number 
                (list->string
                 (reverse accum)))
               scale)))))
  ;;
  (define (scan-ident . initial)
    (let ncname-loop ((a initial))
      (let ((ch (peek-char port)))
        (if (and (not (eof-object? ch))
                 (ncname-continued? ch))
            (ncname-loop (cons (read-char port) a))
            (if (null? a)
                (error "Missing identifier")
                (string->symbol (list->string (reverse! a))))))))
  ;;
  (define (skip-space)
    (if (char-whitespace? (peek-char port))
        (begin
          (read-char port)
          (skip-space))))
  ;;
  (let loop ((r '()))
    (let-syntax ((scan (syntax-form (type token)
                         (loop (cons (cons type token) r))))
                 (scan+ (syntax-form (type token)
                          (read-char port)
                          (loop (cons (cons type token) r)))))
        ;;
      (let ((ch (read-char port)))
        (if (eof-object? ch)
            (reverse! r)
            (case ch
              ((#\() (scan 'delim "("))
              ((#\)) (scan 'delim ")"))
              ((#\[) (scan 'delim "["))
              ((#\]) (scan 'delim "]"))
              ((#\@) (scan 'delim "@"))
              ((#\,) (scan 'delim ","))
              ((#\.)
               (let ((n (peek-char port)))
                 (cond
                  ((eq? n #\.)
                   (read-char port)
                   (scan 'delim ".."))
                  ((char-numeric? n)
                   (scan 'number (scan-decimal-part)))
                  (else
                   (scan 'delim ".")))))
              ((#\" #\')
               (let sloop ((a '()))
                 (let ((s (read-char port)))
                   (if (eof-object? s)
                       (error "EOF in Literal")
                       (if (eq? s ch)
                           (scan 'literal (list->string (reverse! a)))
                           (sloop (cons s a)))))))
              ((#\0 #\1 #\2 #\3 #\4
                #\5 #\6 #\7 #\8 #\9)
               (let nloop ((a (string->number (string ch))))
                 (let ((ch (peek-char port)))
                   (cond
                    ((eof-object? ch)
                     (scan 'number a))
                    ((char-numeric? ch)
                     (nloop (+ (* a 10) (string->number 
                                         (string (read-char port))))))
                    ((char=? ch #\.)
                     (read-char port)
                     (let ((ch (peek-char port)))
                       (if (char-numeric? ch)
                           (scan 'number (+ a (scan-decimal-part)))
                           (scan 'number a))))
                    (else
                     (scan 'number a))))))
              ((#\space #\tab #\newline #\cr)
               (loop r))
              ((#\/)
               (if (eq? (peek-char port) #\/)
                   (scan+ 'operator "//")
                   (scan 'operator "/")))
              ((#\|) (scan 'operator "|"))
              ((#\+) (scan 'operator "+"))
              ((#\-) (scan 'operator "-"))
              ((#\=) (scan 'operator "="))
              ((#\<) 
               (if (eq? (peek-char port) #\=)
                   (scan+ 'operator "<=")
                   (scan 'operator "<")))
              ((#\>) 
               (if (eq? (peek-char port) #\=)
                   (scan+ 'operator ">=")
                   (scan 'operator ">")))
              ((#\!)
               (if (eq? (read-char port) #\=)
                   (scan 'operator "!=")
                   (error "'!' not followed by '='")))
              ((#\*)
               (if (and (pair? r)
                        (not (or (member (car r) '((delim . "@")
                                                   (delim . "::")
                                                   (delim . "(")
                                                   (delim . "[")))
                                 (eq? (caar r) 'operator))))
                   (scan 'operator "*")
                   (scan 'name-test '*)))
              ((#\$)
               (scan 'variable-reference (scan-ident)))
              (else
               (cond
                ((ncname-initial? ch)
                 (let ((id (scan-ident ch)))
                   (cond
                    ((eq? (peek-char port) #\:)
                     (let* ((delim (read-char port))
                            (next (read-char port)))
                       (if (eq? next #\*)
                           (scan 'name-test (list id '*))
                           (if (eq? next #\:)
                               (begin
                                 (set! r (cons (cons 'axis id) r))
                                 (scan 'delim "::"))
                               (scan 'name-test 
                                     (list id (scan-ident next)))))))
                    ;;
                    ((memq id '(comment
                                text
                                processing-instruction
                                node))
                     (scan 'node-type id))
                    ;;
                    ((memq id '(and or mod div))
                     (scan 'operator id))
                    ;;
                    (else
                     (scan 'name id)))))
                ;;
                ((char-whitespace? ch)
                 (loop r))
                ;;
                (else
                 (error "Bad char: ~s" ch))))))))))

(define (ncname-initial? ch) (or (table-lookup *xml-letter-char* ch)
                                 (eq? ch #\_)))

(define (ncname-continued? ch) (table-lookup *ncname-continued* ch))

(define *ncname-continued*
  (char-set-union *xml-letter-char*
                  *xml-digit-char*
                  (members->char-set '(#\. #\- #\_))
                  *xml-combining-char*
                  *xml-extender-char*))

;;;


(define (xpath-grammar #optional (type default: 'expr))
  ;;
  (define (op n) `(terminal (operator . ,n)))
  (define VARREF (list 'terminal
                       (lambda (t)
                         (and (eq? (car t) 'variable-reference) (cdr t)))))
  (define NAME-TEST (list 'terminal 
                          (lambda (t)
                            (and (eq? (car t) 'name-test) (cdr t)))))
  (define NODE-TYPE (list 'terminal 
                          (lambda (t)
                            (and (eq? (car t) 'node-type) (cdr t)))))
  (define LITERAL (list 'terminal
                        (lambda (t)
                          (and (eq? (car t) 'literal) (cdr t)))))
  (define NAME (list 'terminal
                     (lambda (t)
                       (and (eq? (car t) 'name) (cdr t)))))
  ;;
  `((start (,type))
    ;;
    ;; [3]
    ;;
    (relative-location-path (step) ,(lambda (s) (list s)))
    (relative-location-path (relative-location-path
                             (terminal (operator . "/")) 
                             step)
                            ,(lambda (left / right)
                               (append left (list right))))
    (relative-location-path (abbreviated-relative-location-path))
    ;;
    ;; [4]
    ;;
    (step (axis-specifier node-test (* predicate))
          ,(lambda (axis node pred)
             (list 'step axis node pred)))
    (step (abbreviated-step))
    ;;
    ;; [12]
    (abbreviated-step ((terminal (delim . ".")))
                      ,(lambda (op)
                         '(this)))

    (abbreviated-step ((terminal (delim . "..")))
                      ,(lambda (op)
                         '(parent)))
    ;;
    ;; [5]
    ;;
    (axis-specifier ((terminal ,(lambda (t)
                                  (and (eq? (car t) 'axis) (cdr t))))
                     (terminal (delim . "::")))
                    ,(lambda (n d)
                       n))
    (axis-specifier (abbreviated-axis-specifier))
    ;;
    ;; [13]
    ;;
    (abbreviated-axis-specifier () ,(lambda () 
                                      'child))
    (abbreviated-axis-specifier ((terminal (delim . "@"))) 
                                ,(lambda (n) 'attribute))
    ;;
    ;; [7]
    ;;
    (node-test (name-test) ,(lambda (t)
                              (list 'node-test-by-name t)))
    (node-test (,NODE-TYPE (terminal (delim . "("))
                           (terminal (delim . ")")))
               ,(lambda (n < >)
                  (list 'node-test-by-type n)))
    ;;
    ;; [37]
    ;;
    (name-test (,NAME-TEST) ,(lambda (n)
                               (list 'name-test n)))
    (name-test (,NAME) ,(lambda (n)
                          (list 'name-test n)))
    (name-test (,NODE-TYPE)
               ,(lambda (n)
                  (list 'name-test n)))
                               
    ;;
    ;; [8]
    ;;
    (predicate ((terminal (delim . "["))
                predicate-expr
                (terminal (delim . "]")))
               ,(lambda (< e >) e))
    ;;
    ;; [9]
    ;;
    (predicate-expr (expr))
    ;;
    ;; [14]
    ;;
    (expr (or-expr))
    ;;
    ;; [21]
    (or-expr (and-expr))
    (or-expr (or-expr (terminal (operator . or)) and-expr)
             ,(lambda (l op r)
                (list 'or l r)))
    ;;
    ;; [22]
    (and-expr (equality-expr))
    (and-expr (and-expr (terminal (operator . and)) equality-expr)
              ,(lambda (l op r)
                 (list 'and l r)))
    ;;
    ;; [23]
    (equality-expr (relational-expr))
    (equality-expr (equality-expr ,(op "=") relational-expr)
                   ,(lambda (l op r)
                      (list '= l r)))
    (equality-expr (equality-expr ,(op "!=") relational-expr)
                   ,(lambda (l op r)
                      (list '<> l r)))
    ;;
    ;; [24]
    (relational-expr (additive-expr))
    (relational-expr (relational-expr ,(op "<") additive-expr)
                     ,(lambda (l op r)
                        (list '< l r)))
    (relational-expr (relational-expr ,(op ">") additive-expr)
                     ,(lambda (l op r)
                        (list '> l r)))
    (relational-expr (relational-expr ,(op "<=") additive-expr)
                     ,(lambda (l op r)
                        (list '<= l r)))
    (relational-expr (relational-expr ,(op ">=") additive-expr)
                     ,(lambda (l op r)
                        (list '>= l r)))
    ;;
    ;; [25]
    (additive-expr (multiplicative-expr))
    (additive-expr (additive-expr ,(op "+") multiplicative-expr)
                   ,(lambda (l op r)
                      (list '+ l r)))
    (additive-expr (additive-expr ,(op "-") multiplicative-expr)
                   ,(lambda (l op r)
                      (list '- l r)))
    ;;
    ;; [26]
    (multiplicative-expr (unary-expr))
    (multiplicative-expr (multiplicative-expr ,(op "*") unary-expr)
                         ,(lambda (l op r)
                            (list '* l r)))
    (multiplicative-expr (multiplicative-expr ,(op 'div) unary-expr)
                         ,(lambda (l op r)
                            (list 'div l r)))
    (multiplicative-expr (multiplicative-expr ,(op 'mod) unary-expr)
                         ,(lambda (l op r)
                            (list 'mod l r)))
    ;;
    ;; [27]
    (unary-expr (union-expr))
    (unary-expr (,(op "-") unary-expr)
                ,(lambda (op r)
                   (list '- r)))
    ;;
    ;; [18]
    (union-expr (path-expr))
    (union-expr (union-expr ,(op "|") path-expr)
                ,(lambda (l op r)
                   (list 'union l r)))
    ;; [19]
    (path-expr (location-path)
               ,(lambda (p)
                  (list 'path p)))
    (path-expr (filter-expr))
    ;(path-expr (filter-expr "/" relative-location-path) ...)
    ;(path-expr (filter-expr "//" relative-location-path) ...)
    ;;
    ;; [20]
    (filter-expr (primary-expr))
    (filter-expr (filter-expr predicate)
                 ,(lambda (p f)
                    (list 'filter p f)))
    ;;
    ;; [15]
    (primary-expr (,VARREF)
                  ,(lambda (n)
                     (symbol-append "$" n)))
    (primary-expr ((terminal (delim . "("))
                   expr
                   (terminal (delim . ")")))
                  ,(lambda (< e >) e))
    (primary-expr (,LITERAL))
    (primary-expr ((terminal ,(lambda (t)
                                (and (eq? (car t) 'number) (cdr t))))))
    (primary-expr (function-call))
    ;;
    ;; [16]
    (function-call (,NAME (terminal (delim . "("))
                          function-args
                          (terminal (delim . ")")))
                   ,(lambda (n < a >)
                      (cons* 'call n a)))
    (function-args () ,(lambda () '()))
    (function-args (function-args+))
    (function-args+ (expr) ,(lambda (e) (list e)))
    (function-args+ (expr (terminal (delim . ",")) function-args+)
                   ,(lambda (l op r)
                      (cons l r)))
    ;;
    ;; [1]
    (location-path (relative-location-path))
    (location-path (absolute-location-path))
    ;;
    ;; [2]
    (absolute-location-path ((terminal (operator . "/"))
                             relative-location-path)
                            ,(lambda (op p)
                               (cons '(root-node) p)))
    (absolute-location-path ((terminal (operator . "/")))
                            ,(lambda (op)
                               '((root-node))))
    (absolute-location-path (abbreviated-absolute-location-path))
    ;;
    ;; [10]
    (abbreviated-absolute-location-path ((terminal (operator . "//"))
                                         relative-location-path)
                                        ,(lambda (op p)
                                           (cons
                                            '(step
                                              descendant-or-self
                                              (node-test-by-type node)
                                              ())
                                            p)))
    ;; [11]
    (abbreviated-relative-location-path (relative-location-path
                                         (terminal (operator . "//"))
                                         step)
                                        ,(lambda (l // r)
                                           (append
                                            l
                                            '((step
                                               descendant-or-self
                                               (node-test-by-type node)
                                               ()))
                                            (list r))))
    ;;
    ;;===================================================================
    ;;
    ;;  Patterns, from XSLT
    ;;
    ;;===================================================================
    ;;
    ;; [1] 
    (pattern (location-path-pattern))
    (pattern (pattern ,(op "|") location-path-pattern)
             ,(lambda (l op r)
                (list 'or l r)))
    ;; [2]
    (location-path-pattern (,(op "/"))
                           ,(lambda (op)
                              '((root-node))))
    (location-path-pattern (,(op "/") relative-path-pattern)
                           ,(lambda (op p)
                              (cons '(root-node) p)))
    (location-path-pattern (id-key-pattern) ,(lambda (p) (list p)))
    (location-path-pattern (id-key-pattern ,(op "/") relative-path-pattern)
                           ,(lambda (k op p) (cons k p)))
    (location-path-pattern (id-key-pattern ,(op "//") relative-path-pattern)
                           ,(lambda (k op p) (cons* k '(anywhere) p)))
    (location-path-pattern (,(op "//") relative-path-pattern)
                           ,(lambda (op p) (cons* '(root-node) '(anywhere) p)))
    (location-path-pattern (relative-path-pattern))
    ;; [3]
    (id-key-pattern ((terminal (name . id))
                     (terminal (delim . "("))
                     ,LITERAL
                     (terminal (delim . ")")))
                    ,(lambda (h < e >)
                       (list 'id e)))
    (id-key-pattern ((terminal (name . id))
                     (terminal (delim . "("))
                     ,LITERAL
                     (terminal (delim . ","))
                     ,LITERAL
                     (terminal (delim . ")")))
                    ,(lambda (h < a sep b >)
                       (list 'key a b)))
    ;; [4]
    (relative-path-pattern (step-pattern) ,(lambda (s) (list s)))
    (relative-path-pattern (step-pattern ,(op "/") relative-path-pattern)
                           ,(lambda (l op r) (cons l r)))
    (relative-path-pattern (step-pattern ,(op "//") relative-path-pattern)
                           ,(lambda (l op r) (cons* l '(anywhere) r)))
    ;; [5]
    (step-pattern (child-or-attribute-axis-specifier node-test (* predicate))
                  ,(lambda (axis nt pred)
                     (list 'step axis nt pred)))
    ;; [6]
    (child-or-attribute-axis-specifier (abbreviated-axis-specifier))
    (child-or-attribute-axis-specifier ((terminal (axis . child))
                                        (terminal (delim . "::")))
                                       ,(lambda (a d) 'child))
    (child-or-attribute-axis-specifier ((terminal (axis . attribute))
                                        (terminal (delim . "::")))
                                       ,(lambda (a d) 'attribute))
    ))

;;;

;;;

(define-class <xpath-procedure> (<object>)
  name
  argument-formals
  scheme-procedure-name)
  
(define-class <xpath-context-bindings> (<object>)
  context-variables
  context-procedures
  context-namespaces)

(define (compile-xpath expr ctx)
  (dispatch-xpath-compile expr ctx))

;;; self-evaluating items

(define-method dispatch-xpath-compile ((self <number>) ctx)
  self)

(define-method dispatch-xpath-compile ((self <string>) ctx)
  self)

;;; variable lookup

(define-method dispatch-xpath-compile ((self <symbol>) ctx)
  (if (memq self (context-variables ctx))
      self
      (error "Reference to unbound XPath variable: ~s" self)))

;;; something more difficult...

(define-method dispatch-xpath-compile ((self <pair>) ctx)
  (case (car self)
    ((+ - * /)
     (cons (car self)
           (map (lambda (sub)
                  (list 'xpath:to-number (compile-xpath sub ctx)))
                (cdr self))))
    ((=)
     (list 'xpath:equal?
           (compile-xpath (cadr self) ctx)
           (compile-xpath (caddr self) ctx)))
    ((<>)
     (list 'not (list 'xpath:equal?
                      (compile-xpath (cadr self) ctx)
                      (compile-xpath (caddr self) ctx))))
    ((and or)
     (cons (car self)
           (map (lambda (sub)
                  (compile-xpath sub ctx))
                (cdr self))))
    ((path)
     (xpath-compile-path-expr (reverse (cadr self)) ctx))
    ((get-attribute-value)
     `(xpath:get-attribute-value (context-node context) ',(cadr self)))
    ((attribute-value=?)
     (compile-xpath-attribute-value-equal (cadr self) 
                                          (compile-xpath (caddr self) ctx)
                                          ctx))
    ((call)
     (compile-xpath-call (cadr self) (cddr self) ctx))
    (else
     (error "Unknown form: ~s" self))))

;;;

(define-method compile-xpath-call ((self <symbol>) args ctx)
  (let ((f (assq self (context-procedures ctx))))
    (if f
        (compile-xpath-call (cdr f) args ctx)
        (error "~s: No such xpath procedure" self))))

(define-method compile-xpath-call ((self <xpath-procedure>) args ctx)
  (let ((actuals (map (lambda (e)
                        (compile-xpath e ctx))
                      args)))
    (if (= (length actuals)
           (length (argument-formals self)))
        (cons* (scheme-procedure-name self)
               'context
               (map (lambda (actual formal)
                      (case (cadr formal)
                        ((<number>) (list 'xpath:to-number actual))
                        ((<string>) (list 'xpath:to-string actual))
                        ((<boolean>) (list 'xpath:to-boolean actual))
                        ((<node-set>) (list 'xpath:check-nodeset actual))
                        ((<object>) actual)
                        (else (error "bad formal type: ~s" formal))))
                    actuals
                    (argument-formals self)))
        (error "~s: Formal/actual mismatch" (name self)))))
                       

(define (compile-xpath-attribute-value-equal attr value ctx)
  `(xpath:equal? (xpath:get-attribute-value (context-node context)
                                            ',attr)
                 ,value))

(define (xpath-compile-path-expr steps ctx)
  (cond
   ((null? steps)
    '(delist (context-node context)))
   ((equal? (car steps) '(root-node))
    '(delist (context-root-node context)))
   ((equal? (car steps) '(this))
    ;; supposed to be equivalent to self::node() -- anything to do here? XXX
    (xpath-compile-path-expr (cdr steps) ctx))
   ((equal? (car steps) '(parent))
    `(path-parent ,(xpath-compile-path-expr (cdr steps) ctx)))
   (else
    (let ((s (xpath-compile-step (car steps) ctx))
          (filters (cadddr (car steps))))
      (if (null? filters)
          `(path-step (lambda (node) ,s)
                      ,(xpath-compile-path-expr (cdr steps) ctx))
          `(path-step-and-filter 
            (lambda (node) ,s)
            (list ,@(xpath-compile-filters filters ctx))
            ,(xpath-compile-path-expr (cdr steps) ctx)))))))


(define (xpath-compile-step1-no-filters enumerator)
  `(let ((h '())
         (t '()))
     (let-syntax ((include (syntax-form (n)
                             (let ((p (cons n '())))
                               (if (null? h)
                                   (begin
                                     (set! h p)
                                     (set! t p))
                                   (begin
                                     (set-cdr! t p)
                                     (set! t p))))))
                  (done (syntax-form ()
                          (if (null? h)
                              '()
                              (make <delist> head: h tail: t)))))
       ,enumerator)))

#|
(define (xpath-compile-step1-filters enumerator filter-procs)
  `(let ((r '())
         ((size <fixnum>) 0))
     (let-syntax ((include (syntax-form (n)
                             (set! size (add1 size))
                             (set! r (cons n r))))
                  (done (syntax-form ()
                          (if (null? r)
                              '()
                              (xpath-apply-filters r size
                                                   (list ,@filter-procs))))))
       ,enumerator)))
|#

(define (axis-enumerator axis node-test)
  (case axis
    ((child attribute)
     `(let loop ((i (,(symbol-append "axis-" axis "-iterator") node)))
        (if (,(symbol-append "axis-" axis "-end?") i)
            (done)
            (let ((c (,(symbol-append "axis-" axis "-current") i)))
              (if ,node-test
                  (include c))
              (loop (,(symbol-append "axis-" axis "-next") i))))))
    ((descendant-or-self)
     `(letrec ((loop (lambda (c)
                       (if ,node-test
                           (include c))
                       (for-each loop (xpath:children c)))))
        (loop node)
        (done)))
    ((descendant)
     `(letrec ((loop (lambda (c)
                       (if ,node-test
                           (include c))
                       (for-each loop (xpath:children c)))))
        (for-each loop (xpath:children node))
        (done)))
    (else
     (error "axis-enumator: cannot handle axis ~s" axis))))


(define (xpath-compile-step step ctx)
  (assert (eq? (car step) 'step))
  (let* ((axis (cadr step))
         (node-test-expr (xpath-compile-node-test (caddr step) ctx axis)))
    ;;
    (xpath-compile-step1-no-filters (axis-enumerator axis node-test-expr))))

(define (xpath-compile-filters filters ctx)
  (let loop ((f filters)
             (r '())
             (forward? #t))
    (if (null? f)
        r
        (loop (cdr f)
              (cons (xpath-compile-filter (car f)
                                          ctx
                                          forward?
                                          (null? (cdr f)))
                    r)
              (not forward?)))))

(define (xpath-compile-filter predicate ctx forward? return-nodeset?)
  ;; Handle a few special cases...
  (cond
   ;;
   ((number? predicate)
    `(lambda (node-list size)
       (if (< ,(- predicate 1) size)
           (let ((n (list-ref node-list ,(if forward?
                                             (- predicate 1)
                                             `(- size ,predicate)))))
             ,(if return-nodeset?
                  '(delist n)
                  '(values (list n) 1)))
           ,(if return-nodeset?
                ''()
                '(values '() 0)))))
   ;;
   ((equal? predicate '(call last))
    `(lambda (node-list size)
       (if (eq? size 0)
           ,(if return-nodeset?
                ''()
                '(values '() 0))
           (let ((n (,(if forward? 'last 'car) node-list)))
             ,(if return-nodeset?
                  '(delist n)
                  '(values (list n) 1))))))
   (else
    ;;
    (let ((p (xpath-compile-predicate predicate ctx)))
      `(lambda (node-list size)
         (let loop ((s node-list)
                    ((i <fixnum>) ,(if forward? 1 'size))
                    (r '())
                    ((n <fixnum>) 0))
           (if (null? s)
               ,(if return-nodeset?
                    (if forward?
                        '(list->delist (reverse r))
                        '(list->delist r))
                    '(values r n))
               (let ((node (car s)))
                 (if ,p
                     (loop (cdr s)
                           ,(if forward? '(add1 i) '(sub1 i))
                           (cons node r)
                           (add1 n))
                     (loop (cdr s)
                           ,(if forward? '(add1 i) '(sub1 i))
                           r
                           n))))))))))
                         
(define (elim-dups->delist nodelist)
  (let ((u (make-object-table)))
    (let loop ((i nodelist)
               (h '())
               (t '()))
      (if (null? i)
          (if (null? h)
              '()
              (make <delist> head: h tail: t))
          (if (table-lookup u (caar i))
              (loop (cdr i) h t)
              (let ((p (cons (car i) '())))
                (table-insert! u (caar i) #t)
                (if (null? h)
                    (loop (cdr i) p p)
                    (begin
                      (set-cdr! t p)
                      (loop (cdr i) h p)))))))))
      
(define (path-parent nodeset)
  (if (null? nodeset)
      '()
      (elim-dups->delist (map cdr (head nodeset)))))
              

(define (path-step-and-filter proc filters nodeset)
  (if (null? nodeset)
      '()
      (let loop ((l (head nodeset))
                 (r '()))
        (if (null? l)
            (if (null? r)
                '()
                (xpath-apply-filters (head r)
                                     (length (head r))
                                     filters))
            (loop (cdr l) (delist-append r (proc (car l))))))))


(define (xpath-apply-filters nodelist size filters)
  (if (null? filters)
      (values nodelist size)
      (bind ((n s (xpath-apply-filters nodelist size (cdr filters))))
        ((car filters) n s))))
      

(define (path-step proc nodeset)
  (if (null? nodeset)
      '()
      (let loop ((l (head nodeset))
                 (r '()))
        (if (null? l)
            r
            (loop (cdr l) (delist-append r (proc (car l))))))))
    

(define (xpath-compile-predicate p ctx)
  `(xpath-eval-predicate (lambda (context)
                           ,(compile-xpath p ctx))
                         node i size context))

(define (xpath-eval-predicate proc node i size enclosing)
  (let ((r (proc (make <xpath-context>
                       context-node: node
                       context-position: i
                       context-size: size
                       context-bindings: (context-bindings enclosing)))))
    (if (number? r)
        (equal? r i)
        (xpath:to-boolean r))))

(define-method xpath:to-boolean ((self <number>))
  (not (zero? self)))

(define-method xpath:to-boolean ((self <string>))
  (> (string-length self) 0))

(define-method xpath:to-boolean ((self <empty-list>))
  #f)

(define-method xpath:to-boolean ((self <boolean>))
  self)

(define-method xpath:to-boolean ((self <delist>))
  #t)   ; a delist is always non-empty

 

(define (xpath-compile-node-test nt ctx axis)
  (case axis
    ((attribute) (xpath-compile-attribute-node-test nt ctx))
    (else (xpath-compile-element-node-test nt ctx))))

(define (xpath-compile-name-test name-expr nt ctx)
  ;(format #t "name-test: ~s\n" nt)
  (assert (eq? (car nt) 'name-test))
  (let ((arg (cadr nt)))
    (if (pair? arg)
        (let ((ns (expand-namespace ctx (car arg)))
              (gi (cadr arg)))
          (if (eq? gi '*)
              `(in-namespace? ,name-expr ',ns)
              `(eq? ,name-expr ',(symbol-append ns ":" gi))))
        (if (eq? arg '*)
            #t
            `(eq? ,name-expr ',arg)))))


(define (xpath-compile-attribute-node-test nt ctx)
  (case (car nt)
    ((node-test-by-name)
     (xpath-compile-name-test '(caar c) (cadr nt) ctx))
    ((node-test-by-type)
     (error "Can't handle node-test-by-type"))))

(define (xpath-compile-element-node-test nt ctx)
  (case (car nt)
    ((node-test-by-name)
     `(and (sxml:element? (car c))
           ,(xpath-compile-name-test '(caar c) (cadr nt) ctx)))
    ((node-test-by-type)
     (case (cadr nt)
       ((text)
        `(sxml:text? (car c)))
       ((comment)
        #f)             ;; comments don't show up in SXML
       ((node)
        #t)             ;; everything is a node
       ((processing-instruction)
        `(sxml:pi? (car c)))))))

(define *debug-xpath* #f)

(define (parse-xpath str)
  (or (parse-using-grammar (scan-xpath (open-input-string str))
                           (make-grammar (xpath-grammar)))
      (error "Failed to parse xpath: ~s" str)))

(define (xpath->lambda str
                       #key (namespaces default: '())
                            (procedures default: '())
                            (variables default: '())
                            (locals default: '()))
  (let* ((parsed (car (parse-xpath str)))
         (locals (map (lambda (n)
                        (symbol-append "$" n))
                      locals))
         (ctx (make <xpath-context-bindings>
                    context-variables: locals
                    context-procedures: (xpath-standard-procs)
                    context-namespaces: '())))
    (if *debug-xpath*
        (format #t "xpath: ~s\n" parsed))
    (let ((pre (precompile-xpath parsed)))
      (if *debug-xpath*
          (format #t "precompiled: ~s\n" pre))
      (let ((c (compile-xpath pre ctx)))
        (if *debug-xpath*
            (begin
              (format #t "code:\n")
              (pp c)))
        (let ((o (optimize-xpath c)))
          (if *debug-xpath*
              (begin
                (format #t "optimized:\n")
                (pp o)))
          `(lambda '(xpath ,str) (context ,@locals)
             ,o))))))

(define (xpath:get-attribute-value node key)
  ;;
  (define (scan-attrs alist)
    (let ((a (assq key alist)))
      (if a
          (cadr a)
          "")))
  ;;
  (let ((sxml (car node)))
    (if (or (not (pair? sxml))
            (null? (cdr sxml)))
        ""
        (if (pair? (cadr sxml))
            (if (eq? (caadr sxml) '@)
                (scan-attrs (cdadr sxml))
                (if (and (eq? (caadr sxml) '@@)
                         (pair? (cddr sxml))
                         (pair? (caddr sxml))
                         (eq? (caaddr sxml) '@))
                    (scan-attrs (cdaddr sxml))
                    ""))
            ""))))

(define xpath:get-attribute-value* xpath:get-attribute-value)


    
(define (xpath:attributes node)
  (let ((sxml (car node)))
    (if (or (not (pair? sxml))
            (null? (cdr sxml)))
        '()
        (if (pair? (cadr sxml))
            (if (eq? (caadr sxml) '@)
                (map (lambda (a)
                       (cons a node))
                     (cdadr sxml))
                (if (and (eq? (caadr sxml) '@@)
                         (pair? (cddr sxml))
                         (pair? (caddr sxml))
                         (eq? (caaddr sxml) '@))
                    (map (lambda (a)
                           (cons a node))
                         (cdaddr sxml))
                    '()))
            '()))))

(define (axis-descendant-or-self-iterator n)
  (let ((i (proc->iterator
            (lambda (emit)
              (define (traverse x)
                (emit x)
                (for-each traverse (xpath:children x)))
              (traverse n)))))
    (cons (i) i)))

(define-syntax (axis-descendant-or-self-end? i)
  (not (car i)))

(define (axis-descendant-or-self-next i)
  (cons ((cdr i)) (cdr i)))

(define (axis-descendant-or-self-current i)
  (car i))

(define (xpath:children node)
  (map (lambda (c)
         (cons c node))
       (sxml:children (car node))))

(define-patterns precompile-xpath
  ((= (path ((step attribute (node-test-by-name (name-test ?x:s)) ()))) ?y)
   (attribute-value=? ?x:s ?y))
  ((call string (path ((step attribute (node-test-by-name (name-test ?x:s)) ()))))
   (get-attribute-value ?x:s)))

(define-patterns optimize-xpath
  ((xpath:to-number ?k) ?k)
  ((xpath:to-number ?qu) ?x     when: (string? ?qu) 
                                where: ((?x (string->number ?qu))))
  ((xpath:to-string ?qu) ?qu    when: (string? ?qu))

  ;;
  ((xpath:equal? (xpath:get-attribute-value* ?a ?b) ?qu)
   (string=? (xpath:get-attribute-value ?a ?b) ?qu)     when: (string? ?qu))

  ((xpath:equal? (xpath:get-attribute-value* ?a ?b) ?k)
   (= (xpath:to-number (xpath:get-attribute-value ?a ?b)) ?k))

  ;; fold certain common cases of `xpath-eval-predicate'
  ((xpath-eval-predicate (lambda (?v) ?k) ?a ?b ?c ?d) (eq? ?b ?k))
  ((xpath-eval-predicate (lambda (?v) (xpath:equal?
                                       (xpath:get-attribute-value
                                        (context-node ?v)
                                        ?aa)
                                       ?bb))
                         ?a ?b ?c ?d)
   (xpath:equal? (xpath:get-attribute-value* ?a ?aa) ?bb))

  ((xpath-eval-predicate (lambda (?v) (xpath:last ?v))
                         ?a ?b ?c ?d)
   (eq? ?b ?c))

  ((axis-child-iterator ?e)     (xpath:children ?e))
  ((axis-child-end? ?e)         (null? ?e))
  ((axis-child-next ?e)         (cdr ?e))
  ((axis-child-current ?e)      (car ?e))

  ((axis-attribute-iterator ?e) (xpath:attributes ?e))
  ((axis-attribute-end? ?e)     (null? ?e))
  ((axis-attribute-next ?e)     (cdr ?e))
  ((axis-attribute-current ?e)  (car ?e))

  ((- ?x 0)                     ?x)
  ((+ ?x 0)                     ?x)
  ((list-ref ?x 0)              (car ?x))
  ((if #t ?x)                   ?x)
  ((if #f ?x)                   #f)
  ((if #t ?x ?y)                ?x)
  ((if #f ?x ?y)                ?y)
  
  ((list ?x)                    (cons ?x '()))
  ((list ?x ?y)                 (cons* ?x ?y '()))
  )

;;;

(define (xpath:number context item)
  (xpath:to-number item))

(define (xpath:boolean context item)
  (xpath:to-boolean item))

(define (xpath:string context item)
  (xpath:to-string item))

(define (xpath:last context)
  (context-size context))

(define (xpath:self context)
  (context-node context))

(define (xpath-standard-procs)
  (list
   (cons 'number
         (make <xpath-procedure>
               name: 'number
               scheme-procedure-name: 'xpath:number
               argument-formals: '((object <object>))))
   (cons 'boolean
         (make <xpath-procedure>
               name: 'boolean
               scheme-procedure-name: 'xpath:boolean
               argument-formals: '((object <object>))))
   (cons 'string
         (make <xpath-procedure>
               name: 'string
               scheme-procedure-name: 'xpath:string
               argument-formals: '((object <object>))))
   (cons 'self
         (make <xpath-procedure>
               name: 'self
               scheme-procedure-name: 'xpath:self
               argument-formals: '()))
   (cons 'last
         (make <xpath-procedure>
               name: 'last
               scheme-procedure-name: 'xpath:last
               argument-formals: '()))))

(define (expand-namespace (ctx <xpath-context-bindings>) ns)
  (if (assq ns (context-namespaces ctx))
      (cadr (assq ns (context-namespaces ctx)))
      ns))

;;;
;;;

(define-class <xpath-context> (<object>)
  context-node
  context-position
  context-size
  (context-bindings type: <xpath-context-bindings>))

(define (context-root-node ctx)
  (last-pair (context-node ctx)))
                 
(define (xpath-exec proc node)
  (proc (make <xpath-context>
              context-node: node
              context-position: 1
              context-size: 1
              context-bindings: (make <xpath-context-bindings>
                                      context-variables: '()
                                      context-procedures: '()
                                      context-namespaces: '()))))

(define (read-xslt file)
  (call-with-input-file 
      file
    (lambda (port)
      (port->sxml port '((xsl "http://www.w3.org/1999/XSL/Transform"))))))

(define-method xpath:to-number ((self <number>))
  self)

(define $NaN (float/ 0.0 0.0))

(define-method xpath:to-number ((self <string>))
  (or (string->number self)
      $NaN))

(define-method xpath:to-number ((self <boolean>))
  (if self
      1
      0))

(define-method xpath:to-number ((self <object>))
  (xpath:to-number (xpath:to-string self)))

(define-method xpath:to-string ((self <number>))
  (number->string self))

(define-method xpath:to-string ((self <string>))
  self)

(define-method xpath:to-string ((self <empty-list>))
  "")

(define-method xpath:to-string ((self <delist>))
  (string-join ""
               (map (lambda (n)
                      (xpath:flatten->string (car n)))
                    (head self))))

(define-method xpath:flatten->string ((self <string>))
  self)
  
(define-method xpath:flatten->string ((self <pair>))
  (if (memq (car self) '(@ @@))
      ""
      (string-join "" (map xpath:flatten->string (cdr self)))))

(define (xpath:string-value node)
  (xpath:flatten->string (car node)))         ;; XXX same thing?

(define (xpath:equal? a b)
  (cond
   ((and (xpath:nodeset? a) (xpath:nodeset? b))
    (if (null? a)
        #f
        (if (null? b)
            #f
            (xpath-nodeset-intersect? a b))))
   ((xpath:nodeset? b)
    (xpath:equal? b a)) ; canonicalize so that the nodeset is first
   ((xpath:nodeset? a)
    (cond
     ((boolean? b)
      (eq? (xpath:to-boolean a) b))
     ((string? b)
      (if (null? a)
          #f
          (any? (lambda (a-node)
                  (string=? b (xpath:string-value a-node)))
                (head a))))
     ((number? b)
      (if (null? a)
          #f
          (any? (lambda (a-node)
                  (= b (xpath:to-number (xpath:string-value a-node))))
                (head a))))
     (else
      (error "Don't know how to compare nodeset and ~s" b))))
   ((or (boolean? a) (boolean? b))
    (eq? (xpath:to-boolean a) (xpath:to-boolean b)))
   ((or (number? a) (number? b))
    (= (xpath:to-number a) (xpath:to-number b)))
   (else
    (string=? (xpath:to-string a) (xpath:to-string b)))))

(define (xpath-nodeset-intersect? a b)
  (let ((tbl (make-string-table)))
    (for-each (lambda (a-node)
                (table-insert! tbl (xpath:string-value a-node) #t))
              (head a))
    (any? (lambda (b-node)
            (table-lookup tbl (xpath:string-value b-node)))
          (head b))))

(define (xpath:nodeset? x)
  (or (null? x)
      (instance? x <delist>)))

#|
;; Return the introductory paragraphs of each section
(t2 "doc/section/para[@role='intro']")

;; Return the first paragraph in the document that is introductory
(t2 "//para[@role='intro'][1]")

;; Return all the sections that have an introductory paragraph
(t2 "//section[para/@role='intro']")

|#
                                   

;;; NOTE:  The SXML notation does not have information about
;;;        ancestry.  Hence, `sxml:xpath-exec' only makes sense
;;;        for root nodes (although it can muddle along as long
;;;        as the xpath does not include absolute paths or 
;;;        net leading ..'s).  The return value is a list of
;;;        SXML nodes, which also have their ancestry information
;;;        stripped.

(define (sxml:xpath-exec node proc . args)
  (xpath->scheme
   (apply proc 
          (make <xpath-context>
                context-node: (list node)
                context-position: 1
                context-size: 1
                context-bindings: (make <xpath-context-bindings>
                                        context-variables: '()
                                        context-procedures: '()
                                        context-namespaces: '()))
          args)))

(define-method xpath->scheme ((self <object>))
  self)

(define-method xpath->scheme ((self <empty-list>))
  '())

(define-method xpath->scheme ((self <delist>))
  (map car (head self)))
  
#|

(define (para-with-role n r)
  (sxml:xpath-exec n (xpath (role) "//para[@role=$role]") r))

|#

(define (parse-pattern str)
  (parse-using-grammar (scan-xpath (open-input-string str))
                       (make-grammar (xpath-grammar 'pattern))))
 
                       
(define-patterns crunch-pattern-match

  (
   (pattern or ?a ?b)
   (or (pattern . ?a)
       (pattern . ?b))
   )

  (
   (pattern ?a . ?b)
   (pattern* . ?r)
   when: (and (pair? ?a)
              (eq? (car ?a) 'step))
   where: ((?r (reverse (cons ?a (vector->list ?b)))))
   )

  ;; a simple pattern like "para"
  (
   (pattern* (step child (node-test-by-name (name-test ?x:s)) ()))
   (and (dom:element? node)
        (eq? (name node) '?x:s))
   )

  ;; a simple pattern like "@class"
  (
   (pattern* (step attribute (node-test-by-name (name-test ?x:s)) ()))
   (and (dom:attribute? node)
        (eq? (name node) '?x:s))
   )

  ;; a two-step pattern like "a/???"
  (
   (pattern* (step ?axis2 ?nt2 ?filter2)
             (step child ?nt1 ?filter1)
             ?more ...)
   (and (pattern* (step ?axis2 ?nt2 ?filter2))
        (let ((node (dom-axis:parent1 node)))
          (pattern* (step child ?nt1 ?filter1) ?more ...)))
   )

  ;;
  ;; some optimizations
  ;;
  ((or (and ?x ?y) (and ?x ?z))
   (and ?x (or ?y ?z)))

  ((or (eq? ?x '?y)
       (eq? ?x '?z))
   (memq ?x '(?y ?z)))
  
  ((or (memq ?x '?y) (eq? ?x '?z))
   (memq ?x '(?z . ?y)))

  ((or (let ((node ?x)) ?y)
       (let ((node ?x)) ?z))
   (let ((node ?x)) (or ?y ?z)))
  )


(define (compile-pattern str)
  (crunch-pattern-match (cons 'pattern (car (parse-pattern str)))))


#|
(define (xpath* str #rest keys)
  (eval `(with-module usual-inlines ,(apply xpath->lambda str keys))))

(define-macro (xpath formals str)
  `(with-module usual-inlines ,(xpath->lambda str locals: formals)))
|#

(define-macro (xpath formals . opt)
  (case (length opt)
    ((1)
     (let ((f (eval-in-envt (xpath->lambda (car opt) locals: formals) *self*)))
       `',f))
    ((2)
     (let ((f (eval-in-envt (xpath->lambda (cadr opt) 
                                           locals: (map car formals))
                            *self*)))
       `(sxml:xpath-exec ,(car opt) ',f ,@(map cadr formals))))
    (else
     (error "xpath: usage is (xpath (FORMAL ...) XPATH)\n       or (xpath ((FORMAL EXPR) ...) NODE XPATH)\n       not: ~s" `(xpath ,formals ,@opt)))))

(define-macro (xpath-unique . opt)
  (let ((tmp (gensym)))
    `(let ((,tmp (xpath ,@opt)))
       (cond
        ((null? ,tmp) (error "xpath ~s: No match" ,(last opt)))
        ((null? (cdr ,tmp)) (car ,tmp))
        (else (error "xpath ~s: Ambiguous matches" ,(last opt)))))))

(define-macro (xpath-str node expr)
  `(',xpath:node-set->string (xpath () ,node ,expr)))

(define-macro (xpath-str-unique node expr)
  `(',xpath:node->string (xpath-unique () ,node ,expr)))

