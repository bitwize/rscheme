
;;; e.g. tlist ==> (list (list 'IDENT match-ident)
;;;                      (list 'STRING match-string-literal)
;;;                      (list "[" 'open-square-bracket))

(define-macro (grammar . g)

  (define (match-keyword item)
    (lambda (tok)
      (and (eq? tok item) tok)))

  (define (match-char item)
    (lambda (tok)
      (and (eq? tok item) tok)))

  (define (production-seq seq tlist)
    ;;
    (define (tnamex x)
      (cond
       ((assq x tlist)
        => (lambda (p)
             (list 'terminal (cadr p))))
       (else
        x)))
    ;;
    (map (lambda (s)
           (cond
            ((symbol? s)
             (let ((x (string-split (symbol->string s)
                                    #\:)))
               (if (= (length x) 2)
                   (let ((p (car x))
                         (q (cadr x)))
                     (cons (string->symbol p)
                           (tnamex (string->symbol q))))
                   (cons s (tnamex s)))))
            ((string? s)
             (let ((n (assoc s tlist)))
               (if n
                   (cons (gensym)
                         (list 'terminal
                               (match-keyword (cadr n))))
                   (error "Unknown keyword: ~s" s))))
            ((char? s)
             (let ((n (assq s tlist)))
               (if n
                   (cons (gensym)
                         (list 'terminal (match-char s)))
                   (error "Undeclared terminal char: ~s" s))))
            ((pair? s)
             (if (keyword? (car s))
                 (cons (keyword->symbol (car s)) 
                       (cons 'sequence
                             (map cdr (production-seq (cdr s) tlist))))
                 (cons (gensym) (map cdr (production-seq s tlist)))))
            (else
             (error "Unknown Production item: ~s" s))))
         seq))

  (define (Production tlist name seq . body)
    (let ((seq-items (production-seq seq tlist)))
      ;;
      (if (null? body)
          (list name
                (map cdr seq-items))
          (list name
                (map cdr seq-items)
                (list 'unquote `(lambda ,(map car seq-items)
                                  ,@body))))))

  ;;
  ;;
  ;;
  (let ((terminals '())
        (productions '())
        (pure-parse? #f))
    ;;
    (define (preprocess-terminal spec)
      (cond
       ((symbol? spec)
        (list spec (list 'unquote (symbol-append "match-" spec))))
       ((string? spec)
        (list spec (string->symbol spec)))
       ((char? spec)
        (list spec spec))
       ((and (pair? spec)
             (string? (car spec)))
        spec)
       (else
        (error "Unknown terminal spec: ~s" spec))))
    ;;
    (define (preprocess item)
      (if (pair? item)
          (case (car item)
            ((terminal)
             (set! terminals (append! terminals
                                      (map preprocess-terminal (cdr item)))))
            ((pure-parse-if)
             (set! pure-parse? (cadr item)))
            (else
             (set! productions (cons item productions))))
          (error "Unexpected item in grammar: ~s" item)))
    ;;
    (for-each preprocess g)
    ;;
    (list 'make-grammar
          (list 'quasiquote
                (map (lambda (p)
                       (apply Production terminals p))
                     (reverse! productions)))
          pure-parse?)))

#|
(define (t pp?)
  (define (match-IDENT x)
    (and (symbol? x) x))
  (define (match-INT x)
    (and (integer? x) x))
  (parse-using-grammar
   '(foo = 3)
   (grammar
    ;; the start production
    (start (expr))
    ;; declare a flag to use to decide if we are only parsing
    ;; (i.e., to ignore actions)
    (pure-parse-if pp?)
    ;; define what symbols in the grammar denote terminals
    ;; the 'grammar' macro automatically looks for procedures
    ;; named "match-X" where X is a symbol defined here.
    ;;
    ;; terminal strings declared here are turned into symbols and 
    ;; are expected to match themselves in the input list
    (terminal IDENT INT "=")
    ;; 
    ;; the productions of the grammar
    ;;
    (expr (l:IDENT "=" r:INT) (list 'set! l r)))))
|#
