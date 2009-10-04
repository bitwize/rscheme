
(define (not-symbol? x)
  (not (symbol? x)))

(define (eval-charset spec dict)
  (if (memq '- spec)
      (let* ((pos (eval-charset (reverse (cdr (memq '- (reverse spec)))) dict))
             (neg (eval-charset (cdr (memq '- spec)) dict)))
        ;;
        (vector-for-each
         (lambda (ch)
           (table-remove! pos ch))
         (key-sequence neg))
        pos)
      (if (any? symbol? spec)
          (apply
           char-set-union
           ;; the elements that are not referencing other named charsets
           (ranges->char-set (select not-symbol? spec))
           (map (lambda (charset-name)
                  (cond
                   ((assq charset-name dict)
                    => (lambda (term)
                         (if (instance? (cadr term) <char-set>)
                             (cadr term)
                             (error "~s: Does not name a char-set terminal"
                                    charset-name))))
                   (else
                    (error "~s: Does not name any terminal"
                           charset-name))))
                (select symbol? spec)))
          (ranges->char-set spec))))
      
    
(define-macro (grammar . g)

  (define (match-in-char-set cs)
    (lambda (tok)
      (and (table-lookup cs tok) tok)))
  
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
             (if (instance? (cadr p) <char-set>)
                 (list 'terminal (match-in-char-set (cadr p)))
                 (list 'terminal (cadr p)))))
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
            ((charset)
             (set! terminals (append! terminals
                                      (list 
                                       (list (cadr item)
                                             (eval-charset (cddr item)
                                                           terminals))))))
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
    (let ((subst (list 'make-grammar
                       (list 'quasiquote
                             (map (lambda (p)
                                    (apply Production terminals p))
                                  (reverse! productions)))
                       pure-parse?)))
      ;(pp subst)
      subst)))
