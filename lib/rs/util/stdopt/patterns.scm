(define-thread-var *program-name* #f)

(define (print-usage formals patterns)
  ;;
  (define (spec key)
    (let ((q (select (lambda (e)
                       (eq? (cadr e) key))
                     formals)))
      (if (null? q)
          (error "rs.util.stdopt: No option flag can generate `~s'" key)
          (car q))))
  ;;
  (define (inv key)
    (string #\- (car (spec key))))
  ;;
  (define (prn o)
    (if (and (pair? o) (eq? (car o) '?))
        (begin
          (display "[")
          (prn (cadr o))
          (display "]"))
        (if (eq? o '*rest)
            (display "ARG ...")
            (let* ((s (spec o))
                   (n (and (pair? (cddr s)) (caddr s)))
                   (t (and (pair? (cddr s))
                           (pair? (cdddr s))
                           (cadddr s))))
              (display (inv o))
              (if (and n (= n 1))
                  (format #t " ~a" (list->string
                                    (map char-upcase
                                         (string->list (symbol->string o))))))))))
  ;;
  (format #t "Usage:\n")
  (let ((me (last (string-split (or *program-name* (getenv "_") "me")
                                #\/))))
    (for-each
     (lambda (p)
       (newline)
       (format #t "  ~a::\n" (car p))
       (format #t "    ~a" me)
       (for-each
        (lambda (o)
          (display #\space)
          (prn o))
        (append (caadr p) (cadadr p)))
       (newline))
     patterns)))
                 

(define (dispatch-option-patterns actuals formals patterns)
  (let ((accum (make-symbol-table))
        (rest '()))
    ;;
    (define (inv key)
      (let ((q (select (lambda (e)
                         (eq? (cadr e) key))
                       formals)))
        (if (null? q)
            (error "No option for ~s" key))
        (assert (pair? q))
        (assert (pair? (car q)))
        (string #\- (caar q))))
    ;;
    (set! rest
          (for-each-opt
           actuals
           (make <opt-spec>
                 options: formals)
           (lambda (opt #optional arg)
             (if arg
                 (begin
                   (if (table-key-present? accum opt)
                       (error "Argument `~a' supplied more than once" 
                              (inv opt)))
                   (table-insert! accum opt arg))
                 (begin
                   (if (table-key-present? accum opt)
                       (error "Flag `-~a' supplied more than once" 
                              (inv opt)))
                   (table-insert! accum opt #t))))))
    ;;
    (if (pair? rest)
        (table-insert! accum '*rest rest))
    ;;
    (let loop ((p patterns))
      (if (null? p)
          (print-usage formals patterns)
          (let ((m (option-pattern-match (car p) accum inv)))
            (if m
                (m)
                (loop (cdr p))))))))


(define (option-pattern-match pattern given inv)
  (let ((label (car pattern))
        (determinant-set (caadr pattern))
        (determined-set (cadadr pattern))
        (proc (caddr pattern)))
    ;;
    (if (every? (lambda (d)
                  (table-key-present? given d))
                determinant-set)
        (let* ((visit (hash-table-copy given))
               (kwds '()))
          ;;
          (define (more (name <symbol>) value)
            (table-remove! visit name)
            (set! kwds (append kwds (list (symbol->keyword name) value))))
          ;;
          (for-each
           (lambda (d)
             (if (and (pair? d) (eq? (car d) '?))
                 ;; it's optional...
                 (let ((d (cadr d)))
                   (if (table-key-present? given d)
                       (more d (table-lookup given d))))
                 (if (table-key-present? given d)
                     (more d (table-lookup given d))
                     (let ((supply (if (null? determinant-set)
                                       "no flags are supplied"
                                       (~ "~a ~a supplied"
                                          (english-join 
                                           (map (lambda (o)
                                                  (string-append
                                                   "`" (inv o) "'"))
                                                determinant-set))
                                          (if (= (length determinant-set) 1)
                                              "is"
                                              "are")))))
                       (if (eq? d '*rest)
                           (error "Additional arguments are required when ~a"
                                  supply)
                         (error "`~a' is required when ~a"
                                (inv d)
                                supply))))))
           determined-set)
          ;;
          (for-each
           (lambda (d)
             (more d (table-lookup given d)))
           determinant-set)
          ;;
          (lambda ()
            (apply proc kwds)))
        #f)))

(define (english-join lst #optional (logic default: "and"))
  (case (length lst)
    ((1) (car lst))
    ((2) (string-append (car lst) " " logic " " (cadr lst)))
    ((0) "")
    (else
     (string-append
      (string-join ", "
                   (map (lambda (j)
                          (list-ref lst j))
                        (range (- (length lst) 1))))
      " " logic " "
      (last lst)))))


#|
    for example...

(define *option-patterns*
  `(("Create a new build"
     ((create) (family build (? previous)))
     ,(lambda (#key create family build (previous default: #f))
        ...))
    ("Inject a file into a build"
     ((add) (family build (? mimetype) (? kind) (? targetsys)))
     ,(lambda (#key add family build 
                    ;; need to supply 'default:' attributes on these,
                    ;; because the user may not supply them...
                    (mimetype default: #f)
                    (kind default: #f)
                    (targetsys default: #f))
        ...))))

(define (main args)
  (dispatch-option-patterns args 
                            *mbi-options*
                            *option-patterns*))

|#
