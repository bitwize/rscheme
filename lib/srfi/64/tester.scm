
(define (indent->spaces n)
  (make-string (* n 2) #\space))

(define-constant (test-on-test-simple r test-result)
  (format #t "~a: ~a\n" 
          (test-runner-test-name r)
          (cdr (assq 'test-kind test-result))))

(define-constant (test-on-final-simple r)
  (format #t "~d tests: ~d pass, ~d fail, ~d skip\n"
          (+ (test-runner-pass-count r)
             (test-runner-fail-count r)
             (test-runner-xpass-count r)
             (test-runner-xfail-count r)
             (test-runner-skip-count r))
          (+ (test-runner-pass-count r)
             (test-runner-xfail-count r))
          (+ (test-runner-xpass-count r)
             (test-runner-fail-count r))
          (test-runner-skip-count r)))
          

(define-class <test-runner> (<object>)
  (group-stack init-value: '())
  (test-runner-on-test init-value: test-on-test-simple
                       setter: test-runner-on-test!)
  (test-runner-on-final init-value: test-on-final-simple 
                        setter: test-runner-on-final!)
  (test-runner-test-name init-value: "")
  (implicitly-created? init-value: #f)
  (accum init-value: #f)
  (in-progress init-value: #f)  ; may be a <test-result> during execution
  (test-runner-aux-value setter: test-runner-aux-value! init-value: #f))

(define-class <test-group> (<object>)
  name
  location
  (start-time init-function: time)
  (end-time init-value: #f)
  (end-location init-value: #f)
  (results init-value: '())
  (skip-list init-value: '())
  (fail-list init-value: '()))

(define-method close ((self <test-group>) loc)
  (set-end-location! self loc)
  (set-end-time! self (time)))

(define-method initialize ((self <test-runner>))
  (set-accum! self (make-vector $num-result-types 0))
  (set-group-stack! self
                    (list
                     (make <test-group>
                           name: #f   ; the only nameless group is the root
                           location: #f))))

(define (test-runner-null)
  (make <test-runner>
        test-runner-on-final: (lambda (self)
                                ;; do nothing
                                (values))
        test-runner-on-test: (lambda (self info)
                               ;; do nothing
                               (values))))

(define (make-default-test-runner)
  (make <test-runner>
        test-runner-on-final: (lambda (self)
                                (print-test-details
                                 (car (group-stack self))
                                 0
                                 '()))))

(define (test-runner-simple)
  (make <test-runner>))

(define $num-result-types 5)

(define-method print-test-details ((self <test-group>) indent summary)
  (format #t "~a>> BEGIN ~s" (indent->spaces indent) (name self))
  (if (location self)
      (format #t " (~a:~d)" 
              (car (location self))
              (cadr (location self))))
  (newline)
  (let ((s (cons (make-vector $num-result-types 0) summary)))
    (for-each
     (lambda (sub)
       (print-test-details sub (+ indent 1) s))
     (reverse (results self)))
    ;;
    (format #t "~a<< END ~s (~a)" 
            (indent->spaces indent) 
            (name self)
            (time-time (end-time self) (start-time self)))
    (if (end-location self)
        (format #t " (~a:~d)" 
                (car (end-location self))
                (cadr (end-location self))))
    (newline)
    (format #t "~a   ~d tests: ~d pass (~d exp), ~d fail (~d exp), ~d skip\n"
            (indent->spaces indent)
            (reduce + 0 (vector->list (car s)))
            (+ (vector-ref (car s) 0)
               (vector-ref (car s) 2))
            (vector-ref (car s) 0)
            (+ (vector-ref (car s) 1)
               (vector-ref (car s) 3))
            (vector-ref (car s) 3)
            (vector-ref (car s) 4))))

(define-class <test-result> (<object>)
  (properties init-value: '#())
  status
  (signalled init-value: #f)
  (name init-value: #f)
  (location init-value: #f))

(define-class <failure-with-aux> (<condition>)
  auxillary)

(define (signal-aux-failure . info)
  (signal (make <failure-with-aux>
                auxillary: info)))


(define-method auxillary ((self <test-result>))
  (if (instance? (signalled self) <failure-with-aux>)
      (auxillary (signalled self))
      #f))
      
(define (result-status->index s)
  (case s
    ((pass) 0)
    ((fail) 1)
    ((xpass) 2)
    ((xfail) 3)
    ((skip) 4)
    (else
     (error "invalid result status: ~s" s))))

(define (test-runner-pass-count (self <test-runner>)) 
  (vector-ref (accum self) 0))

(define (test-runner-fail-count (self <test-runner>)) 
  (vector-ref (accum self) 1))

(define (test-runner-xpass-count (self <test-runner>)) 
  (vector-ref (accum self) 2))

(define (test-runner-xfail-count (self <test-runner>)) 
  (vector-ref (accum self) 3))

(define (test-runner-skip-count (self <test-runner>)) 
  (vector-ref (accum self) 4))

(define-method print-test-details ((self <test-result>) indent summary)
  (format #t "~a" (indent->spaces indent))
  (if (location self)
      (format #t " (~a:~d)"
              (car (location self))
              (cadr (location self))))
  (if (name self)
      (format #t " ~s" (name self)))
  (format #t " ==> ~s\n" (status self))
  ;;
  (let ((k (result-status->index (status self))))
    (for-each (lambda (s)
                (vector-set! s k (+ 1 (vector-ref s k))))
              summary))
  ;;
  (if (signalled self)
      (format #t "~a   ***signalled***\n~a   ~a"
              (indent->spaces indent)
              (indent->spaces indent)
              (signalled self)))
  ;;
  (if (auxillary self)
      (case (car (auxillary self))
        ((equal?)
         (format #t "~a   (expected `equal?'\n" 
                 (indent->spaces indent))
         (format #t "~a   between ~s\n" 
                 (indent->spaces indent)
                 (caddr (auxillary self)))
         (format #t "~a       and ~s)\n" 
                 (indent->spaces indent)
                 (cadr (auxillary self)))))))


(define-class <test-error> (<condition>) :abstract)

(define-class <test-nesting-error> (<test-error>)
  location
  current
  expect)

(define-class <test-not-testing-error> (<test-error>)
  location)

(define-class <test-count-mismatch-error> (<test-error>)
  location
  current
  expect)

(define-class <test-expectation-error> (<test-error>)
  location)

(define-method display-object ((self <test-nesting-error>) port)
  (format port "~a:~d: test-end: expected to end ~s test\n"
          (car (location self))
          (cadr (location self))
          (expect self))
  (format port "~a:~d: << this is where the current test level, ~s,  started\n"
          (car (location (car (group-stack (current self)))))
          (cadr (location (car (group-stack (current self)))))
          (name (car (current self)))))

(define-method display-object ((self <test-count-mismatch-error>) port)
  (format port "~a:~d: test-end: expected to have done ~d tests in group ~s, really did ~d"
          (car (location self))
          (cadr (location self))
          (expect self)
          (length (results (car (group-stack (current self)))))))

(define-method display-object ((self <test-not-testing-error>) port)
  (if (location self)
      (format port "~a:~d: "
              (car (location self))
              (cadr (location self))))
  (format port "test-end: not currently doing testing\n"))

(define $test-runner-uninit (cons "(no test running)" '()))

(define test-runner-current 
  (make-parameter 
   $test-runner-uninit
   (lambda (item)
     (if (or (eq? item $test-runner-uninit)
             (instance? item <test-runner>))
         item
         (error "test-runner-current: only test runners allowed, not: ~s" item)))
   'test-runner-current))

(define test-runner-factory
  (make-parameter 
   test-runner-simple
   (lambda (x)
     (if (procedure? x)
         x
         (error "test-runner-factory: only procedures allowed, not: ~s" x)))
   'test-runner-factory))
    
(define (get-runner-or-error loc)
  (let ((c (test-runner-current)))
    (if (eq? c $test-runner-uninit)
        (signal (make <test-not-testing-error>
                      location: loc))
        c)))

(define (get-or-create-runner)
  (let ((c (test-runner-current)))
    (if (eq? c $test-runner-uninit)
        (let ((r (make-default-test-runner)))
          (test-runner-current r)
          (set-implicitly-created?! r #t)
          r)
        c)))

(define (push-test-group (tname <string>) loc)
  (let* ((r (get-or-create-runner))
         (new-group (make <test-group>
                          name: tname
                          location: loc
                          fail-list: (fail-list (car (group-stack r)))
                          skip-list: (skip-list (car (group-stack r))))))
    ;;
    (set-results! (car (group-stack r))
                  (cons new-group (results (car (group-stack r)))))
    ;;
    (set-group-stack! r (cons new-group (group-stack r)))
    (values)))

(define (pop-test-group loc tname tcount)
  (let ((r (get-runner-or-error loc)))
    ;;
    (if (and tcount
             (not (= tcount (length (results (car (group-stack r)))))))
        (signal (make <test-count-mismatch-error>
                      location: loc
                      expect: tcount
                      current: r)))
    ;;
    (if (and tname
             (not (string=? tname (name (car (group-stack r))))))
        (signal (make <test-nesting-error>
                      current: r
                      location: loc
                      expect: tname)))
    ;;
    (close (car (group-stack r)) loc)
    ;;
    (set-group-stack! r (cdr (group-stack r)))
    (if (and (null? (cdr (group-stack r)))
             (implicitly-created? r))
        (begin
          (close (car (group-stack r)) loc)
          ((test-runner-on-final r) r)
          (test-runner-current $test-runner-uninit)))
    (values)))

;;;

(define-syntax (test-begin name)
  (push-test-group name (*FUNCTION* form)))
                                   
(define-syntax test-end
  (syntax-form (name)
    (pop-test-group (*FUNCTION* form) (check-string name) #f))
  (syntax-form (name count)
    (pop-test-group (*FUNCTION* form) (check-string name) count))
  (syntax-form ()
    (pop-test-group (*FUNCTION* form) #f #f)))

(define (bump-status-counter (self <test-runner>) status)
  (let ((k (result-status->index status)))
    (vector-set! (accum self) k (+ 1 (vector-ref (accum self) k)))
    (values)))

(define (record-result (run <test-runner>) (c <test-result>))
  (bump-status-counter run (status c))
  (runner-record-result run c))

(define-method runner-record-result ((self <test-runner>) (c <test-result>))
  (let (((r <test-group>) (car (group-stack self))))
    ;;
    (set-results! r (cons c (results r)))
    ;;
    (if (test-runner-on-test self)
        ;; set things up for the hook environment
        (let ((l (location c)))
          (set-test-runner-test-name! self
                                      (or (name c) ""))
          ((test-runner-on-test self)
           self
           (append
            (list (cons 'test-kind (status c)))
            (if l
                (list
                 (cons 'source-file (car (location c)))
                 (cons 'source-line (cadr (location c))))
                '())))))
    (values)))

(define (test-thunk* loc name expr thunk #key (aux default: #f))
  ;;
  (let ((run (get-runner-or-error loc))
        (rez (make <test-result>
                   properties: (if (eq? expr $no-expr)
                                   '#()
                                   (vector 'source-form expr))
                   location: loc
                   name: name
                   status: #f
                   signalled: #f)))
    ;;
    (set-in-progress! run rez)
    ;;
    (bind ((flag err (if (test-skip? loc name)
                         'skip
                         (handler-case
                          (if (thunk) 'pass 'fail)
                          ((<condition> condition: e)
                           (values 'fail e))))))
      (case flag
        ((pass) (if (test-expect-fail? loc name) 
                    (set! flag 'xpass)))
        ((fail) (if (test-expect-fail? loc name) 
                    (set! flag 'xfail))))
      ;;
      (set-status! rez flag)
      (set-signalled! rez err)
      ;;
      (record-result run rez))))

;; can't just use #f to indicate that no expression is available,
;; because #f is a valid expression!

(define-constant $no-expr '#(none))

(define (test-thunk-full . r)
  (case (length r)
    ((1) (test-thunk* #f #f $no-expr (car r)))
    ((2) (test-thunk* #f (car r) $no-expr (cadr r)))
    (else 
     (error "test-thunk: Wrong number of arguments ~d, expected 1 or 2"
            (length r)))))

(define-syntax test-thunk
  (syntax-form (thunk)
    (test-thunk* (*FUNCTION* form) #f (mquote thunk) thunk))
  (syntax-form (name thunk)
    (test-thunk* (*FUNCTION* form) name (mquote thunk) thunk))
  (else
   test-thunk-full))

(define-syntax test-assert
  (syntax-form (expr)      (test-assert #f expr))
  (syntax-form (name expr) (test-thunk*
                            (*FUNCTION* form)
                            name
                            (mquote expr)
                            (lambda () expr))))


(define-syntax test-cmp
  (syntax-form (cmp name e x) 
    (test-thunk*
     (*FUNCTION* form)
     name
     (mquote e)
     (lambda () 
       (let ((tmp e)
             (expect x))
         (test-result-set! 'expected-result x)
         (test-result-set! 'actual-result tmp)
         (or (cmp tmp expect)
             (signal-aux-failure 
              (mquote cmp)
              tmp
              expect)))))))

(define-syntax test-equal
  (syntax-form (e x)      (test-equal #f e x))
  (syntax-form (name e x) (test-cmp equal? name e x)))

(define-syntax test-eqv
  (syntax-form (e x)      (test-eqv #f e x))
  (syntax-form (name e x) (test-cmp eqv? name e x)))

(define-syntax test-eq
  (syntax-form (e x)      (test-eqv #f e x))
  (syntax-form (name e x) (test-cmp eq? name e x)))

(define (error-type-compatible? etype c)
  (cond
   ((eq? etype #t)
    #t)
   ((procedure? etype)
    (etype c))
   (else
    (instance? c etype))))

(define (test-error* loc name etype expr thunk)
  (if (and (not (eq? etype #t))
           (not (and (class? etype) (subclass? etype <condition>)))
           (not (procedure? etype)))
      (error "This implementation of SRFI-64 only supports the `#t' error type, RScheme conditions, and predicate procedures"))
  ;;
  (test-thunk*
   loc
   name
   expr
   (lambda () 
     (handler-case
      (begin
        (thunk)
        #f)
      ((<condition> condition: c)
       (if (error-type-compatible? etype c)
           ;; we pass
            #t
            ;; an error was raised, but it was the wrong one
            ;; XXX: should capture some aux data
            #f))))))

(define-syntax test-error
  (syntax-form (e)        (test-error #f #t e))
  (syntax-form (name e)   (test-error name #t e))
  (syntax-form (name etype e) (test-error* (*FUNCTION* form)
                                           name
                                           etype
                                           (mquote e)
                                           (lambda () e))))

;;;
;;;  Test Specifiers
;;;

(define-method compile-specifier ((self <function>))
  self)

(define-method compile-specifier ((self <string>))
  (test-match-name self))

(define (test-match-name name)
  (lambda (namepath)
    (string=? (car namepath) name)))

(define (test-match-all . sub)
  (let ((sub (map compile-specifier sub)))
    (lambda (np)
      (every? (lambda (f)
                (f np))
              sub))))

(define (test-match-any . sub)
  (let ((sub (map compile-specifier sub)))
    (lambda (np)
      (any? (lambda (f)
              (f np))
            sub))))

(define (test-runner-count-in-group runner)
  (+ 1 (length (results (car (group-stack runner))))))

(define (test-match-nth n #optional (count default: 1))
  (let* ((first (+ (test-runner-count-in-group (test-runner-current)) n -1))
         (last (+ first count -1)))
    (lambda (np)
      (let ((i (test-runner-count-in-group (test-runner-current))))
        ;(format #t "match-nth ~s, count in group: ~s\n" n i)
        (and (>= i first) (<= i last))))))

#|         
(define (test-match-nth n #optional count)
  (let ((invoked 0))
    (if count
        (lambda (np)
          (set! invoked (+ invoked 1))
          (and (>= invoked n)
               (< invoked (+ n count))))
        (lambda (np)
          (set! invoked (+ invoked 1))
          (= invoked n)))))
  |#

;;;

(define-class <specifier> (<object>)
  path-predicate
  (location init-value: #f))

(define (any-specifier? loc new-name runner-getter)
  (let* ((r (get-runner-or-error loc))
         (spec-list (runner-getter (car (group-stack r)))))
    (if (null? spec-list)
        #f
        (let ((path (cons (or new-name "")
                          (map name (group-stack r)))))
          (any? (lambda ((s <specifier>))
                  ((path-predicate s) path))
                spec-list)))))
  
(define (test-expect-fail? loc new-name)
  (any-specifier? loc new-name fail-list))

(define (test-skip? loc new-name)
  (any-specifier? loc new-name skip-list))

(define (test-group* loc name thunk #optional cleanup)
  (if (test-skip? loc name)
      (bump-status-counter 
       (get-runner-or-error loc)
       'skip)
      (dynamic-wind
          (lambda ()
            (push-test-group name loc))
          thunk
          (lambda ()
            (if cleanup (cleanup))
            (pop-test-group loc #f #f)))))

(define (test-skip* (spec <specifier>))
  (let* ((r (get-runner-or-error (location spec))))
    (set-skip-list! (car (group-stack r))
                    (cons spec
                          (skip-list (car (group-stack r)))))))

(define (test-expect-fail* (spec <specifier>))
  (let* ((r (get-runner-or-error (location spec))))
    (set-fail-list! (car (group-stack r))
                    (cons spec
                          (fail-list (car (group-stack r)))))))
  
  
(define (eval-specifier spec #optional loc)
  (make <specifier>
        path-predicate: (if (string? spec)
                            (test-match-name spec)
                            spec)
        location: loc))

(define-syntax (test-expect-fail spec)
  (test-expect-fail* (eval-specifier spec (*FUNCTION* form))))
  
(define-syntax (test-skip spec)
  (test-skip* (eval-specifier spec (*FUNCTION* form))))

(define-syntax (test-group name . body)
  (test-group* (*FUNCTION* form)
               name
               (lambda () (begin . body))))

;;;
;;;  User-defined runners
;;;

(define (test-runner-create)
  (make-default-test-runner))

(define (test-with-runner* (r <test-runner>) thunk)
  (parameterize ((test-runner-current r))
    (thunk)))

(define-syntax (test-with-runner runner . form)
  (test-with-runner*
   runner
   (lambda ()
     (begin . form))))
  
(define (test-runner-group-path (self <test-runner>))
  (cdr (reverse (map name (group-stack self)))))

;;;

(define (skip-if-none specs)
  (make <specifier>
        path-predicate: (lambda (np)
                          (every? (lambda ((s <specifier>))
                                    (not ((path-predicate s) np)))
                                  specs))))

(define (test-apply* (self <test-runner>) args)
  (let* ((thunk (last args))
         (specs (map eval-specifier (reverse (cdr (reverse args)))))
         (g (car (group-stack self)))
         (saved (skip-list g)))
    ;;
    (if (null? specs)
        (error "test-apply: At least one specifier must be supplied"))
    ;;
    (set-skip-list! g (cons (skip-if-none specs)
                            saved))
    (thunk)
    (set-skip-list! g saved)
    (values)))

    
(define (test-apply . args)
  (if (instance? (car args) <test-runner>)
      (test-apply* (car args) (cdr args))
      (test-apply* (get-or-create-runner) args)))

(define-syntax test-group-with-cleanup
  ;;
  (syntax-form (name body cleanup)
    (test-group*
     (*FUNCTION* form)
     name
     (lambda () body)
     (lambda () cleanup)))
  ;;
  (syntax-form (name form1 form2 form3 . more)
    (test-group-with-cleanup
     name
     (begin form1 form2)
     form3
     . more)))

;;;

(define (current-test-result (self <test-runner>))
  (or (in-progress self)
      (car (results (car (group-stack self))))))

(define (test-result-alist* (self <test-runner>))
  (let* ((r (current-test-result self)))
    (append
     (if (location r)
         (list 
          (cons 'source-file (car (location r)))
          (cons 'source-line (cadr (location r))))
         '())
     (if (status r)
         (list
          (cons 'kind (status r)))
         '())
     (properties-as-alist r))))

(define (properties-as-alist self)
  (to-alist (properties self)))

(define-method to-alist ((self <pair>))
  self)

(define-method to-alist ((self <vector>))
  (let loop ((i (- (vector-length self) 2))
             (l '()))
    (if (>= i 0)
        (loop (- i 2)
              (cons (cons (vector-ref self i) (vector-ref self (+ i 1))) l))
        l)))

;;;

(define (test-result-ref* (self <test-runner>) (key <symbol>) dflt)
  ;; chintzy
  (let ((a (test-result-alist* self)))
    (cond
     ((assq key a) => cdr)
     (else dflt))))

(define (test-result-set!* (self <test-runner>) (key <symbol>) val)
  (if (memq key '(kind
                  source-file 
                  source-line
                  source-form))
      (error "Not allowed to set standard properties")
      (set-property! (current-test-result self) key val)))

(define (test-result-clear* (self <test-runner>) (key <symbol>))
  (remove-property! (current-test-result self) key))
                  
(define-syntax test-result-alist
  (syntax-form (runner) (test-result-alist* runner))
  (syntax-form () (test-result-alist* (test-runner-current))))
  
(define-syntax test-result-ref
  (syntax-form (runner p dflt) (test-result-ref* runner p dflt))
  (syntax-form (runner p) (test-result-ref* runner p #f))
  (syntax-form (p) (test-result-ref* (test-runner-current) p #f)))

(define-syntax test-result-set!
  (syntax-form (runner p val) (test-result-set!* runner p val))
  (syntax-form (p val) (test-result-set!* (test-runner-current) p val)))

(define-syntax test-result-clear
  (syntax-form (runner p) (test-result-clear* runner p))
  (syntax-form (p) (test-result-clear* (test-runner-current) p)))
