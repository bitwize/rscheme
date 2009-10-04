,(use tables)

(define-class <getopt-error> (<condition>) :abstract)

(define-class <getopt-unknown-option-error> (<getopt-error>)
  given)

(define-class <getopt-excess-value-error> (<getopt-error>)
  spec)

(define-class <getopt-missing-value-error> (<getopt-error>)
  spec)

(define-class <getopt-invalid-value-error> (<getopt-error>)
  spec
  given)

;;;

(define-method display-object ((self <getopt-unknown-option-error>) port)
  (format port "unknown option: ~s\n" (given self)))

(define-method display-object ((self <getopt-excess-value-error>) port)
  (format port "argument unexpectedly supplied for option '~a'\n" 
          (option-name (spec self))))

(define-method display-object ((self <getopt-missing-value-error>) port)
  (format port "no argument supplied for option '~a'\n" 
          (option-name (spec self))))

(define-method display-object ((self <getopt-invalid-value-error>) port)
  (format port "invalid argument supplied for option '~a': ~s\n" 
          (option-name (spec self))
          (given self)))

;;;

(define (option-arguments-mode o)
  (vector-ref o 0))

(define (option-symbol o)
  (vector-ref o 1))

(define (option-qualify o x)
  (let ((q ((vector-ref o 2) x)))
    (if (instance? q <getopt-error>)
        (begin
          (if (instance? q <getopt-invalid-value-error>)
              (set-spec! q o))
          (signal q))
        q)))

(define (option-name o)
  (vector-ref o 3))

;;;

(define (parse-opt seed args dict found)
  ;;
  (define (lookup-or-unknown key)
    (or (table-lookup dict key)
        (signal (make <getopt-unknown-option-error>
                      given: key))))
  ;;
  (letrec ((major (lambda (seed args)
                    (if (null? args)
                        seed
                        (let (((n <string>) (car args)))
                          (if (or (string=? n "")
                                  (not (char=? (string-ref n 0) #\-)))
                              (major (found seed (cdr args) #f n) (cdr args))
                              (dashed seed args n))))))
           (dashed (lambda (seed args (arg <string>))
                     (cond
                      ;;
                      ;;  Special handling for "--"
                      ;;
                      ((string=? arg "--")
                       (dashdash seed args))
                      ;;
                      ;;  Check for long options
                      ;;
                      ((and (> (string-length arg) 2)
                            (char=? (string-ref arg 0) #\-)
                            (char=? (string-ref arg 1) #\-))
                       (longopt seed args arg))
                      ;;
                      ;;  Process short options
                      (else
                       (shortopt seed args arg 1)))))
           ;;
           (shortopt (lambda (seed args (arg <string>) (i <fixnum>))
                       (if (>= i (string-length arg))
                           (major seed (cdr args))
                           (let ((opt (string-ref arg i)))
                             (cond
                              ((table-lookup dict opt)
                               => (lambda (spec)
                                    (case (option-arguments-mode spec)
                                      ((0)
                                       (shortopt
                                        (found
                                         seed
                                         (delay 
                                           (if (< (+ i 1) 
                                                  (string-length arg))
                                               (cons (string-append
                                                      "-" 
                                                      (substring arg (+ i 1)))
                                                     (cdr args))
                                               (cdr args)))
                                         (option-symbol spec))
                                        args
                                        arg
                                        (+ i 1)))
                                      ((1)
                                       (if (< (+ i 1) (string-length arg))
                                           (major (found 
                                                   seed
                                                   (cdr args)
                                                   (option-symbol spec)
                                                   (option-qualify 
                                                    spec
                                                    (substring arg (+ i 1))))
                                                  (cdr args))
                                           (if (null? (cdr args))
                                               (signal 
                                                (make <getopt-missing-value-error>
                                                      spec: spec))
                                               (major (found
                                                       seed
                                                       (cddr args)
                                                       (option-symbol spec)
                                                       (option-qualify
                                                        spec
                                                        (cadr args)))
                                                      (cddr args))))))))
                              (else
                               (signal (make <getopt-unknown-option-error>
                                             given: opt))))))))
           ;;
           (longopt (lambda (seed args arg)
                      (let ((q (string-search arg #\=)))
                        (if q
                            (let ((spec (lookup-or-unknown 
                                         (substring arg 2 q))))
                              (case (option-arguments-mode spec)
                                ((0)
                                 (signal (make <getopt-excess-value-error>
                                               spec: spec)))
                                ((1)
                                 (major (found seed
                                               (cdr args)
                                               (option-symbol spec)
                                               (option-qualify
                                                spec
                                                (substring arg (+ q 1))))
                                        (cdr args)))))
                            (let ((spec (lookup-or-unknown (substring arg 2))))
                              (case (option-arguments-mode spec)
                                ((0)
                                 (major (found seed
                                               (cdr args)
                                               (option-symbol spec))
                                        (cdr args)))
                                ((1)
                                 (if (null? (cdr args))
                                     (signal (make <getopt-missing-value-error>
                                                   spec: spec))
                                     (major (found seed
                                                   (cddr args)
                                                   (option-symbol spec)
                                                   (option-qualify
                                                    spec
                                                    (cadr args)))
                                            (cddr args))))))))))
           ;;
           (dashdash (lambda (seed args)
                       (cond
                        ((table-lookup dict #\-)        ; short '-' option
                         => (lambda (spec)
                              (eatit1 seed args spec)))
                        ;;
                        ((table-lookup dict "")         ; long "" option
                         => (lambda (spec)
                              (eatit1 seed args spec)))
                        ;;
                        (else
                         (signal (make <getopt-unknown-option-error>
                                       given: "--"))))))
           (eatit1 (lambda (seed args spec)
                     (case (option-arguments-mode spec)
                       ((0)
                        (major (found seed
                                      (cdr args) 
                                      (option-symbol spec))
                               (cdr args)))
                       ((1)
                        (if (null? (cdr args))
                            (signal (make <getopt-missing-value-error>
                                          spec: spec))
                            (major (found seed
                                          (cddr args)
                                          (option-symbol spec)
                                          (option-qualify spec (car args)))
                                   (cddr args))))))))
    ;;
    (major seed args)))

;;;

#|
(define (test-getopt)
  ;;
  (define (optdict . tuples)
    (let ((t (make-table)))
      (let loop ((p tuples))
        (if (null? p)
            t
            (begin
              (table-insert! t (car p) (vector (cadr p) 
                                               (caddr p) 
                                               (or (cadddr p) identity)
                                               (car p)))
              (loop (cddddr p)))))))
  ;;
  (define (report seed follow found #optional arg)
    (let ((n (length seed))
          (f (force follow)))
      (if arg
          (format #t "~d: found ~s with ~s  follows: ~s\n" n found arg f)
          (format #t "~d: found ~s          follows: ~s\n" n found f))
      (cons found seed)))
  ;;
  (define (tcase str . dict)
    (newline)
    (call-with-current-continuation
     (lambda (exit)
       (parse-opt '() 
                  (string-split str #\space) 
                  (apply optdict dict) 
                  (lambda (seed follow found #rest r)
                    (if (eq? found 'stop)
                        (begin
                          (format #t "~d: REST ARE ~s\n" 
                                  (length seed) 
                                  follow)
                          (exit))
                        (apply report seed follow found r)))))))
  ;;
  (tcase "-n3 -n 3 foo bar" #\n 1 'n integer-qualify)
  (tcase "--n=3 --n 3 foo" "n" 1 'n integer-qualify)
  (handler-case
   (tcase "--n=foo" "n" 1 'n integer-qualify)
   ((<getopt-invalid-value-error>) (values)))
  (tcase "-xvf/tmp/abc foo bar"
         #\x 0 'x #f
         #\v 0 'v #f
         #\f 1 'f #f)
  (tcase "-qO- http://foo/ -- bar baz"
         #\q 0 'q #f
         #\O 1 'O #f 
         #\- 0 'stop #f)
  (values))
|#

             
(define-class <opt-spec> (<object>)
  ;;
  ;;  `options' is an alist mapping strings (for long options)
  ;;  or chars (for short options) to the option name
  ;;  (usually a symbol) and the option argument spec
  ;;
  ;; an option spec is (option-name . arg-spec)
  ;;
  ;; an arg-spec is () => no arguments
  ;;             or (1 [qualifier]) => required argument
  ;;
  ;; where the optional qualifier is either a procedure of
  ;;  one argument which transforms the supplied argument,
  ;;  or one of the symbols:
  ;;
  ;;            <integer>       ; argument must be an integer number
  ;;
  ;;
  ;; for example,
  ;;    ((#\v verbose)            ; no arguments
  ;;     (#\f file 1)             ; one required argument
  ;;     (#\c count 1 <integer>)) ; one required integer argument
  ;;
  (options type: <list>)
  (dict init-value: '#uninit))

;;

(define (integer-qualify x)
  (let ((n (string->number x)))
    (if (integer? n)
        n
        ;; we build the exception here but it will get signalled
        ;; by `option-qualify'
        (make <getopt-invalid-value-error>
              spec: '#uninit
              given: x))))
;;

(define-method initialize ((self <opt-spec>))
  (let ((t (make-table)))
    (for-each
     (lambda (o)
       (let ((mode (if (= (length o) 2)
                       0
                       (case (caddr o)
                         ((1) 1)
                         ((?) (error "optional getopt args not supported"))
                         (else (error "invalid getopt arg mode: ~s"
                                      (caddr o))))))
             (qual (if (= (length o) 4)
                       (cond
                        ((eq? (cadddr o) '<integer>) integer-qualify)
                        ((procedure? (cadddr o)) (cadddr o))
                        (else
                         (error "getopt qualifier invalid: ~s" (cadddr o))))
                       identity)))
         (if (not (or (string? (car o))
                      (char? (car o))))
             (error "invalid getopt specifier: ~s" (car o)))
         ;;
         (table-insert! t (car o) (vector mode (cadr o) qual (car o)))))
     (options self))
    ;;
    (set-dict! self t)
    self))

;;;

(define (for-each-opt (args <list>)
                      (opt <opt-spec>)
                      proc)
  (reverse! (parse-opt '()
                       args
                       (dict opt)
                       (lambda (seed follow found #rest r)
                         (if found
                             (begin
                               (apply proc found r)
                               seed)
                             (cons (car r) seed))))))

#|
   To support the polymorphic-wierd 'getopt' interface
   (i.e., where the first call sets the list to parse and returns
    the first option, and subsequent calls return subsequent
    options) we need to use saved continuations to convert
    a callback procedure into an iterator

    ...
        something like what follows, but it hasn't been tested
    ...

(define (getopt contn spec)
  (if (procedure? contn)
      (contn)
      (call-with-current-continuation
       (lambda (exit)
         (reverse! (parse-opt 
                    '()
                    contn
                    (dict spec)
                    (lambda (seed follow found #rest r)
                      (if found
                          (begin
                            (call-with-current-continuation
                             (lambda (resume)
                               (if (null? r)
                                   (values found resume)
                                   (values found resume (car r)))))
                            seed)
                          (cons (car r) seed)))))))))
  
|#
