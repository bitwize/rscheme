
(define-class <extended-type-check-failed> (<error>)
  type-check-required-type
  type-check-actual-value)

(define-method display-object ((self <extended-type-check-failed>) port)
  (format port "type check failed; expected: ~s\n    saw a ~s: ~#@*70s\n"
          (type-check-required-type self)
          (class-name (object-class (type-check-actual-value self)))
          (type-check-actual-value self)))

(define-method hash-code ((self <<class>>))
  (class-hash self))

(define *type-predicate-cache* (make-table))
(define *type-checker-cache* (make-table))

(define (bind-type type envt denvt)
  (cond 
   ((symbol? type)
    (let ((k (lookup-aliased type envt denvt)))
      (if (instance? k <top-level-var>)
          (value k)
          (error "~s: not a TLV" type))))
   ((pair? type)
    (if (memq (car type) '(singleton quote))
        type
        (cons (car type)
              (map (lambda (sub)
                     (bind-type sub envt denvt))
                   (cdr type)))))
   (else
    type)))

(define-macro (is-type? item type)
  (let* ((bound (bind-type type $envt $dynamic-envt))
         (f (table-lookup *type-predicate-cache* bound)))
    (if (not f)
        (begin
          (set! f (eval-in-envt
                   `(lambda '(is-type? ,type) (item)
                            ,(gen-type-predicate 'item bound))
                   *self*))
          (table-insert! *type-predicate-cache* bound f)))
    `(',f ,item)))

;; the difference between `type-assert' and `type-check' is that the
;; former will get turned off if/when assertions are turned off

(define-macro (type-assert item type)
  `(assert (is-type? ,item ,type)))

(define (fail-type-check type value)
  (signal (make <extended-type-check-failed>
                type-check-required-type: type
                type-check-actual-value: value)))

(define-macro (type-check item type)
  (let* ((bound (bind-type type $envt $dynamic-envt))
         (f (table-lookup *type-checker-cache* bound)))
    (if (not f)
        (begin
          (set! f (eval-in-envt
                   `(lambda '(type-check ,type) (item)
                      (if ,(gen-type-predicate 'item bound)
                          (values)
                          (fail-type-check ',type item)))
                   *self*))
          (table-insert! *type-checker-cache* type f)))
    `(',f ,item)))
  
#|
(define-macro (is-type?* item type)
  (if (symbol? item)
      (cond
       ((class? type)
        (cond
          ((eq? type <list>) `(list? ,item))
          ((eq? type <pair>) `(pair? ,item))
          ((eq? type <fixnum>) `(fixnum? ,item))
          ((eq? type <string>) `(string? ,item))
          ((eq? type <vector>) `(vector? ,item))
          ((eq? type <symbol>) `(symbol? ,item))
          (else `(instance? ,item ',type))))
       ((pair? type)
        (case (car type)
          ((list) 
           (let ((loopn (gensym))
                 (loopi (gensym)))
             `(if (null? ,item)
                  #t
                  (if (list? ,item)
                      (let ,loopn (((,loopi <pair> :trust-me) ,item))
                           (if (is-type?* (car ,loopi) ,(cadr type))
                               (let ((next (cdr ,loopi)))
                                 (if (null? next)
                                     #t
                                     (,loopn next)))
                               #f))
                      #f))))
          ((singleton eq?)
           `(eq? ,item ',(cadr type)))
          ((or)
           `(or ,@(map (lambda (sub)
                         `(is-type?* ,item ,sub))
                       (cdr type))))
          ((vector)
           (let ((loopn (gensym))
                 (loopi (gensym)))
             `(and (vector? ,item)
                   (let ,loopn (((,loopi <fixnum>) (vector-length ,item)))
                        (if (eq? ,loopi 0)
                            #t
                            (if (is-type?* (vector-ref ,item (sub1 ,loopi))
                                           ,(cadr type))
                                (,loopn (sub1 ,loopi))
                                #f))))))
          (else
           (error "don't know how to process extended type: ~s" type))))
       (else
        (error "don't know how to process extended type: ~s" type)))
      (let ((save (gensym)))
        `(let ((,save ,item))
           (is-type?* ,save ,type)))))
|#

(define (gen-type-predicate item type)
  (if (symbol? item)
      (cond
       ((class? type)
        (cond
         ((eq? type <list>) `(list? ,item))
         ((eq? type <pair>) `(pair? ,item))
         ((eq? type <fixnum>) `(fixnum? ,item))
         ((eq? type <string>) `(string? ,item))
         ((eq? type <vector>) `(vector? ,item))
         ((eq? type <symbol>) `(symbol? ,item))
         (else `(instance? ,item ',type))))
       ((immob? type)
        `(eq? ,item ',type))
       ((pair? type)
        (case (car type)
          ((cons)
           `(if (pair? ,item)
                (and ,(gen-type-predicate `(car ,item) (cadr type))
                     ,(gen-type-predicate `(cdr ,item) (caddr type)))
                #f))
          ((list) 
           (let ((loopn (gensym))
                 (loopi (gensym)))
             `(if (null? ,item)
                  #t
                  (if (list? ,item)
                      (let ,loopn (((,loopi <pair> :trust-me) ,item))
                           (if ,(gen-type-predicate `(car ,loopi)
                                                    (cadr type))
                               (let ((next (cdr ,loopi)))
                                 (if (null? next)
                                     #t
                                     (,loopn next)))
                               #f))
                      #f))))
          ((singleton quote)
           `(eq? ,item ',(cadr type)))
          ((or)
           `(or ,@(map (lambda (sub)
                         (gen-type-predicate item sub))
                       (cdr type))))
          ((vector)
           (let ((loopn (gensym))
                 (loopi (gensym)))
             `(and (vector? ,item)
                   (let ,loopn (((,loopi <fixnum>) (vector-length ,item)))
                        (if (eq? ,loopi 0)
                            #t
                            (if ,(gen-type-predicate 
                                  `(vector-ref ,item (sub1 ,loopi))
                                  (cadr type))
                                (,loopn (sub1 ,loopi))
                                #f))))))
          (else
           (error "don't know how to process extended type: ~s" type))))
       (else
        (error "don't know how to process extended type: ~s" type)))
      (let ((save (gensym)))
        `(let ((,save ,item))
           ,(gen-type-predicate save type)))))

#|
(define-patterns extended-type-check)

(define (define-type-checker pattern rewrite)
  (add-rule *extended-type-check-patterns* (list pattern rewrite)))

|#
