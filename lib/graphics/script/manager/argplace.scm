(define-module-extend compiler ()

(define (compile-squoted sf form lex-envt dyn-envt mode)
  ;;
  (define (form-at form)
    (let ((t (current-location-table)))
      (print t)
      (cons (or (and t (table-lookup t form))
                #f)
            (compile-point-stack dyn-envt))))
  ;;
  (define (compile-mquoted mquoted-expr lex-envt)
    (if (symbol? mquoted-expr)
	(let ((bdg (lookup-aliased mquoted-expr lex-envt dyn-envt)))
	  (if (substitution? bdg)
	      (compile-mquoted (expr bdg)
                               (envt bdg))
              (compile `(vector ',mquoted-expr
                                ',(form-at mquoted-expr)
                                (lambda ()
                                  ,mquoted-expr)
                                (lambda (%x)
                                  (set! ,mquoted-expr %x)))
                       lex-envt
                       dyn-envt
                       'value)))
        (compile `(vector ',mquoted-expr
                          ',(form-at mquoted-expr)
                          (lambda ()
                            ,mquoted-expr)
                          #f)
                 lex-envt
                 dyn-envt
                 'value)))
  
  ;;
  (if (eq? (length form) 2)
      (compile-mquoted (cadr form) lex-envt)
      (error/syntax "Badly constructed: ~a" form)))

(define (insert-reifier tle)
  (bind! tle (make <special-form>
                   name: 'reify!
                   compiler-proc: compile-squoted
                   compiler-description: 'reify!)))

(&module (export insert-reifier))
)

#|
(with-module compiler (insert-reifier *self*))

(define-syntax (bar x)
  (reify! x))

(define-syntax (foo p q)
  (list (reify! p) (bar q)))

(define (blah)
  (let ((x 1)
        (y 2))
    (foo x (+ x y))))

|#
