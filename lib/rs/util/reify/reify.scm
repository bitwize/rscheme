
(define-class <reified-expression> (<object>)
  expr-form
  expr-source
  expr-getter
  expr-setter)

(define-method write-object ((self <reified-expression>) port)
  (format port "#[~a ~s" (name (object-class self)) (expr-form self))
  (format port "]"))

(define (compile-re form lex dyn mode)
  ;;
  (define (form-at form)
    (let ((t (current-location-table)))
      (cons (list (or (and t (table-lookup t form))
                      #f)
                  form)
            (compile-point-stack dyn))))
  ;;
  (compile
   `(make-gvec
     ',<reified-expression>
     ',form
     ',(form-at form)
     (lambda ()
       ,form)
     ,(if (symbol? form)
          `(lambda (%x)
             (set! ,form %x))
          #f))
   lex
   dyn
   mode))


(define (compile-reify sf form lex-envt dyn-envt mode)
  ;;
  (define (compile-mquoted mquoted-expr lex-envt)
    (if (symbol? mquoted-expr)
	(let ((bdg (lookup-aliased mquoted-expr lex-envt dyn-envt)))
	  (if (substitution? bdg)
	      (compile-mquoted (expr bdg)
                               (envt bdg))
              (compile-re mquoted-expr lex-envt dyn-envt mode)))
        (compile-re mquoted-expr lex-envt dyn-envt mode)))
  ;;
  (if (eq? (length form) 2)
      (compile-mquoted (cadr form) lex-envt)
      (error/syntax "Badly constructed: ~a" form)))

(bind! *self* (make <special-form>
                    name: 'reify!
                    compiler-proc: compile-reify
                    compiler-description: 'reify!))
