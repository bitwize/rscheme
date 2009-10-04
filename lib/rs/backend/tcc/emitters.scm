
(define *bytecode-parsers* (make-vector 256))

(define-macro (declare-dynamic-compiler (code bc i port) . body)
  `(vector-set!
    *bytecode-parsers*
    ,code
    (lambda ((,bc <byte-coded>) (,i <fixnum>) ,port %ctx)
      (let-syntax ((ref (syntax-form (kk)
                          (bvec-ref ,bc (fixnum+ ,i kk))))
                   (insn-size (syntax-form (nn)
                                (fixnum+ ,i nn)))
                   (push (syntax-form (type)
                           (gstack-push %ctx type)))
                   (pop (syntax-form (type)
                          (let ((top (car (stack %ctx))))
                            (assert (eq? (cadr top) type))
                            (set-stack! %ctx (cdr (stack %ctx)))
                            (car top)))))
        ,@body))))
  
(define-macro (declare-opcode (code bc i) . body)
  '(values))

(define-syntax (emit-c-return port n)
  (if (< n 2)
      (format port "  RETURN~d();\n" n)
      (format port "  RETURN(~d);\n" n)))

(define-syntax (emit-c-literal port n)
  (format port "  ~a = LITERAL(~d);\n" (push 'obj) (+ n 1)))

(define-syntax (emit-c-closure port n)
  (format port "  ~a = CLOSURE(~d);\n" (push 'obj) (+ n 1)))

(define-macro (emit-c-apply port n)
  `(format ,port "  APPLY( ~d, ~a );\n" ,n (pop 'obj)))

(load "/tmp/a.scm")
