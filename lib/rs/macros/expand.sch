; Copyright 1992 William Clinger
;
; Permission to copy this software, in whole or in part, to use this
; software for any lawful purpose, and to redistribute this software
; is granted subject to the restriction that all copies made of this
; software must include this copyright notice in full.
;
; I also request that you send me a copy of any improvements that you
; make to this software so that they may be incorporated within it to
; the benefit of the Scheme community.
;
; The external entry points and kernel of the macro expander.
;
; Part of this code is snarfed from the Twobit macro expander.

(define define-syntax-scope
  (let ((flag 'letrec))
    (lambda args
      (cond ((null? args) flag)
            ((not (null? (cdr args)))
             (apply m-warn
                    "Too many arguments passed to define-syntax-scope"
                    args))
            ((memq (car args) '(letrec letrec* let*))
             (set! flag (car args)))
            (else (m-warn "Unrecognized argument to define-syntax-scope"
                          (car args)))))))

(define m-quit             ; assigned by macro-expand
  (lambda (v) v))

(define (macro-expand def-or-exp)
  (call-with-current-continuation
   (lambda (k)
     (set! m-quit k)
     (set! renaming-counter 0)
     (desugar-definitions def-or-exp global-syntactic-environment))))

(define (desugar-definitions exp env)
  (letrec 
    ((define-loop 
       (lambda (exp rest first)
         (cond ((and (pair? exp)
                     (eq? (syntactic-lookup env (car exp))
                          denotation-of-begin)
                     (pair? (cdr exp)))
                (define-loop (cadr exp) (append (cddr exp) rest) first))
               ((and (pair? exp)
                     (eq? (syntactic-lookup env (car exp))
                          denotation-of-define))
                (let ((exp (desugar-define exp env)))
                  (cond ((and (null? first) (null? rest))
                         exp)
                        ((null? rest)
                         (cons begin1 (reverse (cons exp first))))
                        (else (define-loop (car rest)
                                           (cdr rest)
                                           (cons exp first))))))
               ((and (pair? exp)
                     (eq? (syntactic-lookup env (car exp))
                          denotation-of-define-syntax)
                     (null? first))
                (define-syntax-loop exp rest))
               ((and (null? first) (null? rest))
                (m-expand exp env))
               ((null? rest)
                (cons begin1 (reverse (cons (m-expand exp env) first))))
               (else (cons begin1
                           (append (reverse first)
                                   (map (lambda (exp) (m-expand exp env))
                                        (cons exp rest))))))))
     
     (desugar-define
      (lambda (exp env)
        (cond 
         ((null? (cdr exp)) (m-error "Malformed definition" exp))
         ; (define foo) syntax is transformed into (define foo (undefined)).
         ((null? (cddr exp))
          (let ((id (cadr exp)))
            (redefinition id)
            (syntactic-bind-globally! id (make-identifier-denotation id))
            (list define1 id undefined1)))
         ((pair? (cadr exp))
          ; lambda0 is an unforgeable lambda, needed here because the
          ; lambda expression will undergo further expansion.
          (desugar-define `(,define1 ,(car (cadr exp))
                                     (,lambda0 ,(cdr (cadr exp))
                                               ,@(cddr exp)))
                          env))
         ((> (length exp) 3) (m-error "Malformed definition" exp))
         (else (let ((id (cadr exp)))
                 (redefinition id)
                 (syntactic-bind-globally! id (make-identifier-denotation id))
                 `(,define1 ,id ,(m-expand (caddr exp) env)))))))
     
     (define-syntax-loop 
       (lambda (exp rest)
         (cond ((and (pair? exp)
                     (eq? (syntactic-lookup env (car exp))
                          denotation-of-begin)
                     (pair? (cdr exp)))
                (define-syntax-loop (cadr exp) (append (cddr exp) rest)))
               ((and (pair? exp)
                     (eq? (syntactic-lookup env (car exp))
                          denotation-of-define-syntax))
                (if (pair? (cdr exp))
                    (redefinition (cadr exp)))
                (if (null? rest)
                    (m-define-syntax exp env)
                    (begin (m-define-syntax exp env)
                           (define-syntax-loop (car rest) (cdr rest)))))
               ((null? rest)
                (m-expand exp env))
               (else (cons begin1
                           (map (lambda (exp) (m-expand exp env))
                                        (cons exp rest)))))))
     
     (redefinition
      (lambda (id)
        (if (symbol? id)
            (if (not (identifier?
                      (syntactic-lookup global-syntactic-environment id)))
                (m-warn "Redefining keyword" id))
            (m-error "Malformed variable or keyword" id)))))
    
    ; body of letrec
    
    (define-loop exp '() '())))

; Given an expression and a syntactic environment,
; returns an expression in core Scheme.

(define (m-expand exp env)
  (if (not (pair? exp))
      (m-atom exp env)
      (let ((keyword (syntactic-lookup env (car exp))))
        (case (denotation-class keyword)
          ((special)
           (cond
            ((eq? keyword denotation-of-quote)         (m-quote exp))
            ((eq? keyword denotation-of-lambda)        (m-lambda exp env))
            ((eq? keyword denotation-of-if)            (m-if exp env))
            ((eq? keyword denotation-of-set!)          (m-set exp env))
            ((eq? keyword denotation-of-begin)         (m-begin exp env))
            ((eq? keyword denotation-of-let-syntax)    (m-let-syntax exp env))
            ((eq? keyword denotation-of-letrec-syntax) (m-letrec-syntax exp env))
            ((or (eq? keyword denotation-of-define)
                 (eq? keyword denotation-of-define-syntax))
             (m-error "Definition out of context" exp))
            (else (m-bug "Bug detected in m-expand" exp env))))
          ((macro) (m-macro exp env))
          ((identifier) (m-application exp env))
          (else (m-bug "Bug detected in m-expand" exp env))))))

(define (m-atom exp env)
  (cond ((not (symbol? exp))
         ; Here exp ought to be a boolean, number, character, or string,
         ; but I'll allow for non-standard extensions by passing exp
         ; to the underlying Scheme system without further checking.
         exp)
        (else (let ((denotation (syntactic-lookup env exp)))
                (case (denotation-class denotation)
                  ((special macro)
                   (m-error "Syntactic keyword used as a variable" exp env))
                  ((identifier) (identifier-name denotation))
                  (else (m-bug "Bug detected by m-atom" exp env)))))))

(define (m-quote exp)
  (if (= (safe-length exp) 2)
      (list quote1 (m-strip (cadr exp)))
      (m-error "Malformed quoted constant" exp)))

(define (m-lambda exp env)
  (if (> (safe-length exp) 2)
      (let* ((formals (cadr exp))
             (alist (rename-vars (make-null-terminated formals)))
             (env (syntactic-rename env alist))
             (body (cddr exp)))
        (list lambda1
              (rename-formals formals alist)
              (m-body body env)))
      (m-error "Malformed lambda expression" exp)))

(define (m-body body env)
  (define (loop body env defs)
    (if (null? body)
        (m-error "Empty body"))
    (let ((exp (car body)))
      (if (and (pair? exp)
               (symbol? (car exp)))
          (let ((denotation (syntactic-lookup env (car exp))))
            (case (denotation-class denotation)
              ((special)
               (cond ((eq? denotation denotation-of-begin)
                      (loop (append (cdr exp) (cdr body)) env defs))
                     ((eq? denotation denotation-of-define)
                      (loop (cdr body) env (cons exp defs)))
                     (else (finalize-body body env defs))))
              ((macro)
               (m-transcribe exp
                             env
                             (lambda (exp env)
                               (loop (cons exp (cdr body))
                                     env
                                     defs))))
              ((identifier)
               (finalize-body body env defs))
              (else (m-bug "Bug detected in m-body" body env))))
          (finalize-body body env defs))))
  (loop body env '()))

(define (finalize-body body env defs)
  (if (null? defs)
      (let ((body (map (lambda (exp) (m-expand exp env))
                       body)))
        (if (null? (cdr body))
            (car body)
            (cons begin1 body)))
      (let* ((alist (rename-vars '(quote lambda set!)))
             (env (syntactic-alias env alist standard-syntactic-environment))
             (new-quote  (cdr (assq 'quote alist)))
             (new-lambda (cdr (assq 'lambda alist)))
             (new-set!   (cdr (assq 'set!   alist))))
        (define (desugar-definition def)
          (if (> (safe-length def) 2)
              (cond ((pair? (cadr def))
                     (desugar-definition
                      `(,(car def)
                        ,(car (cadr def))
                        (,new-lambda
                          ,(cdr (cadr def))
                          ,@(cddr def)))))
                    ((= (length def) 3)
                     (cdr def))
                    (else (m-error "Malformed definition" def env)))
              (m-error "Malformed definition" def env)))
        (define (expand-letrec bindings body)
          `((,new-lambda ,(map car bindings)
                         ,@(map (lambda (binding)
                                  `(,new-set! ,(car binding)
                                              ,(cadr binding)))
                                bindings)
                           ,@body)
            ,@(map (lambda (binding) `(,new-quote unspecified)) bindings)))
        (m-expand (expand-letrec (map desugar-definition (reverse defs))
                                 body)
                  env))))

(define (m-if exp env)
  (let ((n (safe-length exp)))
    (if (or (= n 3) (= n 4))
        (cons if1 (map (lambda (exp) (m-expand exp env)) (cdr exp)))
        (m-error "Malformed if expression" exp env))))

(define (m-set exp env)
  (if (= (safe-length exp) 3)
      `(,set!1 ,(m-expand (cadr exp) env) ,(m-expand (caddr exp) env))
      (m-error "Malformed assignment" exp env)))

(define (m-begin exp env)
  (if (positive? (safe-length exp))
      `(,begin1 ,@(map (lambda (exp) (m-expand exp env)) (cdr exp)))
      (m-error "Malformed begin expression" exp env)))

(define (m-application exp env)
  (if (> (safe-length exp) 0)
      (map (lambda (exp) (m-expand exp env))
           exp)
      (m-error "Malformed application")))

; I think the environment argument should always be global here.

(define (m-define-syntax exp env)
  (cond ((and (= (safe-length exp) 3)
              (symbol? (cadr exp)))
         (m-define-syntax1 (cadr exp)
                           (caddr exp)
                           env
                           (define-syntax-scope)))
        ((and (= (safe-length exp) 4)
              (symbol? (cadr exp))
              (memq (caddr exp) '(letrec letrec* let*)))
         (m-define-syntax1 (cadr exp)
                           (cadddr exp)
                           env
                           (caddr exp)))
        (else (m-error "Malformed define-syntax" exp env))))

(define (m-define-syntax1 keyword spec env scope)
  (case scope
    ((letrec)  (m-define-syntax-letrec keyword spec env))
    ((letrec*) (m-define-syntax-letrec* keyword spec env))
    ((let*)    (m-define-syntax-let* keyword spec env))
    (else      (m-bug "Weird scope" scope)))
  (list quote1 keyword))

(define (m-define-syntax-letrec keyword spec env)
  (syntactic-bind-globally!
   keyword
   (m-compile-transformer-spec spec env)))

(define (m-define-syntax-letrec* keyword spec env)
  (let* ((env (syntactic-extend (syntactic-copy env)
                                (list keyword)
                                '((fake denotation))))
         (transformer (m-compile-transformer-spec spec env)))
    (syntactic-assign! env keyword transformer)
    (syntactic-bind-globally! keyword transformer)))

(define (m-define-syntax-let* keyword spec env)
  (syntactic-bind-globally!
   keyword
   (m-compile-transformer-spec spec (syntactic-copy env))))

(define (m-let-syntax exp env)
  (if (and (> (safe-length exp) 2)
           (every1? (lambda (binding)
                      (and (pair? binding)
                           (symbol? (car binding))
                           (pair? (cdr binding))
                           (null? (cddr binding))))
                    (cadr exp)))
      (m-body (cddr exp)
              (syntactic-extend env
                                (map car (cadr exp))
                                (map (lambda (spec)
                                       (m-compile-transformer-spec
                                        spec
                                        env))
                                     (map cadr (cadr exp)))))
      (m-error "Malformed let-syntax" exp env)))

(define (m-letrec-syntax exp env)
  (if (and (> (safe-length exp) 2)
           (every1? (lambda (binding)
                      (and (pair? binding)
                           (symbol? (car binding))
                           (pair? (cdr binding))
                           (null? (cddr binding))))
                    (cadr exp)))
      (let ((env (syntactic-extend env
                                   (map car (cadr exp))
                                   (map (lambda (id)
                                          '(fake denotation))
                                        (cadr exp)))))
        (for-each (lambda (id spec)
                    (syntactic-assign!
                     env
                     id
                     (m-compile-transformer-spec spec env)))
                  (map car (cadr exp))
                  (map cadr (cadr exp)))
        (m-body (cddr exp) env))
      (m-error "Malformed let-syntax" exp env)))

(define (m-macro exp env)
  (m-transcribe exp
                env
                (lambda (exp env)
                  (m-expand exp env))))

; To do:
; Clean up alist hacking et cetera.
