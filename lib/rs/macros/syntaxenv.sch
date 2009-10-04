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
; Syntactic environments.
;
; A syntactic environment maps identifiers to denotations,
; where a denotation is one of
;
;    (special <special>)
;    (macro <rules> <env>)
;    (identifier <id>)
;
; and where <special> is one of
;
;    quote
;    lambda
;    if
;    set!
;    begin
;    define
;    define-syntax
;    let-syntax
;    letrec-syntax
;    syntax-rules
;
; and where <rules> is a compiled <transformer spec> (see R4RS),
; <env> is a syntactic environment, and <id> is an identifier.

(define standard-syntactic-environment
  '((quote         . (special quote))
    (lambda        . (special lambda))
    (if            . (special if))
    (set!          . (special set!))
    (begin         . (special begin))
    (define        . (special define))
    (define-syntax . (special define-syntax))
    (let-syntax    . (special let-syntax))
    (letrec-syntax . (special letrec-syntax))
    (syntax-rules  . (special syntax-rules))
    (...           . (identifier ...))
    (:::           . (identifier :::))))

; An unforgeable synonym for lambda, used to expand definitions.

(define lambda0 (string->symbol " lambda "))

; The global-syntactic-environment will always be a nonempty
; association list since there is no way to remove the entry
; for lambda0.  That entry is used as a header by destructive
; operations.

(define global-syntactic-environment
  (cons (cons lambda0
              (cdr (assq 'lambda standard-syntactic-environment)))
        (copy-alist standard-syntactic-environment)))

(define (global-syntactic-environment-set! env)
  (set-cdr! global-syntactic-environment env)
  (values))

(define (syntactic-copy env)
  (copy-alist env))

(define (syntactic-bind-globally! id denotation)
  (if (and (identifier? denotation)
           (eq? id (identifier-name denotation)))
      (letrec ((remove-bindings-for-id
                (lambda (bindings)
                  (cond ((null? bindings) '())
                        ((eq? (caar bindings) id)
                         (remove-bindings-for-id (cdr bindings)))
                        (else (cons (car bindings)
                                    (remove-bindings-for-id (cdr bindings))))))))
        (global-syntactic-environment-set!
         (remove-bindings-for-id (cdr global-syntactic-environment))))
      (let ((x (assq id global-syntactic-environment)))
        (if x
            (begin
	      (set-cdr! x denotation)
	      (values))
            (global-syntactic-environment-set!
             (cons (cons id denotation)
                   (cdr global-syntactic-environment)))))))

(define (syntactic-divert env1 env2)
  (append env2 env1))

(define (syntactic-extend env ids denotations)
  (syntactic-divert env (map cons ids denotations)))

(define (syntactic-lookup-raw env id)
  (let ((entry (assq id env)))
    (if entry
        (cdr entry)
        #f)))

(define (syntactic-lookup env id)
  (or (syntactic-lookup-raw env id)
      (make-identifier-denotation id)))

(define (syntactic-assign! env id denotation)
  (let ((entry (assq id env)))
    (if entry
	(begin
	  (set-cdr! entry denotation)
	  (values))
        (m-bug "Bug detected in syntactic-assign!" env id denotation))))

(define denotation-of-quote
  (syntactic-lookup standard-syntactic-environment 'quote))

(define denotation-of-lambda
  (syntactic-lookup standard-syntactic-environment 'lambda))

(define denotation-of-if
  (syntactic-lookup standard-syntactic-environment 'if))

(define denotation-of-set!
  (syntactic-lookup standard-syntactic-environment 'set!))

(define denotation-of-begin
  (syntactic-lookup standard-syntactic-environment 'begin))

(define denotation-of-define
  (syntactic-lookup standard-syntactic-environment 'define))

(define denotation-of-define-syntax
  (syntactic-lookup standard-syntactic-environment 'define-syntax))

(define denotation-of-let-syntax
  (syntactic-lookup standard-syntactic-environment 'let-syntax))

(define denotation-of-letrec-syntax
  (syntactic-lookup standard-syntactic-environment 'letrec-syntax))

(define denotation-of-syntax-rules
  (syntactic-lookup standard-syntactic-environment 'syntax-rules))

(define denotation-of-...
  (syntactic-lookup standard-syntactic-environment '...))

(define denotation-of-:::
  (syntactic-lookup standard-syntactic-environment ':::))

(define denotation-class car)

(define (special? denotation)
  (eq? (denotation-class denotation) 'special))

(define (macro? denotation)
  (eq? (denotation-class denotation) 'macro))

(define (identifier? denotation)
  (eq? (denotation-class denotation) 'identifier))

(define (make-identifier-denotation id)
  (list 'identifier id))

(define macro-rules cadr)
(define macro-env caddr)
(define identifier-name cadr)

(define (same-denotation? d1 d2)
  (or (eq? d1 d2)
      (and (identifier? d1)
           (identifier? d2)
           (eq? (identifier-name d1)
                (identifier-name d2)))))

; Renaming of variables.

; Given a datum, strips the suffixes from any symbols that appear within
; the datum, trying not to copy any more of the datum than necessary.
; Well, right now I'm just copying the datum, but I need to fix that!

(define (m-strip x)
  (cond ((symbol? x)
         (let ((chars (memv suffix-character
                            (reverse (string->list
                                      (symbol->string x))))))
           (if chars
               (string->symbol
                (list->string (reverse (cdr chars))))
               x)))
        ((pair? x)
         (cons (m-strip (car x))
               (m-strip (cdr x))))
        ((vector? x)
         (list->vector (map m-strip (vector->list x))))
        (else x)))

; Given a list of identifiers, returns an alist that associates each
; identifier with a fresh identifier.

(define (rename-vars vars)
  (set! renaming-counter (+ renaming-counter 1))
  (let ((suffix (string-append (string suffix-character)
                               (number->string renaming-counter))))
    (map (lambda (var)
           (if (symbol? var)
               (cons var
                     (string->symbol
                      (string-append (symbol->string var) suffix)))
               (error "Illegal variable" var)))
         vars)))

; Given a syntactic environment env to be extended, an alist returned
; by rename-vars, and a syntactic environment env2, extends env by
; binding the fresh identifiers to the denotations of the original
; identifiers in env2.

(define (syntactic-alias env alist env2)
  (syntactic-divert
   env
   (map (lambda (name-pair)
          (let ((old-name (car name-pair))
                (new-name (cdr name-pair)))
            (cons new-name
                  (syntactic-lookup env2 old-name))))
        alist)))

; Given a syntactic environment and an alist returned by rename-vars,
; extends the environment by binding the old identifiers to the fresh
; identifiers.

(define (syntactic-rename env alist)
  (syntactic-divert env
                    (map (lambda (old new)
                           (cons old (make-identifier-denotation new)))
                         (map car alist)
                         (map cdr alist))))

; Given a <formals> and an alist returned by rename-vars that contains
; a new name for each formal identifier in <formals>, renames the
; formal identifiers.

(define (rename-formals formals alist)
  (cond ((null? formals) '())
        ((pair? formals)
         (cons (cdr (assq (car formals) alist))
               (rename-formals (cdr formals) alist)))
        (else (cdr (assq formals alist)))))

(define renaming-counter 0)
