#|------------------------------------------------------------*-Scheme-*--|
 | File:    modules/compiler/consts.scm
 |
 |          Copyright (C)1997 Donovan Kolbly <d.kolbly@rscheme.org>
 |          as part of the RScheme project, licensed for free use.
 |          See <http://www.rscheme.org/> for the latest information.
 |
 | File version:     1.5
 | File mod date:    2003-07-17 11:42:36
 | System build:     v0.7.3.4-b7u, 2007-05-30
 | Owned by module:  compiler
 |
 | Purpose:          Manipulate compile-time constant expressions
 `------------------------------------------------------------------------|#

(define (compile-time-const? (ic <icode>))
  (or (instance? ic <ic-const>)
      (and (instance? ic <ic-tl-ref>)
	   (const-tlv? (var ic)))))

(define (compile-time-const-value (ic <icode>))
  (cond
   ((instance? ic <ic-const>)
    (value ic))
   ((instance? ic <ic-tl-ref>)
    (xform (var ic) 'value))
   (else
    (error "compile-time-const-value: invalid ~s" ic))))
      

;; class-constant? checks it's argument (<icode>) for the property
;; of being a compile-time constant whose value is a class.
;;
;; in the current implementation, certain syntactic positions
;; in the object system are required to be class constants,
;; such as the superclasses of class and type declaration forms

(define (const-tlv? b)
  (let ((a (actual-bdg b)))
    (and (instance? a <top-level-var>)
	 (write-prot a))))

(define (class-tlv? b)
  (or (and (const-tlv? b)
           ;; the value may itself be a <patch>, when a variable
           ;; is defined using `define-constant', e.g.,
           ;; (define-constant <tuple> <vector>)
           (target-class? (actual-value (value (actual-bdg b)))))
      (let ((a (actual-bdg b)))
        (if (and (instance? a <top-level-var>)
                 (target-class? (actual-value (value a))))
            (begin
              ;; the value is not a constant, but does have a class value.
              ;; it may or may not wind up being implemented the way the 
              ;; user expects, depending on how they expect it and whether
              ;; we compile in a TLREF or inline the actual value...
              (warning "type expr: ~s is not a constant" (name b))
              #t)
            #f))))

(define (class-constant? (ic <icode>))
  (and (instance? ic <ic-tl-ref>)
       (class-tlv? (var ic))))

(define (parse-const-expr expr lex-envt dyn-envt)
  (let ((ic (fold-const (compile expr lex-envt dyn-envt 'value))))
    (if (compile-time-const? ic)
	(value ic)
	(error/semantic "expression is not ct-constant: ~s" expr))))

(define (fold-const (ic <icode>))
  (if (and (instance? ic <ic-tl-ref>)
	   (const-tlv? (actual-bdg (var ic))))
      (make-const (xform (var ic) 'value) (mode ic))
      ic))

	   