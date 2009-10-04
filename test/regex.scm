#|------------------------------------------------------------*-Scheme-*--|
 | File:    test/regex.scm
 |
 |          Copyright (C)1997 Donovan Kolbly <d.kolbly@rscheme.org>
 |          as part of the RScheme project, licensed for free use.
 |          See <http://www.rscheme.org/> for the latest information.
 |
 | File version:     1.3
 | File mod date:    1997-11-29 23:10:42
 | System build:     v0.7.3.4-b7u, 2007-05-30
 |
 `------------------------------------------------------------------------|#

,(use regex)

;;; 
;;;  test reg-expr->proc
;;;

(define pat0 (reg-expr->proc '(save (+ alpha))))

(define str "1 2 Buckle my shoe")

(check '(4 10 "Buckle")   (values->list (pat0 str)))
(check '(11 13 "my")      (values->list (pat0 str 10)))

;;;
;;;  test `unformat'
;;;

(define u (unformat->proc "foo(~s,~d)"))

(expect-to-fail (u "foo(,10)"))
(check #f (u "foo(foo,)"))
(expect-to-fail (u "foo(foo,..99..)"))

(check 'foo (u "foo(foo,10)"))
(check '(1 (foo)) (u "foo((1 ;blech\n(foo)),10)"))

(expect-to-fail (unformat->proc "foo bar"))

;;;
;;;  test can-start? (cr 611)
;;;

(define p0 (reg-expr->proc '(seq (* #\a) #\b)))

(check #t (reg-expr-can-start? p0 "a"))
(check #t (reg-expr-can-start? p0 "b"))
(check #f (reg-expr-can-start? p0 "c"))

(define p1 (reg-expr->proc '(seq (save (* #\a)) #\b)))
(check #t (reg-expr-can-start? p1 ""))
(check #t (reg-expr-can-start? p1 "a"))
(check #t (reg-expr-can-start? p1 "b"))
(check #f (reg-expr-can-start? p1 "c"))

(define p2 (reg-expr->proc '(seq (or (save (* #\a))
				     (save (* #\b)))
				 (save (+ digit)))))

(check #t (reg-expr-can-start? p2 ""))
(check #t (reg-expr-can-start? p2 "a"))
(check #t (reg-expr-can-start? p2 "b"))
(check #t (reg-expr-can-start? p2 "7"))

(check #t (reg-expr-can-start? p2 "aa"))
(check #t (reg-expr-can-start? p2 "bb"))
(check #t (reg-expr-can-start? p2 "77"))

(check #t (reg-expr-can-start? p2 "aa7"))
(check #t (reg-expr-can-start? p2 "bb6"))
(check #t (reg-expr-can-start? p2 "777"))

(check #f (reg-expr-can-start? p2 "c"))
(check #f (reg-expr-can-start? p2 "ab7"))
(check #f (reg-expr-can-start? p2 "ba6"))
(check #f (reg-expr-can-start? p2 "bbc"))
(check #t (reg-expr-can-start? p2 "77a"))
