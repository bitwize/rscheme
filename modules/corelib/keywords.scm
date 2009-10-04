#|------------------------------------------------------------*-Scheme-*--|
 | File:    modules/corelib/keywords.scm
 |
 |          Copyright (C)1997 Donovan Kolbly <d.kolbly@rscheme.org>
 |          as part of the RScheme project, licensed for free use.
 |          See <http://www.rscheme.org/> for the latest information.
 |
 | File version:     1.6
 | File mod date:    1997-11-29 23:10:38
 | System build:     v0.7.3.4-b7u, 2007-05-30
 | Owned by module:  corelib
 |
 | Purpose:          keyword/flag handling (symbols that end/start with `:')
 |------------------------------------------------------------------------|
 | Notes:
 |      Currently, "keywords" are just symbols with a : on the
 |      end, but they may get their own class at some point
 |      
 |      this is included by compiler/util/keywords.scm
 `------------------------------------------------------------------------|#

(%strategy ccode
(define (keyword? x)
  (and (symbol? x)
       (let* (((s <string>) (symbol->string x))
	      ((n <fixnum>) (string-length s)))
	 (and (fixnum>? n 1)
	      (eq? (bvec-ref s (sub1 n)) 58))))) ;; `58' is the code for #\:

(define (flag? x)
  (and (symbol? x)
       ;; all strings are bvecs of length at least 1 (the terminating NUL
       ;; char), so we can reference byte 0 unconditionally
       (eq? (bvec-ref (symbol->string x) 0) 58)))

(define (keyword->symbol x)
  (assert (keyword? x))
  (let ((s (symbol->string x)))
    (string->symbol (substring s 0 (sub1 (string-length s))))))

(define (symbol->keyword (s <symbol>))
  (string->symbol (string-append (symbol->string s) ":")))

(define (flag->symbol x)
  (assert (flag? x))
  (let ((s (symbol->string x)))
    (string->symbol (substring s 1))))

(define (symbol->flag (s <symbol>))
  (string->symbol (string-append ":" (symbol->string s))))
)

(define (sa/as-strings items)
  (if (null? items)
      '()
      (let ((i (car items)))
	(cond
	 ((string? i)
	  (cons i (sa/as-strings (cdr items))))
	 ((symbol? i)
	  (cons (symbol->string i)
		(sa/as-strings (cdr items))))
	 ((fixnum? i)
	  (cons (fixnum->string i 10)
		(sa/as-strings (cdr items))))
	 (else
	  (error "symbol-append: invalid component `~s'" i))))))
	  
(define (symbol-append . items)
  (string->symbol (apply* (sa/as-strings items) string-append)))

