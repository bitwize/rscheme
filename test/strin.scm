#|------------------------------------------------------------*-Scheme-*--|
 | File:    test/strin.scm
 |
 |          Copyright (C)1997 Donovan Kolbly <d.kolbly@rscheme.org>
 |          as part of the RScheme project, licensed for free use.
 |          See <http://www.rscheme.org/> for the latest information.
 |
 | File version:     1.3
 | File mod date:    1999-01-07 22:23:38
 | System build:     v0.7.3.4-b7u, 2007-05-30
 |
 `------------------------------------------------------------------------|#


(define-class <more> (<buffered-input-port>)
  (more-stuff type: <list>))

(define-method provide-more-input ((self <more>))
  (if (pair? (more-stuff self))
      (let ((s (more-stuff self)))
	(set-more-stuff! self (cdr s))
	(car s))
      #f))

(define-method more-input-ready? ((self <more>))
  (pair? (more-stuff self)))

;;

(define (make-more . strs)
  (make <more>
	buffered-input-buffer: (if (pair? strs)
				   (car strs)
				   "")
	more-stuff: (if (pair? strs)
			(cdr strs)
			'())))

(format #t "===========\n")
(define m (make-more "foo-" "bar "))
(format #t "item => ~s\n" (read m))
(format #t "more ready? ~s\n" (char-ready? m))
(format #t "item => ~s\n" (read m))
(format #t "more ready? ~s\n" (char-ready? m))

(format #t "===========\n")
(define m (make-more "(foo)" "bar "))
(format #t "item => ~s\n" (read m))
(format #t "more ready? ~s\n" (char-ready? m))
(format #t "item => ~s\n" (read m))
(format #t "more ready? ~s\n" (char-ready? m))

(format #t "===========\n")
(define m (make-more "x" "y"))
(format #t "item => ~s ~s\n" (peek-char m) (read-char m))
(format #t "item => ~s ~s\n" (peek-char m) (read-char m))
(format #t "item => ~s ~s\n" (peek-char m) (read-char m))

;;;
;;;  also test std-input-port and string-input-port specialized `collect' meths
;;;

(test-section
 (scan-token)
 ;;
 (test-section
  (std-input-port)
  ;;
  (let ((f (open-input-file "lexical.dat")))
    (bind ((t v (scan-token f)))
      (check '<curly-braced> t)
      (check "this is {a} test" v))
    (bind ((t v (scan-token f)))
      (check '<symbol> t)
      (check 'X v))
    (close-input-port f)))
 ;;
 (test-section
  (string-input-port)
  ;;
  (let ((f (open-input-string "{this is {a} test}X")))
    (bind ((t v (scan-token f)))
      (check '<curly-braced> t)
      (check "this is {a} test" v))
    (bind ((t v (scan-token f)))
      (check '<symbol> t)
      (check 'X v))
    (close-input-port f))))

