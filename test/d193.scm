#|------------------------------------------------------------*-Scheme-*--|
 | File:    test/d193.scm
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

;;
;;  test allocation primitives
;;

(define (port->string port)
  (let ((s (open-output-string)))
    (let loop ()
      (let ((l (read-line port)))
	(if (eof-object? l)
	    (close-output-port s)
	    (begin
	      (write-string s l)
	      (newline s)
	      (loop)))))))

(define (output-from script args)
  (let ((p (open-input-process (format #f "rs -script ~a ~j" script args))))
    (let ((s (port->string p)))
      (close-input-port p)
      s)))

(test-section
 (script-environment)
 ;;
 (check "Hello\n[foo]\n[bar]\n"
	(output-from "d193.script" '("foo" "bar"))))
