#|------------------------------------------------------------*-Scheme-*--|
 | File:    test/all.scm
 |
 |          Copyright (C)1997 Donovan Kolbly <d.kolbly@rscheme.org>
 |          as part of the RScheme project, licensed for free use.
 |          See <http://www.rscheme.org/> for the latest information.
 |
 | File version:     1.6
 | File mod date:    1999-01-23 16:03:30
 | System build:     v0.7.3.4-b7u, 2007-05-30
 |
 `------------------------------------------------------------------------|#

#|
   this file should be loaded from the "test/" directory;
   some test cases rely on it

   % cd test
   % rs all.scm
|#

;;; use a feature-id to let suites run without this driver,
;;; and structure the driver as a module
;;;
;;;  each test suite we load will be in a fresh 
;;;  top-level contour that imports `usual-inlines' 
;;;  and `test.driver'
;;;

(define-module test.driver ()
  (&module
   (import usual-inlines compiler repl)
   (implements Test-Suite-Driver)
   ;;
   (load "expect.scm")
   ;;
   (export check
	   compare-using
	   test-section
	   expect-to-fail
	   get-error-report
	   load-file
	   with-place)))

,(use test.driver)
,(use regex repl compiler mlink)

;,(use compiler)

(define $line-break (string-append (make-string 70 #\=) "\n"))

(define (suite name)
  (display $line-break)
  (format #t "test file: ~s\n" name)
  (let ((envt (make-top-level-contour)))
    (use-in 'usual-inlines envt)
    (use-in 'test.driver envt)
    (load-file (string-append name ".scm") envt)))

(define match-suite-file
  (reg-expr->proc '(entire (seq (let name (+ (not #\.))) ".scm"))))

(define (find-suite-components)
  (let ((p (open-input-process "/bin/ls")))
    (call-with-list-extending
     (lambda (add)
       (let loop ()
	 (let ((l (read-line p)))
	   (if (eof-object? l)
	       (close-input-port p)
	       (bind ((s e name (match-suite-file l)))
		 (if (and s (not (member name '("all" "expect"))))
		     (add name))
		 (loop)))))))))

(define (run)
  ;;
  (for-each 
   suite
   (find-suite-components))
  ;;
  (if (null? (get-error-report))
      #t
      (begin
	(display $line-break)
	(display "ERROR SUMMARY\n")
	(for-each (lambda (e)
		    (display (reverse e))
		    (newline))
		  (get-error-report))
	#f)))
