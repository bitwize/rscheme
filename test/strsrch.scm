#|------------------------------------------------------------*-Scheme-*--|
 | File:    test/strsrch.scm
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


(define-method to-u ((ch <ascii-char>))
  (integer->unicode-char (char->integer ch)))

(define-method to-u ((str <string>))
  (let ((b (bvec-alloc <unicode-string> (* 2 (bvec-length str)))))
    (for-each (lambda (i)
		(bvec-write-unsigned-16 b (* i 2) (bvec-ref str i))
		(values))
	      (range (bvec-length str)))
    b))

(define str1 "ab-01-xy-pq")

(define str2 (clone str1))
(string-set! str2 2 #\nul)
(string-set! str2 5 #\nul)

(define (check-it* str pairs)
  (for-each (lambda (p)
	      (check (cadr p) (string-search str (car p))))
	    pairs))

(define (check-it str pairs)
  (check-it* str pairs)
  (check-it* (to-u str) pairs)
  (let ((u-pairs (map (lambda (s)
			(cons (to-u (car s)) (cdr s)))
		      pairs)))
    (check-it* str u-pairs)
    (check-it* (to-u str) u-pairs)))
    

(check-it str1 '((#\- 2) ("-" 2) ("-x" 5)))
(check-it str2 '((#\nul 2) ("\0" 2) ("\0x" 5)))

(check '("ab\0\.01\0xy" "pq") (string-split str2 #\-))
(check '("ab" "01" "xy-pq") (string-split str2 #\nul))

(check '("ab\0\.01\0xy" "pq") (string-split str2 "-"))
(check '("ab" "01" "xy-pq") (string-split str2 "\0"))

(check  1 (string-search "123" #\2))
(check  1 (string-search "123" #\2 0))
(check  1 (string-search "111" #\1 1))
(check  2 (string-search "111" #\1 2))
(check #f (string-search "111" #\1 3))
(check #f (string-search "111" #\nul))
(expect-to-fail (string-search "111" #\1 4))
(expect-to-fail (string-search "111" #\1 -1))
