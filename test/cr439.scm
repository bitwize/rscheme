#|------------------------------------------------------------*-Scheme-*--|
 | File:    test/cr439.scm
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

(define-syntax (checkr a b)
  (check b a))

(checkr (vector-append '#(1 2 3)
		      '#(4 5)
		      '#(6)
		      '#()
		      '#(7)
		      '#(8 9)
		      '#(10))
       '#(1 2 3 4 5 6 7 8 9 10))

(checkr (vector-append) '#())
(expect-to-fail (vector-append '#(1 2 3) 'x))
(expect-to-fail (vector-append '#(1 2 3) 3))

(checkr (vector-slice '#(0 1 2 3) 0) '#(0 1 2 3))
(checkr (vector-slice '#(0 1 2 3) 1) '#(1 2 3))
(checkr (vector-slice '#(0 1 2 3) 2) '#(2 3))
(checkr (vector-slice '#(0 1 2 3) 3) '#(3))
(checkr (vector-slice '#(0 1 2 3) 4) '#())

(checkr (vector-slice '#(0 1 2 3) 0 1) '#(0))
(checkr (vector-slice '#(0 1 2 3) 3 1) '#(3))
(checkr (vector-slice '#(0 1 2 3) 4 0) '#())

(expect-to-fail (vector-slice '#(0 1 2 3) 0 5))
(expect-to-fail (vector-slice '#(0 1 2 3) 4 1))
(expect-to-fail (vector-slice '#(0 1 2 3) -1 2))
(expect-to-fail (vector-slice '#(0 1 2 3) 2 -1))
(expect-to-fail (vector-slice '#(0 1 2 3) 'x 1))
(expect-to-fail (vector-slice '#(0 1 2 3) 1 'x))

(checkr (subvector '#(0 1 2 3) 0) '#(0 1 2 3))
(checkr (subvector '#(0 1 2 3) 1) '#(1 2 3))
(checkr (subvector '#(0 1 2 3) 2) '#(2 3))
(checkr (subvector '#(0 1 2 3) 3) '#(3))
(checkr (subvector '#(0 1 2 3) 4) '#())

(checkr (subvector '#(0 1 2 3) 0 1) '#(0))
(checkr (subvector '#(0 1 2 3) 3 4) '#(3))
(checkr (subvector '#(0 1 2 3) 4 4) '#())

(expect-to-fail (subvector '#(0 1 2 3) 0 5))
(expect-to-fail (subvector '#(0 1 2 3) 4 5))
(expect-to-fail (subvector '#(0 1 2 3) -1 2))
(expect-to-fail (subvector '#(0 1 2 3) 2 1))
(expect-to-fail (subvector '#(0 1 2 3) 'x 1))
(expect-to-fail (subvector '#(0 1 2 3) 1 'x))
