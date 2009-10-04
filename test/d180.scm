#|------------------------------------------------------------*-Scheme-*--|
 | File:    test/d180.scm
 |
 |          Copyright (C)1997 Donovan Kolbly <d.kolbly@rscheme.org>
 |          as part of the RScheme project, licensed for free use.
 |          See <http://www.rscheme.org/> for the latest information.
 |
 | File version:     1.4
 | File mod date:    1997-11-29 23:10:42
 | System build:     v0.7.3.4-b7u, 2007-05-30
 |
 `------------------------------------------------------------------------|#

;;
;;  test allocation primitives
;;


(test-section
 (explicit-allocation-area)
 ;;
 (check '#(x x x)
	(gvec-alloc-in-area *default-allocation-area*
			    <vector>
			    3
			    'x))
 ;;
 (check "xxx"
	(let ((t (bvec-alloc-in-area *default-allocation-area*
				     <string>
				     4
				     (char->integer #\x))))
	  (bvec-set! t 3 0)
	  t)))
 
;;

(test-section
 (implicit-default-area)
 ;;
 (test-section
  (gvecs)
  ;;
  (check '#(1 2 3) (clone '#(1 2 3)))
  (check '(1 . 2) (clone2 '#(1 2) <pair>))
  (check '#(1 2) (clone2 (cons 1 2) <vector>)))
 ;;
 (test-section
  (bvecs)
  ;;
  (compare-using
   string=?
   ;;
   (check "foo" (clone "foo"))
   (check "blech" (clone2 "blech" <string>))
   (check "blech" (clone2 (clone2 "blech" <byte-vector>) <string>))
   ))
 ;;
 (test-section
  (mixed-modes)

  (expect-to-fail
   (clone2 "foo" <vector>))

  (expect-to-fail
   (clone2 '(1 2) "foo"))))
 ;;
 
