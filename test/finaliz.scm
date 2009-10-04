#|------------------------------------------------------------*-Scheme-*--|
 | File:    test/finaliz.scm
 |
 |          Copyright (C)1997 Donovan Kolbly <d.kolbly@rscheme.org>
 |          as part of the RScheme project, licensed for free use.
 |          See <http://www.rscheme.org/> for the latest information.
 |
 | File version:     1.4
 | File mod date:    1997-11-29 23:10:42
 | System build:     v0.7.3.4-b7u, 2007-05-30
 |
 |------------------------------------------------------------------------|
 | Notes:
 |      Test finalization and weak pointers facility
 `------------------------------------------------------------------------|#

,(use sort objsys)

;; finalization...
   
;;
;;  the interaction between the save list and the <foo-thing>'s
;;  is similar to that between OS file descriptors and file ports.
;;
;;  the invariant is that the three initial entries in the save-list
;;  always exist (perhaps briefly out of touch, in the interval between
;;  the dropping of the last pointer and the invocation of the finalization
;;  routine

(define *unassigned-ids* '(10 20 30))

(define-class <foo-thing> (<object>)
  assigned-id)

(define-method finalize ((self <foo-thing>))
  (format #t "finalizing: ~s\n" self)
  (set! *unassigned-ids* (cons (assigned-id self) *unassigned-ids*)))

(define-method write-object ((self <foo-thing>) port)
  (format port "#[<foo-thing> ~s]" (assigned-id self)))

(define (make-foo)
  (let ((n (car *unassigned-ids*)))
    (set! *unassigned-ids* (cdr *unassigned-ids*))
    (format #t "assigning: ~s\n" n)
    (let ((f (make <foo-thing>
		   assigned-id: n)))
      (register-for-finalization f)
      f)))

(define (test-finalization)
  (define (free-list)
    (sort *unassigned-ids* <))
  (define (gc)
    (format #t " ---- GC\n")
    (gc-now)
    (gc-now))
  ;;
  (test-section
   (finalization)
   ;;
   (make-foo)
   (check (free-list) '(20 30))
   (gc)
   (check (free-list) '(10 20 30))
   ;;
   (make-foo)
   (let ((temp (make-foo)))
     (make-foo)
     (check (free-list) '())
     (gc)
     (check (free-list) '(10 30))
     (set! temp #f)
     (gc)
     (check (free-list) '(10 20 30)))))

;; weak pointers...


(define-class <weak-pair> (<object>) :weak1
  weak-pair-car
  weak-pair-cdr)

(define (weak-cons a b)
  (make-gvec <weak-pair> a b))

(define (weak-assq key lst)
  (let loop ((l lst))
    (if (null? l)
	#f
	(let (((h <weak-pair>) (car l)))
	  (if (eq? (weak-pair-car h) key)
	      h
	      (loop (cdr l)))))))

;;
;;  actually run the tests...
;;

(test-finalization)
