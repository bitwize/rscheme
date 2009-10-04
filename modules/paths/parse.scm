#|------------------------------------------------------------*-Scheme-*--|
 | File:    modules/paths/parse.scm
 |
 |          Copyright (C)1997 Donovan Kolbly <d.kolbly@rscheme.org>
 |          as part of the RScheme project, licensed for free use.
 |          See <http://www.rscheme.org/> for the latest information.
 |
 | File version:     1.5
 | File mod date:    1997-11-29 23:10:37
 | System build:     v0.7.3.4-b7u, 2007-05-30
 | Owned by module:  paths
 |
 | Purpose:          Parse a unix-style pathname into a <{dir,file}-name>
 |------------------------------------------------------------------------|
 | Notes:
 |      completely rewritten 1995.07.18
 `------------------------------------------------------------------------|#

(define (parse-dname dname)
  (string-split dname #\/))

(define (parse-fname fname)
  (let* ((elems (string-split fname #\/))
	 (file (last elems)))
    (if (null? (cdr elems))
	(set! elems '())
	(set-cdr! (list-tail elems (fixnum- (length elems) 2)) '()))
    (split-extn elems file)))

(define (split-extn pre file)
  (let loop ((prev #f))
    (let ((n (string-search file #\. (if prev (add1 prev) 0))))
      (if n
	  (loop n)
	  (if prev
	      (values pre
		      (substring file 0 prev)
		      (substring file (add1 prev)))
	      (values pre file #f))))))
;;
; simplify takes the dirpath output from parse-fname and:
    ; removes "."
    ; removes ""
    ; removes "something/.."
    ; and replaces ".." with up
    ; and other leading forms with appropriate <root-dir>'s
    ; and makes sure it starts with either #f or a <root-dir>
; the output from parse-fname is a list

(define (simplify steps)
  (if (null? steps)
      (values #f '())
      (let ((base (special-root (car steps))))
	(let loop ((i (if base (cdr steps) steps))
		   (r '()))
	  (if (null? i)
	      (values base (reverse r))
	      (let (((s <string>) (car i)))
		(cond
		 ((string=? s "..")
		  (if (and (pair? r) (string? (car r)))
		      (loop (cdr i) (cdr r))
		      (loop (cdr i) (cons 'up r))))
		 ((string=? s ".")
		  (loop (cdr i) r))
		 ((string=? s "")
		  (loop (cdr i) r))
		 (else
		  (loop (cdr i) (cons s r))))))))))
