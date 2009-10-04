#|------------------------------------------------------------*-Scheme-*--|
 | File:    modules/regex/orcompil.scm
 |
 |          Copyright (C)1997 Donovan Kolbly <d.kolbly@rscheme.org>
 |          as part of the RScheme project, licensed for free use.
 |          See <http://www.rscheme.org/> for the latest information.
 |
 | File version:     1.4
 | File mod date:    1997-11-29 23:10:30
 | System build:     v0.7.3.4-b7u, 2007-05-30
 | Owned by module:  regex
 |
 | Purpose:          `or' regex form
 `------------------------------------------------------------------------|#

;; if this node is a single-char, and the previous
;; one was, too, then just merge them into one

(define (compile-reg-expr-or (rexpr <pair>))
  (let loop ((list-of-firsts '())
	     (list-of-lasts '())
	     ((minimal-len <fixnum>) 1000)
	     (remaining-exprs (cdr rexpr)))
    ;(format #t "compiling or: ~s\n" remaining-exprs)
    (if (null? remaining-exprs)
	;;
	;; out of branches to look at, so build the join
	;; and branch nodes if its not degenerate
	;;
	(begin
	  (assert (eq? (length list-of-lasts)
		       (length list-of-firsts)))
	  ;;
	  ;; degenerate case 1 -- (or) => match empty string only
	  ;;
	  (if (null? list-of-lasts)
	      (empty-string)
	      ;;
	      ;; degenerate case 2 -- (or x) ==> x
	      ;;
	      (if (null? (cdr list-of-lasts))
		  (values (car list-of-firsts)
			  (car list-of-lasts)
			  minimal-len)
		  ;;
		  ;; not a degenerate case
		  ;;
		  (generalized-or list-of-firsts list-of-lasts minimal-len))))
	;;
	;; another branch to process
	;;
	(bind ((first last len (compile-reg-expr-sub (car remaining-exprs))))
	  ;;
	  ;; see if we should combine it with the previous node
	  ;;
	  (if (and (eq? len 1)
		   (single-char-form first last)
		   (not (null? list-of-lasts))
		   (single-char-form (car list-of-firsts)
				     (car list-of-lasts)))
	      (let ((n (merge-single-char-forms first (car list-of-firsts))))
		(loop (cons n (cdr list-of-firsts))
		      (cons n (cdr list-of-lasts))
		      ;; we don't need to change minimal len,
		      ;; because something of length 1 was already
		      ;; included in it (ie, the previous one)
		      minimal-len
		      (cdr remaining-exprs)))
	      ;;
	      ;; don't combine
	      ;;
	      (loop (cons first list-of-firsts)
		    (cons last list-of-lasts)
		    (min len minimal-len)
		    (cdr remaining-exprs)))))))

;;
;; turn a list of pdas (represented by their first and last states)
;; into a "OR" of them using $branch nodes
;;
;; ie,
;;
;;      +-------+   +-------+	+-------+
;;      |   a   |   |   b   |  	|   c   |
;;      +-------+   +-------+	+-------+
;;
;;
;;       	     |	|
;;       	     |	|
;;       	    _|  |_
;;       	    \  	 /
;;       	     \ 	/
;;       	      \/
;;
;; first
;;  ------\
;;     	  |
;;	  v
;;     +--------+     +--------+
;;     | BRANCH	|     | BRANCH |
;;     |     *--+---->|     *--+----\
;;     | < * >  |     | < * >  |     \
;;     |   |    |     |   |    |      \
;;     +---+----+     +---+----+       \
;;         |              |		|
;;         v              v		v
;;      +-------+     +-------+	    +-------+
;;      |   a   |     |   b   |     |   c   |
;;      +-------+     +-------+	    +-------+
;;	    |		  |             |
;;	     \            |     	|
;;	      \           |     	|
;;	       \     _____v_           /
;;              \   /       \         /
;;		 ->(   NOP   )<------/
;;		    \_______/
;;		       ^
;;		       |
;; last ---------------/
;;

(define (generalized-or firsts lasts minlen)
  (let ((j (make <pda-state>
		 opcode: $nop)))
    (set-next! (car lasts) j)
    (let loop ((f (cdr firsts))
	       (l (cdr lasts))
	       (p (car firsts)))
      (if (null? f)
	  (values p j minlen)
	  (begin
	    (set-next! (car l) j)
	    (loop (cdr f)
		  (cdr l)
		  (make <pda-state>
			opcode: $branch
			next: p
			data: (list (car f)))))))))
