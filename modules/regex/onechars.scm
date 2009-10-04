#|------------------------------------------------------------*-Scheme-*--|
 | File:    modules/regex/onechars.scm
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
 | Purpose:          single-character regex forms
 `------------------------------------------------------------------------|#

;;  return a single <pda-state> if the given pda
;;  consumes exactly one character in the input in a straightforward
;;  way.  
;;
;;  The "straightforward" constraint means the opcode will be
;;  one of:
;;      $match-char
;;      $match-char-set
;;      $match-any
;;  
;;  in practice, this function relies on the rest of the compiler to
;;  (at least, the compiler for those forms) to always return one and
;;  only one node when possible
;;
;;  FOR EXAMPLE,  (seq (seq) (seq #\a) (seq)) returns exactly one node

(define (single-char-form first last)
  (if (and (eq? first last)
	   (memq (opcode first) (list $match-char $match-char-set $match-any)))
      first
      #f))

;;  furthermore, for the interpreter, for a node returned by
;;  single-char-form, this function returns a procedure of one
;;  argument which recognizes the character

(define (single-char-scanner-proc node)
  (let ((o (opcode node)))
    (cond
     ((eq? o $match-any)
      (lambda (ch) #t))
     ((eq? o $match-char-set)
      (let ((v (data node)))
	(lambda (ch)
	  (vector-bit? v (char->integer ch)))))
     ((eq? o $match-char)
      (let ((s (integer->char (car (data node)))))
	(lambda (ch)
	  (eq? ch s))))
     (else
      (error "internal: single-char scanner invalid: ~s" node)))))

;; a node recognized by single-char-form can be an argument to
;; single-char-bit-vector, which returns a 32-element vector
;; (denoting a bytevector) of the character set membership that is
;; being matched.
;;
;; said vector is not necessarily modifiable

(define (single-char-bit-vector node)
  (let ((o (opcode node)))
    (cond
     ((eq? o $match-any)
      (make-vector 32 #xFF))
     ((eq? o $match-char-set)
      (data node))
     ((eq? o $match-char)
      (let ((v (make-vector 32 0)))
	(vector-bit-set! v (car (data node)))
	v))
     (else
      (error "internal: single-char scanner invalid: ~s" node)))))

;; furthermore, two nodes returned by single-char-form
;; can be arguments to merge-single-char-forms
;; which returns a (possibly same) node that matches the
;; union (or) of the arguments

(define (merge-single-char-forms n1 n2)
  (make <pda-state>
	opcode: $match-char-set
	data: (bit-vector-or (single-char-bit-vector n1)
			     (single-char-bit-vector n2))))
