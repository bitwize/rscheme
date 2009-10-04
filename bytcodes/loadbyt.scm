#|------------------------------------------------------------*-Scheme-*--|
 | File:    bytcodes/loadbyt.scm
 |
 |          Copyright (C)1997 Donovan Kolbly <d.kolbly@rscheme.org>
 |          as part of the RScheme project, licensed for free use.
 |          See <http://www.rscheme.org/> for the latest information.
 |
 | File version:     1.4
 | File mod date:    1997-11-29 23:10:43
 | System build:     v0.7.3.4-b7u, 2007-05-30
 |
 | Purpose:          load the bytecodes into the build compiler
 `------------------------------------------------------------------------|#

(load (pathname->string (locate-dist-resource "compiler/bytecode/bcgen.scm")))

(set! $bytecodes
  (with-input-from-file 
      (pathname->string
       (locate-dist-resource "compiler/bytecode/bctable.dat"))
    read))

(format #t "** ~d bytecodes loaded **\n" (length $bytecodes))

(define $primops
  (let ((t (make-table eq? identity)))
    (for-each
     (lambda (ent)
       (table-insert! t 
		      (cdr (assq 'bytecode (vector-ref ent 3)))
		      ent))
     (with-input-from-file
	 (pathname->string 
	  (locate-dist-resource "compiler/bytecode/potable.dat"))
       read))
    t))

(format #t "** ~d primops loaded**\n" (table-size $primops))
