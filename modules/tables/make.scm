#|------------------------------------------------------------*-Scheme-*--|
 | File:    modules/tables/make.scm
 |
 |          Copyright (C)1997 Donovan Kolbly <d.kolbly@rscheme.org>
 |          as part of the RScheme project, licensed for free use.
 |          See <http://www.rscheme.org/> for the latest information.
 |
 | File version:     1.13
 | File mod date:    2004-07-02 08:05:44
 | System build:     v0.7.3.4-b7u, 2007-05-30
 | Owned by module:  tables
 |
 | Purpose:          initializers for <hash-table>'s
 `------------------------------------------------------------------------|#

(define-method initialize ((self <hash-table>))
  (set-bucket-class! self <table-bucket>)
  (set-directory! self (make-vector (logical-shift-left 1 (directory-bits self))))
  self)

#|
(define $log2-tree '#(128 #(8 #(2 #(1 0 1)
				  #(4 2 3))
			      #(32 #(16 4 5)
			           #(64 6 7)))
			  #(2048 #(512 #(256 8 9)
			               #(1024 10 11))
				 #(8192 #(4096 12 13)
				        #(16384 14 15)))))
				   

(define (binary-search tree (value <fixnum>))
    (if (fixnum? tree)
	tree
	(let (((t <vector>) tree))
	    (if (<= value (vector-ref t 0))
		(binary-search (vector-ref t 1) value)
		(binary-search (vector-ref t 2) value)))))


(define (log2 (n <fixnum>))
  (let loop (((i <fixnum>) 1) ((j <fixnum>) 0))
    (if (fixnum>=? i n)
	j
	(loop (logical-shift-left i 1) (add1 j)))))
|#

(define (make-table #optional (eq-proc default: equal?)
		              (hash-proc default: hash-code)
			      capacity)
  (let ((initial-size (if capacity
			  (inexact->exact 
			   (ceiling 
			    (/ (log capacity)
			       (log 2))))
			  4)))
    (cond
     ((and (eq? eq-proc string=?)
	   (eq? hash-proc string->hash))
      (make <string-table> directory-bits: initial-size))
     ((and (eq? eq-proc string-ci=?)
	   (eq? hash-proc string-ci->hash))
      (make <string-ci-table> directory-bits: initial-size))
     ((and (eq? eq-proc eq?)
	   (eq? hash-proc symbol->hash))
      (make <symbol-table> directory-bits: initial-size))
     ((and (eq? eq-proc eq?)
	   (eq? hash-proc identity))
      (make <integer-table> directory-bits: initial-size))
     ((and (eq? eq-proc eq?)
	   (eq? hash-proc integer->hash))
      (make <hash-integer-table> directory-bits: initial-size))
     ((eq? eq-proc eq?)
      (make <eq-table> directory-bits: initial-size
	    table-hash-function: hash-proc))
     (else
      (make <generic-table> directory-bits: initial-size
	    table-hash-function: hash-proc
	    table-equal-function: eq-proc)))))

(define-safe-glue (hash-table-copy (table <hash-table>))
{
  REG0 = hashtable_copy(table);
  RETURN1();
})

;;; convenience function

(define (make-symbol-table)
  (make <symbol-table>
	directory-bits: 4))

(define (make-string-table)
  (make <string-table>
	directory-bits: 4))

(define (make-string-ci-table)
  (make <string-ci-table>
	directory-bits: 4))

(define (make-fixnum-table)
  (make <hash-integer-table>
	directory-bits: 4))

(define (table? thing)
  (instance? thing <table>))

;;;

(define-method hash-code ((self <<class>>))
  (class-hash self))

(define-method hash-code ((self <char>))
  (immob->hash self))

(define-method hash-code ((self <boolean>))
  (immob->hash self))

(define-method hash-code ((self <empty-list>))
  (immob->hash self))

(define-method hash-code ((self <unique-obj>))
  (immob->hash self))


(define-method hash-code ((self <string>))
  (string->hash self))

(define-method hash-code ((self <symbol>))
  (symbol->hash self))

(define-method hash-code ((self <fixnum>))
  (integer->hash self))

(define-method hash-code ((self <pair>))
  (tuple->hash
   (hash-code (car self))
   (hash-code (cdr self))))

(define-method hash-code ((self <vector>))
  (gvec-hash self))

(define (gvec-hash self)
  (let ((n (gvec-length self)))
    (let loop (((h <fixnum>) #x5712395)
	       ((i <fixnum>) 0))
      (if (eq? i n)
	  h
          (loop (tuple->hash h (hash-code (gvec-ref self i)))
                (add1 i))))))

(define (char->hash (ch <char>)) 
  (immob->hash ch))

(define-safe-glue (hash-table-clear! (table <hash-table>))
{
  hashtable_clear( table );
  RETURN0();
})
