#|------------------------------------------------------------*-Scheme-*--|
 | File:    packages/db/dbtable.scm
 |
 |          Copyright (C)1997 Donovan Kolbly <d.kolbly@rosette.com>
 |          as part of the RScheme project, licensed for free use
 |
 | Version: 1.2
 | Date:    1997-03-17 02:57:39
 | Build:   v0.7.3.4-b7u, 2007-05-30
 |
 | Purpose: Unix database (db) table interface
 `------------------------------------------------------------------------|#

;;  present the <table> interface

(define (make-db-table (path <string>) . opts)
  (let-syntax ((kwd (syntax-form (k dflt)
		      (let ((m (memq (mquote k) opts)))
			(if m
			    (cadr m)
			    dflt)))))
    (let* ((type (assq (kwd type: 'btree)
		       '((btree . 0)
			 (hash . 1)
			 (recno . 2))))
	   (type-num (if type
			 (cdr type)
			 (error "make-db-table: table type `~s' not recognized"
				(kwd type: 'btree))))
	   (raw-db (db-open path
			    type-num
			    ;; default => [O_CREAT]|O_EXLOCK|O_RDWR
			    (kwd flags: (if (os-file-exists? path)
					    #b100100
					    #b100101))
			    (kwd mode: #o666))))
      (if (eq? (car type) 'recno)
	  (make <recno-db>
		path: path
		raw-db: raw-db)
	  (make <db>
		path: path
		raw-db: raw-db)))))

(define-method table-insert! ((self <db>)
			      (key <string>) 
			      (value <string>))
  (db-store self key value #f))

(define-method table-insert! ((self <recno-db>)
			      (key <fixnum>)
			      (value <string>))
  (db-store self key value #f))

(define-method table-lookup ((self <db>) (key <string>))
  (db-get self key))

(define-method table-lookup ((self <recno-db>) (key <fixnum>))
  (db-get self key))

(define-method table-for-each ((self <db>) proc)
  (let loop (((h <fixnum>) 0)
	     (f 0))
    (bind ((k v (db-seq self f)))
      (if k
	  (begin
	    (proc h k v)
	    (loop (add1 h) 1))
	  h))))

(define-method table-for-each ((self <recno-db>) proc)
  (let loop (((h <fixnum>) 0)
	     (f 0))
    (bind ((k v (db-seq self f)))
      (if k
	  (begin
	    (proc h (db-key->int k) v)
	    (loop (add1 h) 1))
	  h))))

(define-method key-sequence ((self <db>))
  (let loop ((key (db-first-key self))
	     (r '()))
    (if key
	(loop (db-next-key self) (cons key r))
	(reverse r))))

(define-method key-sequence ((self <recno-db>))
  (let loop ((key (db-first-key self))
	     (r '()))
    (if key
	(loop (db-next-key self) (cons (db-key->int key) r))
	(reverse r))))
