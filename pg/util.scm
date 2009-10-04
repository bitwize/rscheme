#|------------------------------------------------------------*-Scheme-*--|
 | File:    pg/util.scm
 |
 |          Copyright (C)1997 Donovan Kolbly <d.kolbly@rosette.com>
 |          as part of the RScheme project, licensed for free use
 |
 | Version: 1.8
 | Date:    2003-08-07 07:42:15
 | Build:   v0.7.3.4-b7u, 2007-05-30
 |
 | Purpose: PG95 package utility functions
 `------------------------------------------------------------------------|#

;;;
;;;  this generic function is used to finish the extraction of a
;;;  tuple (object) from the database.  The default implementation
;;;  does nothing except return self.
;;;  (the method has the opportunity to supply a new object by returning
;;;  a value other than self)

(define-method finish-extract! ((self <object>))
  self)


;;; return type type symbol for a given type oid

(define (pg-type-symbol (cnxn <pg-connection>) (oid <fixnum>))
  (if (not (type-cache cnxn))
      (set-type-cache! cnxn (make-table eq? integer->hash)))
  ;;
  (or (table-lookup (type-cache cnxn) oid)
      (pg-with-tuples
       cnxn
       (format #f "SELECT typname FROM pg_type WHERE oid = ~d" oid)
       (lambda (result num-tuples num-fields)
	 (if (eq? num-tuples 0)
	     (error "rs.db.pg: ~d is not an object in pg_type" oid)
	     (let ((n (string->symbol (pg-get-value result 0 0))))
	       (table-insert! (type-cache cnxn) oid n)
	       n))))))


(define-method dflt ((self <slot-descriptor>) (class <<class>>))
  (case (initialization-mode self)
    ((optional prohibited)
     (init-value self))
    ((function)
     ((init-value self)))
    ((required)
     (signal
      (make <initializer-missing>
	    target-class: class
	    slot-descriptor: self)))))

;;;
;;;  returns a function of one argument which extracts the
;;;  nth tuple from a result set
;;;

(define (compile-extraction-plan meta plan-list len)
  (let (((plan <string>) (make-string (* len 2)))
	((k <fixnum>) 0)
	((proto <vector>) (make-vector len #f)))
    ;; emit an action item
    (define (action-item code info)
      (bvec-set! plan k code)
      (bvec-set! plan (add1 k) info)
      (set! k (+ k 2)))
    ;; emit an action item for copying a field
    ;; from the query result tuple into the clone
    ;; of the prototype
    (define (field-action code action)
      (action-item code (caddr (assq (cadr action) meta))))
    ;;
    (for-each
     (lambda ((slot <fixnum>) action)
       (case (car action)
	 ((text) (field-action 1 action))
	 ((int4) (field-action 2 action))
	 ((oid) (field-action 3 action))
	 ((abstime timestamp timestamptz) (field-action 4 action))
	 ((varchar char bpchar) (field-action 5 action))
	 ((date) (field-action 6 action))
	 ((float4) (field-action 7 action))
	 ((float8) (field-action 8 action))
	 ((time) (field-action 9 action))
	 ((bool) (field-action 10 action))
	 ((quote) (action-item 0 0)
		  (vector-set! proto slot (cadr action)))
	 (else
	  (error "rs.db.pg: unsupported extraction action: ~s" (car action)))))
     (range len)
     plan-list)
    (values plan proto)))

(define (result-metadata cnxn result num-fields)
  (map
   (lambda (i n)
     (list (string->symbol n)
	   (pg-type-symbol cnxn (pg-field-type result i))
	   i))
   (range num-fields)
   (pg-field-names result 0 num-fields)))


(define (build-class-plan result-descr class)
  (let ((vec (make-vector (instance-size class))))
    ;; order it by slot #
    (for-each
     (lambda ((sd <slot-descriptor>))
       (let ((a (assq (name sd) result-descr)))
	 (vector-set! vec
		      (index sd)
		      (if a
			  (list (cadr a) (car a))
			  (list 'quote (dflt sd class))))))
     (class-compute-slots class))
    ;;
    (values (vector->list vec) (vector-length vec))))

(define (gen-class-extractor (class <<class>>)
			     cnxn
			     (result <pg-result>) 
			     num-fields
			     meta)
  (bind ((plan n (build-class-plan meta class))
	 (com proto (compile-extraction-plan meta plan n)))
    ;; note that the `proto' does not need to be the same class --
    ;; the `class' argument to `extract-tuple' takes care of that.
    (lambda (i)
      (finish-extract! (extract-tuple result i class com proto)))))


(define (build-vector-plan meta)
  (map
   (lambda (fld)
     (list (cadr fld) (car fld)))
   meta))

(define (gen-vector-extractor cnxn (result <pg-result>) num-fields meta)
  (bind ((plan (build-vector-plan meta))
	 (com proto (compile-extraction-plan meta plan num-fields)))
    (lambda (i)
      (extract-tuple result i <vector> com proto))))

(define (gen-list-extractor cnxn (result <pg-result>) num-fields meta)
  (bind ((plan (build-vector-plan meta))
	 (com proto (compile-extraction-plan meta plan num-fields)))
    (lambda (i)
      (vector->list (extract-tuple result i <vector> com proto)))))

(define (gen-unit-extractor cnxn (result <pg-result>) meta)
  (bind ((plan (build-vector-plan meta))
	 (com proto (compile-extraction-plan meta plan 1)))
    (lambda (i)
      (vector-ref (extract-tuple result i <vector> com proto) 0))))

(define (make-extractor class cnxn (result <pg-result>) num-fields)
  (let ((meta (result-metadata cnxn result num-fields)))
    (cond
     ((not class)
      (if (= num-fields 1)
	  (gen-unit-extractor cnxn result meta)
	  (gen-vector-extractor cnxn result num-fields meta)))
     ((eq? class <vector>)
      (gen-vector-extractor cnxn result num-fields meta))
     ((eq? class <list>)
      (gen-list-extractor cnxn result num-fields meta))
     (else
      (gen-class-extractor class cnxn result num-fields meta)))))

(define (fetch-using-cursor cnxn query proc)
  (let* ((cid (next-cursor-id cnxn))
	 (cname (string-append "cur" (number->string cid))))
    (set-next-cursor-id! cnxn (+ cid 1))
    (pg-exec-command cnxn (string-append "DECLARE "
					 cname
					 " BINARY CURSOR FOR "
					 query))
    (let ((r (pg-with-tuples cnxn
			     (string-append "FETCH ALL IN " cname) 
			     proc)))
      (pg-exec-command cnxn (string-append "CLOSE " cname))
      r)))

;;;
;;;  returns a single instance (by OID)
;;;

(define (oid->instance cnxn (tbl <string>) class (oid <fixnum>))
  (with-transaction
   cnxn
   (lambda ()
     (fetch-using-cursor
      cnxn
      (format #f "select oid,* from ~a WHERE oid=~d" tbl oid)
      (lambda (result num-tuples num-fields)
	(if (eq? num-tuples 1)
	    ((make-extractor class cnxn result num-fields) 0)
	    #f))))))

;;;

(define (query->list (cnxn <pg-connection>) 
		     (query-str <string>)
		     #key (class default: #f)
		          (limit default: #f))
  (with-transaction
   cnxn
   (lambda ()
     (fetch-using-cursor
      cnxn
      query-str
      (lambda (result num-tuples num-fields)
	;; compute the extraction function
	(let ((extractor (make-extractor class 
					 cnxn
					 result
					 num-fields))
	      (num (if limit
		       (min limit num-tuples)
		       num-tuples)))
	  ;; I'm tempted to go in reverse by tuple#,
	  ;; but worried that it would seriously damage
	  ;; the implementation's optimization assumptions!
	  (let loop ((i 0)
		     (r '()))
	    (if (< i num)
		(loop (+ i 1) (cons (extractor i) r))
		(reverse! r)))))))))

;;;
;;;  returns a list of instances
;;;

(define (table->list (cnxn <pg-connection>)
		     (tbl <string>)
		     #key (class default: #f))
  (query->list cnxn
	       (format #f "select oid,* from ~a" tbl)
	       class: class))
