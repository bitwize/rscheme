
(define *env* #f)
(define *dbc* #f)
(define *stmt* #f)
(define p #f)

(define (t)
  (set! *env* (sql-alloc-env))
  (set! *dbc* (sql-alloc-connect *env*))
  (sql-connect *dbc* "donovan" "" "") ;; blank => get out of .odbc.ini
  (set! *stmt* (sql-alloc-stmt *dbc*))
  ;;
  (handler-case
   (begin
     (sql-prepare *stmt* "drop table bob")
     (sql-execute *stmt*))
   ((<sql-error> condition: c)
    (format #t "`drop table' failed: ~a\n" (sql-error-message c))))
  ;;
  (sql-prepare *stmt* "create table bob( id int, val text )")
  (sql-execute *stmt*)
  ;
  (sql-prepare *stmt* "insert into bob values( 1, 'foo' )")
  (sql-execute *stmt*)
  ;
  (sql-prepare *stmt* "insert into bob values( 2, 'bar' )")
  (sql-execute *stmt*)
  ;
  (sql-prepare *stmt* (format #f "insert into bob values( 3, '~a' )"
			      (make-string 5000 #\.)))
  (sql-execute *stmt*)
  ;
  ;(sql-query *stmt* "select * from bob")
  
  (let ((th (query->iterator *dbc* "select length(val), id, val from bob")))
    (let loop ()
      (let ((r (th)))
	(if r
	    (begin
	      (format #t " ==> ~s\n" r)
	      (loop))))))
#|
  (sql-execute *stmt*)
  (set! p (bind-extraction-plan *stmt* *plan-schema*))
  (sql-fetch *stmt*)
  (print p)
  (print (tuple-buffer p))
|#
  (values))


#|
(define (sql-query use str)
  (sql-prepare use str)
  (sql-execute use)
  (format #t "Rows: ~s\n" (sql-row-count use))
  (let ((nrows (sql-row-count use))
	(buf (bvec-alloc <string> 1000))
	(cols (map
	       (lambda (i)
		 (sql-describe-col use (+ i 1)))
	       (range (sql-num-result-cols use))))
	(fthunks '()))
    ;;
    (let loop ((cols cols)
	       (ix 0)
	       (r '()))
      (if (null? cols)
	  (set! fthunks (reverse r))
	  (begin
	    (format #t "   column: ~s\n" (car cols))
	    (let* ((dataw 20)
		   (nix (sql-bind-col use
				      (column-number (car cols))
				      (sql-c-type 'default) 
				      buf ix dataw)))
	      (loop
	       (cdr cols)
	       nix
	       (cons (lambda ()
		       (let ((w (bvec-read-signed-32 buf (+ ix dataw))))
			 (format #t "  ~a: (~d) ~s\n"
				 (column-name (car cols))
				 w
				 (substring buf ix (+ ix (min w dataw))))))
		     r))))))
       ;;
    (do ((i 0 (+ i 1)))
	((= i nrows))
      (format #t "Row ~d:\n" i)
      (sql-fetch use)
      (for-each (lambda (th) (th)) fthunks)
      (newline))))
|#

(define-method write-object ((self <column>) port)
  (format port "#[<column> ~d ~a (~a:~d:~d:~s)]"
	  (column-number self)
	  (column-name self)
	  (column-type self)
	  (column-scale self)
	  (column-precision self)
	  (column-nullable? self)))

(define-class <bob> (<object>)
  props
  bob-value
  identifier)

,(use srfi.4)

(define (make-test-plan)
  (let ((buf (bvec-alloc <byte-vector> 36)))
    ;;
    (bvec-write-signed-32 buf 0 16) ;; offset
    (bvec-write-signed-32 buf 4 16) ;; maxw
    ;;
    (bvec-write-signed-32 buf 8 32) ;; offset
    (bvec-write-signed-32 buf 12 4)  ;; maxw
    ;;
       (make <extraction-plan-schema>
	 result-class: <bob>
	 tuple-buffer: buf
	 instructions: '#u8(0 1 2)
	 plan-literals: '#(#())
	 result-columns: '#(1 2))))

(define *plan-schema* (make-test-plan))
