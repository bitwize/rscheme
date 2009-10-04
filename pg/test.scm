,(use objsys tables)
,(use rs.db.pg)

(define c (postgres-connect "donovan"))

(define (sql/create-table cnxn (name <string>) (columns <list>))
  (define (sql-cols (columns <list>))
    (map (lambda (col)
	   (let ((col-name (car col))
		 (col-type (cadr col)))
	     (if (pair? col-type)
		 (format #f "~a ~a(~a)" 
			 col-name
			 (car col-type)
			 (cadr col-type))
		 (format #f "~a ~a" col-name col-type))))
	 columns))
  ;;
  (pg-exec-command
   cnxn
   (format #f "create table ~a(~a)"
	   name
	   (string-join #\, (sql-cols columns))))
  ;;
  (for-each (lambda (i col)
	      (if (memq ':index (cddr col))
		  (pg-exec-command
		   cnxn
		   (format #f "create index ix_~a_~d on ~a (~a)"
			   name i
			   name
			   (car col)))))
	    (range (length columns))
	    columns)
  (values))

(define (sql/drop-table cnxn (name <string>))
  (pg-exec-command cnxn (format #f "drop table ~a" name))
  (values))

(define (sql/insert cnxn (tbl <string>) (vals <list>))
  (pg-exec-command 
   c
   (format #f "insert into ~a values(~a)"
	   tbl
	   (string-join #\, (map (lambda (v) (format #f "'~a'" v)) vals)))))

(define (dumptbl c (name <string>))
  (pg-with-tuples
   c
   (format #f "select * from ~a" name)
   (lambda (result num-tuples num-fields)
     (format #t "~d tuples, each ~d fields\n" num-tuples num-fields)
     (for-each (lambda (n k)
		 (format #t "field ~s: type ~d\n" 
			 n
			 (pg-field-type result k)))
	       (pg-field-names result 0 num-fields)
	       (range num-fields))
     (newline)
     (for-each (lambda (i)
		 (format #t "~d:" i)
		 (for-each (lambda (k)
			     (write-char #\:)
			     (write (pg-get-value result i k)))
			   (range num-fields))
		 (newline))
	       (range num-tuples)))))
