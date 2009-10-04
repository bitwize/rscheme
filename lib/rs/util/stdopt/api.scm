
(define-macro (define-stdopt-parser name . opts)
  (let ((tbl-name (symbol-append "*" name "-table*")))
    `(with-module
      rs.util.stdopt%
      (define ,name
	(let ((tbl (parse-stdopt-description
		    (list
		     ,@(map (lambda (opt)
			      `(list ',(car opt)
				     ,@(cddr opt)))
			    opts)))))
	  (lambda ',name args
	    (run-stdopt-parser tbl args)))))))

(define (run-stdopt-parser table args)
  (values))

(define (parse-stdopt-description descr)
  (let ((tbl (make-string-table)))
    (parse-stdopt-into-tbl tbl descr)
       (let ((keys (sort (key-sequence tbl) string<?)))
	 (for-each
	  (lambda (k abbrev-list)
	    (let ((v (table-lookup tbl k)))
	      (for-each
	       (lambda (abbrev)
		 (table-insert! tbl abbrev v))
	       abbrev-list)))
	  keys
	  (find-abbreviations keys 1)))
       tbl))
