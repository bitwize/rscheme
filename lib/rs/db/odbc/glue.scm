
(define-macro (define-odbc-glue (name . args) . body)
  `(define-safe-glue
     (,name ,@args)
     type-handler: (<henv>
		    (direct-instance? <henv>)
		    ("HENV ~a" "(*((HENV *)PTR_TO_DATAPTR(~a)))"))
     type-handler: (<hdbc>
		    (direct-instance? <hdbc>)
		    ("HDBC ~a" "(*((HDBC *)PTR_TO_DATAPTR(~a)))"))
     type-handler: (<hstmt>
		    (direct-instance? <hstmt>)
		    ("HSTMT ~a" "(*((HSTMT *)PTR_TO_DATAPTR(~a)))"))
     properties: ((other-h-files "<iodbc.h>"
				 "<isql.h>")
		  (other-lib-dirs
		   "/usr/local/lib/postgresql-6.4.2/lib")
		  (other-include-dirs
		   "/usr/local/lib/postgresql-6.4.2/include/iodbc")
		  (other-libs "psqlodbc"))
     ,@body))

(define-macro (define-odbc-wrapper (name . args) . opts)
  (let ((vars '())
	(call #f)
	(body opts))
    ;;
    (let loop ()
      (if (keyword? (car body))
	  (case (car body)
	    ((vars:)
	     (set! vars (cadr body))
	     (set! body (cddr body))
	     (loop))
	    (else
	     (error "define-odbc-wrapper: unknown keyword `~s'" (car body))))
	  (begin
	    (set! call (car body))
	    (set! body (cdr body)))))
       ;;
       `(define-odbc-glue (,name ,@args)
	  literals: ((& signal-sql-error)
		     ,@(map (lambda (v)
			      `(& ,(car v)))
			    vars))
	  ,(make <curly-braced>
	     text: (with-output-to-string
		     (lambda ()
		       (format #t "  RETCODE rc;\n")
		       (for-each (lambda (v k)
				   (format #t "#define ~a  TLREFB(~d)\n"
					   (cadr v)
					   (+ k 1))
				   (if (pair? (cddr v))
				       (format #t "#define ~a(x)  TLSET(~d,x)\n"
					       (caddr v)
					       (+ k 1))))
				 vars
				 (range (length vars)))
		       (format #t "\n~a\n" call)
					;
		       (format #t "  switch (rc)\n")
		       (format #t "    {\n")
		       (format #t "      case SQL_SUCCESS:\n")
		       (format #t "      case SQL_SUCCESS_WITH_INFO:\n")
		       (format #t "        {\n")
		       (if (null? body)
			   (display "             RETURN0();\n")
			   (display (car body)))
		       (format #t "        }\n")
		       (format #t "      default:\n")
		       (if (and (pair? args)
				(pair? (car args))
				(memq (cadar args) '(<henv> <hstmt> <hdbc>)))
			   (format #t "        REG1 = REG0;\n")
			   (format #t "        REG1 = FALSE_OBJ;\n"))
		       (format #t "        REG0 = int2fx( rc );\n")
		       (format #t "        APPLYF( 2, TLREFB( 0 ) );\n")
		       (for-each (lambda (v)
				   (format #t "#undef ~a\n" (cadr v))
				   (if (pair? (cddr v))
				       (format #t "#undef ~a\n" (caddr v))))
				 vars)
		       (format #t "    }\n"))))
	  ,@(if (null? body)
		'()
		(cdr body)))))
