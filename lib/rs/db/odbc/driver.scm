
(define (signal-sql-error rc op)
  (bind ((s m (cond
	       ((instance? op <hstmt>) (sql-error #f #f op))
	       ((instance? op <hdbc>) (sql-error #f op #f))
	       ((instance? op <henv>) (sql-error op #f #f))
	       ((not op) (sql-error #f #f #f)))))
	(signal (make <sql-error>
		  sql-retcode: rc
		  sql-state: s
		  sql-error-message: m))))

(define-method display-object ((self <sql-error>) port)
  (format port "rs.db.odbc: SQL Error ~s\n" (sql-retcode self))
  (format port "  (~a) ~a\n" (sql-state self) (sql-error-message self)))

;;;


(define-odbc-glue (sql-error henv_opt hdbc_opt hstmt_opt)
{
  RETCODE rc;
  UCHAR sql_state[100];
  UCHAR msg_temp[2048];

  HENV henv = SQL_NULL_HENV;
  HENV hdbc = SQL_NULL_HDBC;
  HENV hstmt = SQL_NULL_HSTMT;

  if (truish( henv_opt ))
    {
      henv = *((HENV *)PTR_TO_DATAPTR( henv_opt ));
    }

  if (truish( hdbc_opt ))
    {
      hdbc = *((HDBC *)PTR_TO_DATAPTR( hdbc_opt ));
    }

  if (truish( hstmt_opt ))
    {
      hstmt = *((HSTMT *)PTR_TO_DATAPTR( hstmt_opt ));
    }

  rc = SQLError( henv, hdbc, hstmt, 
		 sql_state,            /* szSqlState */
		 NULL,                 /* don't get native error info */
		 msg_temp,             /* error message */
		 (sizeof msg_temp)-1,  /* error message max */
		 NULL );               /* actual error message len */

  REG0 = make_string( sql_state );
  REG1 = make_string( msg_temp );
  RETURN(2);
})

;;;

(define-odbc-wrapper (sql-alloc-env)
  vars: ((<henv> HENV_CLASS))
{
  HENV henv;
  rc = SQLAllocEnv( &henv );
}
{
  obj r = bvec_alloc( sizeof henv, HENV_CLASS );
  memcpy( PTR_TO_DATAPTR( r ), &henv, sizeof henv );
  REG0 = r;
  RETURN1();
})

(define-odbc-wrapper (sql-alloc-connect (henv <henv>))
  vars: ((<hdbc> HDBC_CLASS))
{
  HDBC hdbc;
  rc = SQLAllocConnect( henv, &hdbc );
}
{
  obj r = bvec_alloc( sizeof hdbc, HDBC_CLASS );
  memcpy( PTR_TO_DATAPTR( r ), &hdbc, sizeof hdbc );
  REG0 = r;
  RETURN1();
})

(define-odbc-wrapper (sql-alloc-stmt (hdbc <hdbc>))
  vars: ((<hstmt> HSTMT_CLASS))
{
  HSTMT hstmt;
  rc = SQLAllocConnect( hdbc, &hstmt );
}
{
  obj r = bvec_alloc( sizeof hstmt, HSTMT_CLASS );
  memcpy( PTR_TO_DATAPTR( r ), &hstmt, sizeof hstmt );
  REG0 = r;
  RETURN1();
})

;;;

(define-odbc-wrapper (sql-connect (hdbc <hdbc>)
				  (dsn <string>)
				  (uid <string>)
				  (auth <string>))
{
  rc = SQLConnect( hdbc,
		   string_text( dsn ),
		   string_length( dsn ),
		   string_text( uid ),
		   string_length( uid ),
		   string_text( auth ),
		   string_length( auth ) );
})

(define-odbc-wrapper (sql-prepare (hstmt <hstmt>) (stmt <string>))
{
  rc = SQLPrepare( hstmt, string_text( stmt ), string_length( stmt ) );
})

(define-odbc-wrapper (sql-execute (hstmt <hstmt>))
{
  rc = SQLExecute( hstmt );
})

(define-odbc-wrapper (sql-fetch (hstmt <hstmt>))
{
  rc = SQLFetch( hstmt );
})

(define-odbc-wrapper (sql-exec-direct (hstmt <hstmt>) (str <string>))
{
  rc = SQLExecDirect( hstmt, string_text( str ), string_length( str ) );
})

(define-odbc-wrapper (sql-row-count (hstmt <hstmt>))
{
  SDWORD n;
  rc = SQLRowCount( hstmt, &n );
}
{
  REG0 = int2fx( n );
  RETURN1();
})

(define-odbc-wrapper (sql-num-result-cols (hstmt <hstmt>))
{
  SWORD n;
  rc = SQLNumResultCols( hstmt, &n );
}
{
  REG0 = int2fx( n );
  RETURN1();
})

(define-constant *sql-type-symbols*
  '#(unknown
     char
     numeric
     decimal
     integer
     smallint
     float
     real
     double
     varchar))

(define-odbc-glue (sql-c-type (typesym <symbol>))
  literals: ('#(char long short float double default))
{
  static obj ctvec[6] = { int2fx( SQL_C_CHAR ),
			  int2fx( SQL_C_LONG ),
			  int2fx( SQL_C_SHORT ),
			  int2fx( SQL_C_FLOAT ),
			  int2fx( SQL_C_DOUBLE ),
			  int2fx( SQL_C_DEFAULT ) };
  obj vec = LITERAL(0);
  int i;
  for (i=0; i<6; i++)
    {
      if (EQ( gvec_ref( vec, SLOT(i) ), typesym ))
	{
	  REG0 = ctvec[i];
          goto done;
	}
    }
  scheme_error( "rs.db.odbc: invalid sql-c-type `~s'", 1, typesym );
done:
  RETURN1();
})


(define-odbc-wrapper (sql-describe-col (hstmt <hstmt>) 
				       (icol <raw-int>))
  vars: ((<column> COLUMN_CLASS)
	 (*sql-type-symbols* TYPE_SYM_VEC))
{
  UCHAR col_name[256];
  SWORD col_type, col_scale, col_nulls;
  UDWORD col_def;

  rc = SQLDescribeCol( hstmt,
		       icol,
		       col_name,
		       256,
		       NULL,
		       &col_type,
		       &col_def,
		       &col_scale,
		       &col_nulls );
}
{
  int type_sym_ix = SLOT(0);
  switch (col_type)
    {
      case SQL_CHAR:      type_sym_ix = SLOT(1); break;
      case SQL_NUMERIC:   type_sym_ix = SLOT(2); break;
      case SQL_DECIMAL:   type_sym_ix = SLOT(3); break;
      case SQL_INTEGER:   type_sym_ix = SLOT(4); break;
      case SQL_SMALLINT:  type_sym_ix = SLOT(5); break;
      case SQL_FLOAT:     type_sym_ix = SLOT(6); break;
      case SQL_REAL:      type_sym_ix = SLOT(7); break;
      case SQL_DOUBLE:    type_sym_ix = SLOT(8); break;
      case SQL_VARCHAR:   type_sym_ix = SLOT(9); break;
    }
  REG0 = make6( COLUMN_CLASS,
		int2fx( icol ),
		make_string( col_name ),
		gvec_ref( TYPE_SYM_VEC, type_sym_ix ),
		int2fx( col_scale ),
		int2fx( col_def ),
		col_nulls ? TRUE_OBJ : FALSE_OBJ );
  RETURN1();
})

(define-odbc-wrapper (sql-transact/commit (henv <henv>) (hdbc <hdbc>))
{
  rc = SQLTransact( henv, hdbc, SQL_COMMIT );
})

(define-odbc-wrapper (sql-transact/rollback (henv <henv>) (hdbc <hdbc>))
{
  rc = SQLTransact( henv, hdbc, SQL_ROLLBACK );
})

(define-odbc-wrapper (sql-bind-col (hstmt <hstmt>) 
				   (icol <raw-int>)
				   (c_type <raw-int>)
				   buffer
				   (offset <raw-int>)
				   (length <raw-int>)
				   (len_offset <raw-int>))
{
  PTR at = PTR_TO_DATAPTR( buffer ) + offset;
  SDWORD *len_at = (SDWORD *)(PTR_TO_DATAPTR( buffer ) + len_offset);

  rc = SQLBindCol( hstmt, icol, c_type, at, length, len_at );
}
{
  REG0 = int2fx( offset + length );
  RETURN1();
})

#|

SQLTransact( env, dbc, SQL_ROLLBACK );

See <http://teamserver.icat.com/Sybase/00000715.htm>

HDBC dbc;
     HSTMT stmt;
     RETCODE retcode;
     long emp_id;
     char emp_lname[20];
     
     SQLAllocStmt( dbc, &ampstmt );
     SQLExecDirect( stmt,
                     "select emp_id,emp_lname
                     from employee", SQL_NTS );
     SQLBindCol( stmt, 1, SQL_C_LONG, &emp_id,
                     sizeof(emp_id), NULL );
     SQLBindCol( stmt, 2, SQL_C_CHAR, &emp_lname,
                     sizeof(emp_lame), NULL );
     
     for(;;) {
         retcode = SQLFetch( stmt );
         if( retcode == SQL_NO_DATA_FOUND ) break;
         print_employee( emp_id, emp_lname);
     }
     
     /* Using SQL_CLOSE closes the cursor
         but does not free the statement */
     SQLFreeStmt( stmt, SQL_CLOSE );
|#
