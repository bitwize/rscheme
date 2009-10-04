
(define-class <sqlite3-error> (<condition>)
  code
  args)

(define-method display-object ((self <sqlite3-error>) port)
  (format port "SQLITE3:~a: ~s\n"
          (sqlite3-strerror (code self))
          (args self)))

(define-class <sqlite3-database> (<object>)
  raw-pointer
  open-statements
  prep-cache
  name)

(define-class <sqlite3-statement> (<object>)
  raw-pointer
  (container type: <sqlite3-database>)
  (name type: <abstract-string>))

(define-sqlite-glue (sqlite3-strerror (rc <raw-int>))
  literals: ('#(SQLITE_OTHER
                SQLITE_OK
                SQLITE_ERROR
                SQLITE_INTERNAL
                SQLITE_PERM
                SQLITE_ABORT
                SQLITE_BUSY
                SQLITE_LOCKED
                SQLITE_NOMEM
                SQLITE_READONLY
                SQLITE_INTERRUPT
                SQLITE_IOERR
                SQLITE_CORRUPT
                SQLITE_NOTFOUND
                SQLITE_FULL
                SQLITE_CANTOPEN
                SQLITE_PROTOCOL
                SQLITE_EMPTY
                SQLITE_SCHEMA
                SQLITE_TOOBIG
                SQLITE_CONSTRAINT
                SQLITE_MISMATCH
                SQLITE_MISUSE
                SQLITE_NOLFS
                SQLITE_AUTH
                SQLITE_ROW
                SQLITE_DONE))
{
  int k;

  switch (rc) {
    case SQLITE_OK:             k =  1; break;
    case SQLITE_ERROR:          k =  2; break;
    case SQLITE_INTERNAL:       k =  3; break;
    case SQLITE_PERM:           k =  4; break;
    case SQLITE_ABORT:          k =  5; break;
    case SQLITE_BUSY:           k =  6; break;
    case SQLITE_LOCKED:         k =  7; break;
    case SQLITE_NOMEM:          k =  8; break;
    case SQLITE_READONLY:       k =  9; break;
    case SQLITE_INTERRUPT:      k = 10; break;
    case SQLITE_IOERR:          k = 11; break;
    case SQLITE_CORRUPT:        k = 12; break;
    case SQLITE_NOTFOUND:       k = 13; break;
    case SQLITE_FULL:           k = 14; break;
    case SQLITE_CANTOPEN:       k = 15; break;
    case SQLITE_PROTOCOL:       k = 16; break;
    case SQLITE_EMPTY:          k = 17; break;
    case SQLITE_SCHEMA:         k = 18; break;
    case SQLITE_TOOBIG:         k = 19; break;
    case SQLITE_CONSTRAINT:     k = 20; break;
    case SQLITE_MISMATCH:       k = 21; break;
    case SQLITE_MISUSE:         k = 22; break;
    case SQLITE_NOLFS:          k = 23; break;
    case SQLITE_AUTH:           k = 24; break;
    case SQLITE_ROW:            k = 25; break;
    case SQLITE_DONE:           k = 26; break;
    default:                    k =  0; break;
  }
  REG0 = gvec_ref( LITERAL(0), SLOT(k) );
  RETURN1();
})



(define (sqlite3-open (dbn <string>))
  (make <sqlite3-database>
        name: dbn
        raw-pointer: (sqlite3-open* dbn)
        open-statements: (make-object-table)
        prep-cache: (make-string-table)))

(define-sqlite-glue (sqlite3-open* (dbn <raw-string>))
  literals: ((& <sqlite3-database>)
             (& <sqlite3-error>))
{
  sqlite3 *cnx;
  int rc;

  rc = sqlite3_open( dbn, &cnx );
  if (rc != SQLITE_OK) {
    obj eo = make3( TLREF(1), 
                    NIL_OBJ, 
                    int2fx( rc ),
                    raw_dbn );
    raise_error( eo );
  }

  REG0 = RAW_PTR_TO_OBJ( cnx );
  RETURN1();
})

(define-sqlite-glue (sqlite3-close (db <sqlite3-database>))
  literals: ((& <sqlite3-error>))
{
  int rc;
  rc = sqlite3_close( db );
  if (rc != SQLITE_OK) {
    obj eo = make3( TLREF(0),
                    NIL_OBJ, 
                    int2fx( rc ),
                    make_string( sqlite3_errmsg( db ) ) );
    raise_error( eo );
  }
  RETURN0();
})

(define-sqlite-glue (sqlite3-changes (db <sqlite3-database>))
{
  REG0 = int2fx( sqlite3_changes( db ) );
  RETURN1();
})

(define-sqlite-glue (sqlite3-reset (s <sqlite3-statement>))
  literals: ((& <sqlite3-error>))
{
  int rc;

  rc = sqlite3_reset( s );
  if (rc != SQLITE_OK) {
    sqlite3 *db = sqlite3_db_handle( s );
    obj eo = make3( TLREF(0), 
                    NIL_OBJ, 
                    int2fx( rc ),
                    make_string( sqlite3_errmsg( db ) ) );
    raise_error( eo );
  }
  RETURN0();
})

(define-sqlite-glue (sqlite3-finalize (s <sqlite3-statement>))
  literals: ((& <sqlite3-error>))
{
  int rc;
  obj stmt_tbl, prep_tbl;
  obj k;

  rc = sqlite3_finalize( s );
  if (rc != SQLITE_OK) {
    sqlite3 *db = sqlite3_db_handle( s );
    obj eo = make3( TLREF(0), 
                    NIL_OBJ, 
                    int2fx( rc ),
                    make_string( sqlite3_errmsg( db ) ) );
    raise_error( eo );
  }

  stmt_tbl = gvec_ref( gvec_ref( raw_s, SLOT(1) ), SLOT(1) );
  prep_tbl = gvec_ref( gvec_ref( raw_s, SLOT(1) ), SLOT(2) );

  objecttable_remove( stmt_tbl, obj_hash( raw_s ), raw_s );
  k = gvec_ref( raw_s, SLOT(2) );
  objecttable_remove( prep_tbl, hash_string( k ), k );
  RETURN0();
})

(define-sqlite-glue (sqlite3-prepare (db <sqlite3-database>) 
                                     (sql <string>)
                                     (offset <raw-int>))
  literals: ((& <sqlite3-statement>)
             (& <sqlite3-error>)
             (& <substring>))
{
  sqlite3_stmt *stmt;
  int rc;
  obj x;
  const char *tail;
  int n;

  rc = sqlite3_prepare( db,
                        string_text( sql ) + offset,
                        string_length( sql ) - offset,
                        &stmt,
                        &tail );

  if (rc != SQLITE_OK) {
    obj eo = make3( TLREF(1), NIL_OBJ, int2fx( rc ), 
                    cons( make_string( sqlite3_errmsg( db ) ),
                          cons( raw_offset, 
                                cons( sql, NIL_OBJ ) ) ) );
    raise_error( eo );
  }

  if (stmt == NULL) {
    RETURN0();
  } else {
    obj tbl;
    obj text;

    n = tail - string_text( sql );

    if ((offset == 0) && (n == string_length( sql ))) {
      text = sql;
    } else {
      obj text_len = int2fx( n - offset );
      text = make4( TLREF(2), sql, raw_offset, text_len, text_len );
    }

    x = make3( TLREF(0), RAW_PTR_TO_OBJ( stmt ), raw_db, text );

    tbl = gvec_ref( raw_db, SLOT(1) );
    objecttable_insert( tbl, obj_hash(x), x, x );

    REG0 = x;
    REG1 = int2fx( n );
    RETURN(2);
  }
})

(define-sqlite-glue (sqlite3-step (s <sqlite3-statement>))
  literals: ((& <sqlite3-error>))
{
  int rc;

  rc = sqlite3_step( s );
  if (rc == SQLITE_ROW) {
    REG0 = TRUE_OBJ;
    RETURN1();
  } else if (rc != SQLITE_DONE) {
    sqlite3 *db = sqlite3_db_handle( s );
    obj eo = make3( TLREF(0), NIL_OBJ, int2fx( rc ), 
                    make_string( sqlite3_errmsg( db ) ) );
    raise_error( eo );
    RETURN0();
  } else {
    RETURN0();
  }
})

(define-sqlite-glue (sqlite3-bind (s <sqlite3-statement>) #rest r)
  literals: ((& <sqlite3-error>)
             "Wrong number of arguments"
             "Can't handle argument type for binding") 
{
  int i;
  extern int rs_to_sq3( sqlite3_stmt *s, int i, int copy_p, obj sv );

  if (arg_count_reg != (1+sqlite3_bind_parameter_count( s ))) {
    obj eo;
    eo = make3( TLREF(0), NIL_OBJ, int2fx( SQLITE_ERROR ),
                cons( LITERAL(1), 
                      cons( int2fx( arg_count_reg-1 ),
                            cons( int2fx( sqlite3_bind_parameter_count( s ) ),
                                  NIL_OBJ ) ) ) );
    raise_error( eo );
  }

  for (i=1; i<arg_count_reg; i++) {
    int rc;

    /* interestingly, the first BIND parameter in sqlite3 has index 1,
       even though the first COLUMN result has index 0 */

    rc = rs_to_sq3( s, i, 0, reg_ref( i ) );
    if (rc != SQLITE_OK) {
      obj eo;
      sqlite3 *db = sqlite3_db_handle( s );

      eo = make3( TLREF(0), NIL_OBJ, int2fx( SQLITE_ERROR ),
                  cons( (rc == -1) 
                        ? LITERAL(1) 
                        : make_string( sqlite3_errmsg( db ) ),
                        cons( int2fx( i ),
                              cons( reg_ref(i), NIL_OBJ ) ) ) );
      raise_error( eo );
    }
  }
  RETURN0();
})

(define-sqlite-glue (sqlite3-row (st <sqlite3-statement>))
{
  int i, n;
  extern obj sq3_to_rs( sqlite3_stmt *s, int col );

  n = sqlite3_data_count( st );
  if (n == 0) {
    RETURN0();
  } else {
    for (i=0; i<n; i++) {
      obj sv = sq3_to_rs( st, i );
      reg_set( i, sv );
    }
    RETURN(n);
  }
})

(define-sqlite-glue (sqlite3-for-each (s <sqlite3-statement>) (fn <function>))
  literals: ((& <sqlite3-error>))
{
  JUMP( 2, sq4each );
}

("sq4each"
{
  int rc;
  sqlite3_stmt *st;
  extern obj sq3_to_rs( sqlite3_stmt *s, int col );

  st = (*(sqlite3_stmt **)PTR_TO_DATAPTR(REG0));

  rc = sqlite3_step( st );
  if (rc == SQLITE_ROW) {
    int i, n;
    obj the_fn = REG1;

    SAVE_CONT2( sq4next );

    n = sqlite3_data_count( st );
    for (i=0; i<n; i++) {
      obj sv = sq3_to_rs( st, i );
      reg_set( i, sv );
    }
    APPLYF( n, the_fn );
  } else if (rc != SQLITE_DONE) {
    sqlite3 *db = sqlite3_db_handle( st );
    obj eo = make3( TLREF(0), NIL_OBJ, int2fx( rc ), 
                    make_string( sqlite3_errmsg( db ) ) );
    raise_error( eo );
    RETURN0();
  } else {
    sqlite3_reset( st );
    RETURN0();
  }
})

("sq4next"
{
  RESTORE_CONT2();
  BJUMP( 2, sq4each );
}))

;;;
;;;  Return a list of lists
;;;

(define (sqlite3-exec (db <sqlite3-database>) stmt . args)
  (let ((p (sqlite3-prepare db stmt 0))
        (r '()))
    ;;
    (apply sqlite3-bind p args)
    ;;
    (sqlite3-for-each
     p
     (lambda args
       (set! r (cons args r))))
    (sqlite3-finalize p)
    (reverse! r)))

#|
(define d (sqlite3-open ":memory:"))
(sqlite3-step (sqlite3-prepare d "create table foo( x int, y char(30) );" 0))
(sqlite3-step (sqlite3-prepare d "insert into foo values( 3, 'foo' )" 0))
(sqlite3-step (sqlite3-prepare d "insert into foo values( 4, 'bar' )" 0))

(sqlite3-for-each
 (sqlite3-prepare d "select * from foo" 0)
 (lambda (x y)
   (format #t "==> ~s = ~s\n" x y)))

(define s1 (sqlite3-prepare d "insert into foo(x,y) values( ?, ? )" 0))

(sqlite3-bind s1 33 "foofoo")
(sqlite3-step s1)



|#
