#|------------------------------------------------------------*-Scheme-*--|
 | File:    packages/db/dbglue.scm
 |
 |          Copyright (C)1997 Donovan Kolbly <d.kolbly@rosette.com>
 |          as part of the RScheme project, licensed for free use
 |
 | Version: 1.4
 | Date:    1997-03-17 02:57:35
 | Build:   v0.7.3.4-b7u, 2007-05-30
 |
 | Purpose: Unix database glue code (for Berkeley db)
 `------------------------------------------------------------------------|#

(define-class <db> (<table>)
  (path type: <string>)
  raw-db)

(define-class <recno-db> (<db>))

(define-db-glue (db-open (path <raw-string>)
			 (type <raw-int>)
			 (mode <raw-int>)
			 (permissions <raw-int>))
{
DB *db;
int i, flags;
#ifndef O_SHLOCK
#define O_SHLOCK (0)
#endif
#ifndef O_EXLOCK
#define O_EXLOCK (O_EXCL)
#endif
static int flag_conv[] = { O_CREAT,   /* mode bit 0 */
			   O_EXCL,    /* mode bit 1 */
			   O_EXLOCK,  /* mode bit 2 */
			   O_NONBLOCK,/* mode bit 3 */
			   O_RDONLY,  /* mode bit 4 */
			   O_RDWR,    /* mode bit 5 */
			   O_SHLOCK,  /* mode bit 6 */
			   O_TRUNC }; /* mode bit 7 */
static DBTYPE type_conv[] = { DB_BTREE, DB_HASH, DB_RECNO };

    flags = 0;
    for (i=0; i<8; i++)
      {
        if (mode & 1)
	  flags |= flag_conv[i];
	mode >>= 1;
      }
	
    db = dbopen( path, flags, permissions, type_conv[type], NULL );
    if (db)
	REG0 = RAW_PTR_TO_OBJ(db);
    else
	os_error( "dbopen", 1, raw_path );
    RETURN1();
})

(define-db-glue (db-get (db <db>) key)
{
DBT k, v;
int rc;
int temp;
#define db_is_closed(in,d) scheme_error( "~a: db ~s is closed", \
                                         2, make_string(in), d )

    if (!db)
      db_is_closed("db-get",raw_db);
    if (STRING_P(key))
      {
        k.data = string_text(key);
        k.size = string_length(key);
      }
    else if (OBJ_ISA_FIXNUM(key))
      {
        temp = fx2int(key);
        k.data = &temp;
        k.size = sizeof(int);
      }
    else
      scheme_error( "db-get: invalid key ~s", 1, key );

    rc = db->get( db, &k, &v, 0 );
    if (rc == 0)
      {
        REG0 = bvec_alloc( v.size + 1, string_class );
	memcpy( PTR_TO_DATAPTR(REG0), v.data, v.size );
      }
    else if (rc == 1)
        REG0 = FALSE_OBJ;
    else
        os_error( "db->get", 2, raw_db, key );
    RETURN1();
})

(define-db-glue (db-store (db <db>)
			  key
			  (value <string>)
			  insert_only_q)
    literals: ('key-present)
{
DBT k, v;
int temp, rc;

    if (!db)
      db_is_closed("db-store",raw_db);
    if (STRING_P(key))
      {
        k.data = string_text(key);
        k.size = string_length(key);
      }
    else if (OBJ_ISA_FIXNUM(key))
      {
        temp = fx2int(key);
        k.data = &temp;
        k.size = sizeof(int);
      }
    else
      scheme_error( "db-store: invalid key ~s", 1, key );
    v.data = string_text(value);
    v.size = string_length(value);

    rc = db->put( db, &k, &v, truish(insert_only_q) ? R_NOOVERWRITE : 0 );
    if (rc == 0)
      REG0 = FALSE_OBJ;
    else if (rc == 1)
      REG0 = LITERAL(0);
    else
      os_error( "db->put", 3, raw_db, key, value );
    RETURN1();
})

(define-db-glue (db-delete (db <db>) (key <string>))
{
DBT k;
int rc, temp;

    if (!db)
      db_is_closed("db-delete",raw_db);
    if (STRING_P(key))
      {
        k.data = string_text(key);
        k.size = string_length(key);
      }
    else if (OBJ_ISA_FIXNUM(key))
      {
        temp = fx2int(key);
        k.data = &temp;
        k.size = sizeof(int);
      }
    else
      scheme_error( "db-delete: invalid key ~s", 1, key );

    rc = db->del( db, &k, 0 );
    if (rc == 0)
      REG0 = TRUE_OBJ;  /* successfully deleted */
    else if (rc == 1)
      REG0 = FALSE_OBJ; /* not in the file */
    else
      os_error( "db->del", 2, raw_db, key );
    RETURN1();
})

(define (db-first-key db)
  (db-seq db 0))

(define (db-next-key db)
  (db-seq db 1))

(define (db-last-key db)
  (db-seq db 2))

(define (db-prev-key db)
  (db-seq db 3))

;;; also need a version that can set the pointer using R_CURSOR...

(define-db-glue (db-seq (db <db>) (mode <raw-int>))
{
DBT k, v;
int rc;
static int flag_conv[] = { R_FIRST, R_NEXT, R_LAST, R_PREV };

    if (!db)
      db_is_closed("db-seq",raw_db);

    rc = db->seq( db, &k, &v, flag_conv[mode] );
    if (rc == 0)
    {
	REG0 = bvec_alloc( k.size + 1, string_class );
	memcpy( PTR_TO_DATAPTR(REG0), k.data, k.size );
	REG1 = bvec_alloc( v.size + 1, string_class );
	memcpy( PTR_TO_DATAPTR(REG1), v.data, v.size );
        RETURN(2);
    }
    else if ((rc == 1) || (rc == 2))
    {
	RETURN0();
    }
    else
    {
       os_error( "db->seq", 1, raw_db );
       RETURN1();
    }
})

(define-db-glue (db-close (db <db>))
{
  if (!db)
    db_is_closed("db-close",raw_db);
  if (db->close( db ) < 0)
    os_error( "db->close", 1, raw_db );
  /* arrange for future uses to have a NULL db */
  gvec_set( raw_db, SLOT(1), ZERO );
  RETURN0();
})

(define-db-glue (db-key->int (str <string>))
{
    REG0 = int2fx( *(int *)PTR_TO_DATAPTR(str) );
    RETURN1();
})

