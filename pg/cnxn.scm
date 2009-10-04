#|------------------------------------------------------------*-Scheme-*--|
 | File:    pg/cnxn.scm
 |
 |          Copyright (C)1997 Donovan Kolbly <d.kolbly@rosette.com>
 |          as part of the RScheme project, licensed for free use
 |
 | Version: 1.4
 | Date:    1999-02-12 08:49:26
 | Build:   v0.7.3.4-b7u, 2007-05-30
 |
 | Purpose: PG95 Connection establishment
 `------------------------------------------------------------------------|#

(define (postgres-connect db . opts)
  (define (opt kwd)
    (if (memq kwd opts)
	(cadr (memq kwd opts))
	""))
  (pg-connect
   (opt 'host:)
   (opt 'port:)
   (opt 'opts:)
   (opt 'tty:)
   db))

(define-pg-glue (pg-connect (host <raw-string>)
				(port <raw-string>)
				(opts <raw-string>)
				(tty <raw-string>)
				(db <raw-string>))
 literals: ((& <pg-connection>)
	    (& <pg-connect-error>))
{
  PGconn *conn;

  conn = PQsetdb( host[0] ? host : NULL, 
		  port[0] ? port : NULL, 
		  opts[0] ? opts : NULL, 
		  tty[0] ? tty : NULL, 
		  db );
  if (conn && PQstatus(conn) != CONNECTION_BAD)
    {
      REG0 = make4( TLREF(0), raw_db, RAW_PTR_TO_OBJ(conn), FALSE_OBJ, ZERO );
    }
  else
    {
      obj descr;

      if (conn)
	{
	  descr = make_string( PQerrorMessage(conn) );
	  PQfinish(conn);
	}
      else
	{
	  descr = make_string( "bummer" );
	}
      raise_error( make3( TLREF(1), NIL_OBJ, raw_db, descr ) );
    }
  RETURN1();
})

