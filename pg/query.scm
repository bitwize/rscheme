#|------------------------------------------------------------*-Scheme-*--|
 | File:    pg/query.scm
 |
 |          Copyright (C)1997 Donovan Kolbly <d.kolbly@rosette.com>
 |          as part of the RScheme project, licensed for free use
 |
 | Version: 1.3
 | Date:    1999-01-23 15:25:31
 | Build:   v0.7.3.4-b7u, 2007-05-30
 |
 | Purpose: PG95 Query execution
 `------------------------------------------------------------------------|#

(define-pg-glue (pg-exec-command (cnxn <pg-connection>)
		                     (query <raw-string>))
  literals: ((& <pg-result>)
	     (& <pg-exec-error>))
{
  PGresult *res;
 
  res = PQexec( cnxn, query );
  if (!res || (PQresultStatus(res) != PGRES_COMMAND_OK))
    {
      if (res)
        PQclear(res);
      raise_error( make4( TLREF(1),
			  NIL_OBJ,
                          raw_cnxn,
                          raw_query,
                          make_string(PQerrorMessage(cnxn))));
      RETURN0();
    }
   else
    {
      const char *r = PQoidStatus( res );
      if (*r)
         REG0 = int2fx( atoi(r) );
      else
         REG0 = FALSE_OBJ;
      PQclear( res );
      RETURN1();
    }
})

(define-pg-glue (pg-with-tuples (cnxn <pg-connection>)
				    (query <raw-string>)
				    (proc <function>))
  literals: ((& <pg-result>)
	     (& <pg-exec-error>))
{
  PGresult *res;
 
  res = PQexec( cnxn, query );
  if (!res || (PQresultStatus(res) != PGRES_TUPLES_OK))
    {
      if (res)
        PQclear(res);
      raise_error( make4( TLREF(1),
			  NIL_OBJ,
                          raw_cnxn,
                          raw_query,
                          make_string( PQerrorMessage( cnxn ) )));
      RETURN0();
    }
  else
    {
      REG0 = RAW_PTR_TO_OBJ(res);
      SAVE_CONT1(done_with_proc);
      REG0 = make1( TLREF(0), REG0 );
      REG1 = int2fx( PQntuples(res) );
      REG2 = int2fx( PQnfields(res) );
      APPLY(3,proc);
    }
}
("done_with_proc" {
  PGresult *res;

  res = OBJ_TO_RAW_PTR(PARTCONT_REG(0));
  PQclear( res );

  RESTORE_CONT_REG();
  RETURN(arg_count_reg);
}))

