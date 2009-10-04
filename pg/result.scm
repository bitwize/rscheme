#|------------------------------------------------------------*-Scheme-*--|
 | File:    pg/result.scm
 |
 |          Copyright (C)1997 Donovan Kolbly <d.kolbly@rosette.com>
 |          as part of the RScheme project, licensed for free use
 |
 | Version: 1.2
 | Date:    1999-01-12 12:28:45
 | Build:   v0.7.3.4-b7u, 2007-05-30
 |
 | Purpose: PG95 Result C-object processing (for inside a `pg-with-tuples')
 `------------------------------------------------------------------------|#


(define-pg-glue (pg-field-names (result <pg-result>)
				    (first_index <raw-int>)
				    (index_limit <raw-int>))
{
  int i;
  obj r = NIL_OBJ;

  for (i=index_limit; i>first_index;)
    {
      i--;
      r = cons( make_string( PQfname( result, i ) ), r );
    }
  REG0 = r;
  RETURN1();
})

(define-pg-glue (pg-field-number (result <pg-result>) 
				     (name <raw-string>))
{
  int n = PQfnumber( result, name );
  if (n < 0)
    scheme_error( "rs.db.pg: field ~s not in result", 1, raw_name );
  REG0 = int2fx(n);
  RETURN1();
})

(define-pg-glue (pg-field-type (result <pg-result>)
				   (field_num <raw-int>))
{
  REG0 = int2fx( PQftype( result, field_num ) );
  RETURN1();
})

(define-pg-glue (pg-field-size (result <pg-result>)
				   (field_num <raw-int>))
{
  int size = PQfsize( result, field_num );
  REG0 = (size < 0) ? FALSE_OBJ : int2fx(size);
  RETURN1();
})

(define-pg-glue (pg-get-value (result <pg-result>)
				  (tuple_num <raw-int>)
				  (field_num <raw-int>))
{
  int len;
  obj str;

  if (PQgetisnull( result, tuple_num, field_num ))
    {
      REG0 = FALSE_OBJ;
    }
  else
    {
      len = PQgetlength( result, tuple_num, field_num );
      str = bvec_alloc( len+1, string_class );
      memcpy( PTR_TO_DATAPTR(str), 
    	      PQgetvalue( result, tuple_num, field_num ),
              len );
      REG0 = str;
    }
  RETURN1();
})
