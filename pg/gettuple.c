/*-----------------------------------------------------------------*-C-*---
 * File:    pg/gettuple.c
 *
 *          Copyright (C)1997 Donovan Kolbly <d.kolbly@rosette.com>
 *          as part of the RScheme project, licensed for free use
 *
 * Version: 1.5
 * Date:    2000-11-04 11:40:13
 * Build:   v0.7.3.4-b7u, 2007-05-30
 *
 * Purpose: Tuple extractor
 *------------------------------------------------------------------------*/

#include <stdio.h>
#include <math.h>
#include "gettuple.h"

/*
 *  These definitions are lifted from <packages/syscalls/scmtime.h>
 */

struct scheme_time {
    INT_32  sec;	/* seconds */
    INT_32  usec;	/* microseconds */
};
obj make_time( struct scheme_time *t, obj t_class );

/*
 */

obj rspg_extract_tuple( PGresult *result,
			int tuple_num,
			obj gen_class,
			obj plan,
			obj proto,
			obj t_class )
{
  UINT_8 *src = (UINT_8 *)PTR_TO_DATAPTR(plan);
  unsigned i, n;
  obj item;
  obj val = ZERO;

  n = string_length( plan ) / 2;
  item = alloc( SLOT(n), gen_class );
 
  for (i=0; i<n; i++, src+=2)
    {
      int field_num = src[1];

      if (src[0] == 0)
        {
	  /* use prototype's value */
          val = gvec_ref( proto, SLOT(i) );
	}
      else if (PQgetisnull( result, tuple_num, field_num ))
        {
	  val = FALSE_OBJ;
	}
      else
        {
	  int len;
	  char *p = PQgetvalue( result, tuple_num, field_num );

          switch (src[0])
            {
            case 1:  /* text */
	      len = PQgetlength( result, tuple_num, field_num );
	      val = bvec_alloc( len + 1, string_class );
	      memcpy( PTR_TO_DATAPTR(val), p, len );
	      break;
	    case 2:  /* int4 */
	      val = int2fx( *(int *)p );
	      break;
	    case 3:  /* oid */
	      val = int2fx( *(Oid *)p );
	      break;
	    case 5:  /* varchar */
	      len = strlen(p); /* blech... a better way? */
	      val = bvec_alloc( len + 1, string_class );
	      memcpy( PTR_TO_DATAPTR(val), p, len );
	      break;
	    case 6:  /* date */
	      {
                /*
                 *  immob tag 7 is <date>, and
                 *  730120 is the base <date> for 2000-01-01, which
                 *  is the origin for PostgresQL dates
                 */
                val = MAKE_IMMOB( 7, ((*(int *)p) + 730120) );
                /*
		rspg_date d = *(rspg_date *)p;
                unsigned char *d = (unsigned char *)p;
                char temp[50];
                int nchar;
		nchar = sprintf( temp,
                                 "[%d-%d-%04d]", 
                                 d.day, d.month, d.year );
		val = bvec_alloc( nchar+1, string_class );
                memcpy( PTR_TO_DATAPTR(val), temp, nchar );
                */
		break;
	      }
	    case 7: /* float4 */
	      val = make_float( *(float *)p );
	      break;
	    case 9: /* time -- (seconds since midnight) */
	    case 8: /* float8 */
	      val = make_float( *(double *)p );
	      break;
	    case 4:  /* abstime -- (seconds since 2000-01-01) */
	      {
                double t = *(double *)p;
                struct scheme_time st;
                
                /* 946684800 is the offset from the Unix epoch
                   (1970-01-01) to the PostgresQL epoch (2000-01-01) */
                st.sec = (int)floor( t + 946684800.0 );
                st.usec = (int)floor( fmod( t, 1.0 ) * 1.0e6 );

                val = make_time( &st, t_class );
		break;
	      }
	    case 10: /* bool */
	      val = (*(pqbool *)p) ? TRUE_OBJ : FALSE_OBJ;
	      break;
	    }
	}
      gvec_write_init( item, SLOT(i), val );
    }
  return item;
}
