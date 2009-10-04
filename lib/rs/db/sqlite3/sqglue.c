#include <string.h>
#include <rscheme.h>
#include "/u/donovan/p/venti/aux/venti/sqlite-3.2.2/sqlite3.h"

int rs_to_sq3( sqlite3_stmt *s, int i, int copy_p, obj sv )
{
  if (FIXNUM_P( sv )) {
    return sqlite3_bind_int( s, i, fx2int( sv ) );
  } else if (LONGFLOAT_P( sv )) {
    return sqlite3_bind_double( s, i, extract_float( sv ) );
  } else if (LONG_INT_P( sv )) {
    long long int dst;
    INT_64 src = extract_int_64( sv );

    dst = ((unsigned long long)src.digits[0] << 48)
      + ((unsigned long long)src.digits[1] << 32)
      + ((unsigned long long)src.digits[2] << 16)
      + ((unsigned long long)src.digits[3] << 0);
    return sqlite3_bind_int64( s, i, dst );
  } else if (STRING_P( sv )) {
    return sqlite3_bind_text( s, i, 
                              string_text( sv ), string_length( sv ),
                              SQLITE_TRANSIENT );
  } else if (SYMBOL_P( sv )) {

    /* avoid the copy */

    sv = symbol_str( sv );
    return sqlite3_bind_text( s, i,
                              string_text( sv ), string_length( sv ),
                              SQLITE_STATIC );
  } else if (BVEC_P( sv )) {
    return sqlite3_bind_blob( s, i,
                              PTR_TO_DATAPTR( sv ), SIZEOF_PTR( sv ),
                              SQLITE_TRANSIENT );
  } else if (EQ( sv, FALSE_OBJ )) {
    return sqlite3_bind_null( s, i );
  } else if (PAIR_P( sv )) {
    obj type_as = pair_car( sv );
    obj data_is = pair_cdr( sv );

    /* the only retyping cases we allow are to treat a bvec as TEXT
       and a string as a BLOB */
    switch (fx2int( type_as )) {
    case 0:
      /* treat a bvec argument as TEXT */
      if (BVEC_P( data_is )) {
        return sqlite3_bind_text( s, i,
                                  PTR_TO_DATAPTR( data_is ), 
                                  SIZEOF_PTR( data_is ),
                                  SQLITE_TRANSIENT );
      }
      return -1;

    case 1:
      /* treat a string argument as a BLOB */
      if (STRING_P( data_is )) {
        return sqlite3_bind_blob( s, i,
                                  string_text( data_is ), 
                                  string_length( data_is ),
                                  SQLITE_TRANSIENT );
      }
      return -1;
      
    default:
      return -1;
    }
  } else {
    return -1;
  }
}

obj sq3_to_rs( sqlite3_stmt *s, int i )
{
  obj sv;

  switch (sqlite3_column_type( s, i )) {

  case SQLITE_INTEGER:
    {
      long long x = sqlite3_column_int64( s, i );
#ifdef HAVE_INT_64
      sv = int_64_compact( x );
#else
      INT_64 u;
      u.digits[0] = x >> 48;
      u.digits[1] = x >> 32;
      u.digits[2] = x >> 16;
      u.digits[3] = x >> 0;
      sv = int_64_compact( u );
#endif
      break;
    }

  case SQLITE_FLOAT:
    {
      sv = make_float( sqlite3_column_double( s, i ) );
      break;
    }

  case SQLITE_NULL:
    {
      sv = FALSE_OBJ;
      break;
    }

  default:
    /* this should not happen... */
    sv = make_string( sqlite3_column_text( s, i ) );
    break;

  case SQLITE_TEXT:
    {
      unsigned n = sqlite3_column_bytes( s, i );
      sv = bvec_alloc( n+1, string_class );
      memcpy( PTR_TO_DATAPTR( sv ),
              sqlite3_column_text( s, i ),
              n );
      break;
    }

  case SQLITE_BLOB:
    {
      unsigned n = sqlite3_column_bytes( s, i );
      sv = bvec_alloc( n+1, string_class );
      memcpy( PTR_TO_DATAPTR( sv ),
              sqlite3_column_blob( s, i ),
              n );
      break;
    }
  }
  return sv;
}
