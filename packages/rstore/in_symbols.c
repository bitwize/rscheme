#include <string.h>
#include "indirect.h"

obj construct_symbols( struct RStore *store, UINT_32 inst_id, 
		       char *data, UINT_32 data_len )
{
  char *s = data;
  UINT_32 i, num, len;
  obj itemv;

  assert( inst_id == 0 );

  memcpy( &num, s, sizeof(UINT_32) );
  s += sizeof(UINT_32);

  num = LOAD_32(num);
  /* printf( "constructing %u symbols: ", num ); */
  
  itemv = alloc( SLOT(num), vector_class );
  
  for (i=0; i<num; i++)
    {
      obj txt;

      memcpy( &len, s, sizeof(UINT_32) );
      s += sizeof(UINT_32);

      txt = bvec_alloc( len+1, string_class );
      memcpy( PTR_TO_DATAPTR(txt), s, len );
      s += len;
      
      gvec_write_init( itemv, SLOT(i), intern(txt) );
    }
  /* printf( ".\n" ); */
  assert( s == (data + data_len) );
  return itemv;
}


obj unswizzle_symbol_itemv( obj symbol_list )
{
  UINT_32 num = 0, len = 0;
  obj x, indp;
  char *d;

  for (x=symbol_list; PAIR_P(x); x=pair_cdr(x))
    {
      len += string_length( symbol_str(pair_car(x)) );
      num++;
    }
  indp = bvec_alloc( (num + 1) * sizeof(UINT_32) + len + 1, string_class );
  d = (char *)PTR_TO_DATAPTR(indp);

  num = STORE_32(num);

  memcpy( d, &num, sizeof(UINT_32) );
  d += sizeof(UINT_32);

  for (x=symbol_list; PAIR_P(x); x=pair_cdr(x))
    {
      obj txt = symbol_str(pair_car(x));

      len = STORE_32( string_length( txt ) );
      memcpy( d, &len, sizeof(UINT_32) );
      d += sizeof(UINT_32);

      memcpy( d, string_text(txt), string_length(txt) );
      d += string_length(txt);
    }
  return indp;
}
