/*-----------------------------------------------------------------*-C-*---
 * File:    handc/hasht/hashmain.c
 *
 *          Copyright (C)1997 Donovan Kolbly <d.kolbly@rscheme.org>
 *          as part of the RScheme project, licensed for free use.
 *          See <http://www.rscheme.org/> for the latest information.
 *
 * File version:     1.11
 * File mod date:    2004-07-02 08:01:33
 * System build:     v0.7.3.4-b7u, 2007-05-30
 *
 * Purpose:          main set of hash table functionality
 *------------------------------------------------------------------------*/

#include <rscheme/hashmain.h>
#include <rscheme/scheme.h>
#include "htstruct.h"

void hashtable_clear( obj table )
{
  obj vec;
  UINT_32 j, n;

  if (EQ( gvec_ref( table, HASHTABLE_COUNT ), ZERO )) {
    return;
  }

  vec = gvec_read( table, HASHTABLE_DIRECTORY );
  n = SIZEOF_PTR(vec);

  for (j=0; j<n; j+=SLOT(1)) {
    gvec_write( vec, j, FALSE_OBJ );
  }
  gvec_write_non_ptr( table, HASHTABLE_COUNT, ZERO );
}

UINT_32 hashtable_size( obj table )
{
  return fx2int( gvec_ref( table, HASHTABLE_COUNT ) );
}

obj make_bucket( obj bucket_class, int bucket_bits )
{
  obj x;

  assert( CLASS_P(bucket_class) );
  x = gvec_alloc( (3 * BUCKET_CAPACITY) + 2, bucket_class );
  gvec_write_fresh_non_ptr( x, SLOT(0), int2fx( bucket_bits ) );
  return x;
}

/*  Low-level operation:  Install a (key,value) pair into
    a table, having already computed the hash value.
*/

void hashtable_install( obj table, obj hash, obj key, obj value )
{
  obj vec, bucket;
  UINT_32 i;

  inserting_one( table );
  vec = gvec_read( table, HASHTABLE_DIRECTORY );
    
  for (bucket = read_dir(vec,hash);
       !EQ(bucket,FALSE_OBJ);
       bucket = gvec_read( bucket, BUCKET_OVERFLOW ))
    {
      for (i=SLOT(2); i<SLOT(2+BUCKET_CAPACITY); i+=sizeof(obj))
	{
	  if (EQ(read_bucket_hash(bucket,i),FALSE_OBJ))
	    {
	      write_bucket_hash( bucket, i, hash );
	      write_bucket_key( bucket, i, key );
	      write_bucket_value( bucket, i, value );
	      return;
	    }
	}
    }

  /* grow things... */
    
  split_bucket( table, read_dir( vec, hash ), hash, key, value );
}

void hashtable_foreach( obj table, void *info, 
			void (*fn)( void *info, obj h, obj key, obj value ) )
{
  obj vec, bucket;
  UINT_32 mask, i, j, n;

  vec = gvec_read( table, HASHTABLE_DIRECTORY );
  n = SIZEOF_PTR(vec);
  for (j=0; j<n; j+=SLOT(1))
    {
      bucket = gvec_read( vec, j );
      if (!EQ(bucket,FALSE_OBJ))
	{
	  int n = fx2int( gvec_read( bucket, BUCKET_BITS ) );
	  mask = (~(SLOT(1)-1)) << n;
	    
	  if (!(j & mask))
	    {
	      do {
		for (i=SLOT(2); i<SLOT(2+BUCKET_CAPACITY); i+=sizeof(obj))
		  {
		    if (!EQ(read_bucket_hash(bucket,i),FALSE_OBJ))
		      {
			fn( info, read_bucket_hash( bucket, i ),
			    read_bucket_key( bucket, i ),
			    read_bucket_value( bucket, i ) );
		      }
		  }
		bucket = gvec_read( bucket, BUCKET_OVERFLOW );
	      } while (!EQ(bucket,FALSE_OBJ));
	    }
	}
    }
}

obj hashtable_chains( obj table )
{
  obj chain_list, vec, bucket;
  UINT_32 mask, j, n;

  chain_list = NIL_OBJ;
  vec = gvec_read( table, HASHTABLE_DIRECTORY );
  n = SIZEOF_PTR(vec);
  for (j=0; j<n; j+=SLOT(1))
    {
      bucket = gvec_read( vec, j );
      if (!EQ(bucket,FALSE_OBJ))
	{
	  int n = fx2int( gvec_read( bucket, BUCKET_BITS ) );
	  mask = (~(SLOT(1)-1)) << n;
	    
	  if (!(j & mask))
	    {
	      chain_list = cons( bucket, chain_list );
	    }
	}
    }
  return chain_list;
}

static void accum_key( void *info, obj h, obj k, obj v )
{
  *(obj *)info = cons( k, *(obj *)info );
}

static void accum_value( void *info, obj h, obj k, obj v )
{
  *(obj *)info = cons( v, *(obj *)info );
}

obj hashtable_keys_to_list( obj table )
{
  obj list = NIL_OBJ;

  hashtable_foreach( table, &list, accum_key );
  return list;
}

obj hashtable_values_to_list( obj table )
{
  obj list = NIL_OBJ;

  hashtable_foreach( table, &list, accum_value );
  return list;
}

#ifdef INLINES
#define eq_fn_inline inline
#else
#define eq_fn_inline /* nothing */
#endif

/********************* string=?-hash-table *********************/

#define SPECIAL_TABLE_LOOKUP	stringtable_lookup
#define SPECIAL_TABLE_INSERT    stringtable_insert
#define SPECIAL_TABLE_REMOVE    stringtable_remove
#define SPECIAL_TABLE_PROBE     stringtable_probe
#define SPECIAL_CMP(h1,h2,k1,k2) (EQ(h1,h2) && string_eq(k1,k2))

#include "special.ci"

#undef SPECIAL_TABLE_LOOKUP
#undef SPECIAL_TABLE_INSERT
#undef SPECIAL_TABLE_REMOVE
#undef SPECIAL_TABLE_PROBE
#undef SPECIAL_CMP

/********************* string-ci=?-hash-table *********************/

#define SPECIAL_TABLE_LOOKUP	cistringtable_lookup
#define SPECIAL_TABLE_INSERT    cistringtable_insert
#define SPECIAL_TABLE_REMOVE    cistringtable_remove
#define SPECIAL_TABLE_PROBE     cistringtable_probe
#define SPECIAL_CMP(h1,h2,k1,k2) (EQ(h1,h2) && string_ci_eq(k1,k2))

#include "special.ci"

#undef SPECIAL_TABLE_LOOKUP
#undef SPECIAL_TABLE_INSERT
#undef SPECIAL_TABLE_REMOVE
#undef SPECIAL_TABLE_PROBE
#undef SPECIAL_CMP

/********************* eq?-hash-table *********************/

#define SPECIAL_TABLE_LOOKUP	objecttable_lookup
#define SPECIAL_TABLE_INSERT    objecttable_insert
#define SPECIAL_TABLE_REMOVE    objecttable_remove
#define SPECIAL_TABLE_PROBE     objecttable_probe
#define SPECIAL_CMP(h1,h2,k1,k2)	EQ(k1,k2)

#include "special.ci"

#undef SPECIAL_TABLE_LOOKUP
#undef SPECIAL_TABLE_INSERT
#undef SPECIAL_TABLE_REMOVE
#undef SPECIAL_TABLE_PROBE
#undef SPECIAL_CMP_FN

