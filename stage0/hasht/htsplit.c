/*-----------------------------------------------------------------*-C-*---
 * File:    handc/hasht/htsplit.c
 *
 *          Copyright (C)1997 Donovan Kolbly <d.kolbly@rscheme.org>
 *          as part of the RScheme project, licensed for free use.
 *          See <http://www.rscheme.org/> for the latest information.
 *
 * File version:     1.8
 * File mod date:    2003-06-22 18:15:01
 * System build:     v0.7.3.4-b7u, 2007-05-30
 *
 * Purpose:          code for splitting hash table buckets
 *------------------------------------------------------------------------*/

#include <rscheme/hashmain.h>
#include <rscheme/scheme.h>
#include <stdio.h>

#include "htstruct.h"
#include "chain.ci"

void split_bucket( obj table, obj bucket, obj h, obj k, obj v )
{
int i, di, j, dir_bits, bucket_bits;
obj b, vec;
struct bucket_chain hi, lo;
UINT_32 mask;

    dir_bits = fx2int(gvec_read(table,HASHTABLE_DIR_BITS));
    vec = gvec_read( table, HASHTABLE_DIRECTORY );

    if (EQ(bucket,FALSE_OBJ))
    {
	bucket = make_bucket( gvec_read( table, HASHTABLE_BUCKET_CLASS ),
			      dir_bits );
	write_dir( vec, h, bucket ); 

	write_bucket_hash( bucket, SLOT(2), h );
	write_bucket_key( bucket, SLOT(2), k );
	write_bucket_value( bucket, SLOT(2), v );
	return;
    }
    
    bucket_bits = fx2int(gvec_read(bucket,BUCKET_BITS));
    
    /* grow the hash table's directory if necessary */
    
    if (dir_bits == bucket_bits)
    {
    UINT_32 i, old_size;
    obj old_vec = vec;
    
	old_size = 1<<dir_bits;
	dir_bits++;

#ifdef DEBUG_0
	printf( "growing directory from %u entries\n", old_size );
#endif /* DEBUG_0 */
	
	vec = alloc( SLOT(2*old_size), vector_class );

	for (i=0; i<old_size; i++)
	    gvec_write_init( vec, SLOT(i), 
			      gvec_read( old_vec, SLOT(i) ) );
	for (i=0; i<old_size; i++)
	    gvec_write_init( vec, SLOT(i + old_size), 
	    		      gvec_read( old_vec, SLOT(i) ) );

	gvec_write_ptr( table, HASHTABLE_DIRECTORY, vec );
	gvec_write_non_ptr( table, HASHTABLE_DIR_BITS, int2fx(dir_bits) );
    }

    /* initialize the structures for the new chains */
    
#ifdef DEBUG_0
    printf( "initializing hi/lo\n" );
#endif /* DEBUG_0 */

    init_chain( table, &hi, bucket_bits+1 );
    init_chain( table, &lo, bucket_bits+1 );
    
    /* traverse the bucket */
    
    /* this mask selects the bit that distinguishes the "hi"
       bucket from the "lo" bucket
    */
    
    mask = VAL(int2fx(1)) << bucket_bits;
    
#ifdef DEBUG_0
    printf( "mask = %#x\n", mask );
#endif /* DEBUG_0 */

    chain_insert( (VAL(h) & mask) ? &hi : &lo, h, k, v );

    for (b=bucket; !EQ(b,FALSE_OBJ); b=gvec_read(b,BUCKET_OVERFLOW))
    {
	for (i=SLOT(2); i<SLOT(2+BUCKET_CAPACITY); i+=SLOT(1))
	{
	  obj h = gvec_read( b, i );
	  struct bucket_chain *use;
	
	  use = (VAL(h) & mask) ? &hi : &lo;
	  chain_insert( use, 
			h, 
			gvec_read( b, i+SLOT(BUCKET_CAPACITY) ),
			gvec_read( b, i+2*SLOT(BUCKET_CAPACITY) ) );
	}
    }
    
    /* install the new bucket chains in the directory */
    
    i = SLOT( fx2int( h ) & ((1 << bucket_bits) - 1) );
    di = SLOT( 1 << bucket_bits );

    for (j=0; j<(1<<(dir_bits - (bucket_bits+1))); j++)
    {
#ifdef DEBUG_0
	printf( "installing lo at %u\n", i/W );
#endif /* DEBUG_0 */
        gvec_write( vec, i, lo.first );
	i += di;
#ifdef DEBUG_0
	printf( "installing hi at %u\n", i/W );
#endif /* DEBUG_0 */
        gvec_write( vec, i, hi.first );
	i += di;
    }
}
