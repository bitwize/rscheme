/*-----------------------------------------------------------------*-C-*---
 * File:    handc/hasht/tblcopy.c
 *
 *          Copyright (C)1997 Donovan Kolbly <d.kolbly@rscheme.org>
 *          as part of the RScheme project, licensed for free use.
 *          See <http://www.rscheme.org/> for the latest information.
 *
 * File version:     1.6
 * File mod date:    1998-12-04 08:19:44
 * System build:     v0.7.3.4-b7u, 2007-05-30
 *
 * Purpose:          code for copying entire hash tables
 *------------------------------------------------------------------------*/

#include <rscheme/hashmain.h>
#include <rscheme/scheme.h>
#include "htstruct.h"

static obj copy_bucket_chain( obj bucket )
{
  obj next;

    bucket = clone( bucket );
    next = gvec_read( bucket, BUCKET_OVERFLOW );
    if (truish(next))
    {
	gvec_write_fresh( bucket, 
			    BUCKET_OVERFLOW,
			    copy_bucket_chain( next ) );
    }
    return bucket;
}

static obj copy_table_dir( obj src_vec )
{
obj dst_vec, bucket;
UINT_32 mask, j, n;

    n = SIZEOF_PTR(src_vec);
    dst_vec = alloc( n, vector_class );
    for (j=0; j<n; j+=SLOT(1))
    {
        bucket = gvec_read( src_vec, j );
	if (!EQ(bucket,FALSE_OBJ))
	{
	  int n = fx2int( gvec_read( bucket, BUCKET_BITS ) );

	  mask = (~(SLOT(1)-1)) << n;
	    
	  if (j & mask)
	    /*  its a duplicate (shared) bucket, because
	     *  an upper bit (relative to the # bits it represents)
	     *  of its index is non-zero
	     */
	    bucket = gvec_read( dst_vec, (j & ~mask) );
	  else
	    /*  its upper bits are zero, so it is the first occurrence
	     *  of the chain.  hence, copy the chain, too
	     */
	    bucket = copy_bucket_chain( bucket );
	  
	  gvec_write_init_ptr( dst_vec, j, bucket );
	}
	else
	  gvec_write_init_non_ptr( dst_vec, j, FALSE_OBJ );
    }
    return dst_vec;
}

obj hashtable_copy( obj tbl )
{
    tbl = clone( tbl );
    gvec_write_fresh( tbl, 
		      HASHTABLE_DIRECTORY, 
		      copy_table_dir( gvec_read(tbl,HASHTABLE_DIRECTORY) ) );
    return tbl;
}
 
