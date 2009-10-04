#include <stdio.h>
#include <stdlib.h>
#include <zlib.h>
#include <limits.h>
#include <assert.h>
#include <rscheme/pkgs/lss/lsszips.h>

static void *zlib_zip_proc( zip_algorithm *self,
			    void *dest, 
			    zipbuf *src )
{
  z_stream strm;
  int rc;
  void *end_ptr;

  strm.zalloc = Z_NULL;
  strm.zfree = Z_NULL;
  strm.opaque = Z_NULL;

  rc = deflateInit( &strm, *(int *)self->info );
  assert( rc == Z_OK );

  /* we claimed this was enough... */
  strm.avail_out = MAX_ZLEN( zipbuf_len( src ) );
  strm.next_out = dest;

  while (src->ptr)
    {
      char *p = strm.next_out;

      strm.next_in = src->ptr;
      strm.avail_in = (char *)src->limit - (char *)src->ptr;
      
      /* only deflate non-empty source blocks
       *  (deflate() doesn't like empty ones)
       */
      if (strm.avail_in > 0)
	{
	  rc = deflate( &strm, 0 );
#if 0
	  printf( "deflated %ld bytes -", (char *)strm.next_out  - p );
	  print_hex_data( p, (char *)strm.next_out  - p );
	  printf( "\n" );
#endif
	  assert( rc == Z_OK );
	}
      src++;
    }
  strm.next_in = Z_NULL;
  strm.avail_in = 0;
  rc = deflate( &strm, Z_FINISH );

#if 0
  printf( "deflated %ld bytes fini -", (char *)strm.next_out  - p );
  print_hex_data( p, (char *)strm.next_out  - p );
  printf( "\n" );
#endif

  assert( rc == Z_STREAM_END );
  end_ptr = strm.next_out;

  rc = deflateEnd( &strm );
  assert( rc == Z_OK );
  return end_ptr;
}

/*  note that we require the client to know how much content to expect
 *  (ie, the copy length is controlled by the destination size and
 *  not the source size)
 *  and they can provide multiple buffers for it (like a `readv').
 */

static void *zlib_unzip_proc( zip_algorithm *self,
			      zipbuf *dest, 
			      void *src )
{
  z_stream strm;
  int rc;
  void *end_ptr;

  strm.zalloc = Z_NULL;
  strm.zfree = Z_NULL;
  strm.opaque = Z_NULL;

  rc = inflateInit( &strm );

  /* we don't really know how much is available... */
  strm.avail_in = MAX_ZLEN( zipbuf_len( dest ) );
  strm.next_in = src;

  while (dest->ptr)
    {
      char *q = dest->ptr;
      char *p = strm.next_in;
      assert( rc == Z_OK );
      strm.next_out = dest->ptr;
      strm.avail_out = (char *)dest->limit - (char *)dest->ptr;

      /* only fill in non-empty buffers */
      if (strm.avail_out > 0)
	{
	  rc = inflate( &strm, Z_SYNC_FLUSH );
#if 0
	  printf( "output %d (%d left): ", 
		  (char *)strm.next_out - q,
		  strm.avail_out );
	  print_hex_data( q, (char *)strm.next_out - q );
	  printf( "\n" );
	  
	  printf( "input %d (%d left): ",
		  (char *)strm.next_in - p,
		  strm.avail_in );
	  print_hex_data( p, (char *)strm.next_in - p );
	  printf( " ; rc = %d\n", rc );
#endif
	}
      dest++;
    }
  assert( rc == Z_STREAM_END );
  end_ptr = strm.next_in;
  inflateEnd( &strm );
  return end_ptr;
}

static int fast_level = Z_BEST_SPEED;
static int best_level = Z_BEST_COMPRESSION;

/*  declare these algorithm to the configure script  */
/*| define-zip-algorithm zlib-fast lss_zlib_fast |*/

zip_algorithm lss_zlib_fast = {
  "zlib-fast",
  (void *)&fast_level,
  zlib_zip_proc,
  zlib_unzip_proc
};

/*| define-zip-algorithm zlib-best lss_zlib_best |*/

zip_algorithm lss_zlib_best = {
  "zlib-best",
  (void *)&best_level,
  zlib_zip_proc,
  zlib_unzip_proc
};

