#include <string.h>

#include "rstoret.h"
#include "swizzler.h"
#include "pagemgr.h"
#include "alloc.h"
#include "swizzler.h"
#include "scan.h"

struct Compressor {
  struct RStore  *store;
  UINT_8	 *ptr;
  UINT_8	  temp[31744];
};

#define DEBUG_STORING (0)

static void init_compressor( struct RStore *store, struct Compressor *c )
{
  c->store = store;
  c->ptr = c->temp;
}

static void compress_word( struct Compressor *c, UINT_32 word )
{
  *(c->ptr)++ = word >> 24;
  *(c->ptr)++ = word >> 16;
  *(c->ptr)++ = word >> 8;
  *(c->ptr)++ = word >> 0;
}

static void compress_obj( struct Compressor *c, obj item )
{
  compress_word( c, VAL(item) );
}

static void stop_compressor( struct Compressor *c )
{
}

static void close_compressor( struct Compressor *c )
{
}

static UINT_32 compressed_size( struct Compressor *c )
{
  return c->ptr - c->temp;
}

static void *compressed_data( struct Compressor *c )
{
  return c->temp;
}

static void write_compressed( struct Compressor *c, 
			      LSS *lss, 
			      UINT_32 rec )
{
  lss_write( lss, rec, compressed_data(c), compressed_size(c), 
	     c->store->data_zipper );
}

#define THIS_PAGE_WRITE  old_model_write_page

#include "../page_storer.ci"

				   
