#include "rstoret.h"
#include "swizzler.h"
#include "pagemgr.h"
#if HAVE_ALLOCA_H
#include <alloca.h>
#endif

#define DEBUG_LOADING (0)

struct Decompressor {
  UINT_8      *ptr;
  UINT_8      *limit;
  UINT_8      data[32768];
};

/***********************************************************************
 *   ____                                                   
 *  |  _ \  ___  ___ ___  _ __ ___  _ __  _ __ ___  ___ ___ 
 *  | | | |/ _ \/ __/ _ \| '_ ` _ \| '_ \| '__/ _ \/ __/ __|
 *  | |_| |  __/ (_| (_) | | | | | | |_) | | |  __/\__ \__ \
 *  |____/ \___|\___\___/|_| |_| |_| .__/|_|  \___||___/___/
 *                                 |_|                      
 ***********************************************************************/

static void init_decompressor( struct Decompressor *d, 
			       LSS *lss,
			       UINT_32 rec )
{
  LSSAccess *a;
  zipbuf bufvec[2];
  size_t bytes;

  a = lss_read_access( lss, rec );
  if (!a)
    {
      scheme_error( "RStore error: page ~04x_~04x could not be loaded",
		    2,
		    int2fx( rec >> 16 ),
		    int2fx( rec & 0xFFFF ) );
    }

  bytes = lss_access_bytes( a );

  bufvec[0].ptr = d->data;
  bufvec[0].limit = d->data + bytes;
  bufvec[1].ptr = bufvec[1].limit = NULL;

  lss_readv( lss, bufvec, a );
  lss_read_release( lss, a );

  d->ptr = d->data;
  d->limit = d->ptr + bytes;
}

static UINT_32 decompress_word( struct Decompressor *d )
{
  UINT_32 w;

  w = *(d->ptr)++ << 24;
  w += *(d->ptr)++ << 16;
  w += *(d->ptr)++ << 8;
  w += *(d->ptr)++ << 0;
  assert( d->ptr <= d->limit );
  return w;
}

static obj decompress_obj( struct Decompressor *d )
{
  return OBJ(decompress_word(d));
}


static void close_decompressor( struct Decompressor *d )
{
}

#define THIS_PAGE_LOADER old_model_load_page
#define THIS_PAGE_SCANNER  old_model_scan_page

#include "../page_loader.ci"
