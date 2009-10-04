#include <string.h>

#include <rscheme/hashfn.h>
#include <rscheme/hashmain.h>
#include <rscheme/scheme.h>

#include "rstoret.h"
#include "zstore.h"
#include "swizzler.h"
#include "pagemgr.h"
#include "alloc.h"
#include "swizzler.h"
#include "scan.h"

#define DEBUG_STORING (0)

int lru_model_prefill[] = { /* 0-15, 16, 20, 24, ..., 64 are literals */
			    0x20202020,
			    0x0100000f,
			    0x0100000e,
			    0x0100000d,
			    0x0100000c,
			    0x0100000b,
			    0x0100000a,
			    0x01000009,
			    0x01000008,
			    0x01000007,
			    0x01000006,
			    0x01000005,
			    0x01000004,
			    0x01000003,
			    0x01000002,
			    0x01000001,
			    0x01000000,
			    0x000800bb,
			    0x000700bb,
			    0x000600bb,
			    0x000500bb,
			    0x000400bb,
			    0x000300bb,
			    0x000200bb,
			    0x000100bb,
			    0x0001002f,
			    0x00010027,
			    0x0001001f,
			    0x00010007,
			    0x0001000b,
			    0x76766767,
			    0x12121212,
			    0x69699696,
			    0 };

/***********************************************************************
 *    ____                                        
 *   / ___|___  _ __ ___  _ __  _ __ ___  ___ ___ 
 *  | |   / _ \| '_ ` _ \| '_ \| '__/ _ \/ __/ __|
 *  | |__| (_) | | | | | | |_) | | |  __/\__ \__ \
 *   \____\___/|_| |_| |_| .__/|_|  \___||___/___/
 *                       |_|                      
 ***********************************************************************/

static void init_compressor( RStore *store, struct Compressor *c )
{
  c->store = store;

  /*
   *  when using LRU as the high-level model, use zlib-fast
   *  by default as the low-level model
   */
  if (!store->data_zipper)
    store->data_zipper = lss_find_zip_algorithm( "zlib-fast" );

  lru_init_compressor( c );
}

void lru_init_compressor( struct Compressor *c )
{
  unsigned i, j;

  c->long_base = c->worda_b;
  c->cntl_base = (UINT_8 *)(c->worda_b + LRUMODEL_MAX_WORDS);
  c->short_base = c->cntl_base + LRUMODEL_MAX_CNTL;

  c->cntl_ptr = c->cntl_base;
  c->short_ptr = c->short_base;
  c->long_ptr = c->long_base;

  for (i=0; i<CACHE_LINES; i++)
    for (j=0; j<4; j++)
      c->cache[i][j] = 0;

  c->victim_0 = 0;
  c->victim_1 = 0;

  /* pre-fill the cache with common values */

  for (i=0; lru_model_prefill[i]; i++)
    {
      int h = hash(lru_model_prefill[i]);

      if (c->cache[h][3])
	{
	  c->victim_1 = c->victim_0;
	  c->victim_0 = c->cache[h][3];
	}
      c->cache[h][3] = c->cache[h][2];
      c->cache[h][2] = c->cache[h][1];
      c->cache[h][1] = c->cache[h][0];
      c->cache[h][0] = lru_model_prefill[i];
    }
}


static void compress_word( struct Compressor *c, UINT_32 word )
{
  UINT_32 w = word;

  if (DEBUG_STORING)
    printf( "- %08lx  ", w );

  if (w < 16)
    {
      if (DEBUG_STORING)
	printf( "literal-%d\n", (int)w );
      *c->cntl_ptr++ = SYM_LITERAL + w;
    }
  else if ((w <= 64) && ((w & 3) == 0))
    {
      if (DEBUG_STORING)
	printf( "literalX4-%d\n", (int)w );
      *c->cntl_ptr++ = SYM_LITERALX4 + ((w-16)>>2);
    }
  else
    {
      int k, h = hash( w );
      unsigned diff;
      UINT_32 near;

      for (k=0; k<4; k++)
	{
	  if (c->cache[h][k] == w)
	    {
	      if (DEBUG_STORING)
		printf( "hit-%d-%d\n", h, k );
	      *c->cntl_ptr++ = SYM_HIT + h*4 + k;
	      switch (k)
		{
		case 3:
		  c->cache[h][3] = c->cache[h][2];
		case 2:
		  c->cache[h][2] = c->cache[h][1];
		case 1:
		  c->cache[h][1] = c->cache[h][0];
		case 0:
		  c->cache[h][0] = w;
		}
	      return;
	    }
	}
      if (w == c->victim_0)
	{
	  if (DEBUG_STORING)
	    printf( "victim-0\n" );
	  *c->cntl_ptr++ = SYM_VICTIM_0;

	  VICTIM_0_SHIFT(c,w);
	  return;
	}
      else if (w == c->victim_1)
	{
	  if (DEBUG_STORING)
	    printf( "victim-1\n" );
	  *c->cntl_ptr++ = SYM_VICTIM_1;
	  goto sh_done;
	}
      diff = (w >> SIM_SHIFT) & 0xFF;
      near = w & SIM_MASK;
      for (k=0; k<4; k++)
	{
	  if ((c->cache[h][k] & SIM_MASK) == near)
	    {
	      if (DEBUG_STORING)
		printf( "near-%d-%d %02x\n", h, k, diff );
	      *c->cntl_ptr++ = SYM_NEAR + h*4 + k;
	      *c->short_ptr++ = diff;
	      /*  is it better to keep the old exact value,
	       *  or toss it...?  We were tossing it, but
	       *  that leads to nearness fights in the very-recently-used
	       *  part of the cache..., we let's toss it instead
	       *
	       *  (note that load.c's CACHE_NEAR_CASE() has to do the
	       *  same thing!)
	       */
	      goto sh_done;
	    }
	}
      if (near == (c->victim_0 & SIM_MASK))
	{
	  if (DEBUG_STORING)
	    printf( "near-victim-0 %02x\n", diff );
	  *c->cntl_ptr++ = SYM_NEAR_VICTIM_0;
	  *c->short_ptr++ = diff;
	}
      else if (near == (c->victim_1 & SIM_MASK))
	{
	  if (DEBUG_STORING)
	    printf( "near-victim-1 %02x\n", diff );
	  *c->cntl_ptr++ = SYM_NEAR_VICTIM_1;
	  *c->short_ptr++ = diff;
	}
      /* it's a new value...
	 see if there's a compact way to represent it,
	 such as
	 (1) a 2's-complement 8-bit number (ie, -128..127)
	 (2) an unsigned 16-bit number
	 (3) of the form 0000 0000 0000 XXXX 0000 0000 00YY YY011
	 */
      else if (valid_8( w ))
	{
	  if (DEBUG_STORING)
	    printf( "new-8 %02x\n", (UINT_8)(w & 0xFF) );
	  *c->cntl_ptr++ = SYM_NEW_8;
	  *c->short_ptr++ = w & 0xFF;
	}
      else if (valid_4x2( w ))
	{
	  UINT_8 n = encode_4x2( w );
	  if (DEBUG_STORING)
	    printf( "new-4x2 %02x\n", n );
	  *c->cntl_ptr++ = SYM_NEW_4X2;
	  *c->short_ptr++ = n;
	}
      else if (valid_16( w ))
	{
	  if (DEBUG_STORING)
	    printf( "new-16 %04x\n", (UINT_16)(w & 0xFFFF) );
	  *c->cntl_ptr++ = SYM_NEW_16;
	  *c->short_ptr++ = (w >> 8) & 0xFF;
	  *c->short_ptr++ = w & 0xFF;
	}
      else
	{
	  if (DEBUG_STORING)
	    printf( "new-32 %08lx\n", w );
	  *c->cntl_ptr++ = SYM_NEW_32;
	  *c->long_ptr++ = w;
	}

    sh_done:
      VICTIM_1_SHIFT(c,w);
    }
}

static void compress_obj( struct Compressor *c, obj item )
{
  compress_word( c, VAL(item) );
}

static void close_compressor( struct Compressor *c )
{
}

static void write_compressed( struct Compressor *c, 
			      LSS *lss, 
			      UINT_32 rec )
{
  zipbuf vec[5];
  UINT_16 header[3];

  assert( (c->cntl_ptr - c->cntl_base) <= LRUMODEL_MAX_CNTL );
  assert( (c->short_ptr - c->short_base) <= LRUMODEL_MAX_SHORT );
  assert( (c->long_ptr - c->long_base) <= LRUMODEL_MAX_WORDS );

  header[0] = c->cntl_ptr - c->cntl_base;
  header[1] = c->short_ptr - c->short_base;
  header[2] = c->long_ptr - c->long_base;

  vec[0].ptr = &header[0];
  vec[0].limit = &header[3];

  vec[1].ptr = c->long_base;
  vec[1].limit = c->long_ptr;

  vec[2].ptr = c->short_base;
  vec[2].limit = c->short_ptr;

  vec[3].ptr = c->cntl_base;
  vec[3].limit = c->cntl_ptr;

  vec[4].ptr = NULL;

#if DEBUG_STORING
  printf( "[lru_]write_compressed: header %d + (%d + %d + %d W)\n",
	  (char *)vec[0].limit - (char *)vec[0].ptr,
	  (char *)vec[3].limit - (char *)vec[3].ptr,
	  (char *)vec[2].limit - (char *)vec[2].ptr,
	  (UINT_32 *)vec[1].limit - (UINT_32 *)vec[1].ptr );
#endif

  lss_writev( lss, rec, vec, c->store->data_zipper );
}

#define THIS_PAGE_WRITE  lru_model_write_page

#include "../page_storer.ci"

				   
