#include "rstoret.h"
#include "zstore.h"
#include "swizzler.h"
#include "pagemgr.h"
#if HAVE_ALLOCA_H
#include <alloca.h>
#endif

#define DEBUG_LOADING (0)

/***********************************************************************
 *   ____
 *  |  _ \  ___  ___ ___  _ __ ___  _ __  _ __ ___  ___ ___
 *  | | | |/ _ \/ __/ _ \| '_ ` _ \| '_ \| '__/ _ \/ __/ __|
 *  | |_| |  __/ (_| (_) | | | | | | |_) | | |  __/\__ \__ \
 *  |____/ \___|\___\___/|_| |_| |_| .__/|_|  \___||___/___/
 *                                 |_|
 ***********************************************************************/

#define c    (&d->cstate)

static void init_decompressor( struct Decompressor *d, LSS *lss, UINT_32 rec )
{
  LSSAccess *a;
  UINT_16 header[3];
  zipbuf bufvec[3];
  size_t bytes;

  lru_init_compressor( &d->cstate );

  a = lss_read_access( lss, rec );
  if (!a)
    {
      scheme_error( "RStore error: page ~04x_~04x could not be loaded",
		    2,
		    int2fx( rec >> 16 ),
		    int2fx( rec & 0xFFFF ) );
    }

  bytes = lss_access_bytes( a );

  bufvec[0].ptr = &header[0];
  bufvec[0].limit = &header[3];
  bufvec[1].ptr = c->worda_b;
  bufvec[1].limit = bytes - sizeof(header) + (char *)bufvec[1].ptr;
  bufvec[2].ptr = NULL;

  lss_readv( lss, bufvec, a );
  lss_read_release( lss, a );

#if DEBUG_LOADING
  printf( "read %ld bytes: %d + (%u + %u + %u W)\n",
	  (long)bytes,
	  (char *)bufvec[0].limit - (char *)bufvec[0].ptr,
	  header[0],
	  header[1],
	  header[2] );
#endif

  c->long_ptr = c->worda_b;
  c->short_ptr = (UINT_8 *)(c->long_ptr + header[2]);
  c->cntl_ptr = c->short_ptr + header[1];

  c->cntl_base = c->cntl_ptr;
  c->short_base = c->short_ptr;
  c->long_base = c->long_ptr;

  c->cntl_lim = c->cntl_ptr + header[0];
  c->short_lim = c->short_ptr + header[1];
  c->long_lim = c->long_ptr + header[2];

#if DEBUG_LOADING
  printf( "cntl: " );
  rstore_print_hex_data( c->cntl_ptr, header[0] );
  printf( "\n" );

  printf( "shrt: " );
  rstore_print_hex_data( c->short_ptr, header[1] );
  printf( "\n" );

  printf( "long: " );
  rstore_print_hex_data( c->long_ptr, sizeof(UINT_32) * header[2] );
  printf( "\n" );
#endif

  assert( bytes == (sizeof header
		    + (header[0] * sizeof(UINT_8))
		    + (header[1] * sizeof(UINT_8))
		    + (header[2] * sizeof(UINT_32))) );
}


#define CACHE_HIT_CASE(h,k)                                          \
    case SYM_HIT + h*ASSOCIATIVITY + k:                              \
      w = d->cstate.cache[h][k];                                     \
      /* this looks funny, but its designed to get optimized... */   \
      if (k == 3) d->cstate.cache[h][3] = d->cstate.cache[h][2];     \
      if (k >= 2) d->cstate.cache[h][2] = d->cstate.cache[h][1];     \
      if (k >= 1) d->cstate.cache[h][1] = d->cstate.cache[h][0];     \
      if (k != 0) d->cstate.cache[h][0] = w;                         \
      if (DEBUG_LOADING) 					     \
        printf( "- %08lx  hit-%d-%d\n", w, h, k );   		     \
      return w;

#define CACHE_NEAR_CASE(H,k)                                         \
    case SYM_NEAR + H*ASSOCIATIVITY + k:                             \
      {                                                              \
        UINT_8 b = *d->cstate.short_ptr++;                           \
        w = (d->cstate.cache[H][k] & SIM_MASK) + (b << SIM_SHIFT);   \
        if (DEBUG_LOADING) 				             \
          printf( "- %08lx  near-%d-%d %02x\n", w, H, k, b );        \
        h = H; 							     \
        goto sh_done_noh; }


#define CASE_LITERAL(lit)				\
    case SYM_LITERAL + lit:   				\
      w = lit;						\
      if (DEBUG_LOADING)				\
	printf( "- %08lx  literal-%d\n", w, lit );	\
      return w

#define CASE_LITERALX4(lit)				\
    case SYM_LITERALX4 + ((lit-16)/4):                  \
      w = lit;                                          \
      if (DEBUG_LOADING)                                \
	printf( "- %08lx  literalX4-%d\n", w, lit );	\
      return w

static UINT_32 decompress_word( struct Decompressor *d )
{
  UINT_8 b;
  UINT_32 w;
  unsigned h;

  assert( c->cntl_ptr < c->cntl_lim );

  b = *c->cntl_ptr++;

  if (DEBUG_LOADING)
    printf( "%02x: ", b );

  switch (b)
    {
    default:
      w = 0;
      scheme_error( "rs.db.rstore: invalid lru_model byte ~02x", 1,
		    int2fx( b ) );
      return w;

    case SYM_VICTIM_0:
      w = d->cstate.victim_0;
      h = hash( w );
      if (DEBUG_LOADING)
	printf( "- %08lx  victim-0\n", w );
      VICTIM_0_SHIFT( &d->cstate, w );
      return w;

    case SYM_VICTIM_1:
      w = d->cstate.victim_1;
      if (DEBUG_LOADING)
	printf( "- %08lx  victim-1\n", w );
    sh_done:
      h = hash( w );
    sh_done_noh:
      VICTIM_1_SHIFT( &d->cstate, w );
      return w;

    case SYM_NEAR_VICTIM_0:
      {
	UINT_8 b = *d->cstate.short_ptr++;
        w = (d->cstate.victim_0 & SIM_MASK) + (b << SIM_SHIFT);
	if (DEBUG_LOADING)
	  printf( "- %08lx  near-victim-0 %02x\n", w, b );
	/*  Note: The remnants of a near miss remain in the
	 *  cache.  More specifically, the new value shifts
	 *  a value out of victim_1!
	 */
	goto sh_done;
      }

    case SYM_NEAR_VICTIM_1:
      {
	UINT_8 b = *d->cstate.short_ptr++;
        w = (d->cstate.victim_1 & SIM_MASK) + (b << SIM_SHIFT);
	if (DEBUG_LOADING)
	  printf( "- %08lx  near-victim-1 %02x\n", w, b );
	goto sh_done;
      }

    case SYM_NEW_8:
      {
	signed char t = *d->cstate.short_ptr++;
	w = (int)t;
	if (DEBUG_LOADING)
	  printf( "- %08lx  new-8 %02x\n", w, d->cstate.short_ptr[-1] );
	goto sh_done;
      }

    case SYM_NEW_4X2:
      {
	UINT_8 b = *d->cstate.short_ptr++;
	w = decode_4x2( b );
	if (DEBUG_LOADING)
	  printf( "- %08lx  new-4x2 %02x\n", w, b );
	goto sh_done;
      }

    case SYM_NEW_16:
      {
	UINT_8 a = *d->cstate.short_ptr++;
	UINT_8 b = *d->cstate.short_ptr++;
	w = (a << 8) + b;
	if (DEBUG_LOADING)
	  printf( "- %08lx  new-16 %04lx\n", w, w );
	goto sh_done;
      }

    case SYM_NEW_32:
      {
	w = *d->cstate.long_ptr++;
	if (DEBUG_LOADING)
	  printf( "- %08lx  new-32 %08lx\n", w, w );
	goto sh_done;
      }

    /* all the literal values (which don't go into the cache) */

    CASE_LITERAL(0);
    CASE_LITERAL(1);
    CASE_LITERAL(2);
    CASE_LITERAL(3);
    CASE_LITERAL(4);
    CASE_LITERAL(5);
    CASE_LITERAL(6);
    CASE_LITERAL(7);
    CASE_LITERAL(8);
    CASE_LITERAL(9);
    CASE_LITERAL(10);
    CASE_LITERAL(11);
    CASE_LITERAL(12);
    CASE_LITERAL(13);
    CASE_LITERAL(14);
    CASE_LITERAL(15);
    CASE_LITERALX4(16);
    CASE_LITERALX4(20);
    CASE_LITERALX4(24);
    CASE_LITERALX4(28);
    CASE_LITERALX4(32);
    CASE_LITERALX4(36);
    CASE_LITERALX4(40);
    CASE_LITERALX4(44);
    CASE_LITERALX4(48);
    CASE_LITERALX4(52);
    CASE_LITERALX4(56);
    CASE_LITERALX4(60);
    CASE_LITERALX4(64);

    /* all the cache hits and near misses... */

    CACHE_HIT_CASE(0,0)
    CACHE_HIT_CASE(0,1)
    CACHE_HIT_CASE(0,2)
    CACHE_HIT_CASE(0,3)
    CACHE_HIT_CASE(1,0)
    CACHE_HIT_CASE(1,1)
    CACHE_HIT_CASE(1,2)
    CACHE_HIT_CASE(1,3)
    CACHE_HIT_CASE(2,0)
    CACHE_HIT_CASE(2,1)
    CACHE_HIT_CASE(2,2)
    CACHE_HIT_CASE(2,3)
    CACHE_HIT_CASE(3,0)
    CACHE_HIT_CASE(3,1)
    CACHE_HIT_CASE(3,2)
    CACHE_HIT_CASE(3,3)
    CACHE_HIT_CASE(4,0)
    CACHE_HIT_CASE(4,1)
    CACHE_HIT_CASE(4,2)
    CACHE_HIT_CASE(4,3)
    CACHE_HIT_CASE(5,0)
    CACHE_HIT_CASE(5,1)
    CACHE_HIT_CASE(5,2)
    CACHE_HIT_CASE(5,3)
    CACHE_HIT_CASE(6,0)
    CACHE_HIT_CASE(6,1)
    CACHE_HIT_CASE(6,2)
    CACHE_HIT_CASE(6,3)
    CACHE_HIT_CASE(7,0)
    CACHE_HIT_CASE(7,1)
    CACHE_HIT_CASE(7,2)
    CACHE_HIT_CASE(7,3)
    CACHE_HIT_CASE(8,0)
    CACHE_HIT_CASE(8,1)
    CACHE_HIT_CASE(8,2)
    CACHE_HIT_CASE(8,3)
    CACHE_HIT_CASE(9,0)
    CACHE_HIT_CASE(9,1)
    CACHE_HIT_CASE(9,2)
    CACHE_HIT_CASE(9,3)
    CACHE_HIT_CASE(10,0)
    CACHE_HIT_CASE(10,1)
    CACHE_HIT_CASE(10,2)
    CACHE_HIT_CASE(10,3)
    CACHE_HIT_CASE(11,0)
    CACHE_HIT_CASE(11,1)
    CACHE_HIT_CASE(11,2)
    CACHE_HIT_CASE(11,3)
    CACHE_HIT_CASE(12,0)
    CACHE_HIT_CASE(12,1)
    CACHE_HIT_CASE(12,2)
    CACHE_HIT_CASE(12,3)
    CACHE_HIT_CASE(13,0)
    CACHE_HIT_CASE(13,1)
    CACHE_HIT_CASE(13,2)
    CACHE_HIT_CASE(13,3)
    CACHE_HIT_CASE(14,0)
    CACHE_HIT_CASE(14,1)
    CACHE_HIT_CASE(14,2)
    CACHE_HIT_CASE(14,3)
    CACHE_HIT_CASE(15,0)
    CACHE_HIT_CASE(15,1)
    CACHE_HIT_CASE(15,2)
    CACHE_HIT_CASE(15,3)

    CACHE_NEAR_CASE(0,0)
    CACHE_NEAR_CASE(0,1)
    CACHE_NEAR_CASE(0,2)
    CACHE_NEAR_CASE(0,3)
    CACHE_NEAR_CASE(1,0)
    CACHE_NEAR_CASE(1,1)
    CACHE_NEAR_CASE(1,2)
    CACHE_NEAR_CASE(1,3)
    CACHE_NEAR_CASE(2,0)
    CACHE_NEAR_CASE(2,1)
    CACHE_NEAR_CASE(2,2)
    CACHE_NEAR_CASE(2,3)
    CACHE_NEAR_CASE(3,0)
    CACHE_NEAR_CASE(3,1)
    CACHE_NEAR_CASE(3,2)
    CACHE_NEAR_CASE(3,3)
    CACHE_NEAR_CASE(4,0)
    CACHE_NEAR_CASE(4,1)
    CACHE_NEAR_CASE(4,2)
    CACHE_NEAR_CASE(4,3)
    CACHE_NEAR_CASE(5,0)
    CACHE_NEAR_CASE(5,1)
    CACHE_NEAR_CASE(5,2)
    CACHE_NEAR_CASE(5,3)
    CACHE_NEAR_CASE(6,0)
    CACHE_NEAR_CASE(6,1)
    CACHE_NEAR_CASE(6,2)
    CACHE_NEAR_CASE(6,3)
    CACHE_NEAR_CASE(7,0)
    CACHE_NEAR_CASE(7,1)
    CACHE_NEAR_CASE(7,2)
    CACHE_NEAR_CASE(7,3)
    CACHE_NEAR_CASE(8,0)
    CACHE_NEAR_CASE(8,1)
    CACHE_NEAR_CASE(8,2)
    CACHE_NEAR_CASE(8,3)
    CACHE_NEAR_CASE(9,0)
    CACHE_NEAR_CASE(9,1)
    CACHE_NEAR_CASE(9,2)
    CACHE_NEAR_CASE(9,3)
    CACHE_NEAR_CASE(10,0)
    CACHE_NEAR_CASE(10,1)
    CACHE_NEAR_CASE(10,2)
    CACHE_NEAR_CASE(10,3)
    CACHE_NEAR_CASE(11,0)
    CACHE_NEAR_CASE(11,1)
    CACHE_NEAR_CASE(11,2)
    CACHE_NEAR_CASE(11,3)
    CACHE_NEAR_CASE(12,0)
    CACHE_NEAR_CASE(12,1)
    CACHE_NEAR_CASE(12,2)
    CACHE_NEAR_CASE(12,3)
    CACHE_NEAR_CASE(13,0)
    CACHE_NEAR_CASE(13,1)
    CACHE_NEAR_CASE(13,2)
    CACHE_NEAR_CASE(13,3)
    CACHE_NEAR_CASE(14,0)
    CACHE_NEAR_CASE(14,1)
    CACHE_NEAR_CASE(14,2)
    CACHE_NEAR_CASE(14,3)
    CACHE_NEAR_CASE(15,0)
    CACHE_NEAR_CASE(15,1)
    CACHE_NEAR_CASE(15,2)
    CACHE_NEAR_CASE(15,3)
    }
}

static obj decompress_obj( struct Decompressor *d )
{
  return OBJ(decompress_word(d));
}

static void close_decompressor( struct Decompressor *d )
{
  assert( c->cntl_ptr == c->cntl_lim );
  assert( c->short_ptr == c->short_lim );
  assert( c->long_ptr == c->long_lim );
}
#undef c

#define THIS_PAGE_LOADER lru_model_load_page
#define THIS_PAGE_SCANNER lru_model_scan_page

#include "../page_loader.ci"
