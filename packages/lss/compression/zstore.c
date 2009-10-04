#include <stdio.h>
#include <stdlib.h>
#include "zstore.h"
#if HAVE_ZLIB
#include <zlib.h>
#endif
#include "timebase/timebase.h"

#if !HAVE_ZLIB
/* a miniature, builtin compression library */

#define  TRACE_SUBSTRINGS  1

#define SUBSTR_LEN  (4)

typedef struct z_stream {
  void *zalloc;
  void *zfree;
  void *opaque;

  /* input buffer */
  unsigned char   *next_in;
  size_t           avail_in;

  /* output buffer */
  unsigned char   *next_out;
  size_t           avail_out;

#if TRACE_SUBSTRINGS
  unsigned n_past;
  unsigned char past[SUBSTR_LEN];
#endif
} z_stream;

#if TRACE_SUBSTRINGS
static FILE *substring_trace_file = NULL;
#endif

#define Z_NULL ((void *)0)
#define Z_OK (0)
#define Z_STREAM_END (1)

#define Z_FINISH (1)

#ifdef __GNUC__
#define z_inline inline
#else
#define z_inline 
#endif

static int deflateInit( z_stream *strm, int level )
{
#if TRACE_SUBSTRINGS
  strm->n_past = 0;
  if (!substring_trace_file)
    substring_trace_file = popen( "countpop.pl > /tmp/zstore.pop", "w" );

  /*substring_trace_file = fopen( "/tmp/zstore-substr.out", "w" );*/
  /*substring_trace_file = popen( "gzip>/tmp/zstore-substr.out.gz", "w" );*/
  fprintf( substring_trace_file, ";; BEGIN\n" );
#endif
  return Z_OK;
}

static int deflateEnd( z_stream *z )
{
#if TRACE_SUBSTRINGS
  fprintf( substring_trace_file, ";; END\n" );
  fflush( substring_trace_file );
#endif
  return Z_OK;
}

static z_inline char nibble( int v )
{
  return "0123456789abcdef"[v];
}

static int deflate( z_stream *z, int mode )
{
#if TRACE_SUBSTRINGS
/***  instrumentation purposes -- write out the stream contents
 ***  and all up-to-SUBSTR_LEN-byte substrings
 ***/
  size_t i, j, k;
  int only_of_len = 0; /* 0=>all */

  if (getenv( "TRACE_SUBSTR_LEN" ))
    only_of_len = atoi( getenv( "TRACE_SUBSTR_LEN" ) );

  fprintf( substring_trace_file, ";; ZIP %u\n", (unsigned)z->avail_in );

  for (i=0; i<z->avail_in; i++)
    {
      /* for each input character, print out its substrings */
      fprintf( substring_trace_file, ";; [%u] %#02x\n", 
	       (unsigned)i, z->next_in[i] );

      for (j=0; j<SUBSTR_LEN-1; j++)
	z->past[j] = z->past[j+1];

      z->past[SUBSTR_LEN-1] = z->next_in[i];
      z->n_past++;
      if (z->n_past > SUBSTR_LEN)
	z->n_past = SUBSTR_LEN;
      for (j=1; j<=z->n_past; j++)
	{
	  if (!only_of_len || (j == only_of_len))
	    {
	      /* the substring of length `j' */
	      char *p, temp[200];
	      p = temp;
	      for (k=SUBSTR_LEN-j; k<SUBSTR_LEN; k++)
		{
		  *p++ = nibble( z->past[k] >> 4 );
		  *p++ = nibble( z->past[k] & 0xF );
		  *p++ = ' ';
		}
	      *--p = 0;
	      fprintf( substring_trace_file, "%s\n", temp );
	    }
	}
    }
#endif

  memcpy( z->next_out, z->next_in, z->avail_in );
  z->next_out += z->avail_in;
  z->avail_out -= z->avail_in;
  z->next_in += z->avail_in;
  z->avail_in = 0;
  return Z_OK;
}

static z_inline int inflateInit( z_stream *strm )
{
  return deflateInit( strm, 0 );
}

static z_inline int inflateEnd( z_stream *strm )
{
  return deflateEnd( strm );
}

static int inflate( z_stream *z, int mode )
{
  /* just a buffer copy... */
  size_t len = z->avail_in;
  if (z->avail_out < len)
    len = z->avail_out;

  memcpy( z->next_out, z->next_in, len );
  z->next_out += len;
  z->avail_out -= len;
  z->next_in += len;
  z->avail_in -= len;
  return Z_OK;
}

#endif

#define ZSTORE_COMPRESSION  (1)   /* compression level, 0-9, a la gzip */

#define SYM_ZERO            (0)
#define SYM_FALSE           (1)
#define SYM_NIL             (2)
#define SYM_VICTIM_0        (3)
#define SYM_VICTIM_1        (4)
#define SYM_NEAR_VICTIM_0   (5)
#define SYM_NEAR_VICTIM_1   (6)
#define SYM_NEW_32          (7)
#define SYM_NEW_16          (8)
#define SYM_NEW_4X2         (9)
#define SYM_NEW_8           (10)
#define SYM_NEAR            (32)
#define SYM_HIT             (SYM_NEAR+CACHE_LINES*ASSOCIATIVITY)

/* 32 + 2*(16 * 4) ==> 118 spare symbols */

#define VICTIM_0_SHIFT(c,w)                    \
	  (c)->victim_0 = (c)->cache[h][3];    \
	  (c)->cache[h][3] = (c)->cache[h][2]; \
	  (c)->cache[h][2] = (c)->cache[h][1]; \
	  (c)->cache[h][1] = (c)->cache[h][0]; \
	  (c)->cache[h][0] = w;

#define VICTIM_1_SHIFT(c,w)                    \
	  (c)->victim_1 = (c)->victim_0;       \
	  VICTIM_0_SHIFT(c,w)


static inline int valid_8( UINT_32 w )
{
  INT_32 sw = w;

  return ((sw >= -128) && (sw <= 127));
}

static inline int valid_16( UINT_32 w )
{
  return (w <= 0xFFFF);
}

static inline UINT_8 encode_4x2( UINT_32 w )
{
  return ((w >> 12) & 0xF0) + ((w >> 3) & 0xF);
}

static inline UINT_32 decode_4x2( UINT_8 b )
{
  return ((b << 12) & 0xF0000) + ((b & 0xF) << 3) + 3;
}

static inline int valid_4x2( UINT_32 w )
{
  return (decode_4x2( encode_4x2( w ) ) == w);
}

#define SIM_MASK (0xFFFFF807)
#define SIM_SHIFT (3)

static int hash( UINT_32 k )
{
  return ((k >> 16) + (k >> 12) + (k & 3)) & (CACHE_LINES-1);
}

static int prefill[] = { 10, 9, 8, 7, 6, 5, 4, 3, 2, 1,
			 0x01000003,
			 0x01000002,
			 0x01000001,
			 0x01000000,
			 0x00010007,
			 0x0001000b,
			 0x76766767,
			 0 };

#define verbose (0)

/***********************************************************************
 *    ____                                        
 *   / ___|___  _ __ ___  _ __  _ __ ___  ___ ___ 
 *  | |   / _ \| '_ ` _ \| '_ \| '__/ _ \/ __/ __|
 *  | |__| (_) | | | | | | |_) | | |  __/\__ \__ \
 *   \____\___/|_| |_| |_| .__/|_|  \___||___/___/
 *                       |_|                      
 ***********************************************************************/

void init_compressor( struct Compressor *c )
{
  unsigned i, j;

  c->cntl_ptr = c->cntl_b;
  c->short_ptr = c->short_b;
  c->long_ptr = c->long_b;

  for (i=0; i<CACHE_LINES; i++)
    for (j=0; j<4; j++)
      c->cache[i][j] = 0;

  c->victim_0 = 0;
  c->victim_1 = 0;

  /* pre-fill the cache with common values */

  for (i=0; prefill[i]; i++)
    {
      int h = hash(prefill[i]);

      if (c->cache[h][3])
	{
	  c->victim_1 = c->victim_0;
	  c->victim_0 = c->cache[h][3];
	}
      c->cache[h][3] = c->cache[h][2];
      c->cache[h][2] = c->cache[h][1];
      c->cache[h][1] = c->cache[h][0];
      c->cache[h][0] = prefill[i];
    }
}


void compress_word( struct Compressor *c, UINT_32 word )
{
  UINT_32 w = word;

  if (verbose)
    printf( "- %08lx  ", w );

  if (w == 0)
    {
      if (verbose)
	printf( "zero\n" );
      *c->cntl_ptr++ = SYM_ZERO;
    }
  else if (w == 2)
    {
      if (verbose)
	printf( "false\n" );
      *c->cntl_ptr++ = SYM_FALSE;
    }
  else if (w == 6)
    {
      if (verbose)
	printf( "nil\n" );
      *c->cntl_ptr++ = SYM_NIL;
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
	      if (verbose)
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
	  if (verbose)
	    printf( "victim-0\n" );
	  *c->cntl_ptr++ = SYM_VICTIM_0;

	  VICTIM_0_SHIFT(c,w);
	  return;
	}
      else if (w == c->victim_1)
	{
	  if (verbose)
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
	      if (verbose)
		printf( "near-%d-%d %02x\n", h, k, diff );
	      *c->cntl_ptr++ = SYM_NEAR + h*4 + k;
	      *c->short_ptr++ = diff;
	      goto sh_done;
	    }
	}
      if (near == (c->victim_0 & SIM_MASK))
	{
	  if (verbose)
	    printf( "near-victim-0 %02x\n", diff );
	  *c->cntl_ptr++ = SYM_NEAR_VICTIM_0;
	  *c->short_ptr++ = diff;
	}
      else if (near == (c->victim_1 & SIM_MASK))
	{
	  if (verbose)
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
	  if (verbose)
	    printf( "new-8 %02x\n", (UINT_8)(w & 0xFF) );
	  *c->cntl_ptr++ = SYM_NEW_8;
	  *c->short_ptr++ = w & 0xFF;
	}
      else if (valid_4x2( w ))
	{
	  UINT_8 n = encode_4x2( w );
	  if (verbose)
	    printf( "new-4x2 %02x\n", n );
	  *c->cntl_ptr++ = SYM_NEW_4X2;
	  *c->short_ptr++ = n;
	}
      else if (valid_16( w ))
	{
	  if (verbose)
	    printf( "new-16 %04x\n", (UINT_16)(w & 0xFFFF) );
	  *c->cntl_ptr++ = SYM_NEW_16;
	  *c->short_ptr++ = (w >> 8) & 0xFF;
	  *c->short_ptr++ = w & 0xFF;
	}
      else
	{
	  if (verbose)
	    printf( "new-32 %08lx\n", w );
	  *c->cntl_ptr++ = SYM_NEW_32;
	  *c->long_ptr++ = w;
	}

    sh_done:
      VICTIM_1_SHIFT(c,w);
    }
}

void compress_obj( struct Compressor *c, obj item )
{
  compress_word( c, VAL(item) );
}

void close_compressor( struct Compressor *c )
{
}

UINT_32 write_compressed( struct Compressor *c, LSS *lss, UINT_32 rec )
{
  LSSAccess a;
  UINT_16 header[3];
  int n1, n2, n3, rc;
  z_stream strm;
  timebase_t t0, t1, t2, t3, t4, t5, t6;

  /* allocate space in the output buffer for the compressed result */

  lss_write_access( lss, rec, &a, 20000 );

  strm.next_out = a.addr;
  strm.avail_out = 20000;

  /* figure out the sizes of the three sections */
  
  n1 = c->cntl_ptr - c->cntl_b;
  n2 = c->short_ptr - c->short_b;
  n3 = c->long_ptr - c->long_b;
  if (verbose)
    printf( "%u used: %d + %d + 4 * %d", 
	    n1 + n2 + n3 * sizeof(UINT_32), n1, n2, n3 );

  /* compress and write out the 3-short header */

  strm.zalloc = Z_NULL;
  strm.zfree = Z_NULL;
  strm.opaque = Z_NULL;

  t0 = read_timebase();

  rc = deflateInit( &strm, ZSTORE_COMPRESSION );
  assert( rc == Z_OK );

  header[0] = n1;
  header[1] = n2;
  header[2] = n3;

  strm.next_in = (void *)header;
  strm.avail_in = sizeof header;
  rc = deflate( &strm, 0 );
  assert( rc == Z_OK );

  t1 = read_timebase();
  
  /* compress and write out the control data */

  if (n1 > 0)
    {
      strm.next_in = c->cntl_b;
      strm.avail_in = n1;
      rc = deflate( &strm, 0 );
      assert( rc == Z_OK );
    }

  t2 = read_timebase();

  /* compress and write out the short data */

  if (n2 > 0)
    {
      strm.next_in = c->short_b;
      strm.avail_in = n2;
      rc = deflate( &strm, 0 );
      assert( rc == Z_OK );
    }

  t3 = read_timebase();

  /* compress and write out the long data, 
     finishing with the compression stream */

  if (n3 > 0)
    {
      strm.next_in = (void *)c->long_b;
      strm.avail_in = n3 * sizeof(UINT_32);
      rc = deflate( &strm, 0 );
      assert( rc == Z_OK );
    }

  t4 = read_timebase();

  strm.next_in = Z_NULL;
  strm.avail_in = 0;
  rc = deflate( &strm, Z_FINISH );
  assert( rc == Z_STREAM_END );

  t5 = read_timebase();

  /* deallocate the compression info and release the write buffer */

  rc = deflateEnd( &strm );
  assert( rc == Z_OK );

  t6 = read_timebase();

  {
    long d1, d2, d3;
    d1 = timebase_diff_usec( t1, t0 );
    d2 = timebase_diff_usec( t4, t1 );
    d3 = timebase_diff_usec( t6, t4 );

    printf( " compress ; %ld + %ld + %ld = %ld us\n", 
	    d1, d2, d3,
	    timebase_diff_usec( t6, t0 ) );
  }

  {
    unsigned n = ((UINT_8 *)strm.next_out) - ((UINT_8 *)a.addr);
    lss_write_release( lss, &a, n );
    return n;
  }
}

/***********************************************************************
 *   ____                                                   
 *  |  _ \  ___  ___ ___  _ __ ___  _ __  _ __ ___  ___ ___ 
 *  | | | |/ _ \/ __/ _ \| '_ ` _ \| '_ \| '__/ _ \/ __/ __|
 *  | |_| |  __/ (_| (_) | | | | | | |_) | | |  __/\__ \__ \
 *  |____/ \___|\___\___/|_| |_| |_| .__/|_|  \___||___/___/
 *                                 |_|                      
 ***********************************************************************/

/* will this work..? */

#define c    (&d->cstate)

void init_decompressor( struct Decompressor *d, LSS *lss, UINT_32 rec )
{
  LSSAccess a;
  z_stream strm;
  int rc;
  UINT_16 header[3];

  init_compressor( &d->cstate );

  lss_read_access( lss, rec, &a );

  strm.zalloc = Z_NULL;
  strm.zfree = Z_NULL;
  strm.opaque = Z_NULL;

  rc = inflateInit( &strm );
  assert( rc == Z_OK );

  strm.next_in = a.addr;
  strm.avail_in = a.bytes;

  /* read and decompress the 2-short header */

  strm.next_out = (void *)header;
  strm.avail_out = sizeof header;
  rc = inflate( &strm, 0 );
  
  /* read and decompress the control data */

  if (header[0] > 0)
    {
      assert( rc == Z_OK );
      strm.next_out = (void *)c->cntl_b;
      strm.avail_out = header[0]; /* n1 */
      rc = inflate( &strm, 0 );
    }

  /* read and decompress the short data */

  if (header[1] > 0)
    {
      assert( rc == Z_OK );
      strm.next_out = (void *)c->short_b;
      strm.avail_out = header[1]; /* n2 */
      rc = inflate( &strm, 0 );
    }

  /* read and decompress the long data */

  if (header[2] > 0)
    {
      assert( rc == Z_OK );
      strm.next_out = (void *)c->long_b;
      strm.avail_out = header[2] * sizeof( UINT_32 ); /* n3 * (b/w) */
      rc = inflate( &strm, 0 );
    }

  assert( rc == Z_STREAM_END );
  inflateEnd( &strm );

  lss_read_release( lss, &a );
}


#define CACHE_HIT_CASE(h,k)                                          \
    case SYM_HIT + h*ASSOCIATIVITY + k:                              \
      w = d->cstate.cache[h][k];                                     \
      /* this looks funny, but its designed to get optimized... */   \
      if (k == 3) d->cstate.cache[h][3] = d->cstate.cache[h][2];     \
      if (k >= 2) d->cstate.cache[h][2] = d->cstate.cache[h][1];     \
      if (k >= 1) d->cstate.cache[h][1] = d->cstate.cache[h][0];     \
      if (k != 0) d->cstate.cache[h][0] = w;                         \
      if (verbose) printf( "- %08lx  hit-%d-%d\n", w, h, k );        \
      return w;

#define CACHE_NEAR_CASE(h,k)                                         \
    case SYM_NEAR + h*ASSOCIATIVITY + k:                             \
      {                                                              \
        UINT_8 b = *d->cstate.short_ptr++;                           \
        w = (d->cstate.cache[h][k] & SIM_MASK) + (b << SIM_SHIFT);   \
        /* this looks funny, but its designed to get optimized... */ \
        if (k == 3) d->cstate.cache[h][3] = d->cstate.cache[h][2];   \
        if (k >= 2) d->cstate.cache[h][2] = d->cstate.cache[h][1];   \
        if (k >= 1) d->cstate.cache[h][1] = d->cstate.cache[h][0];   \
        if (k != 0) d->cstate.cache[h][0] = w;                       \
        if (verbose) printf( "- %08lx  near-%d-%d %02x\n", w, h, k, b ); \
        return w;                                                    \
      }

UINT_32 decompress_word( struct Decompressor *d )
{
  UINT_8 b = *c->cntl_ptr++;
  UINT_32 w;
  unsigned h;

  switch (b)
    {
    case SYM_ZERO:
      w = 0;
      if (verbose)
	printf( "- %08lx  zero\n", w );
      return w;

    case SYM_NIL:
      w = 6;
      if (verbose)
	printf( "- %08lx  nil\n", w );
      return w;

    case SYM_FALSE:
      w = 2;
      if (verbose)
	printf( "- %08lx  false\n", w );
      return w;

    case SYM_VICTIM_0:
      w = d->cstate.victim_0;
      h = hash( w );
      if (verbose)
	printf( "- %08lx  victim-0\n", w );
      VICTIM_0_SHIFT( &d->cstate, w );
      return w;

    case SYM_VICTIM_1:
      w = d->cstate.victim_1;
      if (verbose)
	printf( "- %08lx  victim-1\n", w );
    sh_done:
      h = hash( w );
      VICTIM_1_SHIFT( &d->cstate, w );
      return w;

    case SYM_NEAR_VICTIM_0:
      {
	UINT_8 b = *d->cstate.short_ptr++;
        w = (d->cstate.victim_0 & SIM_MASK) + (b << SIM_SHIFT);
	h = hash( w );
	VICTIM_0_SHIFT( &d->cstate, w );
	if (verbose)
	  printf( "- %08lx  near-victim-0 %02x\n", w, b );
	return w;
      }

    case SYM_NEAR_VICTIM_1:
      {
	UINT_8 b = *d->cstate.short_ptr++;
        w = (d->cstate.victim_1 & SIM_MASK) + (b << SIM_SHIFT);
	if (verbose)
	  printf( "- %08lx  near-victim-1 %02x\n", w, b );
	goto sh_done;
      }

    case SYM_NEW_8:
      {
	signed char t = *d->cstate.short_ptr++;
	w = (int)t;
	if (verbose)
	  printf( "- %08lx  new-8 %02x\n", w, d->cstate.short_ptr[-1] );
	goto sh_done;
      }

    case SYM_NEW_4X2:
      {
	UINT_8 b = *d->cstate.short_ptr++;
	w = decode_4x2( b );
	if (verbose)
	  printf( "- %08lx  new-4x2 %02x\n", w, b );
	goto sh_done;
      }

    case SYM_NEW_16:
      {
	UINT_8 a = *d->cstate.short_ptr++;
	UINT_8 b = *d->cstate.short_ptr++;
	w = (a << 8) + b;
	if (verbose)
	  printf( "- %08lx  new-16 %04lx\n", w, w );
	goto sh_done;
      }

    case SYM_NEW_32:
      {
	w = *d->cstate.long_ptr++;
	if (verbose)
	  printf( "- %08lx  new-32 %08lx\n", w, w );
	goto sh_done;
      }

    default:
      assert( 0 );
      return 0;

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

obj decompress_obj( struct Decompressor *d )
{
  return OBJ(decompress_word(d));
}

void close_decompressor( struct Decompressor *d )
{
}
