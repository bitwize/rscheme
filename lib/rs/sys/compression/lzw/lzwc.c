#include "lzwc.h"
#include <string.h>
#include <assert.h>
#include <stdlib.h>
#include <stdio.h>
#include <rscheme.h>

#define DEBUG_TRACE (0)

void SOP_write( obj port, const char *src, UINT_32 len );

static void lzw_reset( LZWStream *s );

typedef struct _LZWHashEntry {
  UINT_32               key;
  UINT_16               next;
  UINT_16               value;
} LZWHashEntry;

#define LZW_DICT_SIZE           (8191)
#define LZW_DICT_OVERFLOW       (4096)
#define KEY_EMPTY               (0)
#define WINDOW_SIZE             (0)   /* how far to measure compression */

#define OUTBUF_SIZE     (1024)


struct _LZWStream {
  obj                 port;
  unsigned long       accum;         /* need at least 16+12 bits */
  unsigned char       bits;
  unsigned char      *next_out;
  unsigned            avail_out;
  unsigned char      *next_in;
  unsigned            avail_in;
  unsigned char       enc_width;
  unsigned short      enc_max;
  UINT_16             over_last;        /* last index used in overflow[] */
  UINT_16             next_token;
#if WINDOW_SIZE
  UINT_32             zwindow_sum;
  UINT_16             zwindow_ptr;
  UINT_8              zwindow[WINDOW_SIZE];
#endif
  LZWHashEntry        primary[LZW_DICT_SIZE];
  LZWHashEntry        overflow[LZW_DICT_OVERFLOW];
  unsigned char       outbuf[OUTBUF_SIZE];
};

static inline unsigned char lzw_read( LZWStream *strm )
{
#if WINDOW_SIZE
  strm->zwindow_ptr = (strm->zwindow_ptr + 1) % WINDOW_SIZE;
  strm->zwindow_sum -= strm->zwindow[ strm->zwindow_ptr ];
  strm->zwindow[ strm->zwindow_ptr ] = 0;
#endif

  strm->avail_in--;
  return *(strm->next_in)++;
}

static void lzw_oflush( LZWStream *strm )
{  
#if DEBUG_TRACE
  printf( "lzw_oflush( %d )\n", OUTBUF_SIZE - strm->avail_out );
#endif

  SOP_write( strm->port, &strm->outbuf[0], OUTBUF_SIZE - strm->avail_out );
  strm->next_out = &strm->outbuf[0];
  strm->avail_out = OUTBUF_SIZE;
}


static void lzw_writew( LZWStream *strm, unsigned code, unsigned char width )
{
#if WINDOW_SIZE
  strm->zwindow[ strm->zwindow_ptr ] += width;
  strm->zwindow_sum += width;
#endif

#if DEBUG_TRACE
  printf( "%6d %2d <", strm->next_token, width );
  unsigned mask = 1 << (width - 1);
  unsigned i, c = code;

  for (i=0; i<width; i++) {
    putchar( (c & mask) ? '1' : '0' );
    c <<= 1;
  }
#if WINDOW_SIZE
  printf( "> window=%u", strm->zwindow_sum );
#else
  printf( ">" );
#endif

#endif

  switch (width)
  {
    case 9:
      strm->accum = (strm->accum << 9) + code;
      strm->bits += 9;
      break;
    case 10:
      strm->accum = (strm->accum << 10) + code;
      strm->bits += 10;
      break;
    case 11:
      strm->accum = (strm->accum << 11) + code;
      strm->bits += 11;
      break;
    case 12:
      strm->accum = (strm->accum << 12) + code;
      strm->bits += 12;
      break;
  }

#if DEBUG_TRACE
  printf( "  %08lx [%d]\n", strm->accum, strm->bits );
#endif

  /* nb, guaranteed to have at least one output byte */

  while (strm->bits >= 8) {
    /* XXX note that we leave garbage in the top bits of the accumulator */
    *strm->next_out++ = strm->accum >> (strm->bits - 8);
    strm->avail_out--;

    strm->bits -= 8;
  }
  if (strm->avail_out < 8) {
    lzw_oflush( strm );
  }
}


static void lzw_write( LZWStream *strm, unsigned code )
{
  lzw_writew( strm, code, strm->enc_width );
}

static inline unsigned lzw_hash( unsigned key )
{
  return (key >> 16) ^ (key >> 8) ^ key;
}

static int lzw_lookup( LZWStream *strm, unsigned key, unsigned h )
{
  LZWHashEntry *p;

#if DEBUG_TRACE
  printf( "lzw_lookup( 0x%lx ) = ", key );
#endif
  p = &strm->primary[ h % LZW_DICT_SIZE ];
  while (1) {
    if (p->key == key) {
#if DEBUG_TRACE
      printf( "found %d\n", p->value );
#endif
      return p->value;
    }
    if (p->next == 0) {
#if DEBUG_TRACE
      printf( "not found\n" );
#endif
      return -1;
    }
    p = &strm->overflow[ p->next ];
  }
}

static UINT_16 overalloc( LZWStream *strm )
{
  return ++(strm->over_last);
}

static void lzw_insert( LZWStream *strm, unsigned key, unsigned h )
{
  LZWHashEntry *p;

  unsigned v = strm->next_token++;
  assert( v < 4096 );

#if DEBUG_TRACE
  printf( "lzw_insert( 0x%x, %u ) H=0x%x\n", key, v, h );
#endif

  p = &strm->primary[ h % LZW_DICT_SIZE ];

  if (p->key != KEY_EMPTY) {
    UINT_16 k = overalloc( strm );

    strm->overflow[ k ].next = p->next;

    p = &strm->overflow[ k ];
  }
  p->key = key;
  p->value = v;

  if ((v+1) >= strm->enc_max) {
    strm->enc_max *= 2;
    strm->enc_width++;
  }
}

void lzw_deflate( LZWStream *strm )
{
  if (strm->avail_in == 0) {
    return;
  }

  unsigned enc = lzw_read( strm );      /* (table-lookup dict current) */
  unsigned cur = 0xFFFFFF00 + enc;      /* current */

#if DEBUG_TRACE
    printf( "ch = 0x%x\n", enc );
#endif

  while (strm->avail_in) {
#if DEBUG_TRACE
    printf( "enc = 0x%x, cur = 0x%x\n", enc, cur );
#endif

    unsigned char ch = lzw_read( strm );

#if DEBUG_TRACE
    printf( "ch = 0x%x\n", ch );
#endif

    unsigned key = ((enc << 8) + ch) | 0x80000000;
    unsigned h = lzw_hash( key );
    int val = lzw_lookup( strm, key, h );
    
    if (val >= 0) {
      cur = key;
      enc = val;
    } else {
      lzw_write( strm, enc );
      lzw_insert( strm, key, h );

      if (strm->next_token >= 4096) {
        lzw_write( strm, 256 );
        lzw_reset( strm );
      }
      enc = ch;
      cur = 0xFFFFFF00 + ch;
    }
  }
  lzw_write( strm, enc );

}

static void lzw_reset( LZWStream *s )
{
  unsigned i;

  s->enc_width = 9;
  s->enc_max = 512;

#if WINDOW_SIZE
  memset( &s->zwindow[0], 0, WINDOW_SIZE );
  s->zwindow_ptr = 0;
  s->zwindow_sum = 0;
#endif

  s->over_last = 0;
  memset( &s->primary[0], 0, sizeof( s->primary ) );

  /* initialize the table */
  
  s->next_token = 0;
  for (i=0; i<256; i++) {
    unsigned key = 0xFFFFFF00 + i;
    lzw_insert( s, key, lzw_hash( key ) );
  }
  
  s->next_token = 258;  /* 256=CLEAR and 257=EOD */
}

LZWStream *lzw_create( obj port )
{
  LZWStream *s = malloc( sizeof( LZWStream ) );

  s->port = port;
  s->next_in = NULL;
  s->avail_in = 0;
  s->next_out = NULL;
  s->avail_out = 0;

  lzw_reset( s );
  return s;
}

void lzw_run( LZWStream *s, unsigned char *ptr, unsigned len )
{
  s->next_in = ptr;
  s->avail_in = len;

  s->next_out = &s->outbuf[0];
  s->avail_out = OUTBUF_SIZE;

  lzw_write( s, 256 );
  lzw_deflate( s );
  lzw_write( s, 257 );

  /* flush the remaining bits */
  if (s->bits > 0) {
    assert( s->bits < 8 );
    lzw_writew( s, 0, 8 - s->bits );
  }

  lzw_oflush( s );
#if DEBUG_TRACE
  printf( "\n" );
#endif
}


#if 0
unsigned char test[] = { 45,  45, 45,  45, 45,  65,  45, 45, 45 };

int main( int argc, const char **argv )
{
  LZWStream *s = malloc( sizeof( LZWStream ) );
  unsigned char buf[100];
  FILE *sf;

  s->next_in = &test[0];
  s->avail_in = sizeof( test );
  s->next_out = &buf[0];
  s->avail_out = sizeof(buf);

  s->next_token = 258;  /* 256=CLEAR and 257=EOD */
  s->enc_width = 9;

  lzw_write( s, 256 );
  lzw_deflate( s );
  lzw_write( s, 257 );

  printf( "\n" );
  return 0;
}
#endif
