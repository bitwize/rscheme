#include <rscheme.h>
#include <string.h>
#include <gmp.h>
#include "ber.h"

/*void SOP_write( obj port, const char *src, UINT_32 len );*/


static obj memstr( const void *data, unsigned len )
{
  obj s = bvec_alloc( len+1, string_class );
  memcpy( PTR_TO_DATAPTR( s ), data, len );
  return s;
}

static unsigned emit_header( unsigned char *p, 
                             unsigned char tag, unsigned len )
{
  p[0] = tag;

  if (len < 128) {
    p[1] = len;
    return 2;
  } else if (len < 0x100) {
    p[1] = 0x81;
    p[2] = len;
    return 3;
  } else if (len < 0x10000) {
    p[1] = 0x82;
    p[2] = len >> 8;
    p[3] = len & 0xFF;
    return 4;
  } else if (len < 0x1000000) {
    p[1] = 0x83;
    p[2] = len >> 16;
    p[3] = (len >> 8) & 0xFF;
    p[4] = len & 0xFF;
    return 5;
  } else {
    p[1] = 0x83;
    p[2] = len >> 24;
    p[3] = (len >> 16) & 0xFF;
    p[4] = (len >> 8) & 0xFF;
    p[5] = len & 0xFF;
    return 6;
  }
}

/*
 *  Similar to emit_header(), but write it into the buffer *BEFORE* 'p'
 *
 *      +----+----+----+----+----+----+----+----+----+----+----+
 *      |    |    |    |    |    | 06 | 03 | aa | bb | cc |    |
 *      +----+----+----+----+----+----+----+----+----+----+----+
 *                               <---hdr-->^
 *                                         p
 */

static unsigned emit_header_rev( unsigned char *p, 
                                 unsigned char tag, unsigned len )
{
  if (len < 128) {
    p[-2] = tag;
    p[-1] = len;
    return 2;
  } else if (len < 0x100) {
    p[-3] = tag;
    p[-2] = 0x81;
    p[-1] = len;
    return 3;
  } else if (len < 0x10000) {
    p[-4] = tag;
    p[-3] = 0x82;
    p[-2] = len >> 8;
    p[-1] = len & 0xFF;
    return 4;
  } else if (len < 0x1000000) {
    p[-5] = tag;
    p[-4] = 0x83;
    p[-3] = len >> 16;
    p[-2] = (len >> 8) & 0xFF;
    p[-1] = len & 0xFF;
    return 5;
  } else {
    p[-6] = tag;
    p[-5] = 0x83;
    p[-4] = len >> 24;
    p[-3] = (len >> 16) & 0xFF;
    p[-2] = (len >> 8) & 0xFF;
    p[-1] = len & 0xFF;
    return 6;
  }
}

static UINT_32 extract_uint_32_big( obj x )
{
  if (OBJ_ISA_PTR_OF_CLASS( x, bignum_class )) {
    mpz_t zn;

    OBJ_TO_MPZ( zn, x );
    if (!mpz_fits_ulong_p( zn )) {
      scheme_error( "integer out of range: ~s", 1, x );
    }
    return mpz_get_ui( zn );
  } else {
    scheme_error( "integer expected: ~s", 1, x );
    return 0;
  }
}

static inline UINT_32 extract_uint_32( obj x )
{
  if (FIXNUM_P( x )) {
    if (FX_LT( x, ZERO )) {
      scheme_error( "integer out of range: ~s", 1, x );
    }
    return fx2int( x );
  } else {
    return extract_uint_32_big( x );
  }
}

static volatile void subid_oor( unsigned i, obj val )
{
  scheme_error( "oid subid[~d] out of range: ~s", 2, int2fx(i), val );
}

unsigned ber_encode_oid( obj port, obj vec, obj *phash )
{
  unsigned major, minor;
  unsigned char *p0, *p, *buf;
  unsigned i;
  UINT_32 hash = 0x1234567;

  /* an upper bound...
   *   (1) the header can be at most 6 octets
   *   (2) each subid after the first 2 can be at most 5 octets
   *   (3) the first two subids take 1 octet
   */

  buf = alloca( 6 + 5 * (SIZEOF_PTR(vec)/SLOT(1)) );

  p = p0 = &buf[4];
  
  major = extract_uint_32( gvec_ref( vec, SLOT(0) ) );
  if (major >= 6) {
    subid_oor( 0, gvec_ref( vec, SLOT(0) ) );
  }
  hash = OID_HASH_MIX( hash, major );

  minor = extract_uint_32( gvec_ref( vec, SLOT(1) ) );
  if (minor >= 40) {
    subid_oor( 1, gvec_ref( vec, SLOT(1) ) );
  }
  hash = OID_HASH_MIX( hash, minor );
  *p++ = (major * 40) + minor;

  for (i=SLOT(2); i<SIZEOF_PTR(vec); i+=SLOT(1)) {
    unsigned subid = extract_uint_32( gvec_ref( vec, i ) );

    hash = OID_HASH_MIX( hash, subid );
    
    if (subid < (1<<(1*7))) {
      *p++ = subid;
    } else if (subid < (1<<(2*7))) {
      *p++ = 0x80 | ((subid >>  7) & 0x7F);
      *p++ =         subid & 0x7F;
    } else if (subid < (1<<(3*7))) {
      *p++ = 0x80 | ((subid >> 14) & 0x7F);
      *p++ = 0x80 | ((subid >>  7) & 0x7F);
      *p++ =         subid & 0x7F;
    } else if (subid < (1<<(4*7))) {
      *p++ = 0x80 | ((subid >> 21) & 0x7F);
      *p++ = 0x80 | ((subid >> 14) & 0x7F);
      *p++ = 0x80 | ((subid >>  7) & 0x7F);
      *p++ =         subid & 0x7F;
    } else {
      *p++ = 0x80 | ((subid >> 28) & 0x7F);
      *p++ = 0x80 | ((subid >> 21) & 0x7F);
      *p++ = 0x80 | ((subid >> 14) & 0x7F);
      *p++ = 0x80 | ((subid >>  7) & 0x7F);
      *p++ =         subid & 0x7F;
    }
  }
  
  p0 -= emit_header_rev( p0, 0x06, p - p0 );
  dequeue_push_back( port, memstr( p0, p - p0 ) );
  if (phash) {
    *phash = OID_HASH_TO_OBJ(hash);
  }
  return p - p0;
}

unsigned ber_encode_fx( obj port, obj data )
{
  unsigned char *p, buf[32];
  unsigned len;
  int x = fx2int( data );

  p = &buf[0];

  if ((x >= -128) && (x <= 127)) {
    p += emit_header( p, 0x02, 1 );
    *p++ = (x & 0xFF);
  } else if ((x >= -32768) && (x <= 32767)) {
    p += emit_header( p, 0x02, 2 );
    *p++ = ((x >> 8) & 0xFF);
    *p++ = (x & 0xFF);
  } else if ((x >= -8388608) && (x <= 8388607)) {
    p += emit_header( p, 0x02, 3 );
    *p++ = ((x >> 16) & 0xFF);
    *p++ = ((x >> 8) & 0xFF);
    *p++ = (x & 0xFF);
  } else {
    p += emit_header( p, 0x02, 4 );
    *p++ = ((x >> 24) & 0xFF);
    *p++ = ((x >> 16) & 0xFF);
    *p++ = ((x >> 8) & 0xFF);
    *p++ = (x & 0xFF);
  }

  dequeue_push_back( port, memstr( &buf[0], p - &buf[0] ) );
                     /*SOP_write( port, &buf[0], p - &buf[0] );*/
  return p - &buf[0];
}

unsigned ber_encode_header( obj port, int tag, unsigned len )
{
  unsigned char *p, buf[32];

  p = &buf[0];
  p += emit_header( p, tag, len );
  dequeue_push_front( port, memstr( &buf[0], p - &buf[0] ) );
  return p - &buf[0];
}

unsigned ber_encode_str( obj port, obj str )
{
  unsigned char *p, buf[32];
  unsigned n = string_length( str );

  p = &buf[0];
  p += emit_header( p, 0x04, n );

  dequeue_push_back( port, memstr( &buf[0], p - &buf[0] ) );
  dequeue_push_back( port, str );
  return (p - &buf[0]) + n;
}
