#include <string.h>
#include "packlib.h"
#include <rscheme/longint.h>
#include <rscheme/smemory.h>
#include <netinet/in.h>

static void value_error( obj value, char *type )
{
  scheme_error( "pack: ~s not a ~a",
		2,
		value,
		make_string( type ) );
}

#ifdef HAVE_INT_64
typedef unsigned long long UINT_64;
#else
typedef struct _uint64 {
#ifdef PLATFORM_IS_LITTLE_ENDIAN
   UINT_32 low;
   UINT_32 high;
#else
   UINT_32 high;
   UINT_32 low;
#endif
} UINT_64;
#endif


#define BVEC_SLOT(b,x,t) (*(t *)(((char *)PTR_TO_DATAPTR( b )) + (x)))

static void flip_bytes( void *ptr, unsigned len )
{
  UINT_8 *p, temp[8];
  unsigned i;

  p = &temp[len];
  for (i=0; i<len; i++)
    *--p = ((UINT_8 *)ptr)[i];
  memcpy( ptr, temp, len );
}

/*
 *  Byte-swapping utilities
 */

static inline UINT_16 swab_u16( UINT_16 a )
{
  return ((a >> 8) & 0x00FF) + ((a << 8) & 0xFF00);
}

static inline INT_16 swab_s16( INT_16 a )
{
  return ((a >> 8) & 0x00FF) + ((a << 8) & 0xFF00);
}

static inline UINT_32 swab_u32( UINT_32 a )
{
  return ((a >> 24) & 0xFF) 
    + ((a >> 8) & 0xFF00)
    + ((a << 8) & 0xFF0000)
    + ((a & 0xFF) << 24);
}

static inline INT_32 swab_s32( INT_32 a )
{
  return swab_u32( a );
}

static inline UINT_64 swab_u64( UINT_64 a )
{
  flip_bytes( &a, sizeof(a) );
  return a;
}

static inline INT_64 swab_s64( INT_64 a )
{
#ifdef HAVE_INT_64
  flip_bytes( &a, sizeof(a) );
  return a;
#else
  INT_64 t;
  t.digits[0] = swab_u16( a.digits[3] );
  t.digits[1] = swab_u16( a.digits[2] );
  t.digits[2] = swab_u16( a.digits[1] );
  t.digits[3] = swab_u16( a.digits[0] );
  return t;
#endif
}

static IEEE_32 swab_f32( IEEE_32 x )
{
  flip_bytes( &x, sizeof( IEEE_32 ) );
  return x;
}

static IEEE_64 swab_f64( IEEE_64 x )
{
  flip_bytes( &x, sizeof( IEEE_64 ) );
  return x;
}


#ifdef PLATFORM_IS_BIG_ENDIAN
#include "bo_b.ci"
#else
#ifdef PLATFORM_IS_LITTLE_ENDIAN
#include "bo_l.ci"
/* should we support systems that are neither big nor little endian?
   are there any?  (is the VAX twobyte-swapped?)
*/
#endif
#endif

/************************************************************************/
/*			     8-bit values                               */
/* Note:
 *  these are byte-order independent
 */
/************************************************************************/

void bvec_packn_u8( obj bvec, UINT_32 offset, obj value )
{
  INT_32 v = basic_raw_int( value );
  if ((v < 0) || (v > 255)) value_error( value, "u8" );
  BVEC_SLOT( bvec, offset, UINT_8 ) = v;
}

void bvec_packn_s8( obj bvec, UINT_32 offset, obj value )
{
  INT_32 v = basic_raw_int( value );
  if ((v < -128) || (v > 127)) value_error( value, "s8" );
  BVEC_SLOT( bvec, offset, INT_8 ) = v;
}

obj bvec_unpackn_u8( obj bvec, UINT_32 offset )
{
  return int2fx( BVEC_SLOT( bvec, offset, UINT_8 ) );
}

obj bvec_unpackn_s8( obj bvec, UINT_32 offset )
{
  return int2fx( BVEC_SLOT( bvec, offset, INT_8 ) );
}

/************************************************************************/
/*			     16-bit values                              */
/************************************************************************/

static _rs_inline UINT_16 basic_to_u16( obj value )
{
  INT_32 v = basic_raw_int( value );
  if ((v < 0) || (v > 65535)) value_error( value, "u16" );
  return v;
}

static _rs_inline INT_16 basic_to_s16( obj value )
{
  INT_32 v = basic_raw_int( value );
  if ((v < -32768) || (v > 32767)) value_error( value, "s16" );
  return v;
}

static _rs_inline obj u16_to_basic( UINT_16 value )
{
  return int2fx( value );
}

static _rs_inline obj s16_to_basic( INT_16 value )
{
  return int2fx( value );
}

/************************************************************************/
/*			     32-bit values                              */
/************************************************************************/

static UINT_32 basic_to_u32( obj x )
{
  /* fix this! */
  return basic_raw_int( x );
}

#define basic_to_s32(x) basic_raw_int(x)

static obj u32_to_basic_box( UINT_32 x )
{
  UINT_16 low, high;

  low = (x >> 16) & 0xFFFF;
  high = x & 0xFFFF;

  /* owww... */
  return basic_plus( basic_lshr( int2fx( high ), 16 ),
		     int2fx( low ));
}

static _rs_inline obj u32_to_basic( UINT_32 x )
{
  return uint_32_compact( x );
}

static obj s32_to_basic_box( INT_32 x )
{
  /* not insanely efficient... */
  if (x < 0)
    return basic_minus( ZERO, uint_32_compact( -x ) );
  else
    return uint_32_compact( x );
}

static _rs_inline obj s32_to_basic( INT_32 x )
{
  if ((x >= -536870912) && (x <= 536870911))
    return int2fx( x );
  else
    return s32_to_basic_box( x );
}

/************************************************************************/
/*			     64-bit values                              */
/************************************************************************/

static INT_64 basic_to_s64( obj x )
{
  if (FIXNUM_P(x))
    return fx2int64( x );
  else if (LONG_INT_P(x))
    return extract_int_64( x );
  else
    {
      scheme_error( "basic_int64: ~s not basic", 1, x );
      return int_32_to_int_64(0);
    }
}

static UINT_64 basic_to_u64( obj x )
{
  /* fix this! */
  INT_64 t = basic_to_s64( x );
#ifdef HAVE_INT_64
  return *(UINT_64 *)&t;
#else
  UINT_64 a;
  a.high = (t.digits[0] << 16) + t.digits[1];
  a.low = (t.digits[2] << 16) + t.digits[3];
  return a;
#endif
}

static obj u64_to_basic_box( UINT_64 x )
{
  UINT_16 digits[4];
  obj sum;
  int i;

  digits[0] = (x.high >> 16) & 0xFFFF;
  digits[1] = x.high & 0xFFFF;

  digits[2] = (x.low >> 16) & 0xFFFF;
  digits[3] = x.low & 0xFFFF;

  sum = ZERO;
  /* double owww... */
  for (i=0; i<4; i++)
    sum = basic_plus( basic_lshr( sum, 16 ), int2fx( digits[i] ) );
  return sum;
}

static _rs_inline obj u64_to_basic( UINT_64 x )
{
#ifdef HAVE_INT_64
  if (x <= 536870911)
    return int2fx( x );
#else
  if ((x.high == 0) && (x.low <= 536870911))
    return int2fx( x.low );
#endif
  else
    return u64_to_basic_box( x );
}

static _rs_inline obj s64_to_basic( INT_64 x )
{
  return int_64_compact( x );
}


/************************************************************************/
/*			     32-bit floats                              */
/************************************************************************/

static IEEE_32 basic_to_f32( obj x )
{
  return (IEEE_32)basic_raw_float( x );
}

static obj f32_to_basic( IEEE_32 x )
{
  return make_float( (IEEE_64)x );
}


/************************************************************************/
/*			     64-bit floats                              */
/************************************************************************/

static IEEE_64 basic_to_f64( obj x )
{
  return basic_raw_float( x );
}

static obj f64_to_basic( IEEE_64 x )
{
  return make_float( x );
}

#include "packers.ci"
