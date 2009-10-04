/*-----------------------------------------------------------------*-C-*---
 * File:    handc/runtime/basemath.c
 *
 *          Copyright (C)1997 Donovan Kolbly <d.kolbly@rscheme.org>
 *          as part of the RScheme project, licensed for free use.
 *          See <http://www.rscheme.org/> for the latest information.
 *
 * File version:     1.23
 * File mod date:    2005-09-16 10:34:55
 * System build:     v0.7.3.4-b7u, 2007-05-30
 *
 * Purpose:          Arithmetic for basic (built-in) number system
 *------------------------------------------------------------------------*/

#include <rscheme/scheme.h>
#include <rscheme/smemory.h>
#include <rscheme/longint.h>
#include <stdlib.h>
#include <string.h>

#define WORD_BITS (sizeof(UINT_32)*8)

static _rs_inline obj int_quotient(INT_32 a, INT_32 b )
{
  return int2fx(a/b);
}
static _rs_inline obj int_remainder(INT_32 a, INT_32 b )
{
  return int2fx(REMDR(a,b));
}

static _rs_inline obj int_modulo(INT_32 a, INT_32 b )
{
  return int2fx(MOD(a,b));
}


#define longint_to_float(l) int_64_to_float(extract_int_64(l))
IEEE_64 longint_to_raw_float(obj a)
{
  return longint_to_float(a);
}

#if !FULL_NUMERIC_TOWER

void init_math( void )
{
}

void init_math2( void )
{
  INT_32 x = (((INT_32)1)<<(WORD_BITS-1));

  min_machine_int = make_long_int( int_32_to_int_64( x ) );
  max_machine_int = make_long_int( int_32_to_int_64( ~x ) );
}

#define STUBBED_OUT(r)  scheme_error( "basemath.c:~d: function stubbed out", 1, int2fx(__LINE__) ); return r

obj rational_to_string_obj( obj a, unsigned radix )
{
  STUBBED_OUT(FALSE_OBJ);
}

obj bignum_to_string_obj(obj a, unsigned radix)
{
  STUBBED_OUT(FALSE_OBJ);
}

obj string_to_bignum_obj(char *str, unsigned radix)
{
  STUBBED_OUT(FALSE_OBJ);
}

obj string_to_rational_obj( char *str, unsigned radix )
{
  STUBBED_OUT(FALSE_OBJ);
}

obj mp_remainder(obj a, obj b)
{
  STUBBED_OUT(FALSE_OBJ);
}

obj mp_quotient(obj a, obj b)
{
  STUBBED_OUT(FALSE_OBJ);
}

obj mp_modulo(obj a, obj b)
{
  STUBBED_OUT(FALSE_OBJ);
}

obj float_truncate( IEEE_64 longfloat )
{
  if ((longfloat >= -536870912.0) && (longfloat <= 536870911.0)) {
    int t = (int)longfloat;
    return int2fx( t );
  } else if ((longfloat >= -9.22337e+18) && (longfloat <= 9.22337e+18)) {
    return int_64_compact( float_to_int_64( longfloat ) );
  } else {
    scheme_error( "float_truncate(~d): out of exact range", 
                  1, make_float( longfloat ) );
    return FALSE_OBJ;
  }
}


#include "numsimpl.ci"

#else /* FULL_NUMERIC_TOWER */
#include <gmp.h>
#include <limits.h>
#include <string.h>

#define BIGNUM_P(x)    OBJ_ISA_PTR_OF_CLASS(x,bignum_class)
#define RATIONAL_P(x)  OBJ_ISA_PTR_OF_CLASS(x,mp_rational_class)
#define COMPLEX_P(x)   OBJ_ISA_PTR_OF_CLASS(x,rect_complex_class)

static void *mp_alloc( size_t n )
{
  return PTR_TO_DATAPTR(alloc( n, mp_data_class ));
}

static void *mp_realloc( void *old_data, size_t old_size, size_t new_size )
{
  void *new_data = PTR_TO_DATAPTR(alloc( new_size, mp_data_class ));
  memcpy( new_data, old_data, old_size );
  return new_data;
}

static void mp_free( void *ptr, size_t n )
{
  /* do nothing -- data will get GC'd */
}

#define OBJ_TO_MPZ(mp, a) { (mp)[0]._mp_alloc = fx2int(gvec_ref((a), SLOT(0)));\
			    (mp)[0]._mp_size = fx2int(gvec_ref((a), SLOT(1)));\
			    (mp)[0]._mp_d = PTR_TO_DATAPTR(gvec_ref((a), SLOT(2)));}
#define OBJ_TO_MPQ(mpq, a) { obj tmp;\
			     tmp = gvec_ref((a), SLOT(0)); OBJ_TO_MPZ(mpq_numref(mpq), tmp);\
			     tmp = gvec_ref((a), SLOT(1)); OBJ_TO_MPZ(mpq_denref(mpq), tmp);}

static obj mpz_to_bignum( mpz_t n )
{
  return make3( bignum_class, 
		int2fx( n[0]._mp_alloc ),
		int2fx( n[0]._mp_size ),
		DATAPTR_TO_PTR( n[0]._mp_d ) );
}


static _rs_inline obj int_32_to_bignum(INT_32 a)
{
  mpz_t n;
  mpz_init_set_si( n, a );
  return mpz_to_bignum( n );
}

void init_math( void )
{
  mp_set_memory_functions( mp_alloc, mp_realloc, mp_free );
}

void init_math2( void )
{
  INT_32 x = (((INT_32)1)<<(WORD_BITS-1));

  min_machine_int = int_32_to_bignum( x );
  max_machine_int = int_32_to_bignum( ~x );
}

static _rs_inline obj int_32_to_bignum_u(INT_32 a)
{
  mpz_t n;
  mpz_init_set_ui( n, a );
  return mpz_to_bignum( n );
}

static _rs_inline obj fx_to_bignum( obj x )
{
  return int_32_to_bignum(fx2int(x));
}

static _rs_inline obj fx_to_bignum_u( obj x )
{
  return int_32_to_bignum_u(fx2int(x));
}


static obj int64_to_bignum(INT_64 a);
static obj bignum_to_rational( obj b );
static obj bignum_compact(mpz_t a);
static _rs_inline obj bignum_minus( obj a, obj b );
static _rs_inline obj bignum_plus( obj a, obj b );
static _rs_inline obj bignum_mul( obj a, obj b );
static obj bignum_shr( obj a, INT_32 b);
static obj bignum_shl( obj a, INT_32 b );

/*
#define longint_to_rational(l) bignum_to_rational(longint_to_bignum(l))
*/
static obj _rs_inline longint_to_bignum(obj a);
static _rs_inline obj longint_to_rational(obj l){return bignum_to_rational(longint_to_bignum(l));}

static obj _rs_inline longint_to_bignum(obj a)
{
    return int64_to_bignum(*((INT_64 *)PTR_TO_DATAPTR(a)));
}

obj float_to_bignum(obj X)
{
  IEEE_64 x = extract_float(X);
    mpz_t a;
    mpz_init_set_d(a, x);
    return mpz_to_bignum(a);
}

obj raw_float_to_bignum(IEEE_64 x)
{
    mpz_t a;
    mpz_init_set_d(a, x);
    return mpz_to_bignum(a);
}


_rs_inline IEEE_64 bignum_to_raw_float(obj x)
{
  mpz_t a;

  OBJ_TO_MPZ(a, x);
  return mpz_get_d(a);
}

static _rs_inline obj bignum_to_float(obj x)
{
  mpz_t a;

  if( FIXNUM_P(x) ) {
    return make_float( (float) fx2int(x)  );
  } else if(BIGNUM_P(x)) {
    OBJ_TO_MPZ(a, x);
    return make_float(mpz_get_d(a));
  } else if( LONG_INT_P(x) ) {
    return make_float(int_64_to_float( *((INT_64 *)PTR_TO_DATAPTR(x)) ));
  }
  scheme_error("bignum_to_float type not found for ~a", 1, x);
  return FALSE_OBJ;             /* not reached */
}

static obj int64_to_bignum(INT_64 a)
{
  INT_32 p_hi, p_lo;
  mpz_t z1, z2, z3;

#ifndef HAVE_INT_64
  p_hi = a.digits[0] << 16 | a.digits[1];
  p_lo = a.digits[2] << 16 | a.digits[3];
#else
  p_hi = a >> 32;
  p_lo = a & 0xffffffff;
#endif

  mpz_init_set_si(z1, p_hi);
  mpz_init(z2);
  mpz_init(z3);
  mpz_mul_2exp(z2, z1, 32);
  mpz_set_ui(z1, p_lo);
  mpz_ior(z3, z1, z2);
  return mpz_to_bignum(z3);
}

static obj int64_to_bignum_u(INT_64 a)
{
  INT_32 p_hi, p_lo;
  mpz_t z1, z2, z3;

#ifndef HAVE_INT_64
  p_hi = a.digits[0] << 16 | a.digits[1];
  p_lo = a.digits[2] << 16 | a.digits[3];
#else
  p_hi = ((unsigned)a) >> 32;
  p_lo = a & 0xffffffff;
#endif

  mpz_init_set_ui(z1, p_hi);
  mpz_init(z2);
  mpz_init(z3);
  mpz_mul_2exp(z2, z1, 32);
  mpz_set_ui(z1, p_lo);
  mpz_ior(z3, z1, z2);
  return mpz_to_bignum(z3);
}

static obj bignum_to_rational( obj b )
{
  return make2( mp_rational_class, b, fx_to_bignum( int2fx(1) ) );
}

static obj fx_to_rational( obj fx )
{
  return bignum_to_rational( fx_to_bignum(fx) );
}

IEEE_64 rational_to_raw_float(obj r)
{
  mpq_t a;
  OBJ_TO_MPQ(a, r);
  return mpq_get_d(a);
}


#define STUBBED_OUT(r)  scheme_error( "basemath.c:~d: function stubbed out", 1, int2fx(__LINE__) ); return r


/************************ INT_32 OPERATIONS ************************/

#define HIGHBIT (1<<29)

static _rs_inline obj int_plus( INT_32 a, INT_32 b )
{
  INT_32 c = a + b;
  if ((~(a ^ b) & (c ^ a)) & HIGHBIT)
    {
      INT_64 a2 = int_32_to_int_64(a);
      INT_64 b2 = int_32_to_int_64(b);
      INT_64 c2 = int_64_add( a2, b2 );
      return int_64_compact( c2 );
    }
  return int2fx( c );
}

static _rs_inline obj int_minus( INT_32 a, INT_32 b )
{
  INT_32 c = a - b;
  if ((~(a ^ b) & (c ^ a)) & HIGHBIT)
    {
      INT_64 a2 = int_32_to_int_64(a);
      INT_64 b2 = int_32_to_int_64(b);
      return int_64_compact( int_64_sub( a2, b2 ) );
    }
  return int2fx( a-b );
}

static _rs_inline obj int_mul( INT_32 a, INT_32 b )
{
  INT_32 p_hi, p_lo;
  INT_64 a2, b2;

#ifdef smul_ppmm
  smul_ppmm( p_hi, p_lo, a, b );
#else
  union
    {
      int i32[2];
      long long int i64;
    } u;
  u.i64 = (long long int) a * (long long int) b;
# if __BYTE_ORDER == __LITTLE_ENDIAN
  p_hi = u.i32[0];
  p_lo = u.i32[1];
# else
  p_hi = u.i32[1];
  p_lo = u.i32[0];
# endif
#endif
  if (p_hi == 0)
    {
      if (p_lo < HIGHBIT)
	{
	  return int2fx( p_lo );
	}
    }
  else if (p_hi == -1)
    {
      if (p_lo >= -HIGHBIT)
	{
	  return int2fx( p_lo );
	}
    }
  a2 = int_32_to_int_64(a);
  b2 = int_32_to_int_64(b);
  return int_64_compact( int_64_mul( a2, b2 ) );
}

static _rs_inline obj int_div( INT_32 a, INT_32 b )
{
  return make_float( (IEEE_64)a/(IEEE_64)b );
}

static _rs_inline int int_cmp( INT_32 a, INT_32 b )
{
  if (a < b)
    return -1;
  else if (a > b)
    return 1;
  else
    return 0;
}

static _rs_inline int search_one32(long a)
{
  int i;
  unsigned long m = 0x80000000;

  for(i = 31; i >= 0;  i--, m >>= 1)
    if(a & m)
      break;
  return i;
}

static _rs_inline obj int_ashr(INT_32 a, INT_32 b)
{
  return int2fx(a >> b);
}

static _rs_inline obj int_lshr(INT_32 a, INT_32 b)
{
  return int2fx(((unsigned long)a) >> b);
}

static obj int_ashl(INT_32 a, INT_32 b)
{
  int am;

  if(!a)
    return int2fx(0);
  if(b > 62 || (am = (b + search_one32(a))) > 62)
    return bignum_shl(int_32_to_bignum(a), b);
  if(am > 29)
    return int_64_compact(int_64_shl(int_32_to_int_64(a), b));
  return int2fx(a<<b);
}

static obj int_lshl(INT_32 a, INT_32 b)
{
  int am;

  if(!a)
    return int2fx(0);
  if(b > 63 || (am = (b + search_one32(a))) > 63)
    return bignum_shl(int_32_to_bignum_u(a), b);
  if(am > 30)
    return int_64_compact(int_64_shl(int_32_to_int_64(a), b));
  return int2fx(a<<b);
}

/************************ INT_64 OPERATIONS ************************/

#if !HAVE_INT_64
#define HI_TEST_64(a, b, c) (~((a).digits[0] ^ (b).digits[0]) & ((c).digits[0] ^ (a).digits[0]) & 0x8000)
#else
#define HI_TEST_64(a, b, c) (~((a) ^ (b)) & ((c) ^ (a)) & (((quad_t)1)<<63)
#endif
static _rs_inline obj long_plus( INT_64 a, INT_64 b )
{
  INT_64 c = int_64_add(a, b);
  if(HI_TEST_64(a, b, c))
    return bignum_plus(int64_to_bignum(a), int64_to_bignum(b));
  return int_64_compact( c );
}

static _rs_inline obj long_minus( INT_64 a, INT_64 b )
{
    INT_64 c = int_64_sub(a,b);
  if(HI_TEST_64(a, b, c))
    return bignum_minus(int64_to_bignum(a), int64_to_bignum(b));
  return int_64_compact( c );
}

static int search_one_sub(INT_64 a, int signed_p)
{
  if(signed_p && int_64_neg_q(a))
    a = int_64_neg(a);
#if !HAVE_INT_64
  {
      int msk = 0x8000;
      int m,n;
      for(m = 0; m < 4; m++)
	if(a.digits[m])
	  break;
      if(m >= 4)
	return -1;
      for(n=15; n >= 0; n--,msk>>=1)
	if(a.digits[m] & msk)
	  break;
      return 16*(3-m) + n;
  }
#else
  {
    INT_64 msk = 1 << 63;
    int n;
    for(n = 63; n >= 0; n--,msk >>= 1)
      if(a & msk)
	break;
    return n;
  }  
#endif
}
#define search_one(a) (search_one_sub((a), 1))

static _rs_inline obj long_mul( INT_64 a, INT_64 b )
{
  int am, bm;
  if((am = search_one(a))<0 ||
     (bm = search_one(b))<0)
    return ZERO;
  if(am + bm <= 63)
    return int_64_compact( int_64_mul(a,b) );
  return bignum_mul(int64_to_bignum(a), int64_to_bignum(b));
}

#if !FULL_NUMERIC_TOWER
static _rs_inline obj long_div( INT_64 a, INT_64 b )
{
  return make_float( int_64_to_float(a) / int_64_to_float(b) );
}
#else

static _rs_inline obj long_quotient(INT_64 a, INT_64 b )
{
  return int_64_compact(int_64_quotient(a, b));
}

static _rs_inline obj long_remainder(INT_64 a, INT_64 b )
{
  return int_64_compact(int_64_remainder(a, b));
}

static _rs_inline obj long_modulo(INT_64 a, INT_64 b )
{
  return int_64_compact(int_64_modulo(a, b));
}
#endif

static _rs_inline obj long_and(INT_64 a, INT_64 b)
{
  return int_64_compact(int_64_and(a, b));
}

static _rs_inline obj long_or(INT_64 a, INT_64 b)
{
  return int_64_compact(int_64_or(a, b));
}

static _rs_inline obj long_xor(INT_64 a, INT_64 b)
{
  return int_64_compact(int_64_xor(a, b));
}

static _rs_inline obj long_not(INT_64 a)
{
  return int_64_compact(int_64_not(a));
}


static _rs_inline obj long_ashr(INT_64 a, INT_32 b)
{
  return int_64_compact(int_64_ashr(a, b));
}

static _rs_inline obj long_lshr(INT_64 a, INT_32 b)
{
  return int_64_compact(int_64_lshr(a, b));
}

static obj long_ashl(INT_64 a, INT_32 b)
{
  int am;
  if((am = search_one(a)) < 0)
    return ZERO;
  if(am + b < 63)
    return int_64_compact(int_64_shl(a, b));
  return bignum_shl(int64_to_bignum(a), b);
}

static obj long_lshl(INT_64 a, INT_32 b)
{
  int am;
  if((am = search_one_sub(a, 0)) < 0)
    return ZERO;
  if(am + b <= 63)
    return int_64_compact(int_64_shl(a, b));
  return bignum_shl(int64_to_bignum_u(a), b);
}

#define long_cmp(a,b) int_64_cmp(a,b)

/************************ IEEE_64 OPERATIONS ************************/

static _rs_inline obj fl_plus( IEEE_64 a, IEEE_64 b )
{
  return make_float( a + b );
}

static _rs_inline obj fl_minus( IEEE_64 a, IEEE_64 b )
{
  return make_float( a - b );
}

static _rs_inline obj fl_mul( IEEE_64 a, IEEE_64 b )
{
  return make_float( a * b );
}

static _rs_inline obj fl_div( IEEE_64 a, IEEE_64 b )
{
  return make_float( a / b );
}

static _rs_inline int fl_cmp( IEEE_64 a, IEEE_64 b )
{
  if (a < b)
    return -1;
  else if (a > b)
    return 1;
  else
    return 0;
}

/************************ BIGNUM OPERATIONS ************************/

static int bignum_fit_in_32( mpz_t a )
{
  mpz_t mn, mx;

  OBJ_TO_MPZ( mn, min_machine_int );
  OBJ_TO_MPZ( mx, max_machine_int );

  return ((mpz_cmp( a, mn ) >= 0) && (mpz_cmp( a, mx ) <= 0));
}

static long int bignum_to_int( mpz_t a )
{
  return mpz_get_si( a );
}

static obj bignum_compact(mpz_t a)
{
    INT_64 b;
    UINT_32 c;

    if(abs(a[0]._mp_size) > 64/mp_bits_per_limb)
      return mpz_to_bignum(a);
    if(abs(a[0]._mp_size) < 64/mp_bits_per_limb)
      c = 0;
    else {
	mpz_t d;
	mpz_init(d);
	mpz_tdiv_q_2exp(d, a, CHAR_BIT*sizeof(unsigned long int));
	c=mpz_get_ui(d);
	if(mpz_sgn(a) > 0 && (c&(1<<(CHAR_BIT*sizeof(unsigned long int)-1))))
	  return mpz_to_bignum(a);	  
    }
#ifndef HAVE_INT_64
    b.digits[0] = c >> 16;
    b.digits[1] = c & 0xffff;
    c = mpz_get_ui(a);
    b.digits[2] = c >> 16;
    b.digits[3] = c & 0xffff;
    if(mpz_sgn(a) < 0)
      b = int_64_neg(b);
#else
    b = c;
    b <<= 32;
    b |= mpz_get_ui(a);
    if(mpz_sgn(a) < 0)
      b = -b;
#endif
    return int_64_compact(b);
}


static _rs_inline obj bignum_plus( obj a, obj b )
{
  mpz_t r, a1, b1;
  OBJ_TO_MPZ(a1, a);
  OBJ_TO_MPZ(b1, b);

  mpz_init(r);
  mpz_add(r, a1, b1);
  return bignum_compact(r);
}

static _rs_inline obj bignum_minus( obj a, obj b )
{
  mpz_t r, a1, b1;

  OBJ_TO_MPZ(a1, a);
  OBJ_TO_MPZ(b1, b);

  mpz_init(r);
  mpz_sub(r, a1, b1);
  return bignum_compact(r);
}

static _rs_inline obj bignum_mul( obj a, obj b )
{
  mpz_t r, a1, b1;

  OBJ_TO_MPZ(a1, a);
  OBJ_TO_MPZ(b1, b);

  mpz_init(r);
  mpz_mul(r, a1, b1);
  return bignum_compact(r);
}

/*
 *  bignum_div() is integer division, so it is
 *  morally equivalent to bignum_quotient()
 */

obj bignum_div( obj a, obj b )
{
  mpz_t r, a1, b1;

  OBJ_TO_MPZ(a1, a);
  OBJ_TO_MPZ(b1, b);

  mpz_init(r);
  if ( mpz_sgn(b1) == 0 ) {
    scheme_error( "dividing ~s by zero", 1, a );
  }
  mpz_div(r, a1, b1);
  return bignum_compact(r);
}

static _rs_inline obj bignum_quotient( obj a, obj b )
{
  return bignum_div( a, b );
}

static int bignum_cmp( obj a, obj b )
{
  mpz_t a1, b1;

  OBJ_TO_MPZ(a1, a);
  OBJ_TO_MPZ(b1, b);

  return mpz_cmp(a1, b1);
}


obj bignum_remainder( obj a, obj b )
{
  mpz_t r, a1, b1;

  OBJ_TO_MPZ(a1, a);
  OBJ_TO_MPZ(b1, b);

  mpz_init(r);
  mpz_tdiv_r(r, a1, b1);
  return bignum_compact(r);
}

static _rs_inline obj bignum_and( obj a, obj b )
{
    mpz_t r, a1, b1;

    OBJ_TO_MPZ(a1, a);
    OBJ_TO_MPZ(b1, b);

    mpz_init(r);
    mpz_and(r, a1, b1);
    return mpz_to_bignum(r);
}

static _rs_inline obj bignum_or( obj a, obj b )
{
    mpz_t r, a1, b1;

    OBJ_TO_MPZ(a1, a);
    OBJ_TO_MPZ(b1, b);

    mpz_init(r);
    mpz_ior(r, a1, b1);
    return mpz_to_bignum(r);
}

static _rs_inline obj bignum_xor( obj a, obj b )
{
    mpz_t r, a1, b1, a2, b2;

    OBJ_TO_MPZ(a1, a);
    OBJ_TO_MPZ(b1, b);

    mpz_init(r);
    mpz_init(a2);
    mpz_init(b2);
    mpz_com(a2, a1);
    mpz_com(b2, b1);

    mpz_and(r, a1, b2);
    mpz_and(b2, a2, b1);
    mpz_ior(a1, r, b2);
    return mpz_to_bignum(a1);
}

static _rs_inline obj bignum_not( obj a )
{
    mpz_t r, a1;

    OBJ_TO_MPZ(a1, a);

    mpz_init(r);
    mpz_com(r, a1);
    return mpz_to_bignum(r);
}

static obj bignum_modulo( obj a, obj b )
{
    mpz_t q, r, a1, b1;

    OBJ_TO_MPZ(a1, a);
    OBJ_TO_MPZ(b1, b);

    mpz_init(r);
    mpz_init(q);
    mpz_tdiv_qr(q, r, a1, b1);

    /* if sign of remainder is different from sign of divisor,
       adjust the remainder to fit modulo convention.
       but if remainder is 0, keep it (n.b., mpz_sgn(0) == 0)
    */

    if (mpz_sgn(r) != 0 && (mpz_sgn(b1) != mpz_sgn(r)))
    {
      mpz_set(q, r);
      mpz_add(r, q, b1);
    }
    return mpz_to_bignum(r);
}

static obj bignum_shr( obj a, INT_32 b)
{
  mpz_t r, a1;
  OBJ_TO_MPZ(a1, a);

  mpz_init(r);
  mpz_tdiv_q_2exp(r, a1, b);
  return bignum_compact(r);
}

static obj bignum_shl( obj a, INT_32 b )
{
  mpz_t r, a1;
  OBJ_TO_MPZ(a1, a);

  mpz_init(r);
  mpz_mul_2exp(r, a1, b);
  return bignum_compact(r);
}

static int str2big( MP_INT *v, char *str, unsigned radix )
{
  if(str[0] == '+') {
    str++;
  }
  if(mpz_init_set_str( v, str, radix) != 0) {
    return 0;
  }
  return 1;
}

obj string_to_bignum_obj(char *str, unsigned radix)
{
  mpz_t v;

  if (!str2big( &v[0], str, radix )) {
    return FALSE_OBJ;
  }
  return mpz_to_bignum( v );
}

obj bignum_to_string_obj(obj a, unsigned radix)
{
  mpz_t v;
  size_t sz; 
  obj str, str2;
 
  OBJ_TO_MPZ(v, a);
  sz = mpz_sizeinbase(v, radix) + 1;
  if(mpz_sgn(v)<0)
    sz++;
  str = bvec_alloc(sz, string_class);

  if(!mpz_get_str(PTR_TO_DATAPTR(str), radix, v))
    return FALSE_OBJ;
  if(strlen(PTR_TO_DATAPTR(str)) == sz - 1)
    return str;
  str2 = bvec_alloc(sz - 1, string_class);
  strcpy(PTR_TO_DATAPTR(str2), PTR_TO_DATAPTR(str));
  return str2;
}


/************************ RATIONAL OPERATIONS ************************/

static obj rational_compact( mpq_t a )
{
  mpq_canonicalize( a );
  if(abs( mpq_denref( a )->_mp_size ) == 1 &&
     mpz_get_ui( mpq_denref( a ) ) == 1)
    return bignum_compact( mpq_numref( a ) );
  return make2( mp_rational_class, 
                mpz_to_bignum( mpq_numref( a ) ), 
                mpz_to_bignum( mpq_denref( a ) ) );
}

			       
obj rational_plus( obj a, obj b )
{
  mpq_t r, a1, b1;

  OBJ_TO_MPQ(a1, a);
  OBJ_TO_MPQ(b1, b);

  mpq_init(r);
  mpq_add(r, a1, b1);
  return rational_compact(r);
}

obj rational_minus( obj a, obj b )
{
  mpq_t r, a1, b1;

  OBJ_TO_MPQ(a1, a);
  OBJ_TO_MPQ(b1, b);

  mpq_init(r);
  mpq_sub(r, a1, b1);
  return rational_compact(r);
}

obj rational_mul( obj a, obj b )
{
  mpq_t r, a1, b1;

  OBJ_TO_MPQ(a1, a);
  OBJ_TO_MPQ(b1, b);

  mpq_init(r);
  mpq_mul(r, a1, b1);
  return rational_compact(r);
}

obj rational_div( obj a, obj b )
{
  mpq_t r, a1, b1;

  OBJ_TO_MPQ(a1, a);
  OBJ_TO_MPQ(b1, b);

  if (mpq_sgn( b1 ) == 0) {
    scheme_error( "dividing ~s by zero", 1, a );
  }

  mpq_init(r);
  mpq_div(r, a1, b1);
  return rational_compact(r);
}

obj rational_to_bignum( obj a )
{
  mpq_t a1;
  mpz_t r;
  OBJ_TO_MPQ(a1, a);
  mpz_init(r);
  mpz_div(r, mpq_numref(a1), mpq_denref(a1));
  return mpz_to_bignum(r);
}

static int rational_cmp( obj a, obj b )
{
  mpq_t a1, b1;

  OBJ_TO_MPQ(a1, a);
  OBJ_TO_MPQ(b1, b);

  return mpq_cmp(a1, b1);
}

static int str2rat( mpq_t *b, char *str, unsigned radix )
{
  obj v;
  char *p, *q;

  if(!(p=strchr(str, '/'))) {
    return 0;
  }
  q = p;
  *(p++) = 0;

  if(mpz_init_set_str(mpq_numref(*b), str, radix) != 0 ||
     mpz_init_set_str(mpq_denref(*b), p, radix) != 0) {
    *q = '/';
    return 0;
  }
  *q = '/';
  return 1;
}

obj string_to_rational_obj( char *str, unsigned radix )
{
  char *f;
  mpq_t b, w;

  f = strchr( str, '+' );       /* support WHOLE+NUM/DEN syntax */
  if (f) {
    MP_INT *num;
    mpz_t numa;
    if (!str2rat( &b, f+1, radix )) {
      return FALSE_OBJ;
    }
    *f = '\0';
    num = mpq_numref(w);
    if (!str2big( num, str, radix )) {
      *f = '+';
      return FALSE_OBJ;
    }
    *f = '+';
    mpz_init_set_si( mpq_denref(w), 1 );
    mpq_add( b, b, w );
  } else {
    if (!str2rat( &b, str, radix )) {
      return FALSE_OBJ;
    }
  }
  return rational_compact( b );
}

obj rational_to_string_obj( obj a, unsigned radix )
{
  mpq_t v;
  size_t sz; 
  obj str, str2;
  int len;

  OBJ_TO_MPQ(v, a);
  sz = mpz_sizeinbase(mpq_numref(v), radix) + 2;
  if(mpz_sgn(mpq_numref(v))<0)
    sz++;
  sz += mpz_sizeinbase(mpq_denref(v), radix);
  str = bvec_alloc(sz, string_class);

  if(!mpz_get_str(PTR_TO_DATAPTR(str), radix, mpq_numref(v)))
    return FALSE_OBJ;
  len = strlen(PTR_TO_DATAPTR(str));
  ((char *)PTR_TO_DATAPTR(str))[len++]='/';
  if(!mpz_get_str(PTR_TO_DATAPTR(str) + len, radix, mpq_denref(v)))
    return FALSE_OBJ;

  if(strlen(PTR_TO_DATAPTR(str)) == sz - 1)
    return str;
  str2 = bvec_alloc(sz - 1, string_class);
  strcpy(PTR_TO_DATAPTR(str2), PTR_TO_DATAPTR(str));
  return str2;
}

obj extract_numerator(obj a)
{
   return gvec_ref(a, SLOT(0));
}

obj extract_denominator(obj a)
{
   return gvec_ref(a, SLOT(1));
}

/************************ COMPLEX OPERATIONS ************************/

typedef struct _cmplx {
  obj re;
  obj im;
} cmplx;

static obj make_complex( obj re, obj im )
{
  return make2( rect_complex_class, re, im );
}

static _rs_inline cmplx real_to_complex( obj r )
{
  cmplx c;
  c.re = r;
  c.im = ZERO;
  return c;
}

static _rs_inline cmplx extract_complex( obj ch )
{
  cmplx c;
  c.re = gvec_ref(ch,SLOT(0));
  c.im = gvec_ref(ch,SLOT(1));
  return c;
}

/*
#define RETURN_COMPLEX(re,im) if (EQ(im,ZERO)) return re; else return make_complex( re, im )
*/
#define RETURN_COMPLEX(re,im) if (basic_cmp(im,ZERO)) return make_complex( re, im ); else return re

static _rs_inline obj complex_plus( cmplx a, cmplx b )
{
  obj re = basic_plus( a.re, b.re );
  obj im = basic_plus( a.im, b.im );

  RETURN_COMPLEX(re,im);
}

static _rs_inline obj complex_minus( cmplx a, cmplx b )
{
  obj re = basic_minus( a.re, b.re );
  obj im = basic_minus( a.im, b.im );

  RETURN_COMPLEX(re,im);
}

/*  (a + ib) * (c + id) = (ac - bd) + (ad + cb)i  */

static obj complex_mul( cmplx a_r, cmplx b_r )
{
  obj a = a_r.re;
  obj b = a_r.im;
  obj c = b_r.re;
  obj d = b_r.im;

  obj re = basic_minus( basic_mul( a, c ), basic_mul( b, d ) );
  obj im = basic_plus( basic_mul( a, d ), basic_mul( c, b ) );

  RETURN_COMPLEX(re,im);
}

/*
   (a + ib)      (a + ib) * (c - id)   (ac + bd) + (bc - ad)i
   --------   =  ------------------- = ----------------------
   (c + id)      (c + id) * (c - id)       c * c + d * d       
   
         ac + bd         (bc - ad)
    = -------------- + ------------- i
      c * c + d * d    c * c + d * d
*/

static _rs_inline obj complex_div( cmplx a, cmplx b )
{
    obj cd = basic_plus(basic_mul(b.re, b.re), basic_mul(b.im, b.im));
    obj re = basic_div(basic_plus(basic_mul(a.re, b.re), basic_mul(a.im, b.im)), cd);
    obj im = basic_div(basic_minus(basic_mul(a.im, b.re), basic_mul(a.re, b.im)), cd);
    RETURN_COMPLEX(re,im);
}

static _rs_inline obj complex_abs( cmplx a )
{
  return basic_plus( basic_mul( a.re, a.re ), basic_mul( a.im, a.im ) );
}

static int complex_cmp( cmplx a, cmplx b )
{
  return basic_cmp( complex_abs(a), complex_abs(b) );
}

obj make_complex_obj(obj re, obj im)
{
  RETURN_COMPLEX(re, im);
}

obj extract_real_part(obj a)
{
   return gvec_ref(a, SLOT(0));
}

obj extract_image_part(obj a)
{
   return gvec_ref(a, SLOT(1));
}

obj float_truncate( IEEE_64 longfloat )
{
  /** TODO: 
   **   Refine these tests so that exactly the smallest type is used 
   **/

  if ((longfloat >= -536870912.0) && (longfloat <= 536870911.0))
    {
      int t = (int)longfloat;
      return int2fx( t );
    }
  else if ((longfloat >= -9.22337e+18) && (longfloat <= 9.22337e+18))
    {
      return int_64_compact( float_to_int_64( longfloat ) );
    }
  else
    {
      return raw_float_to_bignum( longfloat );
    }
}

/********************************************************************/

#include "numtower.ci"

#endif /* FULL_NUMERIC_TOWER */

int basic_raw_int_conv_p( obj a )
{
  if (FIXNUM_P( a )) { return 1; } 
  if (LONG_INT_P( a )) { return 1; }
#if FULL_NUMERIC_TOWER
  if (BIGNUM_P( a )) { return 1; }
#endif
  return 0;
}

int basic_raw_int_conv( obj a )
{
  if (FIXNUM_P( a ))
    {
      return fx2int( a );
    }

  if (LONG_INT_P( a ))
    {
      INT_64 i = extract_int_64( a );

      if (int_64_fit_in_32_q( i ))
	{
	  return int_64_to_int_32( i );
	}
      else
	{
	  scheme_error( "long int ~s is out of range for a raw int", 1, a );
	  return 0;
	}
    }
#if FULL_NUMERIC_TOWER
  else if (BIGNUM_P( a ))
    {
      mpz_t z;
      OBJ_TO_MPZ( z, a );

      if (bignum_fit_in_32( z ))
	{
	  return bignum_to_int( z );
	}
      else
	{
	  scheme_error( "bignum ~s is out of range for a raw int", 1, a );
	  return 0;
	}
    }
#endif
  else
    {
      scheme_error( "cannot convert ~s to an exact integer", 1, a );
      return 0;
    }
}

int basic_raw_float_conv_p( obj a )
{
  if (LONG_INT_P( a )) { return 1; }
#if FULL_NUMERIC_TOWER
  if (RATIONAL_P( a )) { return 1; }
#endif
  if (LONGFLOAT_P( a )) { return 1; }
  return 0;
}

double basic_raw_float_conv( obj a )
{
  if (LONG_INT_P( a ))
    {
      return int_64_to_float( extract_int_64( a ) );
    }
#if FULL_NUMERIC_TOWER
  if (RATIONAL_P( a ))
    {
      return rational_to_raw_float( a );
    }
#endif
  if (LONGFLOAT_P( a ))
    {
      return extract_float( a );
    }
    
  scheme_error( "cannot convert ~s to an inexact real", 1, a );
  return 0;
}


UINT_32 basic_raw_uint_conv( obj a )
{
  if (FIXNUM_P(a)) {
    if (FX_LT( a, ZERO )) {
      scheme_error( "fixnum value ~s is negative, not a valid UINT_32", 1, a );
    }
    return fx2int( a );
  } else if (LONG_INT_P( a )) {
    INT_64 *p = (INT_64 *)PTR_TO_DATAPTR( a );
    if ((p->digits[0] == 0) && (p->digits[1] == 0)) {
      return (p->digits[2] << 16) + p->digits[3];
    } else {
      scheme_error( "longint value ~s is not a valid UINT_32", 1, a );
    }
#if FULL_NUMERIC_TOWER
  } else if (BIGNUM_P( a )) {
    mpz_t z;
    int cmp;

    OBJ_TO_MPZ( z, a );
    
    cmp = mpz_sgn( z );
    if (cmp < 0) {
      scheme_error( "bignum value ~s is negative, not a valid UINT_32", 1, a );
    }
    if (cmp == 0) {
      return 0;
    }
    
    cmp = mpz_cmp_ui( z, 0xFFFFFFFFUL );
    if (cmp > 0) {
      scheme_error( "bignum value ~s is too big for a UINT_32", 1, a );
    }
    return mpz_get_ui( z );
#endif
  } else {
    scheme_error( "non-basic-integer value ~s is not a valid UINT_32", 1, a );
  }
  return 0;
}

/* extra 1 (36+1=37) is for NUL which isn't used... */

char value_digits[37] = "0123456789abcdefghijklmnopqrstuvwxyz";

/* fills up buffer right-to-left!  buffer starts out pointing
   to the END of the buffer! */

char *fixnum_to_string( char *buffer, obj value, unsigned radix )
{
  int neg;
  long v;

  *--buffer = 0;
  v = fx2int(value);
  if (v < 0)
    {
      neg = 1;
      v = -v;
    }
  else
    neg = 0;
  if (v == 0)
    *--buffer = '0';
  else
    {
      while (v > 0)
	{
	  *--buffer = value_digits[v % radix];
	  v /= radix;
	}
    }
  if (neg)
    *--buffer = '-';
  return buffer;
}


obj basic_num_to_string_obj( obj a, unsigned radix )
{
  char buf[100];

  if (FIXNUM_P(a)) {
    return make_string( fixnum_to_string( &buf[100], a, radix ) );
  } else if (LONGFLOAT_P(a)) {
    snprintf( buf, 100, "%g", extract_float(a) );
    if (!strchr( buf,'.') && !strchr(buf,'e')) {
      strcat( buf, "." );
    }
    return make_string( buf );
  } else if (OBJ_ISA_PTR_OF_CLASS(a,bignum_class)) {
    return bignum_to_string_obj( a, radix );
  } else if (OBJ_ISA_PTR_OF_CLASS(a,mp_rational_class)) {
    return rational_to_string_obj( a, radix );
  } else if (OBJ_ISA_PTR_OF_CLASS(a,rect_complex_class)) {
    obj r;
    char *str;
    obj re = basic_num_to_string_obj( gvec_ref( a, SLOT(0) ), radix );
    obj im = basic_num_to_string_obj( gvec_ref( a, SLOT(1) ), radix );
    unsigned len = string_length(re) + string_length(im) + 1;

    if (string_text(im)[0] != '-') {
      len++;
    }
    r = bvec_alloc( len+1, string_class );
    str = string_text( r );

    memcpy( str, string_text( re ), string_length( re ) );
    str += string_length( re );
    if (string_text(im)[0] != '-') {
      *str++ = '+';
    }
    memcpy( str, string_text( im ), string_length( im ) );
    str += string_length( im );
    *str++ = 'i';
    *str = 0;
    return r;
  } else {
    return FALSE_OBJ;
  }
}
