/*-----------------------------------------------------------------*-C-*---
 * File:    handc/runtime/longint.c
 *
 *          Copyright (C)1997 Donovan Kolbly <d.kolbly@rscheme.org>
 *          as part of the RScheme project, licensed for free use.
 *          See <http://www.rscheme.org/> for the latest information.
 *
 * File version:     1.14
 * File mod date:    2005-02-16 17:10:02
 * System build:     v0.7.3.4-b7u, 2007-05-30
 *
 *------------------------------------------------------------------------*/

#ifndef UNIT_TEST
#include <rscheme/obj.h>
#include <rscheme/smemory.h>
#include <rscheme/scheme.h>
#else
typedef enum {
  NO = 0,
  YES = !NO
} rs_bool;
#define _rs_volatile /*nothing*/
typedef struct { unsigned long word; } obj;
typedef double IEEE_64;
typedef long INT_32;
typedef unsigned long UINT_32;
typedef unsigned short UINT_16;
typedef short INT_16;
#include "longint.h"
#endif

#include <math.h>

void printit( char *name, INT_64 val )
{
  char buf[70];
  printf( "%-5s %04x %04x %04x %04x (%g) (%s)\n",
	  name,
	  val.digits[0], val.digits[1], val.digits[2], val.digits[3],
	 int_64_to_float(val),
	 (name[0] == '*') ? "--"
	 : int_64_to_string(buf+25,val,10));
}

#ifndef UNIT_TEST

rs_bool LONG_INT_P( obj thing )
{
  return OBJ_ISA_PTR_OF_CLASS(thing,long_int_class);
}

obj make_long_int( INT_64 a )
{
  obj longint = make_bvec( long_int_class, sizeof(INT_64) );
  *((INT_64 *)PTR_TO_DATAPTR(longint)) = a;
  return longint;
}

obj int_64_compact( INT_64 a )
{
  if (int_64_fit_in_30_q(a))
    return int2fx( int_64_to_int_32(a) );
  else
    {
      obj longint = make_bvec( long_int_class, sizeof(INT_64) );
      *((INT_64 *)PTR_TO_DATAPTR(longint)) = a;
      return longint;
    }
}

/* for uint's that don't fit in a fixnum */
obj uint_32_big( UINT_32 a )
{
  INT_64 x;

  x.digits[0] = 0;
  x.digits[1] = 0;
  x.digits[2] = (a >> 16) & 0xFFFF;
  x.digits[3] = a & 0xFFFF;

  return make_long_int( x );
}

#endif

#ifdef UNIT_TEST
_rs_volatile void scheme_error( const char *msg, unsigned num_args, ... );
void printit( char *name, INT_64 val );
#endif

INT_64 INT_64_zero = { { 0, 0, 0, 0 } }; 
INT_64 INT_64_one = { { 0, 0, 0, 1 } };
INT_64 INT_64_two = { { 0, 0, 0, 2 } };
INT_64 INT_64_neg_one = { { 0xFFFF, 0xFFFF, 0xFFFF, 0xFFFF } };


INT_64 int_32_to_int_64( INT_32 x )
{
  INT_64 r;

  r.digits[3] = x;
  r.digits[2] = x >> 16;
  r.digits[1] = r.digits[0] = (x & 0x80000000) ? 0xFFFF : 0;
  return r;
}

INT_64 int_64_add( INT_64 a, INT_64 b )
{
  int i, c = 0;
  INT_32 t;
  INT_64 r;

  for (i=4; i>0;)
    {
      i--;
      t = a.digits[i] + b.digits[i] + c;
      r.digits[i] = t;
      c = t >> 16;
    }
  return r;
}

INT_64 int_64_and( INT_64 a, INT_64 b )
{
  int i, c = 0;
  INT_32 t;
  INT_64 r;

  for (i=4; i>0;)
    {
      i--;
      r.digits[i] = a.digits[i] & b.digits[i] ;
    }
  return r;
}

INT_64 int_64_or( INT_64 a, INT_64 b )
{
  int i, c = 0;
  INT_32 t;
  INT_64 r;

  for (i=4; i>0;)
    {
      i--;
      r.digits[i] = a.digits[i] | b.digits[i] ;
    }
  return r;
}

INT_64 int_64_xor( INT_64 a, INT_64 b )
{
  int i, c = 0;
  INT_32 t;
  INT_64 r;

  for (i=4; i>0;)
    {
      i--;
      r.digits[i] = a.digits[i] ^ b.digits[i] ;
    }
  return r;
}

INT_64 int_64_not( INT_64 a )
{
  int i, c = 0;
  INT_32 t;
  INT_64 r;

  for (i=4; i>0;)
    {
      i--;
      r.digits[i] = ~a.digits[i];
    }
  return r;
}

INT_64 int_64_lshr( INT_64 a, int b )
{
  int i, c, d;
  INT_64 r;

  c = b / 16;
  d = b % 16;

  for(i = 0; i < c; i++)
    r.digits[i] = 0;

  for(;i < 4; i++){
    r.digits[i] = ((a.digits[i - c] >> d) | ((i-c-1<0)?0:(a.digits[i - c -1] << (16 - d)))) & 0xffff;
  }
  return r;
}

INT_64 int_64_ashr( INT_64 a, int b )
{
  int i, c, d, s;
  INT_64 r;

  c = b / 16;
  d = b % 16;
  if(int_64_neg_q(a))
    s = 0xffff;
  else
    s = 0;

  for(i = 0; i < c; i++)
    r.digits[i] = s;

  for(;i < 4; i++){
    r.digits[i] = ((a.digits[i - c] >> d) | (((i-c-1<0)?s:a.digits[i - c -1]) << (16 - d))) & 0xffff;
  }
  return r;
}

INT_64 int_64_shl( INT_64 a, int b )
{
  int i, c, d;
  INT_64 r;

  c = b / 16;
  d = b % 16;

  for(i = 0 ;i < 4 - c; i++){
    r.digits[i] = ((a.digits[i + c] << d) | ((i+c+1>3)?0:(a.digits[i + c +1] >> (16 - d)))) & 0xffff;
  }
  for( ; i < 4; i++)
    r.digits[i] = 0;
  return r;
}

rs_bool int_64_neg_q( INT_64 a )
{
  return (a.digits[0] & 0x8000) ? YES : NO;
}

rs_bool int_64_zero_q( INT_64 a )
{
  return (a.digits[0] || a.digits[1] || a.digits[2] || a.digits[3])
         ? NO
         : YES;
}

#define CHECK_DIGIT(k,otherwise) \
  if (a.digits[k] < b.digits[k]) \
    return -1; \
  else if (a.digits[k] > b.digits[k]) \
    return 1; \
  else { otherwise }

int int_64_cmp( INT_64 a, INT_64 b )
{
  CHECK_DIGIT(0,CHECK_DIGIT(1,CHECK_DIGIT(2,CHECK_DIGIT(3,return 0;))))
}


rs_bool int_64_eq( INT_64 a, INT_64 b )
{
  return ((a.digits[3] == b.digits[3])
	  && (a.digits[2] == b.digits[2])
	  && (a.digits[1] == b.digits[1])
	  && (a.digits[0] == b.digits[0])) ? YES : NO;
}

rs_bool int_64_gt( INT_64 a, INT_64 b )
{
  return (int_64_cmp(a,b) > 0) ? YES : NO;
}

rs_bool int_64_ge( INT_64 a, INT_64 b )
{
  return (int_64_cmp(a,b) >= 0) ? YES : NO;
}

INT_32 int_64_to_int_32( INT_64 a )
{
  return (a.digits[2] << 16) | a.digits[3];
}

INT_64 int_64_neg( INT_64 a )
{
  a.digits[0] ^= 0xFFFF;
  a.digits[1] ^= 0xFFFF;
  a.digits[2] ^= 0xFFFF;
  a.digits[3] ^= 0xFFFF;
  return int_64_add( a, INT_64_one );
}


INT_64 int_64_add_u32( INT_64 a, UINT_32 b, int shift )
{
  int i, c = 0;
  INT_32 t;

  for (i=4-shift; i>0;)
    {
      i--;
      t = a.digits[i] + (b & 0xFFFF) + c;
      a.digits[i] = t;
      c = t >> 16;
      b >>= 16;
    }
  return a;
}

INT_64 int_64_sub( INT_64 a, INT_64 b )
{
  return int_64_add( a, int_64_neg(b) );
}


static INT_64 int_64_umul( INT_64 a, INT_64 b )
{
  INT_64 r = INT_64_zero;
  UINT_32 pp;
  int i, j;

  for (i=4; i>0;)
    {
      i--;
      for (j=4; j>0;)
	{
	  j--;
	  pp = a.digits[i] * b.digits[j];
	  /*printf( " (%d,%d) => %08x\n", i, j, pp );*/
	  r = int_64_add_u32( r, pp, (3-i)+(3-j) );
	}
    }
  return r;
}

INT_64 int_64_mul( INT_64 a, INT_64 b )
{
  rs_bool a_neg = int_64_neg_q(a);
  rs_bool b_neg = int_64_neg_q(b);

  if (a_neg)
    a = int_64_neg(a);
  if (b_neg)
    b = int_64_neg(b);
  a = int_64_umul( a, b );
  if (a_neg != b_neg)
    return int_64_neg(a);
  else
    return a;
}

rs_bool int_64_fit_in_32_q( INT_64 a )
{
  if (a.digits[0] == 0
      && a.digits[1] == 0
      && (a.digits[2] & 0x8000) == 0)
    return YES;
  else if (a.digits[0] == 0xFFFF
	   && a.digits[1] == 0xFFFF
	   && (a.digits[2] & 0x8000))
    return YES;
  else
    return NO;
}

rs_bool int_64_fit_in_30_q( INT_64 a )
{
  if (a.digits[0] == 0
      && a.digits[1] == 0
      && (a.digits[2] & 0xE000) == 0)
    return YES;
  else if (a.digits[0] == 0xFFFF
	   && a.digits[1] == 0xFFFF
	   && (a.digits[2] & 0xE000) == 0xE000)
    return YES;
  else
    return NO;
}

struct INT_64_div2 {
  INT_64 quotient;
  INT_64 remainder;
};

static struct INT_64_div2 int_64_divu( INT_64 a, INT_64 b )
{
  UINT_32 quot_hi, quot_lo, a_hi, a_lo, b_hi, b_lo, temp_hi, temp_lo;
  struct INT_64_div2 r;
  int i;

  quot_hi = quot_lo = 0;

  b_hi = (b.digits[0] << 16) + b.digits[1];
  b_lo = (b.digits[2] << 16) + b.digits[3];

  temp_hi = temp_lo = 0;

  a_hi = (a.digits[0] << 16) + a.digits[1];
  a_lo = (a.digits[2] << 16) + a.digits[3];

/*
  printf( "a = %08x_%08x\n", a_hi, a_lo );
  printf( "b = %08x_%08x\n", b_hi, b_lo );
*/

  for (i=0; i<64; i++)
    {
/*
      printf( "i[%d] quot %08x_%08x t %08x_%08x a %08x_%08x\n",
	     i, quot_hi, quot_lo, temp_hi, temp_lo,
	     a_hi, a_lo );
*/
      quot_hi = (quot_hi << 1) + ((quot_lo & 0x80000000) ? 1 : 0);
      quot_lo <<= 1;
      temp_hi = (temp_hi << 1) + ((temp_lo & 0x80000000) ? 1 : 0);
      temp_lo = (temp_lo << 1) + ((a_hi & 0x80000000) ? 1 : 0);
      a_hi = (a_hi << 1) + ((a_lo & 0x80000000) ? 1 : 0);
      a_lo <<= 1;
      if (temp_hi > b_hi || ((temp_hi == b_hi) && (temp_lo >= b_lo)))
	{
	  quot_lo |= 1;
	  if (temp_lo >= b_lo)
	    {
	      temp_lo -= b_lo;
	      temp_hi -= b_hi;
	    }
	  else
	    {
	      temp_lo -= b_lo;
	      temp_hi -= b_hi - 1;
	    }
	}
    }
  r.quotient.digits[0] = quot_hi >> 16;
  r.quotient.digits[1] = quot_hi;
  r.quotient.digits[2] = quot_lo >> 16;
  r.quotient.digits[3] = quot_lo;

  r.remainder.digits[0] = temp_hi >> 16;
  r.remainder.digits[1] = temp_hi;
  r.remainder.digits[2] = temp_lo >> 16;
  r.remainder.digits[3] = temp_lo;
  return r;
}

/* 
 * these three (_16) functions can be made faster... 
 */

INT_64 int_64_add_16( INT_64 a, INT_16 b )
{
  INT_64 temp = int_32_to_int_64( b );
  return int_64_add( a, temp );
}

static INT_64 int_64_umul_16( INT_64 a, UINT_16 b )
{
  INT_64 temp = int_32_to_int_64( b );
  return int_64_umul( a, temp );
}

static struct INT_64_div2 int_64_divu_16( INT_64 a, UINT_16 b )
{
  INT_64 temp = int_32_to_int_64( b );
  return int_64_divu( a, temp );
}

struct INT_64_div2 int_64_div( INT_64 a, INT_64 b )
{
  struct INT_64_div2 r;
  rs_bool a_neg = int_64_neg_q(a);
  rs_bool b_neg = int_64_neg_q(b);

  if (a_neg)
    a = int_64_neg(a);
  if (b_neg)
    b = int_64_neg(b);

  r = int_64_divu( a, b );

  if (a_neg != b_neg)
    r.quotient = int_64_neg( r.quotient );

  if (a_neg)
    r.remainder = int_64_neg( r.remainder );

  return r;
}

INT_64 int_64_quotient( INT_64 a, INT_64 b )
{
  struct INT_64_div2 r = int_64_div( a, b );
  return r.quotient;
}

INT_64 int_64_remainder( INT_64 a, INT_64 b )
{
  struct INT_64_div2 r = int_64_div( a, b );
  return r.remainder;
}

INT_64 int_64_modulo( INT_64 a, INT_64 b )
{
  struct INT_64_div2 r;
  INT_64 abs_b;
  rs_bool a_neg = int_64_neg_q(a);
  rs_bool b_neg = int_64_neg_q(b);

/*
  printit( "a", a );
  printit( "b", b );
  printf( "... a%s0 b%s0\n", a_neg ? "<" : ">=", b_neg ? "<" : ">=" );
*/
  if (a_neg)
    a = int_64_neg(a);

  if (b_neg)
    abs_b = int_64_neg(b);
  else
    abs_b = b;
/*
  printit( "|a|", a );
  printit( "|b|", abs_b );
*/
  r = int_64_divu( a, abs_b );
/*
  printit( "Q'", r.quotient );
  printit( "R'", r.remainder );
*/
  if (int_64_zero_q(r.remainder))
    {
      return r.remainder;
    }
  else
    {
      /*
       *
       *  we've done |a|/|b| and gotten Q' and R'
       *  (ie, Q' = floor(|a|/|b|)
       *   and Q' * |b| + R' = |a|)
       *
       *  if SGN(a) != SGN(b)
       *  then the quotient a/b is negative,
       *  and hence floor(a/b) = - (floor(|a|/|b|) + 1)
       *
       *  (UNLESS |a|%|b| == 0, that is)
       *
       *  	therefore, since, R = a - Q b
       *  	and Q' = floor(|a|/|b|)
       *  	    Q = - (Q' + 1)
       *
       *   note R = a - Q b
       *          = a + (Q' + 1)b
       *       	  = a + Q'b + b
       *
       *  but R' = |a| - Q' |b|
       *
       *  assume a<0
       *    then |a| = -a, and |b| = b
       *    so R' = -a - Q'b
       *          = -(a + Q'b)
       *    since R = a + Q'b + b
       *       	    = -R' + b
       *       	    = b - R'
       *
       *  on the other hand, assume b<0
       *    then |a| = a, and |b| = -b
       *    so R' = a + Q'b
       *   since R = a + Q'b + b
       *       	   = R' + b
       *	   = b + R'
       */
      if (a_neg)
	{
	  if (b_neg)
	    {
	      /* a<0 & b<0  => Q>=0 & R=-R' */
	      return int_64_neg(r.remainder);
	    }
	  else
	    {
	      /*printit( "b-R'", int_64_sub( b, r.remainder ) ); */
	      /* a<0 & b>=0  => Q<0 & R = b - R' */
	      return int_64_sub( b, r.remainder );
	    }
	}
      else
	{
	  if (b_neg)
	    {
	      /* a>=0 & b<0  => Q<0 & R = b + R' */
	      return int_64_add( b, r.remainder );
	    }
	  else
	    {
	      /* a>=0 & b>=0  => Q>=0 & R = R' */
	      return r.remainder;
	    }
	}
    }
}


IEEE_64 int_64_to_float( INT_64 a )
{
  rs_bool neg = int_64_neg_q(a);
  IEEE_64 f;

  if (neg)
    a = int_64_neg(a);

  f = a.digits[0] * (65536.0 * 4294967296.0)
    + a.digits[1] * 4294967296.0
      + a.digits[2] * 65536.0
	+ a.digits[3];
  if (neg)
    return -f;
  else
    return f;
}

INT_64 float_to_int_64( IEEE_64 a )   /* trunc */
{
  INT_64 r;
  IEEE_64 junk, abs_a = fabs(a);

  r.digits[3] = ldexp( modf( ldexp( abs_a, -16 ), &junk ), 16 );
  r.digits[2] = ldexp( modf( ldexp( abs_a, -32 ), &junk ), 16 );
  r.digits[1] = ldexp( modf( ldexp( abs_a, -48 ), &junk ), 16 );
  r.digits[0] = ldexp( modf( ldexp( abs_a, -64 ), &junk ), 16 );

  if (a < 0)
    return int_64_neg(r);
  else
    return r;
}

#ifdef UNIT_TEST
unsigned char digit_values[256] = {
    99, 99, 99, 99,  99, 99, 99, 99,  99, 99, 99, 99,  99, 99, 99, 99,
    99, 99, 99, 99,  99, 99, 99, 99,  99, 99, 99, 99,  99, 99, 99, 99,
    99, 99, 99, 99,  99, 99, 99, 99,  99, 99, 99, 99,  99, 99, 99, 99,
     0,  1,  2,  3,   4,  5,  6,  7,   8,  9, 99, 99,  99, 99, 99, 99,
    99, 10, 11, 12,  13, 14, 15, 16,  17, 18, 19, 20,  21, 22, 23, 24,
    25, 26, 27, 28,  29, 30, 31, 32,  33, 34, 35, 99,  99, 99, 99, 99,
    99, 10, 11, 12,  13, 14, 15, 16,  17, 18, 19, 20,  21, 22, 23, 24,
    25, 26, 27, 28,  29, 30, 31, 32,  33, 34, 35, 99,  99, 99, 99, 99,

    99, 99, 99, 99,  99, 99, 99, 99,  99, 99, 99, 99,  99, 99, 99, 99,
    99, 99, 99, 99,  99, 99, 99, 99,  99, 99, 99, 99,  99, 99, 99, 99,
    99, 99, 99, 99,  99, 99, 99, 99,  99, 99, 99, 99,  99, 99, 99, 99,
    99, 99, 99, 99,  99, 99, 99, 99,  99, 99, 99, 99,  99, 99, 99, 99,
    99, 99, 99, 99,  99, 99, 99, 99,  99, 99, 99, 99,  99, 99, 99, 99,
    99, 99, 99, 99,  99, 99, 99, 99,  99, 99, 99, 99,  99, 99, 99, 99,
    99, 99, 99, 99,  99, 99, 99, 99,  99, 99, 99, 99,  99, 99, 99, 99,
    99, 99, 99, 99,  99, 99, 99, 99,  99, 99, 99, 99,  99, 99, 99, 99 };
static char value_digits[37] = "0123456789abcdefghijklmnopqrstuvwxyz";
#else
extern unsigned char digit_values[256];
extern char value_digits[37];
#endif

#define digit_value(ch) digit_values[ch]



rs_bool string_to_int_64( char *str, unsigned len, unsigned radix, INT_64 *v )
{
  INT_64 accum;
  rs_bool neg = NO;
  char *lim = str + len;

  static INT_64 preq;
  static UINT_16 prem;
  static unsigned q_cache = 0;

  if (q_cache != radix)
    {
      struct INT_64_div2 pre;

      /* fill the pre-multiply cache */

      accum = INT_64_zero;
      accum.digits[0] = 0x8000;
      
      pre = int_64_divu_16( accum, radix );

      preq = pre.quotient;
      prem = int_64_to_int_32( pre.remainder );
      q_cache = radix;
    }

  if (*str == '-')
    {
      str++;
      neg = YES;
    }
  else if (*str == '+')
    {
      str++;
    }

  if (str >= lim)
    return NO;  /* no digits */

  accum = INT_64_zero;
  while (str < lim)
    {
      unsigned i;
      unsigned ch;
      INT_64 n;

      ch = *str++;
      i = digit_value(ch);
      if (i >= radix)
	{
	  return NO;
	}
      /* make sure the accumulator has room for the multiply-add */
      
      if (int_64_gt( accum, preq )
	  || (int_64_eq( accum, preq )
	      && ((i > prem) || ((i == prem) && !neg))))
	{
	  /* overflow */
	  return NO;
	}

      n = int_64_add_16( int_64_umul_16( accum, radix ), i );
      if (int_64_gt(accum,n))
	{
	  /* must have overflowed! */
	  return NO;
	}
      accum = n;
    }
  if (neg)
    *v = int_64_neg( accum );
  else
    *v = accum;
  return YES;
}

/* fills up buffer right-to-left!  buffer starts out pointing
   to the END of the buffer! */

char *int_64_to_string( char *buffer, INT_64 value, unsigned radix )
{
  rs_bool neg;

  *--buffer = 0;
  neg = int_64_neg_q(value);
  if (neg)
    value = int_64_neg(value);

  if (int_64_zero_q(value))
	*--buffer = '0';
  else
    {
      while (!int_64_zero_q(value))
	{
	  struct INT_64_div2 d = int_64_divu_16( value, radix );

	  /*printit( "*part", value );*/
	  *--buffer = value_digits[int_64_to_int_32(d.remainder)];
	  value = d.quotient;
	}
      if (neg)
	*--buffer = '-';
    }
  return buffer;
}


#ifdef UNIT_TEST

_rs_volatile void scheme_error( const char *msg, unsigned num_args, ... )
{
  printf( "error: %s\n", msg );
  exit(1);
}


int main( int argc, const char **argv )
{
  INT_64 a, b, c;
  struct INT_64_div2 d;
  int i;
  unsigned r = 10;
  
  for (i=1; i<argc; i++)
    {
      rs_bool ok;
      
      if (argv[i][0] == '-' && argv[i][1] == 'r')
	r = atoi( argv[i]+2 );
      else
	{
	  ok = string_to_int_64( (char *)argv[i], strlen(argv[i]), r, &a );
	  
	  printf( "'%s' => ", argv[i] );
	  
	  if (ok)
	    printit( "val", a );
	  else
	    printf( "**ERROR**\n" );
	}
    }
  if (argc > 1)
    return 0;

  a = int_32_to_int_64( 999101 );
  b = int_32_to_int_64( -200000 );
  c = int_64_add( a, b );
  printit( "a", a );
  printit( "b", b );
  printit( "c", c );

  putchar( '\n' );
  a = int_32_to_int_64( 0x3456789a );
  b = int_32_to_int_64( 0x10000000 );
  printit( "a", a );
  printit( "b", b );
  c = int_64_mul( a, b );
  printit( "c", c );

  c = int_64_add( c, INT_64_one );
  d = int_64_divu( c, a );
  printit( "c/a", d.quotient );
  printit( "c%a", d.remainder );

  putchar( '\n' );
  c = int_32_to_int_64( 1234567890 );
  a = int_32_to_int_64( 10 );
  d = int_64_divu( c, a );
  printit( "c", c );
  printit( "a", a );
  printit( "c/a", d.quotient );
  printit( "c%a", d.remainder );

  putchar( '\n' );
  a = int_32_to_int_64( 731234512 );
  b = int_32_to_int_64( 123456 );
  c = int_32_to_int_64( 1234567890 );
  printit( "(c/a)", c );
  printit( "(c%a)", b );
  c = int_64_add( b, int_64_mul( c, a ) );
  printit( "c", c );

  d = int_64_divu( c, a );
  printit( "c/a", d.quotient );
  printit( "c%a", d.remainder );

  printf( "`remainder'\n" );
  for (i=0; i<4; i++)
    {
      char temp[8];
      sprintf( temp, "%c/%c", (i & 1) ? '-' : '+', (i & 2) ? '-' : '+' );

      a = int_32_to_int_64( ((i & 1) ? -1 : 1) * 13 );
      b = int_32_to_int_64( ((i & 2) ? -1 : 1) * 4 );
      c = int_64_remainder( a, b );
      printit( temp, c );
    }

  printf( "`modulo'\n" );
  for (i=0; i<4; i++)
    {
      char temp[8];
      sprintf( temp, "%c/%c", (i & 1) ? '-' : '+', (i & 2) ? '-' : '+' );

      a = int_32_to_int_64( ((i & 1) ? -1 : 1) * 13 );
      b = int_32_to_int_64( ((i & 2) ? -1 : 1) * 4 );
      c = int_64_modulo( a, b );
     /* printit( temp, d.quotient ); */
      printit( temp, c );
    }
  printf( "`shl'\n");
  a = int_32_to_int_64(0x60000000);
  printit( "a" , a);
  c = int_64_shl(a, 6);
  printit("c", c);
}

#endif

