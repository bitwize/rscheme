/**********************************************
THIS FILE WAS AUTOMATICALLY COPIED FROM THE
RSCHEME SOURCE TREE, AND THE ORIGINAL MAY CHANGE.
HENCE, DO NOT MODIFY THIS FILE BY HAND!
RScheme Build (v0.7.3.4-b7u, 2007-05-30)
**********************************************/

#line 1 "modules/corelib/str2num.c"
/*-----------------------------------------------------------------*-C-*---
 * File:    modules/corelib/str2num.c
 *
 *          Copyright (C)1997 Donovan Kolbly <d.kolbly@rscheme.org>
 *          as part of the RScheme project, licensed for free use.
 *          See <http://www.rscheme.org/> for the latest information.
 *
 * File version:     1.14
 * File mod date:    2006-01-28 16:45:01
 * System build:     v0.7.3.4-b7u, 2007-05-30
 * Owned by module:  corelib
 *
 * Purpose:          low-level (C) string<->number utility functions
 *------------------------------------------------------------------------*/

#include <rscheme/scheme.h>
#include <string.h>
#include <math.h>
#include <stdarg.h>
#include <stdlib.h>
#include "strnnum.h"

/* these tables are also used by runtime/longint.c */

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

#define digit_value(ch) digit_values[ch]

obj c_vprintf( char *fmt, int len, ... )
{
char tmp[100], *buf;
obj s;
va_list va;

  if (len > 100)
    buf = malloc(len+1);
  else
    buf = tmp;

  va_start( va, len );
  vsnprintf( buf, len, fmt, va );
  va_end( va );

  s = make_string( buf );

  if (len > 100)
    free(buf);

  return s;
}

char *double_float_to_string( char *buffer, obj value )
{
    sprintf( buffer, "%g", extract_float(value) );
    /* a crude, but hopefully effective fix for 940809-1:
          don't add the ".0" unless there is no 'e' either
       -- dmk 95.01.04
    */
    if (!strchr(buffer,'.') && !strchr(buffer,'e'))
        strcat( buffer, "." );
    return buffer;
}

obj string_to_float( char *str_in, UINT_32 len, unsigned radix )
{
  UINT_8 *str = (UINT_8*)str_in;
  double x = 0.0, r = radix;
  double v = 0.0;
  unsigned i, num_digits = 0;
  int exp, exp_neg;
  rs_bool neg = NO;
  
  if (*str == '-')
    {
      str++;
      neg = YES;
    }
  else if (*str == '+')
    {
      str++;
    }

  while (*str && *str != '.')
    {
      i = digit_value( *str++ );
      if (i >= radix)
	{
	  /* note that this notation cannot be used in radix >= 15 */
	  if (i == 14) /* e|E */
	    {
	      goto float_exp;
	    }
	  return FALSE_OBJ;
	}
      v = v * radix + i;
      num_digits++;
    }
  if (*str == '.')
  {
    str++;
    r = 1.0 / r;
    x = r;
    while (*str)
      {
	i = digit_value( *str++ );
	if (i >= radix)
	  {
	    if (i == 14) /* e|E */
	      {
		goto float_exp;
	      }
	    return FALSE_OBJ;
	  }
	v += i * x;
	x *= r;
	num_digits++;
      }
  }
  if (num_digits == 0)
    return FALSE_OBJ;  /* no digits -- "." is invalid */
  
  return make_float( neg ? -v : v );

 float_exp:

  exp = 0;
  exp_neg = 0;

  if (*str == '-')
    {
      exp_neg = 1;
      str++;
    }
  else if (*str == '+')
    {
      str++;
    }

  while (*str)
    {
      i = digit_value( *str++ );
      if (i >= 10)
	{
	  /* exponents are always in decimal */
	  return FALSE_OBJ;
	}
      exp = (exp * 10) + i;
      if (exp > 1000000)
	return FALSE_OBJ;  /* exponent too big! */
    }
  return make_float( (neg ? -v : v) * pow( radix, exp_neg ? -exp : exp ) );
}

obj string_to_fixnum( char *str_in, UINT_32 len, unsigned radix )
{
int i;
rs_bool neg = NO;
UINT_32 v, preq, prem;
UINT_8 *lim = ((UINT_8*)str_in) + len;
UINT_8 *str = (UINT_8*)str_in;

    if (*str == '-')
    {
	str++;
	neg = YES;
    }
    else if (*str == '+')
    {
	str++;
    }

    /* compute the maximum value & digit that is
       allowed BEFORE a new digit is added.
       For example, if the limit is 1024 (max 1023)
       and we're in base 10, the preq is 102 and prem is 4
       is if the value so far is 102 and we see a 4, we know
       that's too much, but if we see a 3, that's OK */

    preq = (1UL<<(WORD_SIZE_BITS-PRIMARY_TAG_SIZE-1)) / radix;
    prem = (1UL<<(WORD_SIZE_BITS-PRIMARY_TAG_SIZE-1)) % radix;

    /* printf( "preq = %d, prem = %d\n", preq, prem ); */
    
    if (str >= lim)
      return FALSE_OBJ;  /* no digits! */
    
    v = 0;
    while (str < lim)
      {
	i = digit_value( *str++ );
	if (i >= radix)
	  {
	    return FALSE_OBJ;
	  }
	if ((v > preq)
	    || ((v == preq) 
		&& ((i > prem) || ((i == prem) && !neg))))
	  {
	    /* too big -- doesn't fit as a fixnum */
	    return FALSE_OBJ;
	  }
	v = v * radix + i;
      }
    return int2fx( neg ? -v : v );
}

