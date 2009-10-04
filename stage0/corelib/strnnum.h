/**********************************************
THIS FILE WAS AUTOMATICALLY COPIED FROM THE
RSCHEME SOURCE TREE, AND THE ORIGINAL MAY CHANGE.
HENCE, DO NOT MODIFY THIS FILE BY HAND!
RScheme Build (v0.7.3.4-b7u, 2007-05-30)
**********************************************/

#line 1 "modules/corelib/strnnum.h"
/*-----------------------------------------------------------------*-C-*---
 * File:    modules/corelib/strnnum.h
 *
 *          Copyright (C)1997 Donovan Kolbly <d.kolbly@rscheme.org>
 *          as part of the RScheme project, licensed for free use.
 *          See <http://www.rscheme.org/> for the latest information.
 *
 * File version:     1.8
 * File mod date:    1999-01-11 08:19:01
 * System build:     v0.7.3.4-b7u, 2007-05-30
 * Owned by module:  corelib
 *
 * Purpose:          define the interface to str2num.c
 *------------------------------------------------------------------------*/

#ifndef _H_RSCORELIB_STRNNUM
#define _H_RSCORELIB_STRNNUM

#include <rscheme/obj.h>

char *fixnum_to_string( char *buffer, obj value, unsigned radix );
char *double_float_to_string( char *buffer, obj value );

obj string_to_fixnum( char *str, UINT_32 len, unsigned radix );
obj string_to_float( char *str, UINT_32 len, unsigned radix );

obj string_to_bignum_obj(char *str, unsigned radix);
obj bignum_to_string_obj(obj num, unsigned radix);

obj string_to_rational_obj(char *str, unsigned radix);
obj extract_numerator(obj a);
obj extract_denominator(obj a);

obj make_complex_obj(obj re, obj im);
obj extract_real_part(obj a);
obj extract_image_part(obj a);

IEEE_64 bignum_to_raw_float(obj);
IEEE_64 rational_to_raw_float(obj);
IEEE_64 longint_to_raw_float(obj);

obj c_vprintf( char *fmt, int len, ... );

#endif /* _H_RSCORELIB_STRNNUM */
