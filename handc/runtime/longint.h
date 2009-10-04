/*-----------------------------------------------------------------*-C-*---
 * File:    handc/runtime/longint.h
 *
 *          Copyright (C)1997 Donovan Kolbly <d.kolbly@rscheme.org>
 *          as part of the RScheme project, licensed for free use.
 *          See <http://www.rscheme.org/> for the latest information.
 *
 * File version:     1.6
 * File mod date:    1998-12-28 10:26:27
 * System build:     v0.7.3.4-b7u, 2007-05-30
 *
 * Purpose:          Arithmetic and types for 64-bit numbers
 *------------------------------------------------------------------------*/

#ifndef _H_RS_LONGINT
#define _H_RS_LONGINT

#ifndef HAVE_INT_64
typedef struct _int64 {
   UINT_16 digits[4];
} INT_64;

rs_bool LONG_INT_P( obj thing );

INT_64 int_64_add( INT_64 a, INT_64 b );
INT_64 int_64_sub( INT_64 a, INT_64 b );
INT_64 int_64_mul( INT_64 a, INT_64 b );
INT_64 int_64_quotient( INT_64 a, INT_64 b );
INT_64 int_64_modulo( INT_64 a, INT_64 b );
INT_64 int_64_remainder( INT_64 a, INT_64 b );
INT_64 int_64_and(INT_64 a, INT_64 b);
INT_64 int_64_or(INT_64 a, INT_64 b);
INT_64 int_64_xor(INT_64 a, INT_64 b);
INT_64 int_64_not(INT_64 a);
INT_64 int_64_ashr(INT_64 a, int b);
INT_64 int_64_lshr(INT_64 a, int b);
INT_64 int_64_shl(INT_64 a, int b);

INT_64 int_64_neg( INT_64 a );
rs_bool int_64_neg_q( INT_64 a );
rs_bool int_64_zero_q( INT_64 a );
rs_bool int_64_eq( INT_64 a, INT_64 b );

int int_64_cmp( INT_64 a, INT_64 b );
rs_bool int_64_gt( INT_64 a, INT_64 b );
rs_bool int_64_ge( INT_64 a, INT_64 b );
#define int_64_lt(a,b) int_64_gt(b,a)
#define int_64_le(a,b) int_64_ge(b,a)

IEEE_64 int_64_to_float( INT_64 a );
INT_32 int_64_to_int_32( INT_64 a );
INT_64 int_32_to_int_64( INT_32 a );
INT_64 float_to_int_64( IEEE_64 a );   /* trunc */

#else
#define int_64_add(a,b) ((a)+(b))
#define int_64_sub(a,b) ((a)-(b))
#define int_64_mul(a,b) ((a)*(b))
#define int_64_quotient(a,b) ((a)/(b))
#define int_64_modulo(a,b) MOD(a,b)
#define int_64_remainder(a,b) REMDR(a,b)
#define int_64_and(a, b) ((a) & (b))
#define int_64_or(a, b) ((a) | (b))
#define int_64_xor(a, b) ((a) ^ (b))
#define int_64_not(a) (~(a))
#define int_64_ashr(a, b) (((signed INT_64)(a))>>(b))
#define int_64_lshr(a, b)  (((unsigned INT_64)(a))>>(b))
#define int_64_shl(a, b)   ((a)<<(b))

#define int_64_neg(a) (-(a))
#define int_64_neg_q(a) (((a)<0)?YES:NO)
#define int_64_zero_q(a) (((a)==0)?YES:NO)
#define int_64_eq(a,b) (rb_to_bo((a)==(b)))
#define int_64_gt(a,b) (rb_to_bo((a)>(b)))
#define int_64_ge(a,b) (rb_to_bo((a)>=(b)))
#define int_64_lt(a,b) (rb_to_bo((a)<(b)))
#define int_64_le(a,b) (rb_to_bo((a)<=(b)))

#define int_64_to_int_32(x)   ((INT_32)(x))
#define int_32_to_int_64(x)   ((INT_64)(x))
#define int_64_to_float(a)    ((IEEE_64)(a))
#define float_to_int_64(x)    ((INT_64)(x))
#endif

#define fx2int64(x)  int_32_to_int_64(fx2int(x))

obj int_64_compact( INT_64 a );

rs_bool int_64_fit_in_30_q( INT_64 a );
rs_bool int_64_fit_in_32_q( INT_64 a );

obj make_long_int( INT_64 a );

#define extract_int_64(longint) (*((INT_64 *)PTR_TO_DATAPTR(longint)))

/* fills up buffer right-to-left!  buffer starts out pointing
   to the END of the buffer! */

char *int_64_to_string( char *buffer, INT_64 value, unsigned radix );
rs_bool string_to_int_64( char *str, unsigned len, unsigned radix, INT_64 *v );

#endif /* _H_RS_LONGINT */
