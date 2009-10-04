/*-----------------------------------------------------------------*-C-*---
 * File:    handc/hasht/hashfn.h
 *
 *          Copyright (C)1997 Donovan Kolbly <d.kolbly@rscheme.org>
 *          as part of the RScheme project, licensed for free use.
 *          See <http://www.rscheme.org/> for the latest information.
 *
 * File version:     1.7
 * File mod date:    2005-01-20 20:23:42
 * System build:     v0.7.3.4-b7u, 2007-05-30
 *
 *------------------------------------------------------------------------*/

#ifndef _H_HASHFN
#define _H_HASHFN

#include <rscheme/regs.h>

/* 
   These functions [will] assume that `bytes' is word-aligned
   and compute a hash function on an integral number of
   words.
   
   [Actually, for now, they use word operations up to the
   last full word, and then mask to strip out unused bytes.
   The hash function should be endian transparent, also
   (but raw_ci_bytes_hash is very keen on the ASCII char set)
*/

obj raw_bytes_hash( const void *bytes, unsigned length );
obj raw_ci_bytes_hash( const void *bytes, unsigned length );

obj obj_hash( obj x );
obj obj_hash2( obj x, obj y );
obj rehash_fixnum( obj h0 );

UINT_32 crc_hash( const void *bytes, UINT_32 length, int case_insense );
UINT_32 crc_hash_unicode( const UINT_16 *p, UINT_32 len );
UINT_32 crc_hash_int( UINT_32 num );
UINT_32 crc_hash_int2( UINT_32 x, UINT_32 y );

obj hash_unicode_string( obj u_str );

#define hash_string(str) raw_bytes_hash((const void *)string_text(str),\
					string_length(str))
#define hash_string_ci(str) raw_ci_bytes_hash((const void *)string_text(str),\
					string_length(str))

#define NNFIXNUM_MASK         (((1UL<<(WORD_SIZE_BITS-1))-1)&~PRIMARY_TAG_MASK)
#define UINT_32_TO_NNHASH(u)  OBJ((((u) & NNFIXNUM_MASK) + FIXNUM_TAG))

#define uint_hash(x)     UINT_32_TO_NNHASH( crc_hash_int( (x) ) )
#define uint2_hash(x,y)  UINT_32_TO_NNHASH( crc_hash_int2( (x), (y) ) )

#endif
