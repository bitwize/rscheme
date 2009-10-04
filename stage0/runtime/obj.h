/*-----------------------------------------------------------------*-C-*---
 * File:    handc/runtime/obj.h
 *
 *          Copyright (C)1997 Donovan Kolbly <d.kolbly@rscheme.org>
 *          as part of the RScheme project, licensed for free use.
 *          See <http://www.rscheme.org/> for the latest information.
 *
 * File version:     1.18
 * File mod date:    2003-06-22 18:15:02
 * System build:     v0.7.3.4-b7u, 2007-05-30
 *
 * Purpose:          Scheme object type definition, and immob structure
 *------------------------------------------------------------------------*/

#ifndef _H_RSCHEME_OBJ
#define _H_RSCHEME_OBJ

#ifdef INLINES
#define CI_DECL static inline
#define CIH_DECL static inline
#else
#define CI_DECL /* nothing */
#define CIH_DECL /* nothing */
#endif


/*
    WARNING:  Don't think that just because we have defines here
    	      that you can change the values and it will still work.
	      At least some of these values are frozen rock-solid.
*/


#ifdef RS_NEW_BUILD
#include <rscheme/config.h>
#else
#include <rscheme/platform.h>
#include <rscheme/buildsty.h>  /* get build style flags */
#endif

#include <assert.h>

/*  declare the C types for the root classes  */

#ifdef GNU_VINSNS
typedef void *jump_addr;
#else
typedef void *(*jump_addr)( void );
#endif


/*  constructor exprs are useful for static type checking,
 *  but it's not portable
 *
 *  the data type is `obj' and the low-level operations
 *  are convert 32-bit int to one of them,
 *  convert one to a 32-bit unsigned int (VAL)
 *              or a 32-bit signed int (IVAL)
 */

#ifdef USE_CONSTRUCTOR_EXPRS

typedef struct _obj { UINT_32 v; } obj;

#define VAL(x)			((x).v)
#define IVAL(x)			((INT_32)(x).v)
#define OBJ(x)			((obj){x})

#else

typedef UINT_32 obj;

#define VAL(x)			(x)
#define IVAL(x)			((INT_32)(x))
#define OBJ(x)			(x)

#endif

typedef enum {
  NO = 0,
  YES = !NO
} rs_bool;

/*typedef bool raw_bool;*/

/*
 *  macros for converting 32-bit values to and from "network" (big-endian)
 *  format are here recast to convert obj values
 */

#define BIG_ENDIAN_TO_HOST_OBJ(x)	OBJ(BIG_ENDIAN_TO_HOST_32(VAL(x)))
#define HOST_TO_BIG_ENDIAN_OBJ(x)	OBJ(HOST_TO_BIG_ENDIAN_32(VAL(x)))

#define EQ(x,y)			(VAL(x)==VAL(y)?YES:NO)
#define RB_NOT(x)		((x)?NO:YES)

/*
 *   define the tag system
 */

/*
   NOTE:  The only reason we are using `+' instead of `|' herein
          (since these are really bit-wise operations) is because
	  it's more likely that an optimizer will fold + than |
*/

#define PRIMARY_TAG_SIZE	(2)
#define SECONDARY_TAG_SIZE	(3)

#define COMBINED_TAG_SIZE	(PRIMARY_TAG_SIZE + SECONDARY_TAG_SIZE)
#define PRIMARY_TAG_MASK	((1<<PRIMARY_TAG_SIZE)-1)
#define COMBINED_TAG_MASK	((1<<COMBINED_TAG_SIZE)-1)

#define SECONDARY_TAG_MASK	(COMBINED_TAG_MASK & ~PRIMARY_TAG_MASK)

/*  define the tag values  */

/*  NOTE:  Because insns on some platforms (e.g., NeXT Motorola)
           are 16-bit aligned,
           the two even tags are used for immediate objects, so that
	   code pointers look like immobs instead of pointers.

	   some platforms are not even so nice (TURBO C), and code
	   can start on any byte boundary.
*/

enum primary_tag {
    FIXNUM_TAG = 0,
    __unused_tag,		/* reserved for future use */
    IMMOB_TAG,
    POINTER_TAG
};

enum secondary_tag {
    BOOLEAN_TAG = 0,
    NIL_TAG,
    ASCII_CHAR_TAG,
    UNICODE_CHAR_TAG,
    UNIQUE_OBJ_TAG,

    SPARE_3_TAG = 5,
    SPARE_2_TAG = 6,
    SPARE_1_TAG = 7
};

enum unique_object_tag {
    NOVALUE_TAG,
    UNDEFINED_TAG,
    UNINITIALIZED_TAG,
    UNBOUND_TAG,
    REST_TAG,
    KEY_TAG,
    ALL_KEYS_TAG,
    NEXT_TAG,
    MISSING_TAG,    /* used to indicate a missing object at load time */
    DEBUG_TRAP_TAG, /* low-level trap if read or overwritten w/o `init' */
    UNMAPPED_OBJ_TAG
    }; 

#define TEST_PRIMARY_TAG(x,t)	  ((VAL(x) & PRIMARY_TAG_MASK) == t)

#define OBJ_ISA_FIXNUM(x)	  TEST_PRIMARY_TAG( x, FIXNUM_TAG )
#define OBJ_ISA_IMMOB(x)	  TEST_PRIMARY_TAG( x, IMMOB_TAG )
#define OBJ_ISA_PTR(x)	  	  TEST_PRIMARY_TAG( x, POINTER_TAG )

#define SECONDARY_TAG(x)          (enum secondary_tag)\
                                        ((VAL(x) & SECONDARY_TAG_MASK)\
                                                      >> PRIMARY_TAG_SIZE)


/************************* FIXNUMs *************************/

#define MAKE_FIXNUM(x)		OBJ(FIXNUM_TAG \
					+ ((UINT_32)(x) << PRIMARY_TAG_SIZE))

#define FIXNUM_TO_RAWINT(x)	(IVAL(x) >> PRIMARY_TAG_SIZE)
#define RAWINT_TO_FIXNUM(x)	MAKE_FIXNUM(x)
#define ZERO			MAKE_FIXNUM(0)
#define ADD1(x)			OBJ(IVAL(x)+(1<<PRIMARY_TAG_SIZE))
#define SUB1(x)			OBJ(IVAL(x)-(1<<PRIMARY_TAG_SIZE))
#define MUL2(x)			OBJ(IVAL(x)<<1)
#define DIV2(x)			OBJ((IVAL(x)>>1) & ~PRIMARY_TAG_MASK)


#define FX_LT(x,y)		(IVAL(x)<IVAL(y))
#define FX_LE(x,y)		(IVAL(x)<=IVAL(y))
#define FX_GT(x,y)		(IVAL(x)>IVAL(y))
#define FX_GE(x,y)		(IVAL(x)>=IVAL(y))

#define FX_ADD(x,y)		OBJ(IVAL(x)+IVAL(y))
#define FX_SUB(x,y)		OBJ(IVAL(x)-IVAL(y))
#define FX_MUL(x,y)		OBJ(IVAL(x)*fx2int(y))
#define FX_DIV(x,y)		int2fx(IVAL(x)/IVAL(y))

#define FX_AND(x,y)		OBJ(VAL(x)&VAL(y))
#define FX_OR(x,y)		OBJ(VAL(x)|VAL(y))
#define FX_XOR(x,y)		OBJ((VAL(x)^VAL(y)) + FIXNUM_TAG)
#define FX_NOT(x)		OBJ(~VAL(x)-3)

/* nb. the `amt' arg is a raw-int */

#define FX_SHL(x,amt)       	OBJ(IVAL(x)<<(amt))
#define FX_ASHR(x,amt)		OBJ((IVAL(x)>>(amt)) & ~PRIMARY_TAG_MASK)
#define FX_LSHR(x,amt)		OBJ((VAL(x)>>(amt)) & ~PRIMARY_TAG_MASK)

#if WORD_IS_32_BITS
#define RIBYTES_TO_FXWORDS(x)	OBJ(x)
#define FXWORDS_TO_RIBYTES(x)	VAL(x)
#else
#define RIBYTES_TO_FXWORDS(x)	OBJ((x)/(SLOT(1)>>PRIMARY_TAG_SIZE))
#define FXWORDS_TO_RIBYTES(x)	(VAL(x)*(SLOT(1)>>PRIMARY_TAG_SIZE))
#endif

#define fx2int(x)		FIXNUM_TO_RAWINT(x)
#define int2fx(x)		RAWINT_TO_FIXNUM(x)

/************************* IMMOBs *************************/

/* TEST_SECONDARY_TAG implicitly tests the primary tag for IMMOB_TAG */

#define TEST_SECONDARY_TAG(x,t)	  ((VAL(x) & \
				     (SECONDARY_TAG_MASK|PRIMARY_TAG_MASK))\
					== ((t << PRIMARY_TAG_SIZE)\
					    |IMMOB_TAG))

#define OBJ_ISA_BOOLEAN(x)	  TEST_SECONDARY_TAG( x, BOOLEAN_TAG )
#define OBJ_ISA_NIL(x)	  	  TEST_SECONDARY_TAG( x, NIL_TAG )
#define OBJ_ISA_ASCII_CHAR(x)	  TEST_SECONDARY_TAG( x, ASCII_CHAR_TAG )
#define OBJ_ISA_UNICODE_CHAR(x)   TEST_SECONDARY_TAG( x, UNICODE_CHAR_TAG )
#define OBJ_ISA_UNIQUE_OBJ(x)	  TEST_SECONDARY_TAG( x, UNIQUE_OBJ_TAG )

/*** new names for predicates ***/

#define BOOLEAN_P(x)            OBJ_ISA_BOOLEAN(x)
#define BYTE_CHAR_P(x)          OBJ_ISA_ASCII_CHAR(x)
#define UNICODE_CHAR_P(x)       OBJ_ISA_UNICODE_CHAR(x)
#define FIXNUM_P(x)             OBJ_ISA_FIXNUM(x)
#define PTR_P(x)                OBJ_ISA_PTR(x)

#define MAKE_IMMOB(t,x)		OBJ((IMMOB_TAG \
					+ ((UINT_32)(t) << PRIMARY_TAG_SIZE)) \
				  	+ ((UINT_32)(x) << COMBINED_TAG_SIZE))

#define TRUE_OBJ		(MAKE_IMMOB( BOOLEAN_TAG, 1 ))
#define FALSE_OBJ		(MAKE_IMMOB( BOOLEAN_TAG, 0 ))
#define NIL_OBJ			(MAKE_IMMOB( NIL_TAG, 0 ))

#define NOT(bo)                 (EQ((bo),FALSE_OBJ))
#define truish(bo)              ((!EQ((bo),FALSE_OBJ))?YES:NO)
#define rb_to_bo(rb)            ((rb)?TRUE_OBJ:FALSE_OBJ)

#define IMMOB_TO_FX(imm)        OBJ(VAL(imm)-IMMOB_TAG+FIXNUM_TAG)
#define FX_TO_IMMOB(imm)        OBJ(VAL(imm)+IMMOB_TAG-FIXNUM_TAG)

#define MAKE_UNIQ_OBJ(x)        (MAKE_IMMOB( UNIQUE_OBJ_TAG, x ))
#define UNDEFINED_OBJ		(MAKE_UNIQ_OBJ( UNDEFINED_TAG ))
#define NOVALUE_OBJ		(MAKE_UNIQ_OBJ( NOVALUE_TAG ))
#define UNINITIALIZED_OBJ	(MAKE_UNIQ_OBJ( UNINITIALIZED_TAG ))
#define UNBOUND_OBJ		(MAKE_UNIQ_OBJ( UNBOUND_TAG ))
#define KEY_OBJ                 (MAKE_UNIQ_OBJ( KEY_TAG ))
#define REST_OBJ                (MAKE_UNIQ_OBJ( REST_TAG ))
#define DEBUG_TRAP_OBJ          (MAKE_UNIQ_OBJ( DEBUG_TRAP_TAG ))
#define UNMAPPED_OBJ            (MAKE_UNIQ_OBJ( UNMAPPED_OBJ_TAG ))

#define MAKE_ASCII_CHAR(ch)	(MAKE_IMMOB( ASCII_CHAR_TAG, ch ))
#define MAKE_UNICODE_CHAR(ch)	(MAKE_IMMOB( UNICODE_CHAR_TAG, ch ))

#define GET_IMMEDIATE_VALUE(x)	(VAL(x) >> COMBINED_TAG_SIZE)
#define ASCII_CHAR_VALUE(x)     (GET_IMMEDIATE_VALUE(x) & 0xFF)
#define UNICODE_CHAR_VALUE(x)   (GET_IMMEDIATE_VALUE(x) & 0xFFFF)

/*  breaking obj's into two halves, each of which will be
 *  a fixnum, and reassembling them... 
 */

#define HALF_WORD_MASK          ((1UL<<(WORD_SIZE_BITS/2))-1)
#define OBJ_HIGH_HALF_FX(x)     int2fx( HALF_WORD_MASK & \
					(VAL(x) >> (WORD_SIZE_BITS/2) ))
#define OBJ_LOW_HALF_FX(x)      int2fx( HALF_WORD_MASK & VAL(x) )

#define OBJ_FROM_HI_LO(hi,lo)   OBJ(((fx2int(hi) & HALF_WORD_MASK) \
				     << (WORD_SIZE_BITS/2)) \
				    + (fx2int(lo) & HALF_WORD_MASK))

#ifndef __TURBOC__
#define JUMP_ADDR_TO_OBJ(x)	OBJ((UINT_32)(x))
#define OBJ_TO_JUMP_ADDR(x)	((jump_addr)VAL(x))
#else
#define JUMP_ADDR_TO_OBJ(x)	OBJ((((INT_32)(x))-((INT_32)main))<<2)
#define OBJ_TO_JUMP_ADDR(x)	((jump_addr)((IVAL(x)>>2)+((INT_32)main)))
#endif

#define RAW_PTR_TO_OBJ(x)	OBJ((UINT_32)(x))
#define OBJ_TO_RAW_PTR(x)	((void *)VAL(x))

#ifdef __GNUC__
     /*  if we're using GNU C, we can use an expression-block to
      *  verify at compile-time the type of the argument `x', by
      *  assigning it sans cast to a temporary.
      *  However, we mark it const in case the x is really const.
      */
#define C_PTR_TO_OBJ(t,x)       ({ const t _temp = (x); \
				   RAW_PTR_TO_OBJ(_temp); })
#else
#define C_PTR_TO_OBJ(t,x)       RAW_PTR_TO_OBJ(x)
#endif
#define OBJ_TO_C_PTR(t,x)       ((t)OBJ_TO_RAW_PTR(x))

/* x/4 written as a macro so we can define a primop for it
   this does NOT operate on fixnums
*/

#define RAW_DIV4(x)		((x)/4)

obj class_of( obj item );

#include <rscheme/longint.h>
/*
 *   Get local definitions of MOD and REMDR
 */

#ifndef RS_NEW_BUILD
#include <rscheme/modulo.h>
#endif

#if !HAVE_MEMCPY
#define memcpy(dst,src,len)  (bcopy((src),(dst),(len)))
#endif

/* the details of this representation is unspecified (and left to osglue.c) */

struct RSTime {
  INT_32   rstime[2];
};

#endif
