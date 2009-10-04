/*-----------------------------------------------------------------*-C-*---
 * File:    handc/runtime/interim.c
 *
 *          Copyright (C)1997 Donovan Kolbly <d.kolbly@rscheme.org>
 *          as part of the RScheme project, licensed for free use.
 *          See <http://www.rscheme.org/> for the latest information.
 *
 * File version:     1.23
 * File mod date:    2006-02-10 19:47:31
 * System build:     v0.7.3.4-b7u, 2007-05-30
 *
 * Purpose:          miscellaneous accessors, setters & utility code
 *------------------------------------------------------------------------*/

#include <rscheme/runtime.h>
#include <rscheme/hashmain.h>
#include <rscheme/hashfn.h>
#include <rscheme/gcserver.h>
#include <string.h>
#include "vinsns.h"
#include <ctype.h>
#include "regs.h"

#ifndef INLINES
#include "interim.ci"
#include "chektype.ci"
#endif

/* #define CASE_INSENSITIVE */

obj rscheme_global[ NUM_RSCHEME_GLOBALS ];

/***************** miscellaneous *****************/

rs_bool TEMPLATE_P( obj thing )
{
    return PTR_ISA(thing,template_class);
}

rs_bool BYTE_VECTOR_P( obj thing )
{
    return PTR_ISA(thing,byte_vector_class);
}

IEEE_64 extract_float( obj thing )
{
    return *(IEEE_64 *)PTR_TO_DATAPTR(thing);
}

obj make_float( IEEE_64 x )
{
  obj f = bvec_alloc( sizeof(IEEE_64), double_float_class );
  *(IEEE_64 *)PTR_TO_DATAPTR(f) = x;
  return f;
}

IEEE_64 get_float( obj arg, const char *fn, const char *arg_name )
{
    if (OBJ_ISA_FIXNUM(arg))
	return fx2int(arg);
    else if (LONGFLOAT_P(arg))
	return extract_float(arg);
    scheme_error( "~a: Invalid argument `~a' ~s", 
		      3, 
		      make_string( fn ),
		      make_string( arg_name ),
		      REG0 );
    return 0.0;
}

obj fluid_assq( obj key )
{
obj s = dynamic_state_reg;
obj c;

    while (PAIR_P(s))
    {
	c = pair_car(s);
	if (PAIR_P(c) && EQ(pair_car(c),key))
	    return c;
	s = pair_cdr(s);
    }
    return FALSE_OBJ;
}


/***************** vectors *****************/

rs_bool VECTOR_P( obj thing )
{
    return PTR_ISA(thing,vector_class);
}

obj make_empty_vector( UINT_32 length )
{
    return gvec_alloc( length, vector_class );
}

static UINT_32 vcheck( obj vector, obj index, const char *op )
{
  UINT_32 k;

  if (!OBJ_ISA_PTR_OF_CLASS( vector, vector_class )) {
    type_check_error( vector, "<vector>", 0 );
  }

  k = basic_raw_uint( index );

  if (SLOT(k) >= SIZEOF_PTR(vector)) {
    scheme_error( "~a: index ~d out of range [0,~d)",
                  3,
                  make_string( op ),
                  index,
                  RIBYTES_TO_FXWORDS(SIZEOF_PTR(vector)) );
  }
  return k;
}

obj vector_ref( obj vector, obj index )
{
  UINT_32 k = vcheck( vector, index, "vector-ref" );
  return gvec_read( vector, SLOT(k) );
}

obj vector_set( obj vector, obj index, obj item )
{
  UINT_32 k = vcheck( vector, index, "vector-set" );
  gvec_write( vector, SLOT(k), item );
  return item;
}

/***************** symbols *****************/

rs_bool SYMBOL_P( obj thing )
{
    return PTR_ISA(thing,symbol_class);
}

/*
    Note that the hash value for a <Symbol>
    is the rehash of the hash value for the
    string mapping to the symbol
*/

obj intern( obj str )
{
obj hash = hash_string(str);
obj value;

    assert( STRING_P(str) );
    assert( OBJ_ISA_FIXNUM(hash) );

    value = stringtable_lookup( symbol_table, hash, str );
    if (EQ(value,FALSE_OBJ))
    {
      obj new_str;
      UINT_32 len = SIZEOF_PTR(str);

      new_str = bvec_alloc( len, string_class );
      memcpy( PTR_TO_DATAPTR(new_str), PTR_TO_DATAPTR(str), len );
      
      value = make2( symbol_class, new_str, rehash_fixnum( hash ) );
      hashtable_install( symbol_table, hash, new_str, value );
    }
    return value;
}

obj lookup_symbol( const char *str )
{
    return intern( make_string(str) );
}

obj symbol_hash( obj symbol )
{
    assert( SYMBOL_P(symbol) );
    return gvec_read( symbol, SLOT(1) );
}

const char *symbol_text( obj symbol )
{
    assert( SYMBOL_P(symbol) );
    return string_text( gvec_read(symbol,SLOT(0)) );
}

obj symbol_str( obj symbol )
{
    assert( SYMBOL_P(symbol) );
    return gvec_read(symbol,SLOT(0));
}


/***************** top-level environments & vars *****************/

obj tlv_name( obj tlv )
{
    assert( TLV_P(tlv) );
    return gvec_read( tlv, SLOT(0) );
}

void signal_tlv_unbound( obj tlv )
{
  raise_exception( 1, 2, literals_reg, tlv );
}

/***************************** INITIALIZATION *****************************/

void init_runtim( void )
{
  init_smemory();
#ifdef RS_PROFILE
  rs_profile_init();
#endif
  init_stack_cache();
}


#ifdef PLATFORM_IS_LITTLE_ENDIAN
#include "ntohd.ci"
#endif


#if !HAVE_STRERROR
char *strerror( int rc )
{
  static char msg[20];

  sprintf( msg, "error %d", rc );
  return msg;
}
#endif


#ifdef RS_PROFILE

static FILE *fprof = NULL;

void rs_profile_close( void )
{
  fclose( fprof );
}

void rs_profile_init( void )
{
  fprof = popen( "/home/donovan/rscheme/rsprof > /tmp/rsprof.out", "w" );
  /* fprof = fopen( "/tmp/rs.prof", "w" ); */
  if (!fprof)
    {
      fprintf( stderr, "could not open profile file/pipe\n" );
      exit(1);
    }
}

void rs_profile2( const char *label, UINT_32 value_1, UINT_32 value_2 )
{
  fprintf( fprof, "%lu %lu %s\n", value_1, value_2, label );
}

#endif
