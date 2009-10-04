/*-----------------------------------------------------------------*-C-*---
 * File:    handc/runtime/chektype.h
 *
 *          Copyright (C)1997 Donovan Kolbly <d.kolbly@rscheme.org>
 *          as part of the RScheme project, licensed for free use.
 *          See <http://www.rscheme.org/> for the latest information.
 *
 * File version:     1.4
 * File mod date:    1997-11-29 23:10:50
 * System build:     v0.7.3.4-b7u, 2007-05-30
 *
 *------------------------------------------------------------------------*/


CIH_DECL obj check_ptr( obj thing, int fplace_code );
CIH_DECL obj check_immob( obj thing, int fplace_code );
CIH_DECL obj check_fixnum( obj thing, int fplace_code ); 
CIH_DECL obj check_gvec( obj thing, int fplace_code ); 
CIH_DECL obj check_bvec( obj thing, int fplace_code ); 
CIH_DECL obj check_pair( obj thing, int fplace_code ); 
CIH_DECL obj check_symbol( obj thing, int fplace_code ); 
CIH_DECL obj check_vector( obj thing, int fplace_code ); 
CIH_DECL obj check_function( obj thing, int fplace_code ); 
CIH_DECL obj check_class( obj thing, int fplace_code ); 
CIH_DECL obj check_string( obj thing, int fplace_code ); 
CIH_DECL obj check_double_float( obj thing, int fplace_code ); 
CIH_DECL obj check_long_int( obj thing, int fplace_code ); 
CIH_DECL obj check_ascii_char( obj thing, int fplace_code ); 
CIH_DECL obj check_unicode_char( obj thing, int fplace_code ); 
CIH_DECL obj check_boolean( obj thing, int fplace_code ); 
CIH_DECL obj check_instance( obj thing, obj req_class, int fplace_code ); 

#define CHECK_PTR(x)   check_ptr(x,FPLACE_CODE)
#define CHECK_IMMOB(x)   check_immob(x,FPLACE_CODE)
#define CHECK_FIXNUM(x)   check_fixnum(x,FPLACE_CODE)
#define CHECK_GVEC(x)   check_gvec(x,FPLACE_CODE)
#define CHECK_BVEC(x)   check_bvec(x,FPLACE_CODE)
#define CHECK_PAIR(x)   check_pair(x,FPLACE_CODE)
#define CHECK_SYMBOL(x)   check_symbol(x,FPLACE_CODE)
#define CHECK_VECTOR(x)   check_vector(x,FPLACE_CODE)
#define CHECK_FUNCTION(x)   check_function(x,FPLACE_CODE)
#define CHECK_CLASS(x)   check_class(x,FPLACE_CODE)
#define CHECK_STRING(x)   check_string(x,FPLACE_CODE)
#define CHECK_DOUBLE_FLOAT(x)   check_double_float(x,FPLACE_CODE)
#define CHECK_LONG_INT(x)   check_long_int(x,FPLACE_CODE)
#define CHECK_ASCII_CHAR(x)   check_ascii_char(x,FPLACE_CODE)
#define CHECK_UNICODE_CHAR(x)   check_unicode_char(x,FPLACE_CODE)
#define CHECK_BOOLEAN(x)   check_boolean(x,FPLACE_CODE)
#define CHECK_INSTANCE(x,c) check_instance(x,c,FPLACE_CODE)

#define checked_car(x) pair_car(CHECK_PAIR(x))
#define checked_cdr(x) pair_cdr(CHECK_PAIR(x))
