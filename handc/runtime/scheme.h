/*-----------------------------------------------------------------*-C-*---
 * File:    handc/runtime/scheme.h
 *
 *          Copyright (C)1997 Donovan Kolbly <d.kolbly@rscheme.org>
 *          as part of the RScheme project, licensed for free use.
 *          See <http://www.rscheme.org/> for the latest information.
 *
 * File version:     1.57
 * File mod date:    2005-09-16 10:16:32
 * System build:     v0.7.3.4-b7u, 2007-05-30
 *
 * Purpose:          api interface functions and root vector
 *------------------------------------------------------------------------*/

/*
 *   Application interface to standard scheme data structures
 *   and RScheme extensions
 *
 *  
*/

#ifndef _H_RSCHEME_SCHEME
#define _H_RSCHEME_SCHEME

#include <rscheme/obj.h>
#include <stdio.h>
#include <math.h>       /* we use raw cos() etc. as primops */

/*
 *  raise an exception
 */

_rs_volatile void scheme_error( const char *msg, unsigned num_args, ... );
_rs_volatile void raise_exception( int type, int argc, ... );
_rs_volatile void type_check_error( obj thing, 
				    const char *class_name, 
				    int fplace_code );
/* supplies the `stack' property if *capture-stack-on-conditions* is !#f */
_rs_volatile void raise_error( obj err_object );
void rs_crash_dump_vmstate( FILE *f );     /* dump vm state */


#define NUM_RSCHEME_GLOBALS (60)

extern obj rscheme_global[NUM_RSCHEME_GLOBALS];

/*  these are the "well-known" objects
 *  they are initialized from a vector in the
 *  boot image
 */

#define boot_image              rscheme_global[0]  /* *IS* the bootvec */
#define boot_args               rscheme_global[1]  /* boot arglist */
#define symbol_table 		rscheme_global[2]
#define interrupt_handlers	rscheme_global[3]
#define exception_handler_proc	rscheme_global[4]  /* scheme_error(), etc. */
#define dequeue_class	        rscheme_global[5]
#define continue_intr_tmpl	rscheme_global[6]

#define class_class 		rscheme_global[7]

#define pair_class 		rscheme_global[8]
#define vector_class 		rscheme_global[9]
#define string_class 		rscheme_global[10]
#define symbol_class 		rscheme_global[11]

#define closure_class 		rscheme_global[12]
#define template_class		rscheme_global[13]
#define partcont_class		rscheme_global[14]
#define binding_envt_class      rscheme_global[15]

#define tlv_class		rscheme_global[16]
#define byte_vector_class	rscheme_global[17]

#define boolean_class 		rscheme_global[18]
#define nil_class 		rscheme_global[19]
#define ascii_char_class 	rscheme_global[20]
#define unicode_char_class 	rscheme_global[21]
#define unique_obj_class 	rscheme_global[22]
#define fixnum_class 		rscheme_global[23]

#define double_float_class	rscheme_global[24]
#define function_class 		rscheme_global[25]

#define spare_1_class 	rscheme_global[26]         /* <date> */
#define spare_2_class 	rscheme_global[27]
#define spare_3_class 	rscheme_global[28]

#define install_dir             rscheme_global[29] /* filled in at runtime */
#define allocation_area_class   rscheme_global[30]
#define default_alloc_area      rscheme_global[31] /* filled in at patchtime */
#define long_int_class          rscheme_global[32]
#define finalizing_list         rscheme_global[33]
#define unicode_string_class    rscheme_global[34]
#define os_error_class          rscheme_global[35]
#define condition_class         rscheme_global[36]
#define type_check_failed_class rscheme_global[37]
#define function_place_class    rscheme_global[38]
#define return_from_call_scheme_template rscheme_global[39]
#define c_signal_names          rscheme_global[40] /* filled in at runtime */
#define load_cache_and_call_proc rscheme_global[41]
#define gf_dispatch_template    rscheme_global[42]

#define mp_data_class           rscheme_global[43]
#define rect_complex_class      rscheme_global[44]
#define mp_rational_class       rscheme_global[45]
#define bignum_class            rscheme_global[46]

#define condition_stack_class   rscheme_global[47]
#define capture_stack_var       rscheme_global[48]
#define max_machine_int         rscheme_global[49]
#define min_machine_int         rscheme_global[50]

#define profiler_name_map       rscheme_global[51]

#define rscheme_global_ref(offset) rscheme_global[(offset)/sizeof(obj)]
#define rscheme_global_set(offset,expr) (rscheme_global[(offset)/sizeof(obj)]\
					 = (expr))

/***************** profiling *****************/
/*  functions for general-purpose (user) profiling */

void rsprof_start( const char *path, rs_bool append_q, rs_bool start_now_q );
rs_bool rsprof_stop( void );
void rsprof_collect_objects( obj setup, obj otbl );
void rsprof_app_defn_rec( obj key, obj val );

/*
 *  functions and macros to support profiling the system
 *  intended for my personal research use
 */

#ifdef RS_PROFILE

void rs_profile_init( void );
void rs_profile_close( void );

void rs_profile2( const char *label, UINT_32 value_1, UINT_32 value_2 );
#else
#define rs_profile2(label,v1,v2) 0 /* do nothing */
#endif

#define rs_profile1(label,val) rs_profile2(label,val,0)
#define rs_profile0(label) rs_profile2(label,0,0)

/***************** arithmetic & math *****************/
/*
 *  these builtin operations handle arithmetic on
 *  the basic number system, which is only <fixnum> 
 *  and <double-float>
 */

obj basic_plus( obj a, obj b );
obj basic_minus( obj a, obj b );
obj basic_mul( obj a, obj b );
obj basic_div( obj a, obj b );

obj basic_and(obj a, obj b);
obj basic_or(obj a, obj b);
obj basic_xor(obj a, obj b);
obj basic_not(obj a);

obj basic_bitwise_and( obj x, obj y );
obj basic_bitwise_or( obj x, obj y );
obj basic_bitwise_xor( obj x, obj y );
obj basic_bitwise_not( obj x );

obj basic_lshr(obj a, int b);
obj basic_ashr(obj a, int b);
obj basic_lshl(obj a, int b);
obj basic_ashl(obj a, int b);

obj basic_quotient(obj a, obj b);
obj basic_remainder(obj a, obj b);
obj basic_modulo(obj a, obj b);

int basic_cmp( obj a, obj b );

#define basic_gt_q(a,b) ((basic_cmp(a,b)>0)?YES:NO)
#define basic_ge_q(a,b) ((basic_cmp(a,b)>=0)?YES:NO)
#define basic_lt_q(a,b) ((basic_cmp(a,b)<0)?YES:NO)
#define basic_le_q(a,b) ((basic_cmp(a,b)<=0)?YES:NO)
#define basic_eq_q(a,b) ((basic_cmp(a,b)==0)?YES:NO)
#define basic_ne_q(a,b) ((basic_cmp(a,b)!=0)?YES:NO)

CIH_DECL INT_32 basic_raw_int( obj a );
CIH_DECL IEEE_64 basic_raw_float( obj a );
CIH_DECL UINT_32 basic_raw_uint( obj a );

int basic_raw_int_conv( obj a );
UINT_32 basic_raw_uint_conv( obj a );

double basic_raw_float_conv( obj a );

CIH_DECL obj uint_32_compact( UINT_32 a );
obj uint_32_big( UINT_32 a );   /* for uint's that don't fit in a fixnum */

/*
 *  Note:  All (basic) numbers should be convertible
 *         to one or the other
 */

int basic_raw_int_conv_p( obj a );           /* is the type convertible? */
int basic_raw_float_conv_p( obj a );         /* is the type convertible? */

obj basic_num_to_string_obj( obj a, unsigned radix );

obj string_to_rational_obj( char *str, unsigned radix );
obj string_to_bignum_obj( char *str, unsigned radix );
obj bignum_to_string_obj( obj num, unsigned radix );

/* Warning: this procedure fills up the buffer in reverse... */
char *fixnum_to_string( char *buffer, obj value, unsigned radix );

/***************** miscellaneous *****************/

CIH_DECL rs_bool NULL_P( obj thing );
rs_bool TEMPLATE_P( obj thing );
rs_bool BYTE_VECTOR_P( obj thing );

CIH_DECL rs_bool LONGFLOAT_P( obj thing );
IEEE_64 extract_float( obj longfloat );
obj make_float( IEEE_64 longfloat );
obj float_truncate( IEEE_64 longfloat );
IEEE_64 get_float( obj arg, const char *fn, const char *arg_name );

char *procedure_name( obj tmpl );
obj fluid_assq( obj key );
obj addr_to_name( obj tmpl, obj addr );
obj clone( obj thing );
obj clone2( obj thing, obj new_class );

obj all_instances( obj of_class );
obj all_pointers_to( obj an_instance );
void gc_now( void );
void gc_work( UINT_32 work_amount );
obj get_gc_cycle_id( void );

/***************** functions *****************/

CIH_DECL rs_bool FUNCTION_P( obj thing );
CIH_DECL obj make_closure( obj envt, obj tmpl );
obj template_scope( obj tmpl );

/***************** partial continuations *****************/

/* build a <condition-stack> w/current state;
 * stored in <condition> objects
 */

obj make_exception_stack( void );

/***************** pairs *****************/

CIH_DECL rs_bool PAIR_P( obj thing );
CIH_DECL obj cons( obj car, obj cdr );
CIH_DECL obj pair_car( obj pair );
CIH_DECL obj pair_cdr( obj pair );

#define pair_set_car(pair,item) gvec_write(pair,SLOT(0),item)
#define pair_set_cdr(pair,item) gvec_write(pair,SLOT(1),item)

/***************** vectors *****************/

rs_bool VECTOR_P( obj thing );
obj make_empty_vector( UINT_32 length );
obj vector_ref( obj vector, obj index );
obj vector_set( obj vector, obj index, obj value );
obj subvector( obj vector, obj index, obj limit );

/***************** strings *****************/

CIH_DECL rs_bool STRING_P( obj thing );

CIH_DECL char *string_text( obj str );
CIH_DECL UINT_32 string_length( obj str );
void ensure_string_mapped( obj str );/* make sure string's pages are mapped */
void ensure_memory_mapped( void *base, UINT_32 len );

rs_bool string_eq( obj str1, obj str2 );
rs_bool string_ci_eq( obj str1, obj str2 );
int string_cmp( obj str1, obj str2 );
int string_ci_cmp( obj str1, obj str2 );
obj make_string( const char *text );
obj string_ref( obj str, UINT_32 index );
void string_set( obj str, UINT_32 index, obj ch );

void bvec_copy(obj dst, INT_32 dst_offset,
	       obj src, INT_32 src_offset, INT_32 len );
obj bvec_hash( obj bvec, INT_32 offset, INT_32 len );
obj bvec_ci_hash( obj bvec, INT_32 offset, INT_32 len );
obj rs_string_search( obj str, obj seek, INT_32 skipn );

#define byte_string_length(s) (SIZEOF_PTR(s)-1)
#define byte_string_text(s)  ((UINT_8 *)PTR_TO_DATAPTR(s))

#define unicode_string_length(str) ((SIZEOF_PTR(str)/2)-1)
#define unicode_string_text(str) ((UINT_16*)(PTR_TO_DATAPTR(str)))

#define BYTE_STRING_P(s) STRING_P(s)
#define UNICODE_STRING_P(s) OBJ_ISA_PTR_OF_CLASS(s,unicode_string_class)

/***************** symbols *****************/

rs_bool SYMBOL_P( obj thing );
obj intern( obj str );
obj symbol_hash( obj symbol );
const char *symbol_text( obj symbol );
obj symbol_str( obj symbol );

obj lookup_symbol( const char *str );

/***************** top-level vars *****************/

CIH_DECL rs_bool TLV_P( obj thing );

CIH_DECL obj tlv_value( obj tlv );
obj tlv_name( obj tlv );  /* not necessarily available (returns #f if not) */

CIH_DECL void tlv_set_value( obj tlv, obj value );

/***************** classes *****************/

CIH_DECL obj object_class( obj thing );
CIH_DECL rs_bool CLASS_P( obj thing );
rs_bool class_is_gvec( obj a_class );
obj class_name( obj a_class );
CIH_DECL obj class_supers( obj a_class );
CIH_DECL obj class_category( obj a_class );
unsigned class_image_mode( obj a_class );

/* returns YES for indirect instances/subclasses as well */

CIH_DECL rs_bool instance_p( obj thing, obj a_class );
CIH_DECL rs_bool subclass_p( obj class1, obj class2 );
CIH_DECL rs_bool instance_cat_p( obj thing, obj a_category );
CIH_DECL rs_bool in_category_p( obj a_class, obj a_category );

#define FUNCTION_CATEGORY  (int2fx(1))
#define CLASS_CATEGORY     (int2fx(2))

/***************** dequeues *****************/

obj make_dequeue( void );
rs_bool dequeue_empty( obj deq );
obj dequeue_count( obj deq );
obj dequeue_state( obj deq );
void dequeue_push_back( obj deq, obj item );
void dequeue_push_front( obj deq, obj item );
obj dequeue_pop_back( obj deq );
obj dequeue_pop_front( obj deq );
obj dequeue_ref( obj deq, obj index );           /* index is a <fixnum> */
obj dequeue_set( obj deq, obj index, obj item ); /* return old item at index */
int dequeue_delq( obj deq, obj item );          /* return # items deleted */
obj dequeue_memq( obj deq, obj item );          /* return #f or fixnum index */

/**************** type checking **************/

#include <rscheme/chektype.h>

/** INITIALIZATION **/

void init_runtim( void );
obj load_initial_heap( const char *path, rs_bool verbose );
void init_math( void );
void init_math2( void );

#ifdef INLINES
#include <rscheme/interim.ci>
#endif

#include <rscheme/hashmain.h>
#include <rscheme/hashfn.h>
#include <rscheme/stdiox.h>
#include <rscheme/allocns.h>
#include <rscheme/osglue.h>

/*  handy, but almost obsolete, low-level debugging fns.
 *
 *  (these functions print a subset of scheme data sensibly, but are
 *  very useful from, say, gdb, or when the scheme runtime is wedged)
 *
 *  !!!fprinto should NOT be considered a usual way to render things!!!
 *
 */

int snprinto( char *dest, obj item, unsigned len );
void fnprinto( FILE *dest, obj item, unsigned len );
void fprinto( FILE *dest, obj item );
void debug( obj item );
void debug_slots( obj item );  /* print the slots, too */
void fdebug_slots( FILE *dest, obj item );

/*  define this to provide a SIGUSR-based leak detection tool
    #define SIGUSR_HOOKS

*/

void rscheme_intr_call0( obj thunk );
void rscheme_intr_call1( obj proc, obj arg );
void rscheme_intr_call2( obj proc, obj arg1, obj arg2 );

#endif /* _H_RSCHEME_SCHEME */
