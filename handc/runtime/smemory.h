/*-----------------------------------------------------------------*-C-*---
 * File:    handc/runtime/smemory.h
 *
 *          Copyright (C)1997 Donovan Kolbly <d.kolbly@rscheme.org>
 *          as part of the RScheme project, licensed for free use.
 *          See <http://www.rscheme.org/> for the latest information.
 *
 * File version:     1.19
 * File mod date:    2003-10-13 13:02:16
 * System build:     v0.7.3.4-b7u, 2007-05-30
 *
 * Purpose:          scheme heap interface
 *------------------------------------------------------------------------*/

#ifndef _H_RSCHEME_SMEMORY
#define _H_RSCHEME_SMEMORY

#include <rscheme/obj.h>
#include <rscheme/gcserver.h>

typedef struct _POBHeader {
    UINT_32	pob_size;
    obj		pob_class;
} POBHeader;

#define SLOT(i) ((i)*sizeof(obj))

/*  Low-level storage allocators.  Return uninitialized bits.  */

CIH_DECL obj alloc( UINT_32 bytes, obj obj_class );
CIH_DECL obj alloc1( obj obj_class );
CIH_DECL obj alloc2( obj obj_class );
CIH_DECL obj alloc3( obj obj_class );
CIH_DECL obj alloc4( obj obj_class );
CIH_DECL obj alloc5( obj obj_class );
CIH_DECL obj alloc6( obj obj_class );
CIH_DECL obj alloc7( obj obj_class );
CIH_DECL obj alloc8( obj obj_class );
CIH_DECL obj alloc9( obj obj_class );

/*  Allocate a gvec filled with FALSE_OBJ's  */

CIH_DECL obj gvec_alloc( UINT_32 num_slots, obj obj_class );
obj make_gvec( obj the_class, UINT_32 size, obj fill );

/*  Allocate a bvec, with last word set to zeros */

CIH_DECL obj bvec_alloc( UINT_32 num_bytes, obj bvec_class );

/**
 *  Test whether a tagged value (`obj') has a pointer tag,
 *  and indeed is a pointer to a direct instance of the given 
 *  class
 */

#define OBJ_ISA_PTR_OF_CLASS(x,c) ((OBJ_ISA_PTR(x) \
				    && EQ(CLASSOF_PTR(x),c)) \
				  ? YES : NO)

/*
 *  note the `_P' is the (new) preferred way to indicate
 *  a macro which returns a <raw-bool> (aka rs_bool)
 */

#define PTR_IN_CATEGORY_P(x,c) ((OBJ_ISA_PTR(x) \
				 && in_category_p(CLASSOF_PTR(x),c)) \
				  ? YES : NO)

/***********************************************************************
 *
 *  This figure illustrates the relationship between the various
 *  kinds of pointers manipulated by the following macros and
 *  inline functions
 *
 * 
 *  POBHeader *     gc_obj_addr
 *  (a.k.a.          (a.k.a.
 *   HDRPTR)          GCPTR)
 *      |               |
 *      |               |         +----------+
 *      |               |         |    GC    |
 *      |               |         |  header  |
 *      +---------------+-------->+----------+
 *                                |   POB    |
 *                                |  header  |
 *              void *   -------->+----------+<--- obj
 *             (a.k.a.            |   data   |     w/PTR_TAG for low tag
 *             DATAPTR)           |          |     (a.k.a.
 *                                |          |     PTR)
 *
 *  Note that POBHeader *'s point to the same place as
 *  gc_obj_addr's; they are just of a more refined type. 
 */

#ifdef DEBUG_SMEMORY

CIH_DECL POBHeader *PTR_TO_HDRPTR( obj ptr );
CIH_DECL void *PTR_TO_DATAPTR( obj ptr );

CIH_DECL obj GCPTR_TO_PTR( gc_obj_addr x );
CIH_DECL obj HDRPTR_TO_PTR( POBHeader *hdr );
CIH_DECL gc_obj_addr PTR_TO_GCPTR( obj x );

CIH_DECL UINT_32 SIZEOF_PTR( obj ptr );
CIH_DECL obj CLASSOF_PTR( obj ptr );

#else

#define PTR_TO_HDRPTR( ptr )     ((POBHeader *)(VAL( ptr ) - POINTER_TAG \
					      - sizeof( POBHeader )))
#define PTR_TO_DATAPTR( ptr )    ((void *)(VAL( ptr ) - POINTER_TAG))

#define GCPTR_TO_PTR( gcp )      OBJ(((UINT_32)( gcp )) \
				   + sizeof( POBHeader ) \
				   + POINTER_TAG )
#define HDRPTR_TO_PTR( p )       GCPTR_TO_PTR( p )
#define DATAPTR_TO_PTR( p )      OBJ(((UINT_32)(p))+POINTER_TAG)

#define PTR_TO_GCPTR( ptr )      ((gc_obj_addr)PTR_TO_HDRPTR( ptr ))

#define SIZEOF_PTR( ptr )        (PTR_TO_HDRPTR( ptr )->pob_size)
#define CLASSOF_PTR( ptr )       (PTR_TO_HDRPTR( ptr )->pob_class)

#endif /* DEBUG_SMEMORY */

CIH_DECL void safe_point( UINT_32 headroom );


/******************** Memory Read Functions ********************/

#define bvec_read(b,o) bvec_read_uint8(b,o)

CIH_DECL UINT_8  bvec_read_uint8(  obj bvec, UINT_32 byte_offset );
          INT_8  bvec_read_int8(   obj bvec, UINT_32 byte_offset );
         UINT_16 bvec_read_uint16( obj bvec, UINT_32 byte_offset );
          INT_16 bvec_read_int16(  obj bvec, UINT_32 byte_offset );
          INT_32 bvec_read_int32(  obj bvec, UINT_32 byte_offset );
          INT_64 bvec_read_int64(  obj bvec, UINT_32 byte_offset );
         IEEE_32 bvec_read_ieee32( obj bvec, UINT_32 byte_offset );
         IEEE_64 bvec_read_ieee64( obj bvec, UINT_32 byte_offset );

#if INCLUDE_READ_BARRIER
CIH_DECL obj read_barrier( obj item );
#define READ_BARRIER(x)   read_barrier(x)
#else
#define READ_BARRIER(x)   (x)
#endif

#ifdef DEBUG_SMEMORY
CIH_DECL obj gvec_read( obj gvec, UINT_32 byte_offset );
#else
#define gvec_read(gvec,byte_offset) READ_BARRIER(*(obj *)(((char *)PTR_TO_DATAPTR(gvec))\
					     + (byte_offset)))

#endif

/******************** Memory Write Functions ********************/

void bvec_write_uint8(  obj bvec, UINT_32 byte_offset, UINT_8 v );
void bvec_write_int8(   obj bvec, UINT_32 byte_offset, INT_8 v );
void bvec_write_uint16( obj bvec, UINT_32 byte_offset, UINT_16 v );
void bvec_write_int16(  obj bvec, UINT_32 byte_offset, INT_16 v );

void bvec_write_int32(  obj bvec, UINT_32 byte_offset, INT_32 v );
void bvec_write_int64(  obj bvec, UINT_32 byte_offset, INT_64 v );
void bvec_write_ieee32(  obj bvec, UINT_32 byte_offset, IEEE_32 v );
void bvec_write_ieee64(  obj bvec, UINT_32 byte_offset, IEEE_64 v );

CIH_DECL void gvec_write( obj gvec, UINT_32 byte_offset, obj value );
CIH_DECL void gvec_write_ptr( obj gvec, UINT_32 byte_offset, obj value );
CIH_DECL void gvec_write_non_ptr( obj gvec, UINT_32 byte_offset, obj value );

/* an object is FRESH from the time it is allocated to
   the next GC_SAFE_POINT.  This is extraordinarily useful
   if you are allocating white, in which case the write
   barrier becomes a NOP
*/

CIH_DECL void gvec_write_fresh( obj gvec, UINT_32 byte_offset, obj value );
CIH_DECL void gvec_write_fresh_ptr( obj gvec, UINT_32 byte_offset, obj value );
CIH_DECL void gvec_write_fresh_non_ptr( obj gvec, UINT_32 byte_offset, obj value );

CIH_DECL void gvec_write_init( obj gvec, UINT_32 byte_offset, obj value );
CIH_DECL void gvec_write_init_ptr( obj gvec, UINT_32 byte_offset, obj value );
CIH_DECL void gvec_write_init_non_ptr( obj gvec, UINT_32 byte_offset, obj value );

/* lower-level allocation */

CIH_DECL void *malloc_aligned_32( UINT_32 bytes );
CIH_DECL void free_aligned_32( void *ptr );

/************************ weak pointers and finalization *******************/

void mark_as_finalizable( obj thing );
void found_weak_slot( obj in_thing, obj *slot ); 
rs_bool gc_cycle_finish_ok( void );
int rs_relocate_objects( obj tbl );

/********************** new names ***********************/

#define gvec_ref(g,i) gvec_read(g,i)
#define gvec_set(g,i,v) gvec_write(g,i,v)

#define make_bvec(class,size) bvec_alloc(size,class)

/********************** allocation areas ***********************/

struct _AllocArea;

typedef obj allocator_fn( struct _AllocArea *area, 
			  obj obj_class,
			  UINT_32 bytes );

typedef struct _AllocArea {
  obj                 entry;      /* each AllocArea get's its own entry */
  obj                 reserved;   /* a reserved object, for now */
  allocator_fn       *allocfn;    /* allocation implementation */
  void               *info;       /* guaranteed NULL for default impl */
} AllocArea;

/* 
 *   default allocation implementation  
 */
obj default_alloc_obj( AllocArea *area, obj obj_class, UINT_32 bytes );

/*
 *   wrapper functions
 */

#define AS_ALLOCAREA(p) ((AllocArea *)PTR_TO_DATAPTR(p))
#define ALLOCAREA_P(x) (OBJ_ISA_PTR(x) \
			&& EQ(CLASSOF_PTR(x),allocation_area_class))

obj alloc_in_area( AllocArea *in_area, obj the_class, UINT_32 size );
obj make_gvec_in_area( obj in_area, obj the_class, UINT_32 size, obj fill );
obj make_bvec_in_area( obj in_area, obj the_class, UINT_32 size, UINT_8 fill );

#ifdef INLINES

#if !(defined(_CI_GCCLIENT1) || defined(_CI_GCCLIENT2))
#ifndef FOREIGN

#ifdef gcserver_h  /* we're running RTGC */
#include <rtgc/gcserver1.ci>
#include <rtgc/objmgr.ci>
#include <rtgc/colorset1.ci>
#include <rtgc/gcserver2.ci>
#else
#include <rscheme/gcserver1.ci>
#include <rscheme/gcserver2.ci>
#endif
#endif
#endif

#include <rscheme/smemory.ci>
#include <rscheme/readwrit.ci>
#endif

/* recognize bvecs & gvecs, and classes of them */

#define CLASS_GVEC_P(c) (EQ(gvec_read((c),SLOT(1)),int2fx(0)))
#define CLASS_BVEC_P(c) (EQ(gvec_read((c),SLOT(1)),int2fx(1)))

#define GVEC_P(x) (OBJ_ISA_PTR(x) && CLASS_GVEC_P(CLASSOF_PTR(x)))
#define BVEC_P(x) (OBJ_ISA_PTR(x) && CLASS_BVEC_P(CLASSOF_PTR(x)))

typedef void gc_scan_found_fn( void *info, obj item );

struct HeapTypeDecl {
  const char *name;
  void (*gc_scan)( obj item, gc_scan_found_fn *found, void *info );
};

#define MAX_HEAP_TYPES (100)

extern struct HeapTypeDecl *(rs_heap_type_info[MAX_HEAP_TYPES]);

void rs_add_heap_type( int type, struct HeapTypeDecl *info );
void init_smemory( void );

#endif /* _H_SMEMORY */
