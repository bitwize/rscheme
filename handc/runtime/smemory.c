/*-----------------------------------------------------------------*-C-*---
 * File:    handc/runtime/smemory.c
 *
 *          Copyright (C)1997 Donovan Kolbly <d.kolbly@rscheme.org>
 *          as part of the RScheme project, licensed for free use.
 *          See <http://www.rscheme.org/> for the latest information.
 *
 * File version:     1.25
 * File mod date:    2005-09-16 10:34:54
 * System build:     v0.7.3.4-b7u, 2007-05-30
 *
 * Purpose:          RScheme<-->Memory interface (scheme side)
 *------------------------------------------------------------------------*/

#include <string.h>
#include <rscheme/gcadapt.h>
#include <rscheme/scheme.h>
#include <rscheme/smemory.h>
#include <rscheme/gcserver.h>
#include <rscheme/runtime.h>
#ifdef ATRACE
#include <rscheme/atrace.h>
#endif
#include <rscheme/profile.h>

#ifdef COUNT_ALLOCS
unsigned total_alloc_bytes = 0;
unsigned total_alloc_objects = 0;
#endif

#ifndef INLINES
#define SMLINK_TYPE /* nothing */
#include "smemory.ci"
#include "readwrit.ci"
#endif

struct HeapTypeDecl *(rs_heap_type_info[MAX_HEAP_TYPES]);

static void gvec_scanner( obj item, gc_scan_found_fn *found, void *info )
{
  obj *p, *limit;

  p = (obj *)PTR_TO_DATAPTR(item);
  limit = (obj *)((char *)p + SIZEOF_PTR(item));

  while (p < limit) {
    found( info, *p++ );
  }
}
                          
static struct HeapTypeDecl gvec_heap_type_info = {
  "gvec",
  gvec_scanner
};

static void null_scanner( obj item, gc_scan_found_fn *found, void *info )
{
  return;
}

static struct HeapTypeDecl bvec_heap_type_info = {
  "bvec",
  null_scanner
};

static struct HeapTypeDecl fsw_heap_type_info = {
  "first-slot-weak",
  gvec_scanner          /* scans the same as a gvec */
};

static void aa_scanner( obj item, gc_scan_found_fn *found, void *info )
{
  obj *p = (obj *)PTR_TO_DATAPTR(item);

  found( info, p[0] );
  found( info, p[1] );
}

static struct HeapTypeDecl aa_heap_type_info = {
  "alloc-area",
  aa_scanner
};

static struct HeapTypeDecl immob_heap_type_info = {
  "immob",
  null_scanner
};

static struct HeapTypeDecl abstract_heap_type_info = {
  "abstract",
  null_scanner
};

void init_smemory( void )
{
  unsigned i;

  for (i=0; i<MAX_HEAP_TYPES; i++) {
    rs_heap_type_info[i] = NULL;
  }
  rs_heap_type_info[0] = &gvec_heap_type_info;
  rs_heap_type_info[1] = &bvec_heap_type_info;
  rs_heap_type_info[2] = &immob_heap_type_info;
  rs_heap_type_info[3] = &abstract_heap_type_info;
  rs_heap_type_info[4] = &fsw_heap_type_info;
  rs_heap_type_info[5] = &aa_heap_type_info;
}

void rs_add_heap_type( int type, struct HeapTypeDecl *info )
{
  if ((type < 0) || (type >= MAX_HEAP_TYPES)) {
    scheme_error( "add-heap-type: ~d is out of range", 1, int2fx( type ) );
  }
  if (rs_heap_type_info[ type ]) {
    scheme_error( "add-heap-type: heap type ~d is already in use by ~s", 
                  2, 
                  int2fx( type ), 
                  make_string( rs_heap_type_info[type]->name ) );
  }
  rs_heap_type_info[ type ] = info;
}

void gc_work( UINT_32 amt )
{
  INT_32 t = amt;

  while (amt > 0) {
    /*
     *  guarantee progress each time through the loop
     */
    if (gc_alloc_time <= 1) {
      amt -= 1;
    } else if (gc_alloc_time > amt) {
      amt = 0;
    } else {
      amt -= gc_alloc_time;
    }
    gc_alloc_time = -1;
    gc_safe_point( 5000 );
  }
}


#ifdef NDEBUG
#define BVEC_ACCESS(bvec,byte_offset,opn,type) \
    ((type *)((char *)PTR_TO_DATAPTR(bvec) + byte_offset))
#else
#define BVEC_ACCESS(bvec,byte_offset,opn,type) \
     ((type *)bvec_access_check( bvec, byte_offset, opn, sizeof(type) ))

static void *bvec_access_check( obj bvec, UINT_32 byte_offset, 
			       char *opn, size_t type_size )
{
  if (!BVEC_P(bvec))
    {
      scheme_error( "bvec-~a: invalid argument (~s not a bvec)",
		    2, make_string(opn), bvec );
    }
  else if (byte_offset + type_size > SIZEOF_PTR(bvec))
    {
      scheme_error( "bvec-~a: invalid offset\n"
		    "(~#*@40s at +~d for ~d is out of range (~d max))",
		    5, make_string(opn), bvec,
		    int2fx(byte_offset), int2fx(type_size), 
		    int2fx(SIZEOF_PTR(bvec)) );
    }
  return (char *)PTR_TO_DATAPTR(bvec) + byte_offset;
}
#endif

INT_8  bvec_read_int8( obj bvec, UINT_32 byte_offset )
{
  return *BVEC_ACCESS( bvec, byte_offset, "read-signed-8", INT_8 );
}

UINT_16 bvec_read_uint16( obj bvec, UINT_32 byte_offset )
{
  return *BVEC_ACCESS( bvec, byte_offset, "read-unsigned-16", UINT_16 );
}

INT_16 bvec_read_int16(  obj bvec, UINT_32 byte_offset )
{
  return *BVEC_ACCESS( bvec, byte_offset, "read-signed-16", INT_16 );
}

INT_32 bvec_read_int32(  obj bvec, UINT_32 byte_offset )
{
  return *BVEC_ACCESS( bvec, byte_offset, "read-signed-32", INT_32 );
}

INT_64 bvec_read_int64(  obj bvec, UINT_32 byte_offset )
{
  return *BVEC_ACCESS( bvec, byte_offset, "read-signed-64", INT_64 );
}

IEEE_32 bvec_read_ieee32(  obj bvec, UINT_32 byte_offset )
{
  return *BVEC_ACCESS( bvec, byte_offset, "read-ieee-32", IEEE_32 );
}

IEEE_64 bvec_read_ieee64(  obj bvec, UINT_32 byte_offset )
{
  return *BVEC_ACCESS( bvec, byte_offset, "read-signed-16", IEEE_64 );
}

void bvec_write_uint8(  obj bvec, UINT_32 byte_offset, UINT_8 v )
{
  *BVEC_ACCESS( bvec, byte_offset, "write-unsigned-8", UINT_8 ) = v;
}

void bvec_write_int8(   obj bvec, UINT_32 byte_offset, INT_8 v )
{
  *BVEC_ACCESS( bvec, byte_offset, "write-signed-8", INT_8 ) = v;
}

void bvec_write_uint16( obj bvec, UINT_32 byte_offset, UINT_16 v )
{
  *BVEC_ACCESS( bvec, byte_offset, "write-unsigned-16", UINT_16 ) = v;
}

void bvec_write_int16(  obj bvec, UINT_32 byte_offset, INT_16 v )
{
  *BVEC_ACCESS( bvec, byte_offset, "write-signed-16", INT_16 ) = v;
}

void bvec_write_int32(  obj bvec, UINT_32 byte_offset, INT_32 v )
{
  *BVEC_ACCESS( bvec, byte_offset, "write-signed-32", INT_32 ) = v;
}

void bvec_write_int64(  obj bvec, UINT_32 byte_offset, INT_64 v )
{
  *BVEC_ACCESS( bvec, byte_offset, "write-signed-64", INT_64 ) = v;
}

void bvec_write_ieee32(  obj bvec, UINT_32 byte_offset, IEEE_32 v )
{
  *BVEC_ACCESS( bvec, byte_offset, "write-ieee-32", IEEE_32 ) = v;
}

void bvec_write_ieee64(  obj bvec, UINT_32 byte_offset, IEEE_64 v )
{
  *BVEC_ACCESS( bvec, byte_offset, "write-ieee-64", IEEE_64 ) = v;
}

/************************************************************************/

/*  Object relocation support 
 *
 *  given a hash table mapping source->dest objects,
 *    (1) do all mappings in the root set
 *    (2) arrange to trip the read barrier for all source objects
 *
 *  (returns the number of root pointers replaced)
 */

#if INCLUDE_READ_BARRIER
static void setup_read_barrier_1( void *info, obj h, obj k, obj v )
{

  /* can't replace non-pointers, and don't allow replacement with non-ptrs */
  if (PTR_P(k) && PTR_P(v))
    {
      POBHeader *p = PTR_TO_HDRPTR(k);
      if (CLASS_P(k))
	{
	  if (CLASS_P(v))
	    {
	      int *q = (int *)info;
	      *q = 1;
	    }
	  else
	    {
	      scheme_error( "relocate-objects: can't replace class ~s with ~s\n",
			    2, k, v );
	    }
	}
      p->pob_class = ZERO;
      p->pob_size = VAL(v);
    }
}

static int do_class_rplc( void *info, void *ptr )
{
  POBHeader *p = (POBHeader *)ptr;
  obj h, k, v, tbl = *(obj *)info;

  k = p->pob_class;
  h = obj_hash(k);
  v = objecttable_lookup( tbl, h, k );
  if (truish(v))
    {
      p->pob_class = v;
    }
  return 0;
}

struct root_rplc_info {
  obj     tbl;
  int     count;
};

static int do_root_rplc( obj *proot, void *info )
{
  if (PTR_P(*proot))
    {
      struct root_rplc_info *i = (struct root_rplc_info *)info;
      obj h, k, v;
      k = *proot;
      h = obj_hash(k);
      v = objecttable_lookup( i->tbl, h, k );
      if (truish(v))
	{
	  printf( "relocating root: %#x => %#x\n", VAL(k), VAL(v) );
	  *proot = v;
	  i->count++;
	}
    }
  return 0;
}
#endif

int rs_relocate_objects( obj tbl )
{
#if INCLUDE_READ_BARRIER
  int need_heap_scan_q = 0;
  struct root_rplc_info i;

  hashtable_foreach( tbl, (void *)&need_heap_scan_q, setup_read_barrier_1 );

  i.tbl = tbl;
  i.count = 0;

  process_all_roots( do_root_rplc, &i );

  if (need_heap_scan_q)
    {
      gc_for_each( do_class_rplc, &tbl );
    }
  return i.count;
#else
  scheme_error( "relocate-objects: not implemented w/o read barrier", 0 );
  return 0;
#endif
}


/************************************************************************/

#define BUCKET_SIZE (200)

struct item_ent { 
  obj item; 
  UINT_32 offset; 
};

struct all_bucket {
  struct all_bucket *next;
  struct item_ent elements[BUCKET_SIZE];
};

struct all_list {
  struct all_bucket *head;
  unsigned head_count;
  unsigned total_count;
  obj  seek;
};

static void add_to_list( void *info, obj item, UINT_32 offset )
{
  struct all_list *list = (struct all_list *)info;
  struct all_bucket *buck;

  if (list->head_count >= BUCKET_SIZE) {
    /* add a new bucket */
    buck = (struct all_bucket *)malloc( sizeof(struct all_bucket) );
    buck->next = list->head;
    list->head = buck;
    list->head_count = 0;
  } else {
    buck = list->head;
  }
  buck->elements[list->head_count].item = item;
  buck->elements[list->head_count].offset = offset;
  list->head_count++;
  list->total_count++;
}

static int check_1_obj( void *info, void *ptr )
{
  POBHeader *p = (POBHeader *)ptr;
  struct all_list *list = (struct all_list *)info;

  if (EQ(p->pob_class,list->seek)) 
    {
      add_to_list( info, GCPTR_TO_PTR(ptr), 0 );
    }
  return 0;
}

static int check_1_ptrs( void *info, void *ptr )
{
  POBHeader *p = (POBHeader *)ptr;
  struct all_list *list = (struct all_list *)info;

  if (CLASS_GVEC_P(p->pob_class))
    {
      obj *s = (obj *)(p+1);
      UINT_32 i;

      for (i=0; i<p->pob_size; i+=SLOT(1))
	{
	  if (EQ(*s,list->seek)) {
	    add_to_list( info, GCPTR_TO_PTR(ptr), i );
	  }
	  s++;
	}
    }
  return 0;
}


obj all_instances( obj of_class )
{
  struct all_list list;
  struct all_bucket *b, temp;
  obj v;
  unsigned i, j, n;

  temp.next = NULL;
  list.head_count = list.total_count = 0;
  list.head = &temp;
  list.seek = of_class;
  
  gc_for_each( check_1_obj, &list );
  
  v = alloc( SLOT(list.total_count), vector_class );
  j = 0;
  for (b=list.head, n=list.head_count; b; b=b->next, n=BUCKET_SIZE) 
    {
      for (i=0; i<n; i++)
	{
	  gvec_write_init( v, j, b->elements[i].item );
	  j += SLOT(1);
	}
    }
  while (1) {
    b = list.head;
    if (b == &temp)
      break;
    list.head = b->next;
    free( b );
  }
  return v;
}

/* A variant of all_pointers_to() that is useful during gdb() sessions */

void rs_find_pointers( obj instance )
{
  struct all_list list;
  struct all_bucket *b, temp;
  unsigned i, j, n;

  temp.next = NULL;
  list.head_count = list.total_count = 0;
  list.head = &temp;
  list.seek = instance;
  
  gc_for_each( check_1_ptrs, &list );

  fprintf( stderr, "Found %u pointers to {%08lx}\n", 
           list.total_count, 
           VAL(instance) );
           

  j = 0;
  for (b=list.head, n=list.head_count; b; b=b->next, n=BUCKET_SIZE) 
    {
      struct item_ent *e;
      e = b->elements;
      for (i=0; i<n; i++, e++)
	{
          fprintf( stderr, "  [%u]  from {%08lx} SLOT(%lu)  ; a %s\n",
                   j,
                   VAL(e->item),
                   e->offset / SLOT(1),
                   symbol_text( class_name( CLASSOF_PTR( e->item ) ) ) );
          j++;
	}
    }
  while (1) {
    b = list.head;
    if (b == &temp)
      break;
    list.head = b->next;
    free( b );
  }
}

obj all_pointers_to( obj instance )
{
  struct all_list list;
  struct all_bucket *b, temp;
  obj v;
  unsigned i, j, n;

  temp.next = NULL;
  list.head_count = list.total_count = 0;
  list.head = &temp;
  list.seek = instance;
  
  gc_for_each( check_1_ptrs, &list );
  
  v = alloc( SLOT(list.total_count), vector_class );
  j = 0;
  for (b=list.head, n=list.head_count; b; b=b->next, n=BUCKET_SIZE) 
    {
      struct item_ent *e;
      e = b->elements;
      for (i=0; i<n; i++, e++)
	{
	  gvec_write_init( v, j, cons( e->item, 
				        RIBYTES_TO_FXWORDS(e->offset) ) );
	  j += SLOT(1);
	}
    }
  while (1) {
    b = list.head;
    if (b == &temp)
      break;
    list.head = b->next;
    free( b );
  }
  return v;
}

obj clone( obj from )
{
  return clone2( from, object_class(from) );
}

obj clone2( obj from, obj new_class )
{
obj htype, the_class, newobj = FALSE_OBJ;
UINT_32 length, i;

    if (!OBJ_ISA_PTR(from))
      scheme_error( "clone2: ~s not a heap object", 1, from );

    the_class = CLASSOF_PTR(from);

    htype = gvec_read( the_class, SLOT(1) );

    if (!EQ(htype,gvec_read(new_class,SLOT(1))))
      {
	scheme_error( "clone2: new class ~s is incompatible with ~s",
		     2,
		     new_class,
		     the_class );
      }
    length = SIZEOF_PTR(from);
	
    switch (fx2int(htype))
      {
      case 0:
	/* gvec */
	newobj = alloc( length, new_class );
	for (i=0; i<length; i+=SLOT(1))
	  gvec_write_init( newobj, i, gvec_read( from, i ) );
	break;

      case 1:
	/* bvec */
	newobj = bvec_alloc( length, new_class );
	memcpy( PTR_TO_DATAPTR(newobj), PTR_TO_DATAPTR(from), length );
	break;
	
      default:
	scheme_error( "clone2: internal error: heap type ~s invalid",
		      2, htype );
      }
    return newobj;
}


/********************** allocation areas ***********************/

/* 
 *   default allocation implementation  
 */

obj default_alloc_obj( AllocArea *area, obj obj_class, UINT_32 bytes )
{
  return alloc( bytes, obj_class );
}

obj make_gvec( obj the_class, UINT_32 size, obj fill )
{
  obj item = alloc( size, the_class );

  if (OBJ_ISA_PTR(fill))
    while (size > 0)
      {
	size -= SLOT(1);
	gvec_write_init_ptr( item, size, fill );
      }
  else
    while (size > 0)
      {
	size -= SLOT(1);
	gvec_write_init_non_ptr( item, size, fill );
      }
  return item;
}

obj subvector( obj vec, obj start, obj end )
{
  UINT_32 k;
  obj result, len;

  assert( VECTOR_P(vec) );
  assert( OBJ_ISA_FIXNUM(start) );
  assert( OBJ_ISA_FIXNUM(end) );

  /* range checking */

  if (FX_LT(start,ZERO) 
      || FX_LT(end,start) 
      || FX_GT(end,RIBYTES_TO_FXWORDS(SIZEOF_PTR(vec))))
    {
      scheme_error( "subvector: invalid interval ~d - ~d for ~s",
		    3, start, end, vec );
    }
  len = FX_SUB(end,start);
  result = alloc( FXWORDS_TO_RIBYTES(len), vector_class );
  
  for (k=0; k<FXWORDS_TO_RIBYTES(len); k+=SLOT(1))
    {
      gvec_write_init( result, k, gvec_ref( vec, FXWORDS_TO_RIBYTES(start) ) );
      start = ADD1(start);
    }
  return result;
}

obj alloc_in_area( AllocArea *area, obj the_class, UINT_32 size )
{
  return area->allocfn( area, the_class, size );
}

obj make_gvec_in_area( obj area, obj the_class, UINT_32 size, obj fill )
{
  obj item;

  assert( ALLOCAREA_P(area) );

  item = alloc_in_area( AS_ALLOCAREA(area), the_class, size );

  if (OBJ_ISA_PTR(fill))
    {
      while (size > 0)
	{
	  size -= SLOT(1);
	  
	  /* note that in the current, slightly askew scheme of
	     things, gvec_write_fresh doesn't apply to allocations
	     from other areas.  Since we may have allocated
	     from another area, we better use the regular
	     write barrier.
	     */
	  gvec_write_init_non_ptr( item, size, ZERO );
	  gvec_write( item, size, fill );
	}
    }
  else
    {
      while (size > 0)
	{
	  size -= SLOT(1);
	  gvec_write_init_non_ptr( item, size, fill );
	}
    }
  return item;
}

obj make_bvec_in_area( obj in_area, obj the_class, UINT_32 size, UINT_8 fill )
{
  obj item;

  assert( ALLOCAREA_P(in_area) );
  
  item = alloc_in_area( AS_ALLOCAREA(in_area), the_class, size );
  
  memset( PTR_TO_DATAPTR(item), fill, size );
  return item;
}

#ifdef RS_PROFILE
/*
 *  `rs_profile_alloc' writes the profiling record for an allocation
 *  called from smemory.ci:alloc()
 */

void rs_profile_alloc( obj the_class, UINT_32 bytes )
{
  rs_profile1( loading_image
	      ? "<*from-system-image*>"
	      : symbol_text(class_name(obj_class)),
	      bytes );
}
#endif

#ifdef SIGUSR_HOOKS

static int mark_thing( void *info, void *ptr )
{
  IRCH(ptr)->flagBits |= 0x80000000;
  return 0;
}

static int find_unmarked_things( void *info, void *ptr )
{
  if (!(IRCH(ptr)->flagBits & 0x80000000))
    {
      fdebug_slots( (FILE*)info, GCPTR_TO_PTR(ptr) );
    }
  return 0;
}

void run_sigusr_hook( int sigusr_num )  /* 1 or 2 */
{
  FILE *f;

  switch (sigusr_num)
    {
    case 1:
      gc_for_each( mark_thing, (void *)0 );
      break;
    case 2:
      gc_now();
      f = fopen( "/tmp/newobjs.dat", "w" );
      gc_for_each( find_unmarked_things, (void *)f );
      fclose(f);
      break;
    }
}
#endif /* SIGUSR_HOOKS */
