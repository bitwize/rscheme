/*-----------------------------------------------------------------*-C-*---
 * File:    handc/heapi/savetrav.c
 *
 *          Copyright (C)1997 Donovan Kolbly <d.kolbly@rscheme.org>
 *          as part of the RScheme project, licensed for free use.
 *          See <http://www.rscheme.org/> for the latest information.
 *
 * File version:     1.6
 * File mod date:    2003-10-13 13:02:28
 * System build:     v0.7.3.4-b7u, 2007-05-30
 *
 * Purpose:          Traversal portion of 0.7 image saver
 *------------------------------------------------------------------------*/

#include <stdlib.h>
#include <rscheme/smemory.h>
#include <rscheme/hashmain.h>
#include <rscheme/hashfn.h>
#include <rscheme/linktype.h>
#include "saveimg.h"

#define DEBUG_SAVE  0  /* define to 1 for some trace info */

/***********************************************************************
 *
 *  The traversal makes use of the `pob_class' field of heap objects.
 *  The status of a given object is determined as follows, where the
 *  pob_class is `c'
 *  
 *  OBJ_ISA_PTR(c)  ==>  a normal object that has not been reached yet
 *                       (this works because the pob_class field of all
 *                       heap objects during normal execution is itself
 *                       a pointer)
 *
 *  c == FX(0)      ==>  an object (normal or reference) that has been
 *                       reached and is in one of the various queues.
 *
 *  c == FX(1)      ==>  a "reference" object that has not been reached
 *                       
 *  OBJ_ISA_IMMOB(c)==>  a "replaced" object.  The replacement object
 *                       is OBJ(VAL(c) - IMMOB_TAG + POINTER_TAG)
 *
 *  After the traversal proper, objects that have been reached (ie,
 *  are in a queue and have pob_class == FX(0)) are assigned unswizzled
 *  names, which are also stored in the pob_class field in the form of
 *  a small (compact) number with a POINTER tag.
 *
 *  These three kinds of pob_class clobberings are cleaned up after
 *  the data has been written out.
 *
 ***********************************************************************/

static int scan_gvecs( SaveQueue *q );
static int scan_classes_only( SaveQueue *q );
static int scan_nothing( SaveQueue *q );
static int scan_alloc_areas( SaveQueue *q );
static int scan_templates( SaveQueue *q );
static int scan_bignums( SaveQueue *q );

static SaveQueue used_refs, rplc_queue;

#define NUM_USEFUL_LOAD2_MODES (6)

static enum load2_mode useful_load2_modes[NUM_USEFUL_LOAD2_MODES] =
    { LOAD2_CLASS_ONLY, 
      LOAD2_GVEC,
      LOAD2_TEMPLATE,
      LOAD2_PARTCONT,
      LOAD2_MIXVEC_2,
      LOAD2_BIGNUM };

static struct { 
    int           (*queue_scanner)( SaveQueue *q );
    SaveQueue       queue;
} image_modes[NUM_CLASS_MODES] = {
  { /* 0: gvec */ 	scan_gvecs },
  { /* 1: bvec */	scan_classes_only },
  { /* 2: symbol */	scan_nothing },
  { /* 3: part */	scan_nothing },
  { /* 4: template */	scan_templates },
  { /* 5: part-cont */	scan_gvecs },
  { /* 6: longfloat */	scan_classes_only },
  { /* 7: uint32*n */	scan_classes_only },
  { /* 8: alloc-area */ scan_alloc_areas },
  { /* 9: bignum     */ scan_bignums } };

/************************************************************************
 **
 **  Pass 1 -- Object Graph Traversal
 **
 ************************************************************************/

/*  Spot a heap object referred to by another object (or root)
    that has not been traversed yet
    (it may be a REF, however).

    Adds the object to the appropriate queue, thereby arranging
    to have the object traversed later (unless it's a REF)
*/

static void spot_new_object( obj thing )
{
POBHeader *p =(POBHeader *)PTR_TO_HDRPTR(thing);
obj the_class;
SaveQueue *q;

    the_class = p->pob_class;
    if (EQ(the_class,int2fx(1)))
    {
      q = &used_refs;
      /*printf( "spot REF %08x\n", VAL(thing) );*/
    }
    else 
    {
	/* a normal heap object that hasn't been spotted yet */
	obj rplc;
	int imode;

	assert( OBJ_ISA_PTR(the_class) );

	rplc = replace_ptr(the_class);
	
	imode = class_image_mode( rplc );
	/*printf( "spot NORMAL[%d] %08x\n", imode, VAL(thing) );*/
	if (imode < 0 || imode >= NUM_CLASS_MODES)
	{
	    fprintf( stderr, "Warning: Class %s has illegal image-mode = %d\n",
			symbol_text( class_name( rplc ) ),
			imode );
	    imode = 0;
	}
	q = &image_modes[imode].queue;
    }

    /* insert us in the appropriate queue */

    hi_enqueue_item( q, thing );
    p->pob_class = ZERO;
}

/*  Encounter a strong object reference
 *
 *  If the object reference is a pointer, this involves making sure
 *  the referred-to object is spotted
 */

static _rs_inline void spot_object( obj thing )
{
POBHeader *p;

    if (OBJ_ISA_PTR(thing))
    {
      thing = replace_ptr(thing);

      assert( OBJ_ISA_PTR(thing) );

      p = (POBHeader *)PTR_TO_HDRPTR(thing);
      if (!EQ(p->pob_class,ZERO))
	spot_new_object(thing);
    }
}

static void traverse_all( void )
{
  int any_new;
  SaveQueue *q;
  unsigned i;
  int (*scan_all)( SaveQueue *q );

  do {
    any_new = 0;
    for (i=0; i<NUM_CLASS_MODES; i++)
      {
	q = &image_modes[i].queue;
	scan_all = image_modes[i].queue_scanner;
	if (scan_all( q ))
	  any_new = 1;
      }
  } while (any_new);
}

/************************************************************************
 **
 **  Heap Object Methods
 **
 ************************************************************************/

static void scan_as_gvec( obj item, UINT_32 offset, UINT_32 lim )
{
  obj *s, *limit;

  s = (obj *)(offset + (char *)PTR_TO_DATAPTR( item ));
  limit = (obj *)(lim + (char *)PTR_TO_DATAPTR( item ));
  
  while (s < limit)
    spot_object( *s++ );
}

static void scan_1_gvec( obj item, obj orig_class )
{
  spot_object( orig_class );
  scan_as_gvec( item, 0, SIZEOF_PTR(item) );
}


/*======================================================================*/

static _rs_inline void scan_1_class_only( obj item, obj orig_class )
{
  spot_object( orig_class );
}

static void scan_1_bignum( obj item, obj orig_class )
{
  spot_object( orig_class );
  spot_object( CLASSOF_PTR( gvec_ref( item, SLOT(2) ) ) );
}

/*======================================================================*/

static void scan_1_alloc_area( obj item, obj orig_class )
{
  spot_object( orig_class );
  scan_as_gvec( item, 0, SLOT(2) );
}

/*======================================================================*/

static void scan_1_template( obj item, obj orig_class )
{
  obj lnk2 = gvec_read( item, TEMPLATE_LINKAGE );

  if (OBJ_ISA_PTR(lnk2))
    {
      scan_1_gvec( item, orig_class );
    }
  else
    {
      struct function_descr *fn;

      fn = (struct function_descr *)OBJ_TO_RAW_PTR(lnk2);

      /* make sure it's not a stub */

      if (fn->in_part->tag >= STUB_PART_TAG)
	{
	  template_unstub( item );
	  lnk2 = gvec_read( item, TEMPLATE_LINKAGE );
	  fn = (struct function_descr *)OBJ_TO_RAW_PTR(lnk2);
	}
      
      if (!fn->in_part->unswizzled_as)
	{
	  fn->in_part->unswizzled_as = 1;
	  hi_enqueue_item2( &used_refs,
			    RAW_PTR_TO_OBJ(fn->in_part),
			    FALSE_OBJ );
	}
      spot_object( orig_class );
      scan_as_gvec( item, SLOT(2), SIZEOF_PTR(item) );
    }
}

/*======================================================================*/

#define QUEUE_SCANNER(NAME,SCAN1) static int NAME( SaveQueue *q ) \
{ \
  struct queue_entry *e; \
  e = hi_dequeue_item(q); \
  if (e) \
    { \
      do { /*printf("dequeue: %08x class %08x\n",e->thing,e->orig_class );*/\
	SCAN1( e->thing, e->orig_class ); \
	e = hi_dequeue_item(q); \
      } while (e); \
      return 1; \
    } \
  else \
    return 0; \
}

QUEUE_SCANNER(scan_bignums,scan_1_bignum)
QUEUE_SCANNER(scan_gvecs,scan_1_gvec)
QUEUE_SCANNER(scan_templates,scan_1_template)
QUEUE_SCANNER(scan_classes_only,scan_1_class_only)
QUEUE_SCANNER(scan_alloc_areas,scan_1_alloc_area)

static int scan_nothing( SaveQueue *q )
{
  return 0;
}

static void cleanup_reference_objects( obj ref_vec, 
				       obj *saved_class )
{
  UINT_32 i, n = SIZEOF_PTR(ref_vec) / SLOT(1);
 
  if (n == 0)
    return;

  for (i=0; i<n; i++)
    {
      obj ref_item = gvec_ref( ref_vec, SLOT(i) );
      PTR_TO_HDRPTR(ref_item)->pob_class = saved_class[i];
    }
}

static void init_rplc( void *q, obj h, obj k, obj v )
{
  assert( OBJ_ISA_PTR(k) );
  assert( OBJ_ISA_PTR(v) );
  assert( OBJ_ISA_PTR(CLASSOF_PTR(k)) );

  hi_enqueue_item( (SaveQueue *)q, k );
  PTR_TO_HDRPTR(k)->pob_class = OBJ( VAL(v) - POINTER_TAG + IMMOB_TAG );
}

static void setup_replacement_objects( obj rplc_tbl )
{
  hi_init_queue_n( &rplc_queue, hashtable_size(rplc_tbl) );
  hashtable_foreach( rplc_tbl, (void *)&rplc_queue, init_rplc );
}

static void cleanup_queued_objects( SaveQueue *q )
{
  UINT_32 i, n = q->count;
  struct queue_entry *p;

  for (i=0, p=q->contents; i<n; i++, p++)
    PTR_TO_HDRPTR(p->thing)->pob_class = p->orig_class;
}

static obj *setup_reference_objects( obj ref_vec )
{
  UINT_32 i, n = SIZEOF_PTR(ref_vec) / SLOT(1);
  obj *saved_class;

  if (n == 0)
    return NULL;

  saved_class = (obj *)malloc( n * sizeof(obj) );
  for (i=0; i<n; i++)
    {
      obj ref_item = gvec_ref( ref_vec, SLOT(i) );
      /*printf( "REF item %08x\n", VAL(ref_item) ); */
      saved_class[i] = PTR_TO_HDRPTR(ref_item)->pob_class;
      PTR_TO_HDRPTR(ref_item)->pob_class = int2fx(1);
    }
  return saved_class;
}

static obj slurp_queue( SaveQueue *q )
{
  UINT_32 i, n = q->count;
  struct queue_entry *p;
  obj x;
  
  x = alloc( SLOT(n), vector_class );

  for (i=0, p=q->contents; i<n; i++, p++)
    gvec_write_init_ptr( x, SLOT(i), p->thing );
  return x;
}

static void clear_part_descr_labels( SaveQueue *q )
{
  UINT_32 i, n = q->count;
  struct queue_entry *p;

  for (i=0, p=q->contents; i<n; i++, p++)
    if (EQ(p->orig_class,FALSE_OBJ))
      {
	struct part_descr *part;

	part = (struct part_descr *)OBJ_TO_RAW_PTR(p->thing);
	part->unswizzled_as = 0;
      }
}

#define NEXT_NAME(x) OBJ(VAL(x)+(1<<PRIMARY_TAG_SIZE))

static obj assign_ref_queue_names( SaveQueue *q, obj name )
{
  UINT_32 i, n = q->count;
  struct queue_entry *p;

  for (i=0, p=q->contents; i<n; i++, p++)
    {
      if (OBJ_ISA_PTR(p->thing))
	{
	  /* if it's in a queue, it should have pob_clas ZERO
	   * (except if it's not a PTR, that is)
	   */
	  assert( EQ(PTR_TO_HDRPTR(p->thing)->pob_class,ZERO) );
	  PTR_TO_HDRPTR(p->thing)->pob_class = name;
	}
      else
	{
	  struct part_descr *pd = OBJ_TO_RAW_PTR(p->thing);
	  pd->unswizzled_as = VAL(name);
	}
      name = NEXT_NAME(name);
    }
  return name;
}

static obj assign_queue_names( SaveQueue *q, obj name )
{
  UINT_32 i, n = q->count;
  struct queue_entry *p;

  for (i=0, p=q->contents; i<n; i++, p++)
    {
      /* if it's in a queue, it should have pob_clas ZERO */
      assert( EQ(PTR_TO_HDRPTR(p->thing)->pob_class,ZERO) );
      PTR_TO_HDRPTR(p->thing)->pob_class = name;
      name = NEXT_NAME(name);
    }
  return name;
}

static void do_file_output( FILE *file_strm,
			    obj refs_vec, obj ref_names, obj root )
{
  unsigned i;
  UINT_32 n;

  n = used_refs.count;
  for (i=0; i<NUM_CLASS_MODES; i++)
    n += image_modes[i].queue.count;

  hi_init_output( file_strm, refs_vec, ref_names, root, &used_refs, n );

#if DEBUG_SAVE
  printf( "(%u reference objects)\n", used_refs.count );
#endif
  hi_output_refs( &used_refs );

  for (i=0; i<NUM_CLASS_MODES; i++)
    hi_writers[i].queue_writer( &image_modes[i].queue );

  for (i=0; i<NUM_USEFUL_LOAD2_MODES; i++)
    {
      SaveQueue *(q[NUM_CLASS_MODES]);
      unsigned j, nq = 0;
      UINT_32 cnt = 0;

#if DEBUG_SAVE
      printf( "mode2 = %d: ", useful_load2_modes[i] );
#endif

      for (j=0; j<NUM_CLASS_MODES; j++)
	if (hi_writers[j].mode2 == useful_load2_modes[i])
	  {
#if DEBUG_SAVE
	    printf( " %d", j );
#endif
	    q[nq++] = &image_modes[j].queue;
	    cnt += image_modes[j].queue.count;
	  }

#if DEBUG_SAVE
      printf( ". (%u objects)\n", cnt );
#endif
      hi_output_mode2( useful_load2_modes[i], q, nq );
    }

  hi_done_output();
}

static obj do_vector_output( void )
{
  obj result;
  unsigned i;

  result = alloc( SLOT(NUM_CLASS_MODES+1), vector_class );
  for (i=0; i<NUM_CLASS_MODES; i++)
    gvec_write_init( result,
		     SLOT(i+1),
		     slurp_queue( &image_modes[i].queue ) );
  gvec_write_init( result, SLOT(0), slurp_queue( &used_refs ) );
  return result;
}

obj rs_save_image_file( obj root, obj ref_vec, obj ref_names, 
			obj rplc, obj out_info )
{
  int i;
  obj *save, result, output_id;
  char *outfile_name = NULL;
  FILE *outfile_strm = NULL;

  /*  Phase 1 --- Setup  */

  if (STRING_P(out_info))
    {
      outfile_name = string_text(out_info);
      outfile_strm = fopen( outfile_name, "wb" );
      if (!outfile_strm)
	{
	  scheme_error( "~a: error opening image output", 1, out_info );
	}
    }

  for (i=0; i<NUM_CLASS_MODES; i++)
    hi_init_queue( &image_modes[i].queue );
  hi_init_queue( &used_refs );

  if (OBJ_ISA_PTR(rplc))
    setup_replacement_objects( rplc );

  save = setup_reference_objects( ref_vec );

  /*  Phase 2 --- Traversal  */

  spot_object( root );
  traverse_all();

  /*  Phase 3 --- Name Assignment  */

  output_id = OBJ(POINTER_TAG);

  output_id = assign_ref_queue_names( &used_refs, output_id );
  for (i=0; i<NUM_CLASS_MODES; i++)
    output_id = assign_queue_names( &image_modes[i].queue, output_id );

#if DEBUG_SAVE
  printf( "%u objects named in output\n", VAL(output_id)>>PRIMARY_TAG_SIZE );
#endif

  /*  Phase 4 --- Output  */

  if (outfile_strm)
    {
#if DEBUG_SAVE
      printf( "writing image to file: \"%s\"\n", outfile_name );
#endif
      do_file_output( outfile_strm, ref_vec, ref_names, root );
    }

  /*  Phase 5 --- Cleanup  */

  for (i=0; i<NUM_CLASS_MODES; i++)
    cleanup_queued_objects( &image_modes[i].queue );

  cleanup_reference_objects( ref_vec, save );
  cleanup_queued_objects( &rplc_queue );

  if (EQ(out_info,TRUE_OBJ))
    result = do_vector_output();
  else
    result = TRUE_OBJ;

  for (i=0; i<NUM_CLASS_MODES; i++)
    hi_free_queue( &image_modes[i].queue );
  clear_part_descr_labels( &used_refs );
  hi_free_queue( &used_refs );

  return result;
}
