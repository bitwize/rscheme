/*-----------------------------------------------------------------*-C-*---
 * File:    handc/heapi/savewrit.c
 *
 *          Copyright (C)1997 Donovan Kolbly <d.kolbly@rscheme.org>
 *          as part of the RScheme project, licensed for free use.
 *          See <http://www.rscheme.org/> for the latest information.
 *
 * File version:     1.11
 * File mod date:    2003-10-13 13:02:28
 * System build:     v0.7.3.4-b7u, 2007-05-30
 *
 * Purpose:          Output-file portion of 0.7 image saver
 *------------------------------------------------------------------------*/

#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <rscheme/smemory.h>
#include <rscheme/hashmain.h>
#include <rscheme/hashfn.h>
#include <rscheme/linktype.h>
#include "saveimg.h"

#define ROUND_UP(n) (((n) + sizeof(XUINT_32) - 1) & (~(sizeof(XUINT_32)-1)))

#define XNAME_INDEX(xn)  ((VAL(xn) - POINTER_TAG) >> PRIMARY_TAG_SIZE)

#define DEBUG_SAVE  0  /* define to 1 for some trace info */

typedef struct {
  XUINT_32 val;
} xobj;

static xobj mk_xobj( obj a )
{
  xobj x;
  x.val = HOST_TO_BIG_ENDIAN_32( VAL(a) );
  return x;
}

#define XOBJ(o) mk_xobj(o)
#define XINT(v) ((XUINT_32)(v))

static obj *ref_str_tbl;
static struct file_header hdr;
static UINT_32 num_objects;
static UINT_32 known_num_objects;
static UINT_32 ref_str_tbl_size;

/************************************************************************
 **
 **  Output buffer management (inlined)
 **
 ************************************************************************/

static char *output_ptr;
static char *output_buffer;
static char *output_limit;
static FILE *output_file;
static UINT_32 output_bytes_written;

static UINT_32 output_posn( void )
{
  return output_bytes_written + (output_ptr - output_buffer);
}

static void init_output( FILE *f, UINT_32 len )
{
  output_buffer = output_ptr = (char *)malloc( len );
  output_limit = output_buffer + len;
  output_file = f;
  output_bytes_written = 0;
}

static void output_flush( UINT_32 len )
{
  UINT_32 flsh = output_ptr - output_buffer;

  /*printf( "** flushing %u bytes\n", flsh );*/
  if (flsh && (fwrite( output_buffer, 1, flsh, output_file ) != flsh))
    {
      perror( "save-image" );
    }

  output_bytes_written += flsh;

  output_ptr = output_buffer;
  if (output_ptr + len > output_limit)
    {
      UINT_32 elbow_room = len + 1024;
      free( output_buffer ); /* free+malloc saves the copy in realloc */
      output_buffer = (char *)malloc( elbow_room );
      output_ptr = output_buffer;
      output_limit = output_ptr + elbow_room;
    }
  output_ptr = output_buffer + len;
}

static void output_close( void )
{
  output_flush(0);
  free( output_buffer );
}


static _rs_inline void *output_alloc( UINT_32 len )
{
  if (output_ptr + len <= output_limit)
    {
      char *p = output_ptr;
      output_ptr += len;
      return (void *)p;
    }
  else
    {
      output_flush(len);
      return (void *)output_buffer;
    }
}

/************************************************************************
 **
 **  Output Reference Transforming
 **
 ************************************************************************/

static _rs_inline obj unswizzle( obj item )
{
  if (OBJ_ISA_PTR(item))
    {
      obj swz = PTR_TO_HDRPTR(replace_ptr(item))->pob_class;
      assert( OBJ_ISA_PTR(swz) 
	      && ((VAL(swz)>>PRIMARY_TAG_SIZE) < known_num_objects) );
      return swz;
    }
  else
    {
      return item;
    }
}

/************************************************************************
 **
 **  Output Processing
 **
 ************************************************************************/

static void write_part_ref( struct part_descr *pd );

static void *alloc_output_frame( UINT_32 size_in_bytes, 
				 obj preamble,
				 enum load1_mode mode )
{
  XUINT_32 *p;
  UINT_32 alen;

  alen = ROUND_UP( size_in_bytes + 3*sizeof(XUINT_32) );

  p = (XUINT_32 *)output_alloc( alen );

  *p++ = HOST_TO_BIG_ENDIAN_32( size_in_bytes );
  *p++ = HOST_TO_BIG_ENDIAN_32( mode );
  *(xobj *)p = XOBJ( unswizzle(preamble) );
  p++;
  return (void *)p;
}

/*
 *  Allocate an output frame with TWO preamble words;
 *  Use of this must be matched by appropriate logic
 *  in loadimage.c (e.g., see the LOAD1_BIGNUM entry in load_image_file())
 */

static void *alloc_output_frame2( UINT_32 size_in_bytes, 
                                  obj pre1,
                                  obj pre2,
                                  enum load1_mode mode )
{
  XUINT_32 *p;
  UINT_32 alen;

  alen = ROUND_UP( size_in_bytes + 4*sizeof(XUINT_32) );

  p = (XUINT_32 *)output_alloc( alen );

  *p++ = HOST_TO_BIG_ENDIAN_32( size_in_bytes );
  *p++ = HOST_TO_BIG_ENDIAN_32( mode );
  *(xobj *)p = XOBJ( unswizzle(pre1) );
  p++;
  *(xobj *)p = XOBJ( unswizzle(pre2) );
  p++;
  return (void *)p;
}

static void write_gvec( obj gvec, obj orig_class )
{
obj *s, *limit;
xobj *d;
UINT_32 len;

    len = SIZEOF_PTR( gvec );
    s = (obj *)PTR_TO_DATAPTR( gvec );
    limit = (obj *)(((char *)s) + len);
    
    len = (len / sizeof(obj)) * sizeof(xobj);

    d = (xobj *)alloc_output_frame( len, orig_class, LOAD1_ARRAY32 );

    while (s < limit)
      *d++ = XOBJ( unswizzle( *s++ ) );
}

static void write_mixvec( obj mixvec, obj orig_class, UINT_32 nslots )
{
obj *s;
xobj *d;
UINT_32 i, len, *s_word, *limit_word;
XUINT_32 *d_word;

    s = (obj *)PTR_TO_DATAPTR( mixvec );

    len = SIZEOF_PTR( mixvec );
    len = (len / sizeof(obj)) * sizeof(xobj);
    d = (xobj *)alloc_output_frame( len, orig_class, LOAD1_ARRAY32 );

    for (i=0; i<nslots; i++)
      {
	*d++ = XOBJ( unswizzle( *s++ ) );
      }

    /* write the rest as an array of 32bits */

    limit_word = (UINT_32 *)(((char *)s) + SIZEOF_PTR(mixvec));
    s_word = (UINT_32 *)s;
    d_word = (XUINT_32 *)d;

    while (s_word < limit_word)
	*d_word++ = HOST_TO_BIG_ENDIAN_32( *s_word++ );
}

static void write_alloc_area( obj item, obj orig_class )
{
  write_mixvec( item, orig_class, 2 );
}



void write_template( obj tmpl, obj orig_class )
{
obj link2;
struct function_descr *fn;
obj *s, *limit;
xobj *d;
UINT_32 len;
struct function_descr **pfns;
unsigned i;

    link2 = gvec_read(tmpl,TEMPLATE_LINKAGE);
    if (OBJ_ISA_PTR(link2))
    {
#ifdef DEBUG_0
	printf( "template %#x is already unswizzled\n", VAL(tmpl) );
#endif /* DEBUG_0 */
	/* this template is already unswizzled, so just
	   write it out as a gvec */
	write_gvec( tmpl, orig_class );
    }
    else
    {
#ifdef DEBUG_0
	printf( "template %#x is being unswizzled\n", VAL(tmpl) );
#endif /* DEBUG_0 */

	fn = (struct function_descr *)OBJ_TO_RAW_PTR(link2);
	assert( fn );

	/* it can't be a stub; it was unstubbed during traversal */

	assert( fn->in_part->tag < STUB_PART_TAG );

	/* and, it's had a name assigned */

	assert( fn->in_part->unswizzled_as );
	
	/* find the function's index in the part */

	pfns = fn->in_part->functions;
	for (i=0; pfns[i]; i++)
	{
	    if (pfns[i] == fn)
		break;
	}
#ifdef DEBUG_0
	if (pfns[i])
	    printf( "\t%s is %s.%d\n", fn->name, fn->in_part->name, i );
#endif /* DEBUG_0 */
	if (!pfns[i])
	  {
	    UINT_8 *s;

	    fprintf( stderr, "Warning: (function_descr %p) `", fn );
	    for (s = (UINT_8 *)fn->name; *s; s++)
	      {
		int ch = *(unsigned char *)s;
		if (isprint(ch))
		  fputc( ch, stderr );
		else
		  fprintf( stderr, "\\%03o", *(UINT_8 *)s );
	      }
	    fprintf( stderr, "'\nis not found in (part %p) `", fn->in_part );
	    for (s = (UINT_8 *)fn->in_part->name; *s; s++)
	      {
		if (isprint(*s))
		  fputc( *s, stderr );
		else
		  fprintf( stderr, "\\%03o", *s );
	      }
	    fprintf( stderr, "'\n" );
	  }

	len = SIZEOF_PTR( tmpl );
	s = (obj *)PTR_TO_DATAPTR( tmpl );
	limit = (obj *)(((char *)s) + len);
	
	len = (len / sizeof(obj)) * sizeof(xobj);
	d = (xobj *)alloc_output_frame( len, orig_class, LOAD1_ARRAY32 );
    
	*d++ = XOBJ( int2fx(i) );
	*d++ = XOBJ( OBJ(fn->in_part->unswizzled_as) );
	s += 2;
	while (s < limit)
	    *d++ = XOBJ( unswizzle( *s++ ) );
    }
}

void write_array_32( obj item, obj orig_class )
{
UINT_32 *s, *limit, len;
XUINT_32 *d;

    len = SIZEOF_PTR( item );
    s = (UINT_32 *)PTR_TO_DATAPTR( item );
    limit = (UINT_32 *)(((char *)s) + len);

    len = (len / sizeof(UINT_32)) * sizeof(XUINT_32);
    d = (XUINT_32 *)alloc_output_frame( len, orig_class, LOAD1_ARRAY32 );
    
    while (s < limit)
	*d++ = HOST_TO_BIG_ENDIAN_32( *s++ );
}


void write_array_64( obj item, obj orig_class )
{
UINT_32 len;
IEEE_64 *s, *limit, *d;

    len = SIZEOF_PTR( item );
    s = (IEEE_64 *)PTR_TO_DATAPTR( item );
    limit = (IEEE_64 *)(((char *)s) + len);

    d = (IEEE_64 *)alloc_output_frame( len, orig_class,  LOAD1_ARRAY64 );
    
    while (s < limit)
	*d++ = HOST_TO_BIG_ENDIAN_IEEE_64( *s++ );
}

/*  Writes a frame of the form

    +---------------+
    |   size        |
    +---------------+
    |    mode       |
    +---------------+
    |   preamble    |
    +---------------+
    |               |
    :     text      :
    |      ....     |
    +---------------+

   which is the format used by LOAD1_REF, LOAD1_SYMBOL,
   LOAD1_PART, and LOAD1_ARRAY8

   (where the content of the `preamble' word depends on the mode:

        mode               use of preamble word
        ---------------    ---------------------------------------------------
   	LOAD1_REF	=> string hash value
	LOAD1_SYMBOL	=> string hash value (NOT same as symbol's hash value)
	LOAD1_PART	=> tag of part
	LOAD1_ARRAY8	=> bytevector class, e.g. reference to <string>
        LOAD1_BIGNUM    => 0
   )

   Note that the string ('text') had better be ZERO padded to a
   full word length!
*/

static void write_string_frame( enum load1_mode mode, obj preamble, obj text )
{
void *d;
UINT_32 len;

    len = SIZEOF_PTR( text );
    d = alloc_output_frame( len, preamble, mode );
    memcpy( d, PTR_TO_DATAPTR(text), ROUND_UP( len ) );
}


static void write_array_8( obj item, obj orig_class )
{
    write_string_frame( LOAD1_ARRAY8, orig_class, item );
}

static void write_ref( obj item, obj orig_class )
{
  if (EQ(orig_class,FALSE_OBJ))
    {
      /* it's a part descriptor */
      /*struct part_descr *p = (struct part_descr *)OBJ_TO_RAW_PTR(item);
      printf( "ref: PART %s %d\n", p->in_module->name, p->tag );*/
      write_part_ref( (struct part_descr *)OBJ_TO_RAW_PTR(item) );
    }
  else
    {
      /* it's a normal reference -- write out the key string */

      obj str, ext_name = PTR_TO_HDRPTR(item)->pob_class;
      obj hash;
      unsigned i = XNAME_INDEX( ext_name );

      assert( i < ref_str_tbl_size );

      str = ref_str_tbl[i];
      hash = raw_bytes_hash( PTR_TO_DATAPTR(str), SIZEOF_PTR(str)-1 );

      /*printf( "ref: %s\n", PTR_TO_DATAPTR(str) );*/

      write_string_frame( LOAD1_REF, hash, str );
    }
}

static void write_symbol( obj item, obj orig_class )
{
  write_string_frame( LOAD1_SYMBOL, 
		      ZERO,
		      gvec_read( item, SYMBOL_STR ) );
}

static void write_bignum( obj item, obj orig_class )
{
  extern rs_bool loading_image;
  void *d;
  UINT_32 len;
  obj bigstr;

  /* bignum_to_string_obj() is going to allocate a string,
   * and <string> is probably totally munged up right now.
   * Hence, we set loading_image to TRUE so that we don't
   * trap in alloc()'s assertions about <string>
   */
  loading_image = 1;
  bigstr = bignum_to_string_obj( item, 10 );
  loading_image = 0;

  len = SIZEOF_PTR( bigstr );
  d = alloc_output_frame2( len, 
                           orig_class, 
                           CLASSOF_PTR( gvec_ref( item, SLOT(2) ) ),
                           LOAD1_BIGNUM );
  memcpy( d, PTR_TO_DATAPTR( bigstr ), ROUND_UP( len ) );
}


/*
  Here, we are writing out a <PartDescr>.  At load time,
  it will be swizzled to point directly to the part_descr
*/

static void write_part( obj item, obj orig_class )
{
  /* NOTE: you can't use string_text() on a <string> here
   *       because the string has probably been munged
   *       as part of the traversal process
   */
  /*
  printf( "write_part_desc %s %d\n", 
          (char*)PTR_TO_DATAPTR( gvec_read( item, PART_DESCR_MODULE_NAME ) ),
          fx2int( gvec_read( item, PART_DESCR_PART_TAG ) ) );
  */
  write_string_frame( LOAD1_PART,
                      gvec_read( item, PART_DESCR_PART_TAG ),
                      gvec_read( item, PART_DESCR_MODULE_NAME ) );
}

static void write_part_ref( struct part_descr *pd )
{
char *d;
UINT_32 len;

/* 
 printf( "write_part_ref %s %s %u\n", 
         pd->in_module->name,
         pd->name,
         (unsigned)pd->tag );
*/
#ifdef DEBUG_0
    printf( "Writing part %#x", (UINT_32)pd );
    fflush(stdout);
    printf( " (%s) as %#x\n", pd->name, pd->unswizzled_as );
#endif /* DEBUG_0 */


    if (pd->in_module->loaded_from)
      {
	len = strlen( pd->in_module->name )
	      + 1 + strlen(pd->in_module->loaded_from);
      }
    else
      {
	len = strlen( pd->in_module->name );
      }
    d = (char *)alloc_output_frame( len+1, int2fx(pd->tag), LOAD1_PART );
    if (pd->in_module->loaded_from)
      {
	sprintf( d, "%s|%s", pd->in_module->name, pd->in_module->loaded_from );
      }
    else
      {
	memcpy( d, pd->in_module->name, len );
      }
    /*  the above memcpy only filled in `len' bytes, and
     *  alloc_output_frame() allocated `ROUND_UP(len+1)',
     *  so clear out the rest
     */
    memset( d + len, 0, ROUND_UP(len+1) - len );
}

static void queue_write( SaveQueue *q, 
			 void (*proc)( obj item, obj orig_class ) )
{
  UINT_32 i, n = q->count;
  struct queue_entry *p;

  num_objects += n;

  for (i=0, p=q->contents; i<n; i++, p++)
    proc( p->thing, p->orig_class );
}

static obj alloc_hash_table( void )
{
  return make4( vector_class,
		make_empty_vector(8),
		int2fx(3),
		ZERO,
		vector_class );
}

/************************************************************************
 **
 **  Top-level Interface
 **
 ************************************************************************/

void hi_init_output( FILE *f,
		     obj refs_vec, obj ref_names, obj root, 
		     SaveQueue *used_refs,
		     UINT_32 n_obj )
{
  unsigned i;

  /*
   *  for those reference heap objects that have been used
   *  (as we can tell, because their pob_class is non-ONE)
   *  create an entry in the ref_str_table for their reference
   *  string.
   */

  ref_str_tbl_size = used_refs->count;
  ref_str_tbl = (obj *)malloc( used_refs->count * sizeof(obj) );

  for (i=0; i<SIZEOF_PTR(refs_vec); i+=SLOT(1))
    {
      obj key, val, ext_name;

      key = gvec_ref( refs_vec, i );
      val = gvec_ref( ref_names, i );
      ext_name = PTR_TO_HDRPTR(key)->pob_class;
      
      if (!EQ(ext_name,int2fx(1)))
	{
	  UINT_32 xname = XNAME_INDEX( ext_name );

	  assert( OBJ_ISA_PTR(ext_name) );
	  assert( xname < used_refs->count );
	  ref_str_tbl[ xname ] = val;
	}
    }

  memset( &hdr, 0, sizeof hdr );
  
  init_output( f, 1280000 );
  num_objects = 0;  /* we're going to count ourselves, to verify */
  known_num_objects = n_obj;
  hdr.entry_object_offt = HOST_TO_BIG_ENDIAN_32( VAL( unswizzle( root ) ) );

  memset( output_alloc( FILE_HDR_OFFSET ), 
	  ' ', 
	  FILE_HDR_OFFSET );
  memset( output_alloc( sizeof(struct file_header) ),
	  0, 
	  sizeof(struct file_header) );
  assert( output_posn() == FILE_DATA_OFFSET );
}

void hi_done_output( void )
{
  output_close();

  hdr.num_objects = HOST_TO_BIG_ENDIAN_32( num_objects );
  hdr.magic = HOST_TO_BIG_ENDIAN_32( IMAGE_MAGIC_NUMBER );
  hdr.version = HOST_TO_BIG_ENDIAN_32( IMAGE_VERSION_NUMBER );

  fseek( output_file, FILE_HDR_OFFSET, SEEK_SET );
  fwrite( &hdr, sizeof(struct file_header), 1, output_file );
  fclose( output_file );
  free( ref_str_tbl );

#if DEBUG_SAVE
  printf( "wrote out %u objects in %u bytes\n", 
	  num_objects, output_bytes_written );
#endif
}

void hi_output_mode2( enum load2_mode mode2,
		      SaveQueue **queues, unsigned num_queues )
{
  UINT_32 i, cnt = 0;
  unsigned k;
  xobj *d;

  if (!hdr.data_area_length)
    {
      UINT_32 dalen = output_posn() - FILE_DATA_OFFSET;
      hdr.data_area_length = HOST_TO_BIG_ENDIAN_32( dalen );
    }

  for (k=0; k<num_queues; k++)
    cnt += queues[k]->count;

  hdr.load2_offset[mode2] = HOST_TO_BIG_ENDIAN_32( output_posn() );
  hdr.load2_count[mode2] = HOST_TO_BIG_ENDIAN_32( cnt );
  d = (xobj *)output_alloc( cnt * sizeof( xobj ) );

  for (k=0; k<num_queues; k++)
    {
      UINT_32 n = queues[k]->count;
      struct queue_entry *p = queues[k]->contents;

      for (i=0; i<n; i++, p++)
	{
	  obj save_name = PTR_TO_HDRPTR(p->thing)->pob_class;
	  *d++ = XOBJ(save_name);
	}
    }
}

void hi_output_refs( SaveQueue *q )
{
  queue_write( q, write_ref );
}

/************************************************************************
 **
 **  Exported Function Structure
 **
 ************************************************************************/

#define QWRITE(n,one) static void n( SaveQueue *q ) { queue_write( q, one ); }

QWRITE(writeq_gvec,       write_gvec)      
QWRITE(writeq_array_8,    write_array_8)   
QWRITE(writeq_symbol,     write_symbol)    
QWRITE(writeq_part,       write_part)      
QWRITE(writeq_template,   write_template)  
QWRITE(writeq_array_64,   write_array_64)  
QWRITE(writeq_array_32,   write_array_32)  
QWRITE(writeq_alloc_area, write_alloc_area)
QWRITE(writeq_bignum,     write_bignum)

struct writer_info hi_writers[NUM_CLASS_MODES] = {
  { /* 0: gvec */ 	writeq_gvec,         LOAD2_GVEC },
  { /* 1: bvec */	writeq_array_8,      LOAD2_CLASS_ONLY },
  { /* 2: symbol */	writeq_symbol,       LOAD2_NOP },
  { /* 3: part */	writeq_part,         LOAD2_NOP },
  { /* 4: template */	writeq_template,     LOAD2_TEMPLATE },
  { /* 5: part-cont */	writeq_gvec,         LOAD2_PARTCONT },
  { /* 6: longfloat */	writeq_array_64,     LOAD2_CLASS_ONLY },
  { /* 7: uint32*n */	writeq_array_32,     LOAD2_CLASS_ONLY },
  { /* 8: alloc-area */ writeq_alloc_area,   LOAD2_MIXVEC_2 },
  { /* 9: bignum     */ writeq_bignum,       LOAD2_BIGNUM } };

