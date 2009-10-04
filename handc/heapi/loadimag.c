/*-----------------------------------------------------------------*-C-*---
 * File:    handc/heapi/loadimag.c
 *
 *          Copyright (C)1997 Donovan Kolbly <d.kolbly@rscheme.org>
 *          as part of the RScheme project, licensed for free use.
 *          See <http://www.rscheme.org/> for the latest information.
 *
 * File version:     1.22
 * File mod date:    2003-10-13 13:02:28
 * System build:     v0.7.3.4-b7u, 2007-05-30
 *
 * Purpose:          image loader (0.5 style)
 *------------------------------------------------------------------------*/

#include "heapi.h"
#include "imagfile.h"
#include <rscheme/scheme.h>
#include <rscheme/hashmain.h>
#include <rscheme/hashfn.h>
#include "mapf.h"
#include <string.h>
#include <rscheme/smemory.h>
#include <rscheme/linktype.h>
#include <rscheme/allocns.h>
#include <stdlib.h>

#define ROUND_UP(n) (((n) + sizeof(XUINT_32) - 1) & (~(sizeof(XUINT_32)-1)))

static obj pd_class = FALSE_OBJ;

/*  This is a special flag that tells the allocation subsystem
    that the "class" pointer may not be a real pointer
*/

rs_bool loading_image;

#define NO_PART_TOKEN MAKE_UNIQ_OBJ(MISSING_TAG)

#define IGNORE_FAULTS

/* #define LOADIMAGE_TRACE */
/* #define PAUSE_IN_MODE2 */

#ifdef LOADIMAGE_TRACE
#define INCLUDE_COMMENTS 0

#include <stdarg.h>
/* #define SKIP_TEMPLATE_SWIZZLING */

FILE *debug_file;
#if INCLUDE_COMMENTS
#define COMMENT_TABLE_SIZE (700000)

struct cmtrec {
  obj key;
  char *value;
} comments[COMMENT_TABLE_SIZE];

/*
static void init_comments( void )
{
  comments = calloc( COMMENT_TABLE_SIZE, sizeof( struct cmtrec ) );
}
*/
extern obj obj_hash( obj key );

static unsigned initix( obj key )
{
  obj h = obj_hash( key );
  unsigned i = fx2int(h);
  return i % COMMENT_TABLE_SIZE;
}
 
static char *get_comment( obj key )
{
  unsigned i = initix( key );
  while (!EQ(comments[i].key,key))
    {
      if (EQ(comments[i].key,ZERO))
	return NULL;

      i = (i + 1) % COMMENT_TABLE_SIZE;
    }
  return comments[i].value;
}

static void add_comment( obj key, char *cmt )
{
  unsigned i = initix( key );

  while (!EQ(comments[i].key,key) && !EQ(comments[i].key,ZERO))
    {
      i = (i + 1) % COMMENT_TABLE_SIZE;
    }
  comments[i].value = strdup( cmt );
  comments[i].key = key;
}

#endif
char *comment_str, info_buffer[1024*1024];

static void tri_printf( const char *msg, ... )
{
  va_list a;
  
  va_start(a,msg);
  vfprintf( debug_file, msg, a );
  va_end(a);
  fflush( debug_file );
}

static void tri_swizzled( int offset, obj from, obj to )
{
  char *comment = NULL;
  char temp[1000], *d;

  d = temp;

#if INCLUDE_COMMENTS
  if (OBJ_ISA_PTR(from))
    {
      obj k = OBJ( VAL(from) - POINTER_TAG );
      comment = get_comment( k );
    }
#endif
  if (offset == -1)
    d += sprintf( d, "  class" );
  else if (offset == -2)
    d += sprintf( d, "   self" );
  else
    d += sprintf( d, "    [%d]", offset/SLOT(1) );
  
  d += sprintf( d, " %#lx => %#lx", VAL(from), VAL(to) );
    
  if (OBJ_ISA_PTR(to))
    {
      d += sprintf( d, " *" );
    }

  if (comment)
    {
      d += sprintf( d, " %s", comment );
    }
  tri_printf( "%s\n", temp );
}


#endif

#define NUM_USEFUL_LOAD2_MODES (6)


static rs_bool remap_constants; /* do certain constants (like #none) need
				to get switched around? (ie, for loading
				version 1 images) */

typedef void swizzler_fn( obj item );

static void swizzle_class_only( obj item );
static void swizzle_gvec( obj item );
static void swizzle_alloc_area( obj item );
static void swizzle_template( obj item );
static void swizzle_partcont( obj item );
static void swizzle_bignum( obj item );

static struct {
    enum load2_mode mode;
    swizzler_fn		*swizzler;
} useful_load2_modes[NUM_USEFUL_LOAD2_MODES] =
    { { LOAD2_CLASS_ONLY, swizzle_class_only },
      { LOAD2_GVEC, swizzle_gvec },
      { LOAD2_TEMPLATE, swizzle_template },
      { LOAD2_PARTCONT, swizzle_partcont },
      { LOAD2_MIXVEC_2, swizzle_alloc_area },
      { LOAD2_BIGNUM, swizzle_bignum } };


static obj load_ref( void *src, UINT_32 len, obj preamble );
static obj load_symbol( void *src, UINT_32 len, obj preamble );
static obj load_part( void *src, UINT_32 len, obj preamble );
static obj load_array_8( void *src, UINT_32 len, obj preamble );
static obj load_array_16( void *src, UINT_32 len, obj preamble );
static obj load_array_32( void *src, UINT_32 len, obj preamble );
static obj load_array_64( void *src, UINT_32 len, obj preamble );
static obj load_bignum( void *src, UINT_32 len, obj pre1, obj pre2 );

static void *find_a_part( const char *module, int tag );

static obj *obj_table, *obj_table_limit;
static obj reference_table;

static _rs_volatile void load_image_error( const char *msg )
{
    fprintf( stderr, "Load error: %s\n", msg );
    abort();
    while (1);
}

#if WORD_IS_32_BITS
#define SIGN_EXT_FROM_32(x)  (x)
#else
#define SIGN_EXT_FROM_32(x)  sign_ext_from_32( x )

static _rs_inline obj sign_ext_from_32( obj x )
{
  if (VAL(x) & 0x80000000UL)
    {
      /* plug in the sign bits by hand... */
      return OBJ( VAL(x) | 0xFFFFFFFF00000000UL );
    }
  else
    {
      return x;
    }
}
#endif

static _rs_inline obj swizzle( obj item )
{
  if (OBJ_ISA_PTR(item))
    {
      obj result, *p;
      UINT_32 k = (VAL(item) - POINTER_TAG) >> PRIMARY_TAG_SIZE;
      
      p = &obj_table[k];
      assert( p < obj_table_limit );
      
      result = *p;
#ifdef LOADIMAGE_TRACE
      tri_printf( "\t%#x ==> %#x\n", VAL(item), VAL(result) );
#endif
      return result;
    }
  else if (OBJ_ISA_FIXNUM(item))
    {
      return SIGN_EXT_FROM_32(item);
    }
  else
    {
      if (remap_constants && OBJ_ISA_IMMOB(item))
	{
	  switch ((int)SECONDARY_TAG(item))
	    {
	    case 4: /* old #undef */
	      item = UNDEFINED_OBJ;
	      break;
	    case 5: /* old #uninit */
	      item = UNINITIALIZED_OBJ;
	      break;
	    case 6: /* old #none */
	      item = NOVALUE_OBJ;
	      break;
	    case 7: /* old #unbound */
	      item = UNBOUND_OBJ;
	      break;
	    }
	}
      return item;
    }
}


static void swizzle_class_only( obj item )
{
POBHeader *h = PTR_TO_HDRPTR(item);
obj n;

    assert( OBJ_ISA_PTR(item) );
    n = swizzle( h->pob_class );
#ifdef LOADIMAGE_TRACE
    tri_swizzled( -1, h->pob_class, n );
#endif
    h->pob_class = n;
}

static void swizzle_mixvec( obj item, UINT_32 num_slots )
{
POBHeader *h = PTR_TO_HDRPTR(item);
obj n, *d = (obj *)PTR_TO_DATAPTR(item);
UINT_32 i, len;

    assert( OBJ_ISA_PTR(item) );
    n = swizzle( h->pob_class );
    h->pob_class = n;

    len = SLOT(num_slots);
    for (i=0; i<len; i+=SLOT(1))
    {
	n = swizzle( *d++ );
#ifndef NDEBUG
	/* if we're in debug mode, gvec_write_init() will assert that
	 * the target slot is truly uninitialized (alloc() fills w/
	 * DEBUG_TRAP_OBJ to indicate uninitialzed data
	 */
	d[-1] = DEBUG_TRAP_OBJ;
#endif
        gvec_write_init( item, i, n );
    }
}

static void swizzle_alloc_area( obj item )
{
  AllocArea *aa = (AllocArea *)PTR_TO_DATAPTR(item);

  assert( OBJ_ISA_PTR(item) );
  swizzle_mixvec( item, 2 );

  /* reinstall the function pointer */
  aa->allocfn = default_alloc_obj;
}

static void swizzle_gvec( obj item )
{
POBHeader *h = PTR_TO_HDRPTR(item);
obj n, *d = (obj *)PTR_TO_DATAPTR(item);
UINT_32 i, len = SIZEOF_PTR(item);

    assert( OBJ_ISA_PTR(item) );
    n = swizzle( h->pob_class );
#ifdef LOADIMAGE_TRACE
    tri_swizzled( -1, h->pob_class, n );
#endif
    h->pob_class = n;

    for (i=0; i<len; i+=SLOT(1))
    {
	n = swizzle( *d );
#ifdef LOADIMAGE_TRACE
	tri_swizzled( i, *d, n );
#endif /* LOADIMAGE_TRACE */
	/* this acts like an initializing write because we
	 * don't consider the previous write (during unpacking)
	 * to have been a "real" write -- we were just temporarily
	 * storing some random bits there
	 * ... if we did `fresh' write here, then those random bits
	 *     would be interpreted as a pointer, Which Could Be Bad
	 *     (in particular, those random bits DO look like a pointer,
	 *     if we are storing a pointer into that slot)
	 */
#ifndef NDEBUG
	/* if we're in debug mode, gvec_write_init() will assert that
	 * the target slot is truly uninitialized (alloc() fills w/
	 * DEBUG_TRAP_OBJ to indicate uninitialzed data
	 */
	*((obj *)((char *)PTR_TO_DATAPTR(item) + i)) = DEBUG_TRAP_OBJ;
#endif
        gvec_write_init( item, i, n );
	d++;
    }
}

static void swizzle_template( obj item )
{
struct part_descr *part_ptr;
UINT_32 ix;
jump_addr entry_point;
struct function_descr *fn;
obj in_part;

    assert( OBJ_ISA_PTR(item) );
    swizzle_gvec( item );
    if (SIZEOF_PTR(item) < 8)
    {
	fprintf( stderr, 
	    "Warning: %#lx (TEMPLATE) is too small! (code_ptr = %#lx)\n",
	    VAL(item), VAL(gvec_read(item,0)) );
	return;
    }
#ifndef SKIP_TEMPLATE_SWIZZLING

    in_part = gvec_read( item, SLOT(1) );
    /* if the "in_part" is a pointer, then it is presumably
       a pointer to a <part-descr> which we created because
       the part so referred to doesn't exist, in which case we
       should leave the <template> as it is (no wierd swizzling
       of code pointers)
     */

    if (!OBJ_ISA_PTR(in_part) && !EQ(in_part,NO_PART_TOKEN))
      {
	part_ptr = (struct part_descr *) OBJ_TO_RAW_PTR(in_part);
	
	if (!part_ptr)
	  {
	    fprintf( stderr, "Error: %#lx (TEMPLATE) has no part ptr\n",
		     VAL(item) );
	    abort();  /* really oughtn't to have gotten here */
	  }
	ix = fx2int( gvec_read( item, SLOT(0) ) );
	fn = part_ptr->functions[ix];
	entry_point = fn->monotones[0];
	
#if 0
	printf( "template %#x ==> {%d.%d.0} ==> ", 
	       VAL(item), part_ptr->tag, ix );
	printf( " %#x\n", entry_point );
#endif
	gvec_write_fresh_non_ptr( item, 
				  SLOT(0), 
				  JUMP_ADDR_TO_OBJ(entry_point) );
	gvec_write_fresh_non_ptr( item, 
				  SLOT(1), 
				  RAW_PTR_TO_OBJ(fn) );
      }
#if 0
else
  {
	printf( "template %#x ==> in_part %#x\n", VAL(item), VAL(in_part) );
      }
#endif
#endif /* SKIP_TEMPLATE_SWIZZLING */
}

static void swizzle_partcont( obj item )
{
    assert( OBJ_ISA_PTR(item) );
    swizzle_gvec( item );
}

/* options is either #f or the class of used to create 
   <part-descr>'s for unresolved parts
*/

obj load_image_file( const char *path, 
		     obj the_reference_table, 
		     obj options,
		     int *pformat_version )
{
struct file_header *hdr;
XUINT_32 i, *input_ptr, *input_buffer;
obj result;
int format_version;

#ifdef LOADIMAGE_TRACE
extern obj alloc_hash_table( void );

    debug_file = fopen( "/tmp/loadimg.trc", "w" );
#endif /* LOADIMAGE_TRACE */
    pd_class = options;

    if (!mapf_open( path ))
	return FALSE_OBJ;
	
    reference_table = the_reference_table;
    mapf_seek( FILE_HDR_OFFSET );
    hdr = (struct file_header *)mapf_read( sizeof( struct file_header ) );
#ifndef PLATFORM_IS_BIG_ENDIAN
    hdr->magic = BIG_ENDIAN_TO_HOST_32( hdr->magic );
    hdr->version = BIG_ENDIAN_TO_HOST_32( hdr->version );
    for (i=0; i<NUM_LOAD2_MODES; i++)
    {
	hdr->load2_offset[i] = BIG_ENDIAN_TO_HOST_32( hdr->load2_offset[i] );
	hdr->load2_count[i] = BIG_ENDIAN_TO_HOST_32( hdr->load2_count[i] );
    }
    hdr->data_area_length = BIG_ENDIAN_TO_HOST_32( hdr->data_area_length );
    hdr->num_objects = BIG_ENDIAN_TO_HOST_32( hdr->num_objects );
    hdr->entry_object_offt = BIG_ENDIAN_TO_HOST_32( hdr->entry_object_offt );
#endif

    if (hdr->magic != IMAGE_MAGIC_NUMBER)
    {
      load_image_error( "Not an image file" );
    }
    
    remap_constants = NO;
    format_version = 0;

    switch (hdr->version)
    {
    case IMAGE_VERSION_06_BOOT:
      /* an 0.6 bootable image file */
      if (truish(the_reference_table))
	load_image_error( "Cannot load bootable into running system" );
      format_version = FMTV_RSCHEME_0_6_BOOT;
      break;

    case IMAGE_VERSION_06:
      /* 0.6-format image file */
      if (!truish(the_reference_table))
	load_image_error( "Cannot boot from regular image" );
      format_version = FMTV_RSCHEME_0_6;
      break;
      
    case IMAGE_VERSION_NUMBER:
      format_version = FMTV_RSCHEME_0_5;
      break;

	case 1:
		/* we can handle version 1 images by remapping
		    some small constants */
	    	remap_constants = YES;
		break;
	default:
	    load_image_error( "Version number is incorrect" );
    }
    
    obj_table = (obj *)malloc( sizeof(obj) * hdr->num_objects );
    if (!obj_table)
    {
	load_image_error( "Could not malloc space for object table" );
    }
    obj_table_limit = obj_table + hdr->num_objects;

    /*  Pass I. Allocate space for and 
    		load the data for the objects
    		(and execute LOAD1 commands, involving
		 table lookups (REF and SYMBOL), and convert
		 endianess) */

    input_buffer = (XUINT_32 *)mapf_read( hdr->data_area_length );
    input_ptr = input_buffer;

    loading_image = YES;
    
    for (i=0; i<hdr->num_objects; i++)
    {
    UINT_32 len;
    enum load1_mode mode1;
    obj preamble, item;
    void *p;
    
	len = BIG_ENDIAN_TO_HOST_32( input_ptr[0] );
	mode1 = (enum load1_mode) BIG_ENDIAN_TO_HOST_32( input_ptr[1] );
	preamble = OBJ( BIG_ENDIAN_TO_HOST_32( input_ptr[2] ) );
	p = &input_ptr[3];
#ifdef LOADIMAGE_TRACE
	tri_printf( "Loading %#x := mode(I) %d (%u bytes) preamble %#x\n",
		    (i*4+3), mode1, len, VAL(preamble) );
	comment_str = NULL;
#endif /* LOADIMAGE_TRACE */
	switch (mode1)
	{
	    case LOAD1_REF:	item = load_ref( p, len, preamble );
	    			break;
	    case LOAD1_SYMBOL:	item = load_symbol( p, len, preamble );
	    			break;
	    case LOAD1_PART:	item = load_part( p, len, preamble );
	    			break;
	    case LOAD1_ARRAY8:	item = load_array_8( p, len, preamble );
	    			break;
	    case LOAD1_ARRAY16:	item = load_array_16( p, len, preamble );
	    			break;
	    case LOAD1_ARRAY32:	item = load_array_32( p, len, preamble );
	    			break;
	    case LOAD1_ARRAY64:	item = load_array_64( p, len, preamble );
	    			break;
            case LOAD1_BIGNUM:  
              {
                obj pre2;

                pre2 = OBJ( BIG_ENDIAN_TO_HOST_32( input_ptr[3] ) );
                p = &input_ptr[4];
                item = load_bignum( p, len, preamble, pre2 );
                len += sizeof(XUINT_32);
              }
              break;
	    default:		item = ZERO;
	    			assert( !EQ(item,ZERO) );
				break;
	}
#ifdef LOADIMAGE_TRACE
	if (comment_str)
	{
	  obj key = int2fx(i);
	
#if INCLUDE_COMMENTS
	  add_comment( key, comment_str );
#endif
	}
        tri_printf( "\t\t:= %#x %s\n", 
		    VAL(item), 
		    comment_str ? comment_str : "" );
#endif /* LOADIMAGE_TRACE */
	obj_table[i] = item;
	input_ptr = (XUINT_32 *)(((char *)input_ptr) 
				 + ROUND_UP( len + 3*sizeof(XUINT_32) ) );
    }
#ifdef LOADIMAGE_TRACE
    fflush( debug_file );
#endif /* LOADIMAGE_TRACE */

    /*  Pass II.  Go through the LOAD2 commands, executing
    		  them in order
    */
    
    for (i=0; i<NUM_USEFUL_LOAD2_MODES; i++)
    {
    enum load2_mode mode2 = useful_load2_modes[i].mode;
    swizzler_fn *proc = useful_load2_modes[i].swizzler;
    XUINT_32 *table;
    UINT_32 j, count;
    
	count = hdr->load2_count[mode2];
#ifdef LOADIMAGE_TRACE
        tri_printf( "** LOAD2 (mode2 = %d): %lu\n", mode2, count );
#endif
        if (count == 0)
	  continue;

	table = (XUINT_32 *)mapf_read( sizeof(XUINT_32) * count );

#if defined(LOADIMAGE_TRACE) && defined(PAUSE_IN_MODE2)
	if (i > 0)
	{ 
	char temp[10];
	UINT_32 n = count;
	int getpid( void );

	    if (n > 20)
		n = 20;
		
	    boot_list = alloc( SLOT(n), ZERO );
	    for (j=0; j<n; j++)
		gvec_write( boot_list, SLOT(j), swizzle( table[j] ) );
	    printf( "-m %d %d -- about to load %u in mode(II) %d?", 
			    getpid(), (int)master_table, count,  mode2 );
	    gets(temp);
	}
#endif /* LOADIMAGE_TRACE */

	for (j=0; j<count; j++)
	{
	obj item;
	UINT_32 t;

	t = BIG_ENDIAN_TO_HOST_32( table[j] );
	/* printf( "swizzling table[%d] = %x... ", j, t ); */
	item = swizzle( OBJ( t ) );
	/* printf( " = %lx\n", item ); */

#ifdef LOADIMAGE_TRACE
	    tri_printf( "Loading mode(II) %d %#x\n", mode2, VAL(item) );
	    tri_swizzled( -2, BIG_ENDIAN_TO_HOST_OBJ( OBJ(table[j]) ), item );
#endif /* LOADIMAGE_TRACE */
	    proc( item );
#ifdef LOADIMAGE_TRACE
	    tri_printf( "\n    done\n" );
#endif /* LOADIMAGE_TRACE */
	}
    }
    result = swizzle( OBJ( hdr->entry_object_offt ) );
#ifdef LOADIMAGE_TRACE
    fclose( debug_file );
#endif /* LOADIMAGE_TRACE */
    mapf_close();
    free( obj_table );

    loading_image = NO;
    *pformat_version = format_version;
    return result;
}

obj load_ref( void *src, UINT_32 len, obj preamble )
{
obj str = load_array_8( src, len, string_class );
obj item;

#ifdef LOADIMAGE_TRACE
    sprintf( info_buffer, "(REF '%s')", string_text(str) );
    comment_str = info_buffer;
#endif /* LOADIMAGE_TRACE */

    /* in the future, this will not be computed here... */
    preamble = raw_bytes_hash( src, len-1 );
    /* assert( EQ(preamble,raw_bytes_hash( src, len-1 )) ); */

    /* note that you can't bind a REF to #f, because we
       would think that it's not bound! */
  
    item = stringtable_lookup( reference_table, preamble, str );
    if (EQ(item,FALSE_OBJ))
    {
	fprintf( stderr, "REF `%s' not bound\n", (char *)src );
    }
    return item;
}

obj load_symbol( void *src, UINT_32 len, obj preamble )
{
obj str = load_array_8( src, len, string_class );

#ifdef LOADIMAGE_TRACE
    sprintf( info_buffer, " (SYMBOL '%s')", string_text(str) );
    comment_str = info_buffer;
#endif /* LOADIMAGE_TRACE */
    return intern( str );
}

obj load_bignum( void *src, UINT_32 len, obj pre1, obj pre2 )
{
  obj str = load_array_8( src, len, string_class );
  obj item;
  item = make3( pre1, pre2, ZERO, str );
  return item;
}

static void swizzle_bignum( obj item )
{
  obj n, mpd, numstr, bignum, mp_class;
  POBHeader *h = PTR_TO_HDRPTR(item);
  
  numstr = gvec_ref( item, SLOT(2) );
  mp_class = gvec_ref( item, SLOT(0) );

  bignum = string_to_bignum_obj( string_text( numstr ), 10 );

  gvec_write_non_ptr( item, SLOT(0), gvec_ref( bignum, SLOT(0) ) );
  gvec_write_non_ptr( item, SLOT(1), gvec_ref( bignum, SLOT(1) ) );

  assert( OBJ_ISA_PTR(item) );

  n = swizzle( h->pob_class );
#ifdef LOADIMAGE_TRACE
  tri_swizzled( -1, h->pob_class, n );
#endif
  h->pob_class = n;

  /* now, go swizzle the <mp-data> */

  mpd = gvec_ref( bignum, SLOT(2) );
  h = PTR_TO_HDRPTR( mpd );

  h->pob_class = swizzle( mp_class );
  gvec_set( item, SLOT(2), mpd );
}

obj load_part( void *src, UINT_32 len, obj preamble )
{
obj objp;
void *p;

    /* preamble is the part's tag,
       and the string is the name of the module.
       Returns a pointer to the part_descr (which is NOT
       a heap object, note) */
    assert( OBJ_ISA_FIXNUM(preamble) );
    if (!EQ(pd_class,FALSE_OBJ))
      {
	return make3( pd_class, 
		      make_string((char *)src),
		      preamble,
		      FALSE_OBJ );
      }
    
    p = find_a_part( (const char *)src, fx2int(preamble) );

/*printf( "load part: {%s.%d} => %#x\n", src, fx2int(preamble), p );*/

    if (!p)
      {
	/* the part isn't defined */
	fprintf( stderr, 
		 "Module %s has no part with tag %ld\n",
		 (char *)src, fx2int(preamble) );
	objp = NO_PART_TOKEN;
      }
    else
      {
	objp = RAW_PTR_TO_OBJ(p);
	assert( !OBJ_ISA_PTR(objp) );
      }
#ifdef LOADIMAGE_TRACE
    if (p)
    {
	if (EQ(preamble,ZERO))
	    sprintf( info_buffer, 
		    " (PART %s)", ((struct module_descr *)p)->name );
	else
	    sprintf( info_buffer, 
		    " (PART %s, %s)", 
		    ((struct part_descr *)p)->in_module->name, 
		    ((struct part_descr *)p)->name );
    }
    else
	sprintf( info_buffer, " (NO PART)");
    comment_str = info_buffer;
#endif /* LOADIMAGE_TRACE */
    return objp;
}

obj load_array_8( void *src, UINT_32 len, obj preamble )
{
obj item = bvec_alloc( len, preamble );

    memcpy( PTR_TO_DATAPTR(item), src, len );
#ifdef LOADIMAGE_TRACE
    if (1)
    {
      unsigned char *p = (unsigned char *)src;
      UINT_32 i, n;
      char *d, temp[1024];
      
      d = temp;

      if (len > 64)
	n = 64;
      else
	n = len;

      for (i=0; i<n; i++)
	{
	  unsigned char ch = *p++;
	  
	  if ((ch >= 32) && (ch < 127))
	    *d++ = ch;
	  else
	    *d++ = '.';
	}
      *d++ = 0;
      if (len > 64)
	tri_printf( "\t\tarray8[%u] \"%s...\"\n", len, temp );
      else
	tri_printf( "\t\tarray8[%u] \"%s\"\n", len, temp );
    }
#endif /* LOADIMAGE_TRACE */
    return item;
}

obj load_array_16( void *src, UINT_32 len, obj preamble )
{
obj item = bvec_alloc( len, preamble );
UINT_16 *s, *d, *limit;

    assert( (len & (sizeof(UINT_16)-1)) == 0 );
    s = (UINT_16 *)src;
    limit = (UINT_16 *)(len + (char *)s);
    
    d = (UINT_16 *)PTR_TO_DATAPTR(item);

    while (s < limit)
	*d++ = BIG_ENDIAN_TO_HOST_16(*s++);

    return item;
}

obj load_array_32( void *src, UINT_32 len, obj preamble )
{
  obj item = alloc( len * (SLOT(1) / 4), preamble );
  UINT_32 *d;
  XUINT_32 *s, *limit;
  
  assert( (len & (sizeof(XUINT_32)-1)) == 0 );
  s = (XUINT_32 *)src;
  limit = (XUINT_32 *)(len + (char *)s);
  d = (UINT_32 *)PTR_TO_DATAPTR(item);
  
  while (s < limit)
    *d++ = BIG_ENDIAN_TO_HOST_32(*s++);
  
  return item;
}

obj load_array_64( void *src, UINT_32 len, obj preamble )
{
obj item = alloc( len, preamble );
UINT_32 *d32, *s32, *limit32;

    assert( (len & (sizeof(IEEE_64)-1)) == 0 );
    s32 = (UINT_32 *)src;
    d32 = (UINT_32 *)PTR_TO_DATAPTR(item);
    limit32 = (UINT_32 *)(len + (char *)s32);

    while (s32 < limit32)
    {
	*d32++ = *s32++;
	*d32++ = *s32++;
    }
#ifndef PLATFORM_IS_BIG_ENDIAN
    {
    IEEE_64 *d, *limit;
	
	d = (IEEE_64 *)PTR_TO_DATAPTR(item);
	limit = (IEEE_64 *)(len + (char *)d);
    
	while (d < limit)
	{
	    *d = BIG_ENDIAN_TO_HOST_IEEE_64(*d);
	    d++;
	}
    }
#endif
    return item;
}

static void *find_a_part( const char *module, int tag )
{
  struct module_descr *m;

  m  = find_module( module ); /* handles "foo|/lib/libfoo.so" notation */

  if (tag)
    {
      if (m)
	return (void *)find_part( m, tag );
      else
	return NULL;
    }
  else
    {
      return (void *)m;
    }
}
