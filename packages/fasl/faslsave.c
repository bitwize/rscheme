#include <stdio.h>
#include <string.h>
#include <errno.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <time.h>
#include <unistd.h>
#include <sys/mman.h>
#include <fcntl.h>
#include "fasldef.h"
#include <rscheme/smemory.h>
#include <rscheme/scheme.h>
#include <rscheme/api.h>
#include <rscheme/bcextend.h>
#include <rscheme/rlseconf.h>
#include <rscheme/heapi.h>

#if defined(HAVE_MACH_H) && (defined(PLATFORM_NEXT)||defined(PLATFORM_DARWIN))
#include <mach/mach.h>
#include <mach/exception.h>
#include <mach/mach_error.h>
#define USE_MACH_API (1)
#else
#define USE_MACH_API (0)
#endif

/* #define DEBUG_ENABLE */

#ifdef PLATFORM_LINUX
#if PLATFORM_ARCH_PPC
/* presume its MkLinux... */
#define FASL_FIXED_ADDR     (0x30000000)
#define FASL_FIXED_ADDR_ALT (0x38000000)
#else
#if PLATFORM_ARCH_ALPHA
#define FASL_FIXED_ADDR     (0x6100000000UL)
#define FASL_FIXED_ADDR_ALT (0x6200000000UL)
#else
#if PLATFORM_ARCH_S390
#define FASL_FIXED_ADDR     (0x60000000)
#define FASL_FIXED_ADDR_ALT (0x68000000)
#else
/* presume its i386 */
#define FASL_FIXED_ADDR     (0xA0000000)
#define FASL_FIXED_ADDR_ALT (0xA8000000)
#endif
#endif
#endif
#endif

#if defined(PLATFORM_RHAPSODY) || defined(PLATFORM_DARWIN)
#define FASL_FIXED_ADDR     (0x98000000)
#define FASL_FIXED_ADDR_ALT (0x7C000000)
#endif

void *fasl_loaded_at = NULL;

#define _const const

void fasl_error( _const char *msg, _const char *info )
{
  fprintf( stderr, "FASL error: %s (%s)\n", msg, info );
  exit(1);
}

static size_t max_fasl_size = 30*1024*1024;
static struct FASL_Header *hdr;
static void *rgn_ptr, *rgn_limit;

static void *heap_reserve( int fd, void *at );
static void heap_write( const char *path, int fd );
static int fasl_open_file( const char *path );

void *fasl_alloc( size_t len )
{
  void *p = rgn_ptr;
  len = ((len - 1) | 3) + 1;
  rgn_ptr = ((char *)rgn_ptr) + len;
  if (rgn_ptr > rgn_limit)
    {
      fprintf( stderr, 
	       "image exceeds max size (%ldMb)...\n",
	       (long)(max_fasl_size / (1024 * 1024)) );
      exit(1);
    }
  return p;
}

/*
 *  Data structures used during the saving of an image...
 *
 *   (1) all (output) objects queue (all_dst), including templates
 *   (2) all template object queue (all_templates)
 *   (3) pending source object queue (pending_src)
 *   (4) translation map from old to new address (translation_map)
 */

static obj all_dst;
static obj all_templates;
static obj pending_src;
static obj translation_map;
static void tmcheck( void );

static obj make_object_table( void )
{
  return make4( vector_class,
                gvec_alloc( (1<<10), vector_class ),
                int2fx( 10 ),
                ZERO,
                vector_class );
}


/*
 *  Translate an arbitrary item, except that if it
 *  is a pointer to an object that has *NOT* been moved
 *  into the new heap, then translate it to FALSE (#f).
 *
 *  This is intended to be used with weak pointers.  The
 *  usual translation API is xlate_pob(), which is unforgiving
 *  about objects which haven't been moved.
 */

static inline obj xlate_obj_or_false( obj item )
{
  obj h = obj_hash( item );
  obj v;
  v = objecttable_lookup( translation_map, h, item );
  /*printf( "{%08lx/%08lx=%08lx}", VAL(h), VAL(item), VAL(v) );*/
  return v;
}

static obj xlate_pob( obj item )
{
  obj x;

  /*printf( "xlate_pob " );*/
  x = xlate_obj_or_false( item );
  /*printf( "\n" );*/

  if (truish( x )) {
    return x;
  }
  fprintf( stderr, "%08lx: Could not translate OBJ\n", VAL(item) );
  abort();
}

static gc_obj_addr xlate_ptr( gc_obj_addr ptr )
{
  return PTR_TO_GCPTR( xlate_pob( GCPTR_TO_PTR( ptr ) ) );
}

static IRC_Header *my_alloc_big( struct IRC_Heap *heap, UINT_32 size )
{
  return (IRC_Header *)fasl_alloc( size + sizeof(IRC_Header) );
}

static void my_alloc_chunk( struct IRC_Heap *heap )
{
  void *new;

  new = fasl_alloc( ALLOCATION_CHUNK_SIZE );
  if (new == (void *)(((char *)heap->moreSpacePtr + heap->spaceLeft)))
    {
      /* extend the old rgn instead of replacing it, thereby reducing
         external fragmentation */
       heap->spaceLeft += ALLOCATION_CHUNK_SIZE;
    }
  else
    {
      heap->spaceLeft = ALLOCATION_CHUNK_SIZE;
      heap->moreSpacePtr = new;
    }
}


IRC_Heap *build_fasl_heap( gc_obj_addr *roots, unsigned num_roots );

static int fasl_count_size( void *info, void *ptr )
{
  unsigned use;
  unsigned long *accum = info;

  IRC_SizeClass *sc = IRCH(ptr)->sizeClass;

  if (sc->isLargeObject) {
    use = SIZEOF_PTR( GCPTR_TO_PTR( ptr ) )
      + sizeof( POBHeader )
      + sizeof( IRC_Header );
  } else {
    use = sc->itemSize;
  }

  accum[0] += use;
  accum[1] += 1;
  return 0;
}


static unsigned long fasl_est_size( void )
{
  unsigned long fasl_size[2] = { 0, 0 };

  gc_for_each( fasl_count_size, &fasl_size[0] );
  printf( "fasl heap size estimate = %luK (%lu objects)\n", 
          (fasl_size[0]+1023) / 1024,
          fasl_size[1] );
  return fasl_size[0];
}

void fasl_save( const char *path,
	        gc_obj_addr *roots,
	        unsigned num_roots,
	        const char *comment_str )
{
  void *rgn;
  int fd;
  unsigned i;
  void *at;

  /* Round estimated size up to 64K and toss in an extra MB */

  max_fasl_size = (fasl_est_size() | 0xFFFF) + 1 + 1024*1024;
  printf( "fasl heap max size = %luK\n", 
          (unsigned long)(max_fasl_size / 1024) );

  fd = fasl_open_file( path );
  if (fd < 0) {
    scheme_error( "save-fasl-image: could not create ~s", 
                  1, make_string(path) );
  }

#ifdef FASL_FIXED_ADDR
  at = (void *)FASL_FIXED_ADDR;
  if (at == (void *)fasl_loaded_at) {
    /*  we have already loaded an image at the primary address;
     *  try somewhere else
     */
    at = (void *)FASL_FIXED_ADDR_ALT;
  }
#else
  at = NULL;
#endif

  rgn = heap_reserve( fd, at );
  if (!rgn)
    {
      close(fd);
      fasl_error( "couldn't map file", path );
    }
  printf( "fasl load address: %p\n", rgn );

  rgn_ptr = rgn;
  rgn_limit = ((char *)rgn + max_fasl_size);

  hdr = FASL_ALLOC( struct FASL_Header );

  hdr->pre_loaded_at = (void *)rgn;
  memset( hdr->skip, ' ', sizeof hdr->skip );
  hdr->skip[0] = '\n';

  hdr->image_magic = FASL_MAGIC;
  hdr->fasl_version = FASL_VERSION;

  hdr->for_arch = FASL_ARCH;
  hdr->spare = 0;
  hdr->build_date = time(NULL);
  hdr->first_alloc_area = NULL;

  printf( "setup (%p) magic %08x\n", hdr, hdr->image_magic );


  /* build the output image */

  hdr->heap = build_fasl_heap( roots, num_roots );

#ifdef DEBUG_ENABLE
  if (fasl_verbose >= 2)
  {
    struct IRC_Gen *g = &hdr->heap->theGenerations[0];
    int i;

    printf( "loaded generation (#%u):\n", g->genNum );
    for (i=0; i<NUM_PHYSICAL_SIZE_CLASSES; i++)
      {
	IRC_SizeClass *sc = &g->theSizeClasses[i];
	printf( "----  size class #%d (%u max) ----\n", i, sc->itemSize );
	printSizeClass( sc );
      }
  }
#endif


  hdr->root_list = FASL_ALLOCN( void *, (1+num_roots) );
  for (i=0; i<num_roots; i++)
  {
    hdr->root_list[i] = roots[i] ? xlate_ptr( roots[i] ) : NULL;
  }

  if (comment_str)
    {
      int len = (strlen(comment_str) | 15)+1;

      hdr->root_list[num_roots] = FASL_ALLOCN( char, len );
      printf( "root[%d] => %p => '%s'\n",
	      num_roots, 
	      hdr->root_list[num_roots], 
	      comment_str );
      strcpy( (char *)hdr->root_list[num_roots], comment_str );
    }
  else
    hdr->root_list[num_roots] = NULL;
  
  /* finish up */

  hdr->total_size = ((char *)rgn_ptr) - ((char *)hdr->pre_loaded_at);

  /* write out the data */

  if (fasl_verbose >= 1)
    printf( "loaded at: %p -- %lu bytes\n", 
	   hdr->pre_loaded_at, (unsigned long)hdr->total_size );

  heap_write( path, fd );
}

/*
 *  We use the 'client byte' in the IRC heap to keep track
 *  of which objects we've moved over.
 *
 *    00 => not moved
 *    FF => moved
 */

static void do_enq_src_obj( gc_obj_addr item )
{
  obj x;
  unsigned im;
  IRC_Header *cell = IRCH(item);

  x = GCPTR_TO_PTR(item);

  im = class_image_mode( CLASSOF_PTR(x) );

  if (im == 4) { /* image mode = 4 iff (instance? x <template>) */
    /* make sure its been unstubbed */

    struct function_descr *fn;

    fn = (struct function_descr *)OBJ_TO_RAW_PTR(gvec_ref(x,SLOT(1)));
    if (fn->in_part->tag >= STUB_PART_TAG) {
      template_unstub( x );
    }

    /* if it's a template, put it at the end */

    dequeue_push_back( pending_src, x );
  } else {
    /* otherwise, put it in the front */
    dequeue_push_front( pending_src, x );
  }

  cell->flagBits |= IRC_MASK_CLIENTBYTE;
}

static inline void enq_src_obj( gc_obj_addr item )
{
  IRC_Header *cell = IRCH(item);

  if (cell->flagBits & IRC_MASK_CLIENTBYTE) {
    return;
  }

  do_enq_src_obj( item );
}

/*
 *  Find all the reachable objects and create the corresponding
 *  storage in the new (destination) heap ("dheap").  Also, go
 *  ahead and copy all the bits into the the new heap.
 *
 *  Later on, we'll actually translate any pointers (c.f. translate_dst())
 */

static void scan_obj_slots( obj *vec, unsigned limit )
{
  unsigned i;

  for (i=0; i<limit; i+=SLOT(1))
    {
      obj slotv = *vec++;

      if (fasl_verbose >= 5)
        printf( "  [%lu]: %08lx", 
                (unsigned long)(i/SLOT(1)), 
                (unsigned long)VAL(slotv) );
      if (OBJ_ISA_PTR(slotv))
        {
          if (fasl_verbose >= 5)
            printf( " *\n" );
          enq_src_obj( PTR_TO_GCPTR(slotv) );
        }
      else
        {
          if (fasl_verbose >= 5)
            printf( "\n" );
        }
    }
}

#define CCHECK (0)

void flush_src_queue( struct IRC_Heap *dheap )
{
  unsigned seqn = 0;

  while (1) {
    UINT_32 spc;
    void *dst;
    POBHeader *pob;
    obj thing;                /* the obj being copied over */
    unsigned im;
    obj heap_type;
    IRC_Header *cell;
    obj dptr;                   /* the destination (output) obj */

    if (dequeue_empty( pending_src )) {
      break;
    }

    thing = dequeue_pop_front( pending_src );

    cell = IRCH( PTR_TO_GCPTR( thing ) );

    spc = cell->sizeClass->itemSize - sizeof( IRC_Header );
    if (CCHECK||fasl_verbose >= 3) {
      printf( "[%u] copying object: %p (sc: %lu)", seqn++, cell, (unsigned long)spc );
    }

    pob = PTR_TO_HDRPTR(thing);

    if (cell->sizeClass->isLargeObject) {
      spc = pob->pob_size + sizeof( POBHeader );
      if (CCHECK||fasl_verbose >= 3) {
        printf( " (large)" );
      }
    }

    dst = IRC_alloc( dheap, spc );
    IRCH(dst)->flagBits |= IRC_MASK_MAPPED;
    dptr = GCPTR_TO_PTR( dst );
    
    {
      obj hthing = obj_hash( thing );

      objecttable_insert( translation_map, hthing, thing, dptr );
      if (CCHECK) {
        printf( "{%08lx/%08lx=%08lx}", VAL(hthing), VAL(thing), VAL(dptr) );
      }
      dequeue_push_back( all_dst, dptr );
    }

    if (CCHECK||fasl_verbose >= 3) {
      printf( " ==> %p", dst );
    }

    /* interpret it's class... */

    im = class_image_mode( pob->pob_class );
    if (CCHECK||fasl_verbose >= 3) {
      printf( " (%s, %lu bytes, mode %u)\n", 
              symbol_text( class_name(pob->pob_class) ),
              (unsigned long)pob->pob_size,
              im );
      fflush( stdout );
    }
    tmcheck();

    memcpy( dst, cell+1, spc );
    if (im == 4) {
      /* it's a <template> */
      dequeue_push_back( all_templates, dptr );
    } else if (im == 8) {
      /* it's an <allocation-area>, push it onto the list
         of them using the allocfn slot as the link ptr */
        
      AllocArea *aa = (AllocArea *)((char *)dst + sizeof(POBHeader));
        
      printf( "alloc area at: %p\n", aa );
      aa->allocfn = (allocator_fn *)(hdr->first_alloc_area);
      hdr->first_alloc_area = aa;
    }
      
    /* traverse it... */

    heap_type = gvec_read( pob->pob_class, SLOT(1) );

    /* heap type 0 is a gvec */
    if (EQ(heap_type,ZERO)) {
      unsigned i;
      if (fasl_verbose >= 4) {
        printf( "  gvec (%lu slots)\n", 
                (unsigned long)(SIZEOF_PTR(thing)/SLOT(1)) );
      }
        
      scan_obj_slots( (obj *)PTR_TO_DATAPTR(thing),
                      SIZEOF_PTR(thing) );
    } else if (EQ(heap_type,int2fx(5))) {
      /* heap type 5 is mixvec(2) == 2 gvec slots, the rest bvec */
      unsigned i;
      if (fasl_verbose >= 4) {
        printf( "  mixvec(2) (%lu bytes)\n", 
                (unsigned long)SIZEOF_PTR(thing) );
      }

      scan_obj_slots( (obj *)PTR_TO_DATAPTR(thing),
                      SLOT(2) );
    } else if (EQ(heap_type,int2fx(4))) {
      /* heap type 4 is weak(1) == 1 weak slot, rest plain gvec */
      if (fasl_verbose >= 4) {
        printf( "  weak(1) (%lu bytes)\n", 
                (unsigned long)SIZEOF_PTR(thing) );
      }
      scan_obj_slots( ((obj *)PTR_TO_DATAPTR(thing)) + 1,
                      SIZEOF_PTR(thing) - SLOT(1) );
    }
  }
}


/*
 *  Go through and fix up all the data in the new copy
 *  of the image (that is, the dest).  This is where we
 *  translate pointers from referring to the old addresses
 *  (in our current heap) to pointing to the corresponding 
 *  objects in the new heap.
 */

static void translate_obj_slots( obj *vec, unsigned limit )
{
  unsigned i;

  for (i=0; i<limit; i+=SLOT(1), vec++) {
    if (fasl_verbose >= 5)
      printf( "  [%lu]: %#lx", 
              (unsigned long)(i/SLOT(1)), 
              (unsigned long)VAL(*vec) );

    if (OBJ_ISA_PTR(*vec)) {
        *vec = xlate_pob( *vec );
        if (fasl_verbose >= 5) {
          printf( " => %#lx\n", (unsigned long)VAL(*vec) );
        }
    } else {
      if (fasl_verbose >= 5) {
        printf( "\n" );
      }
    }
  }
}

static void tmcount( void *info, obj h, obj k, obj v )
{
  unsigned *cnt = info;
  (*cnt)++;
}

static void tmprint( void *info, obj h, obj k, obj v )
{
  unsigned *i = info;

  printf( "  [%u] {%08lx/%08lx=%08lx}\n", (*i)++, VAL(h), VAL(k), VAL(v) );
}

static void tmcheck( void )
{
  unsigned n = 0;

  return;

  hashtable_foreach( translation_map, &n, tmcount );
  /*hashtable_foreach( translation_map, &n, tmcount );*/
  if (n != hashtable_size( translation_map )) {
    printf( "translation_map: size %lu, counted %u\n", 
            (unsigned long)hashtable_size( translation_map ),
            n );
    abort();
  }
}

void translate_dst( void )
{
  unsigned i, dcount;

  dcount = fx2int( dequeue_count( all_dst ) );
  printf( "%u objects copied (translation table: %lu)\n", 
          dcount, (unsigned long)hashtable_size( translation_map ) );

  tmcheck();

  /*
  i = 0;
  hashtable_foreach( translation_map, &i, tmprint );
  */

  for (i=0; i<dcount; i++) {
    obj thing = dequeue_ref( all_dst, int2fx( i ) );
    POBHeader *pob;
    obj heap_type;
    
    pob = PTR_TO_HDRPTR(thing);
    if (fasl_verbose >= 3) {
      printf( " (%s, %lu bytes, mode %u)\n", 
              symbol_text( class_name(pob->pob_class) ),
              (unsigned long)pob->pob_size,
              class_image_mode( pob->pob_class ) );
      fflush(stdout);
    }
    
    /* traverse it... */
    pob->pob_class = xlate_pob( pob->pob_class );
    
    heap_type = gvec_read( pob->pob_class, SLOT(1) );
    
    if (EQ(heap_type,ZERO)) {
      obj *body = PTR_TO_DATAPTR(thing);

      if (fasl_verbose >= 4)
        printf( "  gvec (%lu slots)\n", 
                (unsigned long)(SIZEOF_PTR(thing)/SLOT(1)) );
	      
      translate_obj_slots( body, SIZEOF_PTR(thing) );
    } else if (EQ(heap_type, int2fx(5))) {
      obj *body = PTR_TO_DATAPTR(thing);

      if (fasl_verbose >= 4)
        printf( "  mixvec(2) (%lu slots)\n", 
                (unsigned long)SIZEOF_PTR(thing) );
	      
      translate_obj_slots( body, SLOT(2) );
    } else if (EQ(heap_type, int2fx(4))) {
      /* first slot is weak, the rest is a gvec */
              
      obj *body = PTR_TO_DATAPTR(thing);
              
      body[0] = xlate_obj_or_false( body[0] );
      translate_obj_slots( body+1, SIZEOF_PTR(thing) - SLOT(1) );
    }
  }
}

struct IRC_Heap *build_fasl_heap( gc_obj_addr *roots, unsigned num_roots )
{
  struct IRC_Heap *vheap = FASL_ALLOC(struct IRC_Heap);
  unsigned i;
  
  irc_init_heap( vheap );

  vheap->alloc_chunk_meth = my_alloc_chunk;
  vheap->alloc_big_meth = my_alloc_big;
  
  /* recursively copy the source image */

  all_dst = make_dequeue();
  all_templates = make_dequeue();
  pending_src = make_dequeue();
  translation_map = make_object_table();

  for (i=0; i<num_roots; i++) {
    if (roots[i]) {
      enq_src_obj( roots[i] );
    }
  }

  flush_src_queue( vheap );
  translate_dst();

  /* fasl_verbose = 9; */
  gen_code_ptrs( hdr, all_templates );
  
  return vheap;
}


void fasl_save_vec( const char *path, obj vec, const char *comment )
{
  gc_obj_addr *rv;
  unsigned i, n = SIZEOF_PTR(vec) / SLOT(1);

  rv = malloc( n * sizeof(gc_obj_addr) );

  for (i=0; i<n; i++)
    {
      obj ent = gvec_ref( vec, SLOT(i) );

      if (OBJ_ISA_PTR(ent))
	rv[i] = PTR_TO_GCPTR(ent);
      else if (EQ(ent,FALSE_OBJ))
	rv[i] = NULL;
      else
	scheme_error( "save-fasl-image: root vector entry ~s is invalid",
		      1, ent );
    }

  fasl_save( path, rv, n, comment );
  free( rv );
}

/**/


#ifdef PLATFORM_AIX
#define DO_MMAP_XFLAGS ( MAP_VARIABLE )
#else
#define DO_MMAP_XFLAGS 0
#endif

#if defined(PLATFORM_SUNOS) || defined(__FreeBSD__) || \
    defined(PLATFORM_IRIX) || defined(PLATFORM_BSDI) || \
    defined(PLATFORM_BSD) || defined(PLATFORM_LINUX) || \
    defined(PLATFORM_AIX) || defined(PLATFORM_DARWIN)
#define DO_MMAP_SHARING MAP_SHARED
#else
#define DO_MMAP_SHARING MAP_PRIVATE
#endif

#if USE_MACH_API

#include <mach/mach.h>
/*
#include <mach/cthreads.h>
#include <mach/exc_server.h>
*/
#include <mach/exception.h>
#include <mach/mach_error.h>

#if defined(PLATFORM_NEXT)
#define mach_task_self()  task_self()
#endif

static void *do_mmap( int fd, void *try_at )
{
  kern_return_t rc;
  vm_address_t addr;

  addr = (vm_address_t)try_at;

  rc = vm_allocate( mach_task_self(), &addr, 
                    max_fasl_size, 
                    /* anywhere */ try_at ? FALSE : TRUE );

  if (rc != KERN_SUCCESS) {
    return NULL;
  }
  return (void *)addr;
}

#else /* USE_MACH_API */

static void *do_mmap( int fd, void *try_at )
{
  caddr_t try, at;
  int mode, flags, mm_fd;
  int ret;

  flags = DO_MMAP_SHARING|DO_MMAP_XFLAGS;

  if (try_at) {
    flags |= MAP_FIXED;
  }
  try = (caddr_t)try_at;

  at = mmap( try, max_fasl_size, PROT_READ|PROT_WRITE, flags, fd, 0 );

  if (at == MAP_FAILED) {
    return NULL;
  }
  return (void *)at;
}

#endif

static void *heap_reserve( int fd, void *try_at )
{
  void *p;
  
  if (try_at) {
    /* try at the recommended address */
    p = do_mmap( fd, try_at );
    if (p) {
      return p;
    }
  }

  /* fallback -- try at any old place */
  return do_mmap( fd, NULL );
}


#if FASL_NSHARE || USE_MACH_API
#define FASL_NEED_WRITEBACK (1)
#else
#define FASL_NEED_WRITEBACK (0)
#endif

static void heap_write( const char *path, int fd )
{
  unsigned long actual_size = hdr->total_size;

  printf( "total heap size: %luK (had estimated %luK)\n", 
          (actual_size + 1023)/1024,
          (unsigned long)(max_fasl_size / 1024) );

  printf( "image (%p) magic %08x\n", hdr, hdr->image_magic );
#if FASL_NEED_WRITEBACK
  {
    int n;
    printf( "(writing image)\n" );
    n = write( fd, hdr, hdr->total_size );
    if (n != hdr->total_size)
      {
	perror( "writing fasl image" );
	exit(1);
      }
  }
#endif

#if USE_MACH_API
  if (vm_deallocate( task_self(), (vm_address_t)hdr, max_fasl_size )
      != KERN_SUCCESS) {
    mach_error( "vm_deallocate", rc );
  }
#else
  if (munmap( (caddr_t)hdr, max_fasl_size ) < 0) {
    perror( "munmap" );
  }
#endif
  hdr = NULL;

  ftruncate( fd, actual_size );
  close( fd );
}

#define FASL_PERM (S_IRUSR|S_IWUSR|S_IRGRP|S_IWGRP|S_IROTH|S_IWOTH)

#ifdef PLATFORM_AIX
#define FASL_NSHARE O_NSHARE
#else
#define FASL_NSHARE 0
#endif

#define FASL_NEED_TRUNCATE (1)

static int fasl_open_file( const char *path )
{
  int fd;

  fd = open( path, O_RDWR|O_CREAT|O_TRUNC|FASL_NSHARE, FASL_PERM );

  if (fd < 0) {
    fasl_error( "couldn't create", path );
  }

#if FASL_NEED_TRUNCATE
  if (ftruncate( fd, max_fasl_size ) < 0) {
    fasl_error( "couldn't allocate space", path );
  }
#endif
  return fd;
}

