/*------------------------------------------------------------------*-C-*-*
 * %Z%1.40  %G% 21:07:01 %W%
 *
 * Purpose:	
 *
 *------------------------------------------------------------------------*
 * Notes:
 *------------------------------------------------------------------------*/

char *__rstore_util_sccsid__() { return "%Z%%W% 1.40 %G% 21:07:01"; }

#include <stdio.h>
#include <string.h>
#include "rstoret.h"
#include "alloc.h"
#include "mmglue.h"
#include <rscheme/scheme.h>
#include <rscheme/writebar.h>
#include "indirect.h"
#include "swizzler.h"

#include <rscheme/vinsns.h>

#define CLIENT_INFO_RECNUM  (0)
    
/* #define VERBOSE */

/* usage of lss's client misc[] area */

struct swiz_mode_handler *(app_swiz_mode_handlers[10]);

extern IRC_Heap *gc_arena;

#define MAX_ACTIVE_RSTORES  (10)

static RStore *(rstores[MAX_ACTIVE_RSTORES]);
static unsigned num_rstores;
obj alloc_area_class; /* initialized by (open-rstore) */

void rstore_error_handler( LSS *lss, void *info, int code, char *msg )
{
  scheme_error( "lss error: ~a (~d)\nin ~s",
	        3,
	        make_string(msg),
	        int2fx(code), 
	        ((RStore *)info)->owner );
}

void plain_lss_error_handler( LSS *lss, void *info, int code, char *msg )
{
  scheme_error( "lss error: ~a (~d)",
	        2,
	        make_string(msg),
	        int2fx(code) );
}

struct swiz_mode_handler *get_swiz_mode_handler( RStore *sto,
						 enum SwizzleMode mode )
{
  struct swiz_mode_handler **p;

  for (p=sto->swiz_mode_handlers; *p; p++)
    {
      if ((*p)->handles == mode)
	return *p;
    }
  scheme_error( "image mode ~d not handled by ~s", 
		2, int2fx(mode), sto->owner );
  return NULL;
}

void rstore_add_swiz_mode_handler( RStore *sto, struct swiz_mode_handler *h )
{
  struct swiz_mode_handler **p;

  if (h->handles >= SWIZ_MODE_APP_0 && h->handles <= SWIZ_MODE_APP_9)
    {
      for (p=sto->swiz_mode_handlers; *p; p++)
	{
	  if ((*p)->handles == h->handles)
	    {
	      scheme_error( "swizzle mode ~d already handled", 
			    1, int2fx( h->handles ) );
	    }
	}
      if (p >= (sto->swiz_mode_handlers + 10))
	scheme_error( "swizzle mode handler table full", 0 );
      *p++ = h;
      *p = NULL;
    }
  else
    scheme_error( "swizzle mode ~d not valid for app swizzler",
		  1, int2fx( h->handles ) );
}

static rs_bool mminit = NO;

static void init_new( RStore *store )
{
  store->next_page = 0x1000000;
  store->next_indirect_page = 256;   /* 0...63 reserved for the system,
					64..255 for app. pivot pages */
  
  /* create the first allocation area */
  
  store->default_area = make_alloc_area( store, FALSE_OBJ );
  store->using_rich_model = LRU_RICH_MODEL;
}

obj meta_root_info( LSS *lss )
{
  struct RStoreCommitInfo ci;
  zipbuf v[2];
  LSSAccess *a;
  obj m;

  a = lss_read_access( lss, CLIENT_INFO_RECNUM );

  if (!a)
    {
      return FALSE_OBJ;
    }

  v[0].ptr = &ci;
  v[0].limit = 1 + &ci;
  v[1].ptr = v[1].limit = NULL;
  
  lss_readv( lss, v, a );
  lss_read_release( lss, a );
  
  if (strcmp( ci.tag, "RStore-3.0" ) == 0)
    {
      m = int2fx( LRU_RICH_MODEL );
    }
  else if (strcmp( ci.tag, "TKG-RStore-1.0" ) == 0)
    {
      m = int2fx( OLD_RICH_MODEL );
    }
  else
    {
      return FALSE_OBJ;
    }
  return cons( m, cons( int2fx( ci.default_area.base_page_num ),
                        int2fx( ci.default_area.offset ) ) );
}

static void init_existing( RStore *store, struct RStoreCommitInfo *ci )
{
  obj aa = translate_LR( store, ci->default_area );

  store->next_page = ci->next_page_num;
  store->next_indirect_page = ci->next_indirect_page;
  store->default_area = PTR_TO_DATAPTR( aa );
  
  if (strcmp( ci->tag, "RStore-3.0" ) == 0)
    {
      store->using_rich_model = LRU_RICH_MODEL;
    }
  else if (strcmp( ci->tag, "TKG-RStore-1.0" ) == 0)
    {
      store->using_rich_model = OLD_RICH_MODEL;
    }
}

obj rstore_get_live_objects( RStore *owner, obj otbl )
{
  scheme_error( "obsolete! what are you doing?", 0 );
  return ZERO;
}

/*
 *  This is called on the WB_PERSISTENT code, which is
 *  used for tracking live *transient* objects
 */

int rstore_write_barrier( void *info,
                          IRC_Heap *heap,
                          void *lvalue, 
                          UINT_32 offset, 
                          void *rvalue )
{
  struct PHeapHdr *p = (struct PHeapHdr *)((char *)lvalue - 
                                           sizeof(struct IRC_Header));

  /*
   *  An object we own (and hence is in generation 7)
   *  is being written to with a pointer to some other generation
   *  (XXX note: if there are multiple pstores open, then
   *  we should track cross-pstore writes, too, even though they're
   *  all in the same generation=7.  Ideally, the write barrier would
   *  only be enabled for that combination when that situation could
   *  arise, so we don't have to trip the WB on pstore/pstore writes
   *  if we only have one pstore open.  [For that matter, we could reserve
   *  a few generation codes, one each for several pstores, but that
   *  won't carry forward to the future compact header format which only
   *  has 4 generations])
   *
   *  If we return non-zero, then the address being written
   *  to will be added to the extraHeapPointers of the transient
   *  generation
   */

  /*
   *  This is expensive (in memory) but avoids any pernicious
   *  cases that would cause the extraHeapPointers list to
   *  grow essentially without bound
   *
   *  What we do is keep a bit map of which words on this
   *  page have been added to the extraHeapPointers list.
   *
   *  If the object is no more than 24 words long, then we store
   *  the bit map in the object header, in the high 24 bits of
   *  what we call the gc_flag_bits and what the IRC_Header calls
   *  the flagBits.  Otherwise, we store it in a per-pstore
   *  fixnum hash table.
   */

#ifdef GC_TRACE
  printf( "rstore_write_barrier( %p, %p, %p[0x%x], %p )\n",
          info,
          heap,
          lvalue,
          (unsigned)offset, 
          rvalue );
  printf( "  clobbering: 0x%08x\n", *(unsigned *)((char *)lvalue + offset) );
#endif

#if 0
  assert( (((unsigned long)rvalue) & 0xF0000000) != 0x40000000 ); /* XXX LINUX TEST HACK */
#endif

#if 0
  printf( "  %p.mem_size = %lu\n", p, p->mem_size );
  printf( "  %p.pstore_flags = %lu\n", p, p->pstore_flags );
  printf( "  %p.size_class = %p\n", p, p->size_class );
  printf( "  %p.gc_flag_bits = 0x%08x\n", p, (unsigned)p->gc_flag_bits );
#endif

  if (p->mem_size <= SLOT(24)) {
    unsigned mask = (1 << 8) << (offset / SLOT(1));
    assert( offset < p->mem_size );
    assert( mask );

#if 0
    printf( "  object inline bits: %08x[%u]=%u\n", 
            (unsigned)(p->gc_flag_bits >> 8),
            (unsigned)(offset/SLOT(1)),
            ((p->gc_flag_bits & mask) ? 1 : 0) );
#endif
    
    if (p->gc_flag_bits & mask) {
      /* already in the list */
      return 0;
    } else {
      p->gc_flag_bits |= mask;
      return 1;
    }
  } else {
    pos_ptr_addr ppa = (pos_ptr_addr)(offset + (char *)lvalue);
    UINT_32 card = ((UINT_32)ppa) / SLOT(16);
    unsigned bit_on_card = (((UINT_32)ppa) / SLOT(1)) & 15;
    struct FirstPageHdr *pg;
    RStore *sto;
    obj h, k, v, v1, mask;

#if 0
    printf( "  pos_ptr_addr(%p) card: %08x bit: %u\n", 
            ppa, 
            card, bit_on_card );
#endif

    /*  Find the pstore associated with this object.
     *  Thankfully, we have the object header, so we can
     *  find the FirstPageHdr.  If all we had was the 
     *  pos_ptr_addr, like in Texas for C++, then we'd
     *  have to hit the hash table to find the VMPageRecord
     */
    pg = FIRST_PAGE_HDR_OF( lvalue );
    sto = pg->area->owner;
    assert( card < 0x1FFFFFFF );
    k = int2fx( card );
    h = obj_hash( k );

    /*printf( "  card hash: #[%08x]\n", VAL(h) );*/
    /*printf( "  pstore: %p\n", sto );*/
    v = objecttable_lookup( sto->dirty_cards, h, k );
    /*printf( "  dirty_cards[%u] = #[%08x]\n", card, VAL(v) );*/
    if (EQ( v, FALSE_OBJ )) {
      v = ZERO;
    }
    mask = int2fx( 1<<bit_on_card );

    assert( !EQ( mask, ZERO ) );

    v1 = FX_OR( v, mask );
    assert( EQ( FX_AND( v1, mask ), mask ) );

    if (EQ(v1,v)) {
      /*printf( "  -- bit[%u] already set\n", bit_on_card );*/
      return 0;
    }

    /*printf( "  dirty_cards[%u] := #[%08x]\n", card, VAL(v1) );*/
    objecttable_insert( sto->dirty_cards, h, k, v1 );
    return 1;
  }
}

static void rstore_load_client_info( RStore *store )
{
  LSSAccess *a;

  a = lss_read_access( store->lss, CLIENT_INFO_RECNUM );

  if (!a) {
      /* no client info -- must be a new rstore */
      init_new( store );
  } else {
    struct RStoreCommitInfo ci;
    zipbuf v[2];

    v[0].ptr = &ci;
    v[0].limit = 1 + &ci;
    v[1].ptr = v[1].limit = NULL;

    lss_readv( store->lss, v, a );
    lss_read_release( store->lss, a );

    init_existing( store, &ci );
  }
}

static void rstore_init_vm( RStore *store )
{
  fflush( stdout );
  setlinebuf( stdout ); /* XXX hack; this should be in base system */

  store->first_dirty = NULL;
  store->last_dirty = NULL;
  store->num_dirty = 0;
  store->first_loaded = NULL;
  store->first_reserved = NULL;
  store->write_protect = 0;

  htable_init( &store->vm_page_records );
  htable_init( &store->reserved_base_pages );

  rstore_load_client_info( store );
}

RStore *rstore_open( obj owner, LSS *lss )      /* this is used for NEW pstores, too */
{
  RStore *store;

  if (num_rstores >= MAX_ACTIVE_RSTORES)
    scheme_error( "rstore: too many active pstores", 1, owner );

  if (!mminit)
    {
      init_mm();
      mminit = YES;
      rstore_init_psizeclass();
    }

  store = ALLOC( struct RStore );
  store->owner = owner;
  store->data_zipper = NULL;    /* by default, don't compress */
  store->id_pages = 0;          /* not ready for prime time yet... */

  rstores[num_rstores++] = store;

  store->swiz_mode_handlers[0] = NULL;
  store->lss = lss;

  /* extract appropriate tables and values from the owner */

  store->pivot_table = gvec_ref( owner, SLOT(1) );
  store->local_code_ptrs = gvec_ref( owner, SLOT(2) );
  store->local_fn_descrs = gvec_ref( owner, SLOT(3) );
  store->dirty_cards = gvec_ref( owner, SLOT(5) );
  store->reloc_table = FALSE_OBJ;

  /* initialize our fake size class, generation, and GC status */

  store->new_flag_bits = 0xE;   /* allocate WHITE until further notice */
  store->is_lazy_cleaning = 0;
  store->is_eager_cleaning = 0;
  store->is_persistent_traversal = 0;
  store->is_twb_enabled = 0;
  store->is_pwb_enabled = 0;
  store->is_alloc_black = 0;
  store->pgc_phase = PGC_IDLE;
  store->num_pages_w_gray = 0;

  store->the_gen.heap = gc_arena;
  irc_init_pstore_gen( &store->the_gen );
  store->the_gen.flip_hook_info = store;
  store->the_gen.igp_hook_info = store;

#ifdef VERBOSE
  printf( "initializing pstore generation <%p>\n", &store->the_gen );
#endif

  store->the_size_class.gen = &store->the_gen;
  store->the_size_class.heap = gc_arena;

  /* install write-barrier hooks */

  irc_init_pstore_writebarrier( NULL, rstore_write_barrier );
      
  /* empty the lists of various VMPs */

  rstore_init_vm( store );

  return store;
}

static void rstore_check_unmappable( struct RStore *store )
{
  /*
   *  force a GC to make sure we don't try to examine
   *  a pointer into the storage we are now deallocating
   */
  gc_now();

#ifdef VERBOSE
  printf( "unmapping generation <%p>\n", &store->the_gen );
#endif

  /*
   *  Mark this store as getting unmapped, and
   *  mark ``Generation 7'' as needing traversal
   *  work
   */
#if INCLUDE_PSTORE_UNMAPPER
  found_unmapped = FALSE_OBJ;
  store->the_gen.unmapped = 1;
  gc_arena->theGenerations[0].traversalWork[14] = 1;
  gc_arena->theGenerations[0].traversalWork[15] = 1;
  gc_now();

  if (truish(found_unmapped))
    {
      store->the_gen.unmapped = 2;
      gc_arena->theGenerations[0].traversalWork[14] = 0;
      gc_arena->theGenerations[0].traversalWork[15] = 0;

      scheme_error( "unmapped pointers found", 1, found_unmapped );
    }
#endif
}

static void free_vmpr( void *datum )
{
  struct VMPageRecord *vmpr = (struct VMPageRecord *)datum;

  if (!vmpr->ref.indirect)
    {
      if (vmpr->ref.first)
        {
          int n = vmpr->ref.nth_page;
          mm_free( vmpr->mem_address, n * MM_PAGE_SIZE );
        }
      else
        {
          mm_free( vmpr->mem_address, MM_PAGE_SIZE );
        }
    }
  free( vmpr );
}

static void rstore_unmap_all( struct RStore *store )
{
  struct VMPageRecord *j;

  /*
   *  go through and free up any VM storage
   */
  for (j=store->first_reserved; j;)
    {
      struct VMPageRecord *n = j->next_reserved;
      free_vmpr( j );
      j = n;
    }
  /*
   *  clean up htable storage
   */
  htable_free( &store->vm_page_records );
  htable_free( &store->reserved_base_pages );
}

#if 0
static void rstore_unload_all( struct RStore *store )
{
  struct VMPageRecord *p;

  for (p=store->first_loaded; p; p=p->next_loaded) {
    mm_unload( p->mem_address, MM_PAGE_SIZE );
    /* we'll be taking it off the list momentarily... */
    p->dirty = 0;
    p->loaded = 0;
  }
  /* now, nothing is loaded... */
  store->first_loaded = NULL;

  /* and, perforce, nothing is dirty */
  store->first_dirty = NULL;
  store->last_dirty = NULL;
}
#endif

static void rstore_unreserve_missing( struct RStore *store )
{
  unsigned n = 0;
  unsigned nret = 0;
  struct VMPageRecord *p, *prev, *next;
  struct htable base_ok;

  /*
   *  This has table is going to keep track of VMPRs
   *  that correspond to base addresses that are still OK
   */
  htable_init( &base_ok );

  for (p=store->first_reserved; p; p=p->next_reserved) {
    LSSAccess *a;

    if (p->ref.first && !p->ref.indirect) {
      /*
       *  check to see if it exists
       */
      /*
      printf( "%08x * %u: ", 
              (unsigned)p->ref.base_page_num,
              p->ref.nth_page);
      */
      a = lss_read_access( store->lss, p->ref.base_page_num );
      
      if (a) {
        /*printf( "@ %p\n", p->mem_address );*/
        htable_insert( &base_ok, (void*)p->ref.base_page_num )->value = p;
        lss_read_release( store->lss, a );

        if (p->ref.loaded) {
          /* recall that `nth_page' is num_pages if p->ref.first */
          mm_unload( p->mem_address, MM_PAGE_SIZE * p->ref.nth_page );
        }
      } /* else {
        printf( "missing\n" );
        }*/
    }
    /* after this, nothing will be loaded, not even non-first pages */
    p->ref.dirty = 0;
    p->ref.loaded = 0;
  }
  /*
   *  At this point, nothing is loaded or dirty.  We took care
   *  of that in the first pass through.
   */
  store->first_loaded = NULL;
  store->first_dirty = NULL;
  store->last_dirty = NULL;
  store->num_dirty = 0;

  /*
   *  However, we still need to go through and unmap any reserved
   *  pages that aren't in our base_ok table
   */
  prev = NULL;
  next = NULL;
  for (p=store->first_reserved; p; p=next) {
    next = p->next_reserved;

    if (htable_lookup( &base_ok, (void *)p->ref.base_page_num )) {
      prev = p;
      nret++;
    } else {
      if (!p->ref.indirect) {
        /*
         *  free up the data memory (note that we are guaranteed
         *  to find the FIRST PAGE on the reserved list)
         */
        if (p->ref.first) {
          mm_free( p->mem_address, MM_PAGE_SIZE * p->ref.nth_page );
        }
      }
      /*
       *  take it out of the hash tables
       */
      if (!p->ref.indirect) {
        htable_remove( &store->vm_page_records, 
                       p->mem_address );
      }
      if (p->ref.first) {
        htable_remove( &store->reserved_base_pages, 
                       (void*)p->ref.base_page_num );
      }
      n++;
      /*
       *  free up the meta-data memory
       */
      free( p );

      if (prev) {
        prev->next_reserved = next;
      } else {
        store->first_reserved = next;
      }
    }
  }

  htable_free( &base_ok );
  /*printf( "%u pages unreserved, %u retained\n", n, nret );*/
}


RStore *rstore_of_object( obj item )
{
  if (OBJ_ISA_PTR( item )) {
    struct IRC_Header *h = IRCH( PTR_TO_GCPTR( item ) );
    /* layout is the same for persistent as well as transient objects */
    struct IRC_SizeClass *sc = h->sizeClass;

    if (sc->gen->genNum == 7) {
      struct FirstPageHdr *pg;
      pg = FIRST_PAGE_HDR_OF( h );
      return pg->area->owner;
    } else {
      /* it's not in a pstore */
      return NULL;
    }
  } else {
    return NULL;    /* it's an immob */
  }
}

int rstore_did_rollback( struct RStore *store )
{
  /*
   *  If the underlying LSS rolled back, we have a lot of work
   *  to do...
   *
   *  (1) unmap everything
   *
   *  (2) reload the initial record
   */

#if !INCLUDE_PSTORE_UNMAPPER
  /*XXX we should complain about this, but not for now...
  fprintf( stderr, "** WARNING ** rstore-did-rollback without PSTORE_UNMAPPER\n" );
  */
#endif

  /*rstore_check_unmappable( store );*/
  /* unload all pages */
  /*rstore_init_vm( store );*/

  /* unreserve any page that is not in the new LSS */

  rstore_unreserve_missing( store );

  /* reload the client info from the LSS */
  
  rstore_load_client_info( store );

  /*
   *  Note that our Scheme-level caller is responsible
   *  for repopulating any necessary indirect pages
   *  as if this pstore was being newly opened.  Automatic
   *  indirects will be repopulated when any pages are
   *  *loaded*.
   */

  /*
   *  They will have rebuilt these tables, so pull them
   *  back out of the owner
   */
  store->pivot_table = gvec_ref( store->owner, SLOT(1) );
  store->local_code_ptrs = gvec_ref( store->owner, SLOT(2) );
  store->local_fn_descrs = gvec_ref( store->owner, SLOT(3) );
  store->dirty_cards = gvec_ref( store->owner, SLOT(5) );
  store->reloc_table = FALSE_OBJ;

  return 0;
}

/* allocate some new storage... */

void install_new_vmpr( RStore *store, struct VMPageRecord *vmpr )
{
    vmpr->next_reserved = store->first_reserved;
    store->first_reserved = vmpr;

#ifdef VERBOSE
    printf( "new_vmpr(%p) : %p\n", vmpr, vmpr->mem_address );
#endif

    if (vmpr->ref.loaded)
      {
	vmpr->next_loaded = store->first_loaded;
	store->first_loaded = vmpr;
      }

    if (vmpr->ref.dirty)
      {
	if (store->first_dirty)
	  {
	    /* put this after the previously last dirty page */
	    store->last_dirty->next_dirty = vmpr;
	  }
	else
	  {
	    /* this is the first dirty page... */
	    store->first_dirty = vmpr;
	  }
	store->last_dirty = vmpr;
	vmpr->next_dirty = NULL;
	store->num_dirty++;
      }

    if (!vmpr->ref.indirect)
      htable_insert( &store->vm_page_records, vmpr->mem_address )->value = vmpr;
    if (vmpr->ref.first)
      htable_insert( &store->reserved_base_pages,
		    (void *)vmpr->ref.base_page_num )->value = vmpr;
}

/*
 *  remove a set of VMPageRecords (corresponding to a single "first" VMPR
 *  and all the subsequent ones) from all of the data structures that they
 *  are participating in, which includes (up to):
 *
 *     - the linked list of reserved (mapped) VMPRs
 *     - the linked list of loaded (in-core) VMPRs
 *     - the linked list of dirty (r/w) VMPRs
 *     - the hash table of all VMPRs (mapping page address => vmpr)
 *     - the hash table of reserved base pages (mapping pers. address => vmpr)
 */

static void uninstall_vmpr_set( RStore *store, struct VMPageRecord *first )
{
  struct VMPageRecord *i, *p;
  void *mem_start, *mem_limit;
  char *memi;
  unsigned n;

  mem_start = first->mem_address;
  mem_limit = (char*)mem_start + first->ref.nth_page * MM_PAGE_SIZE;

  RS_LVerbose( 463, 4209, "unreserving %u pages <%lx:>... @ %p",
               first->ref.nth_page,
               first->ref.base_page_num,
               mem_start );
  /*
  printf( "uninstall vmpr set [%08x, %08x): ", 
          (unsigned long)mem_start,
          (unsigned long)mem_limit );
  fflush( stdout );
  */

  /*
   *  Remove them from the list of reserved pages
   */

  for (n=0, p=NULL, i=store->first_reserved; i; i=i->next_reserved) {
    if ((i->mem_address >= mem_start) && (i->mem_address < mem_limit)) {
      if (p) {
        p->next_reserved = i->next_reserved;
      } else {
        store->first_reserved = i->next_reserved;
      }
      n++;
    } else {
      p = i;
    }
  }
  /*printf( " (%u r", n );
    fflush( stdout );*/

  /*
   *  Remove them from the list of loaded pages
   */
  for (n=0, p=NULL, i=store->first_loaded; i; i=i->next_loaded) {
    if ((i->mem_address >= mem_start) && (i->mem_address < mem_limit)) {
      if (p) {
        p->next_loaded = i->next_loaded;
      } else {
        store->first_loaded = i->next_loaded;
      }
      n++;
    } else {
      p = i;
    }
  }
  /*printf( ", %u l", n );
    fflush( stdout );*/

  /*
   *  Remove them from the list of dirty pages
   */
  for (n=0, p=NULL, i=store->first_dirty; i; i=i->next_dirty) {
    if ((i->mem_address >= mem_start) && (i->mem_address < mem_limit)) {
      if (p) {
        p->next_dirty = i->next_dirty;
      } else {
        store->first_dirty = i->next_dirty;
      }
      store->num_dirty--;
      n++;
    } else {
      p = i;
    }
  }
  /*printf( ", %u d", n );*/
  if (p != store->last_dirty) {
    store->last_dirty = p;
    /*printf( " (L)" );*/
  }
  /*fflush( stdout );*/


  {
    void *entry;
    n = 0;
    entry = htable_remove( &store->reserved_base_pages, 
                           (void *)first->ref.base_page_num );
    if (entry) {
      n++;
    }
  }
  /*printf( ", %u rb", n );
    fflush( stdout );*/

  for (n=0, memi=mem_start; memi<(char*)mem_limit; memi += MM_PAGE_SIZE) {
    void *entry;
    entry = htable_remove( &store->vm_page_records, memi );
    assert( entry );
    assert( (n != 0) || (entry == first) );
    if (entry) {
      n++;
      free( entry );
    }
  }

  /*printf( ", %u h)\n", n );
    fflush( stdout );*/
}

void dealloc_ppages( RStore *store, 
                     struct VMPageRecord *first,
                     UINT_32 num_pages )
{
  mm_free( first->mem_address, num_pages * MM_PAGE_SIZE );
  uninstall_vmpr_set( store, first );
  /* by the time `uninstall_vmpr_set' returns, the VMPR is invalid
     because it has been freed */
  first = NULL;
}

struct VMPageRecord *alloc_vmpr( void )
{
  struct VMPageRecord *vmpr = ALLOC( struct VMPageRecord );
  memset( vmpr, 0, sizeof( struct VMPageRecord ) );
  return vmpr;
}


struct VMPageRecord *alloc_ppages( RStore *store, UINT_32 num_pages )
{
  struct VMPageRecord *first, *vmpr;
  void *addr;
  unsigned i;
 
  UINT_32 base_page_num = 0;

  if (lss_alloc_recs( store->lss, 0x2000000, num_pages, &base_page_num ) < 0) {
    base_page_num = store->next_page;
    store->next_page += num_pages;
  }
 
  /* allocate the first */

  addr = mm_alloc( num_pages * MM_PAGE_SIZE, MM_MODE_READ_WRITE );

  first = vmpr = alloc_vmpr();

  vmpr->ref.base_page_num = base_page_num;
  vmpr->ref.first = 1;
  vmpr->ref.dirty = 1;
  vmpr->ref.loaded = 1;
  
  vmpr->ref.nth_page = num_pages;  /* for FIRST, is # pages */
  vmpr->mem_address = addr;

  /*printf( "-- allocated first page of %d at %p\n", 
    (int)num_pages, addr );*/

  RS_LVerbose( 463, 4201, "allocated %u pages <%lx:>... @ %p",
               num_pages,
               base_page_num,
               addr );

  install_new_vmpr( store, vmpr );
    
  /* allocate the rest */

  for (i=1; i<num_pages; i++) {
    vmpr = alloc_vmpr();

    vmpr->ref.base_page_num = base_page_num;
    vmpr->ref.first = 0;
    vmpr->ref.indirect = 0;
    vmpr->ref.dirty = 1;
    vmpr->ref.loaded = 1;
    vmpr->ref.nth_page = i;
    addr = ((char *)addr) + MM_PAGE_SIZE;
    vmpr->mem_address = addr;
	
    install_new_vmpr( store, vmpr );
  }
  return first;
}

/* reserve a new page (or pages) */

static struct VMPageRecord *reservation( RStore *store, 
					 struct PageRef *pr, void *addr )
{
struct VMPageRecord *vmpr = alloc_vmpr();

    vmpr->ref = *pr;

    /*
     *  clear the bits used internally; who knows what they look
     *  like coming from our caller... 
     */
    vmpr->ref.dirty = 0;
    vmpr->ref.loaded = 0;
    vmpr->ref.has_gray = 0;
    vmpr->ref.has_livetbl = 0;

    vmpr->next_reserved = store->first_reserved;
    store->first_reserved = vmpr;

    vmpr->next_dirty = NULL;
    vmpr->next_loaded = NULL;

    vmpr->mem_address = addr;

    htable_insert( &store->vm_page_records, addr )->value = vmpr;
    if (pr->first)
      {
	htable_insert( &store->reserved_base_pages,
		       (void *)pr->base_page_num )->value = vmpr;
      }
    return vmpr;
}

struct VMPageRecord *reserve_multi_page( struct RStore *store, 
					 struct PageRef *pr )
{
  unsigned i, n;
  char *a;
  struct VMPageRecord *first;

  assert( pr->first ); /* only works if we encounter the 1st page! */

  n = pr->nth_page;
  a = (char *)mm_alloc( MM_PAGE_SIZE * n, MM_MODE_NO_ACCESS );

  RS_LVerbose( 463, 4203, "reserving %u pages <%lx:>... @ %p",
               n,
               pr->base_page_num,
               a );

  first = reservation( store, pr, a );

  for (i=1; i<n; i++)
    {
      struct PageRef interior;

      interior.base_page_num = pr->base_page_num;
      interior.first = 0;
      interior.indirect = 0;
      interior.nth_page = i;

      a += MM_PAGE_SIZE;
      reservation( store, &interior, a );
    }
  return first;
}

struct VMPageRecord *reserve_single_page( struct RStore *store, 
					  struct PageRef *pr )
{
  void *addr = mm_alloc( MM_PAGE_SIZE, MM_MODE_NO_ACCESS );

  RS_LVerbose( 463, 4202, "reserving page <%lx:> @ %p",
               pr->base_page_num,
               addr );

  return reservation( store, pr, addr );
}

/* returns an existing vmpr, or reserves it */

struct VMPageRecord *get_vmpr( struct RStore *store, struct PageRef *pr )
{
struct htent *e;

    e = htable_lookup( &store->reserved_base_pages,
		       (void *)pr->base_page_num );
    if (e)
	return (struct VMPageRecord *)e->value;

    if (pr->indirect)
	return build_indirect_page( store, pr );
    else if (pr->first && pr->nth_page == 1)
	return reserve_single_page( store, pr );
    else
	return reserve_multi_page( store, pr );
}

#if INCLUDE_PSTORE_UNMAPPER
extern obj found_unmapped;
#endif

static void free_rstore( struct RStore *store )
{
  lss_close( store->lss );
  rstore_unmap_all( store );
  free( store );
}


void rstore_close( struct RStore *store )
{
  int i;

#if !INCLUDE_PSTORE_UNMAPPER
  fprintf( stderr, "** WARNING ** rstore-close without PSTORE_UNMAPPER\n" );
#endif

  rstore_check_unmappable( store );

  assert( store->lss );
  for (i=0; i<num_rstores; i++)
    {
      if (rstores[i] == store)
        {
#ifdef VERBOSE
          printf( "rstore_close[%d] %p\n", i, store );
#endif
          irc_close_pstore_gen( &store->the_gen );
          memmove( &rstores[i], 
                   &rstores[i+1], 
                   sizeof( struct RStore * ) * (num_rstores - i - 1) );
          num_rstores--;
          free_rstore( store );
#ifdef VERBOSE
          printf( "rstore_closed[%d]\nGC Testing...", i );
          fflush( stdout );
          gc_now();  /* this should not fault */
          printf( "OK\n" );
#endif
          gc_arena->theGenerations[0].traversalWork[14] = 0;
          gc_arena->theGenerations[0].traversalWork[15] = 0;
          return;
        }
    }
  scheme_error( "abandoned rstore", 1, store->owner );
}

static obj attempt_to_flush_pages( struct RStore *store, obj root_obj )
{
  set_rstore_root( store, root_obj );
  return write_dirty_pages( store );
}

static obj do_commit( struct RStore *store, obj root )
{
  struct RStoreCommitInfo ci;
  UINT_32 key;

  memset( &ci, 0, sizeof ci );

  switch (store->using_rich_model)
    {
    case OLD_RICH_MODEL:
      strcpy( ci.tag, "TKG-RStore-1.0" );
      break;
    case LRU_RICH_MODEL:
      strcpy( ci.tag, "RStore-3.0" );
      break;
    }
  
  ci.next_page_num = store->next_page;
  ci.next_indirect_page = store->next_indirect_page;
  ci.default_area = create_LR_first( store,
				     DATAPTR_TO_PTR(store->default_area) );

  lss_write( store->lss, CLIENT_INFO_RECNUM, &ci, sizeof ci, NULL );
  key = lss_commit( store->lss, 0 );

  /* printf( "committed lss => %lu\n", key ); */

  return int_64_compact( int_32_to_int_64( key ) );
}

obj rstore_get_scheme_object( RStore *owner )
{
  return owner->owner;
}

void set_rstore_root( RStore *store, obj item )
{
  /* don't WRITE to the page unless it needs to be changed */
  if (!EQ( item, store->default_area->entry ))
    {
      gvec_set( DATAPTR_TO_PTR(store->default_area), SLOT(0), item );
    }
}

obj rstore_root( RStore *store )
{
  return store->default_area->entry;
}

/*
 *
 *  rstore_commit() -- attempt to commit the state of the persistent store
 *
 *  Returns either a list of unresolved objects if there are some objects
 *  that still need to be copied to the store; else returns a commit 
 *  identifier if the commit was successful.
 *
 * ---XXX-- live_tbl is obsolete
 *  If `live_tbl' is not #f, then it is an object hash table, and
 *  a persistent GC cycle is going to start with the state of this
 *  commit.  In which case, if we succeed in committing the store,
 *  we will fill the table with all the persistent objects referenced
 *  by the transient heap.
 */

obj rstore_commit( struct RStore *store, obj root, obj reloc, obj live_tbl )
{
  obj unresolved;
  struct timeval t0;

  gettimeofday( &t0, NULL );

  RS_LVerbose( 463, 2201, "starting commit attempt" );

  store->reloc_table = reloc;
  unresolved = attempt_to_flush_pages( store, root );

  if (EQ( unresolved, NIL_OBJ ))
    {
      obj at = do_commit( store, root );

      RS_LVerbose( 463, 2209, "pstore commit {%08lx}", VAL(at) );

#ifdef GC_TRACE
      printf( ": do_commit -> {%08lx}\n", VAL(at) );
#endif

      hashtable_clear( reloc );

      /*  everything is peachy from an intergeneration-pointer
       *  perspective; clear out the extraHeapPointers list,
       *  and our own dirty_cards map (objects' gc_flag_bits
       *  were cleared as they were being written out)
       */
      irc_pstore_gen_did_commit( &store->the_gen );
      hashtable_clear( store->dirty_cards );

      store->reloc_table = FALSE_OBJ;

      if ((store->pgc_phase == PGC_PREP) && (store->num_pages_w_gray == 0)) {
        /* we were in the PREP phase, but there are no more pages with
           gray objects, so all eager cleaning is complete.  Now that
           we've committed, we can switch to the PENDING phase, wherein
           we can start the persistent traversal and await the next
           full transient GC cycle */
        rstore_gc_set_phase( store, PGC_PENDING );

        /* create the live queue */
        gvec_write( store->owner, SLOT(6), make_dequeue() );

        /* communicate the commit id back to the GC process */
        dequeue_push_back( gvec_ref( store->owner, SLOT(6) ), 
                           make1( vector_class, at ) );
      }
      return at;
    }
  store->reloc_table = FALSE_OBJ;
  return unresolved;
}

void dirty_page( RStore *store, struct VMPageRecord *vmp )
{
    assert( !vmp->ref.dirty );
    if (store->first_dirty)
      {
	store->last_dirty->next_dirty = vmp;
      }
    else
      {
	store->first_dirty = vmp;
      }
    store->last_dirty = vmp;
    store->num_dirty++;

    vmp->ref.dirty = 1;
    vmp->next_dirty = NULL;
    mm_set_prot( vmp->mem_address, MM_PAGE_SIZE, MM_MODE_READ_WRITE );
}

void *mmc_in_failure = NULL;

static void pstore_write_protected( RStore *ps, void *addr )
{
  char temp[12];

  sprintf( temp, "@%04lx_%04lx", 
           (((unsigned long)addr) >> 16) & 0xFFFF,
           ((unsigned long)addr) & 0xFFFF );
  scheme_error( "wrote to access-protected store: ~a in ~s",
                2, make_string( temp ), ps->owner );
}

static void rscheme_memory_fault( void *addr )
{
  char temp[12];

  fprintf( stderr, "\n\nrscheme_memory_fault( %p )\n", addr );
  fflush( stderr );     /* make sure this gets out in case we have more trouble */

  rs_crash_dump_vmstate( stderr );

  abort();              /* this should not happen */

  sprintf( temp, "@%04lx_%04lx", 
           (((unsigned long)addr) >> 16) & 0xFFFF,
           ((unsigned long)addr) & 0xFFFF );
  scheme_error( "access protection error at address ~a",
                1, make_string(temp) );
}


void mmc_access_failed( void *addr )
{
  struct VMPageRecord *vmp;
  RStore *store;

  /* we should never get a recursive fault */

  if (mmc_in_failure) {
    fprintf( stderr, "\n****** mmc_access_failed() double fault *********\n" );
    fprintf( stderr, "******         fault on %p\n", addr );
    fprintf( stderr, "****** while processing %p\n", mmc_in_failure );
    abort();
  }

  assert( !mmc_in_failure );
  mmc_in_failure = addr;

  /* look up the address */
  vmp = find_owner_and_vmpr( addr, &store );
#ifdef VERBOSE
  printf( "/ protection fault at: %p vmp={%p}\n", addr, vmp );
#endif
  
  if (vmp)
    {
      if (!vmp->ref.loaded)
	{
#ifdef VERBOSE
	  printf( "  load page %lu\n", vmp->ref.base_page_num );
#endif
	  /* load it in */
	  load_page( store, vmp );
	}
      else 
	{
#ifdef VERBOSE
	  printf( "  scribbling on page %lu\n", vmp->ref.base_page_num );
#endif
	  /* mark it dirty */
	  assert( !vmp->ref.dirty );
          if (store->write_protect) {
            mmc_in_failure = NULL;      /* clear this so we can fault again */
            pstore_write_protected( store, addr );
          }
	  dirty_page( store, vmp );
	}
    }
  else
    {
#ifdef VERBOSE
      printf( "  could not find owner of %p\n", addr );
      abort();
#endif
      /*
        XXX It's still too dangerous to clear this; somebody much higher
            in the error recovery stack should clear it...
        mmc_in_failure = NULL;     
      */
      rscheme_memory_fault( addr );
    }
#ifdef VERBOSE
  printf( "\\ done with fault at %p\n", addr );
#endif
  mmc_in_failure = NULL;
}

struct VMPageRecord *find_owner_and_vmpr( void *addr, RStore **s )
{
  int i;
  struct VMPageRecord *vmp;

  for (i=0; i<num_rstores; i++)
    {
#ifdef VERBOSE
      printf( "(%d %p?) ", i, rstores[i] );
#endif
      vmp = addr_to_vm_page_record( rstores[i],
				    MM_PAGE_BASE_ADDR(addr) );
      if (vmp)
	{
	  *s = rstores[i];
	  return vmp;
	}
    }
  return NULL;
}
#undef VERBOSE


enum SwizzleMode mode_for_object( struct PHeapHdr *hdr )
{
  obj imode = gvec_read( hdr->rs_header.pob_class, SLOT(2) );
  enum SwizzleMode m;

  m = OBJ_ISA_FIXNUM(imode) ? fx2int(imode) : SWIZ_MODE_GVEC;
  switch (m)
    {
    case SWIZ_MODE_GVEC:
    case SWIZ_MODE_BVEC:
    case SWIZ_MODE_TEMPLATE:
    case SWIZ_MODE_UINT32:
    case SWIZ_MODE_FLOAT:
    case SWIZ_MODE_PADDR_VEC:
    case SWIZ_MODE_PART_DESCR:
    case SWIZ_MODE_ALLOC_AREA:
    case SWIZ_MODE_APP_0:
    case SWIZ_MODE_APP_1:
    case SWIZ_MODE_APP_2:
    case SWIZ_MODE_APP_3:
    case SWIZ_MODE_APP_4:
    case SWIZ_MODE_APP_5:
    case SWIZ_MODE_APP_6:
    case SWIZ_MODE_APP_7:
    case SWIZ_MODE_APP_8:
    case SWIZ_MODE_APP_9:
      return m;
    }
  scheme_error( "don't know how to swizzle ~s (image-mode ~s)",
	        2, PHH_TO_PTR(hdr), imode );
  return 0; /* hush up -Wall, since scheme_error() never returns */
}

void rstore_copy_gvec_proc( obj new_item, obj old_item )
{
  UINT_32 i, size = PTR_TO_HDRPTR(old_item)->pob_size;

  for (i=0; i<size; i+=SLOT(1)) {
    gvec_write_init_non_ptr( new_item, i, ZERO );
    /* the write barrier may want to trap these down pointers */
    gvec_write( new_item, i, gvec_ref( old_item, i ) );
  }
}

/* copy an object into the store */

obj rstore_copy_in( AllocArea *aa, obj item )
{
  PAllocArea *area;
  obj new_item;
  POBHeader *h;
  enum SwizzleMode m;

  /*  this is the only crude check we have right now for making
   *  sure this AllocArea is really a PAllocArea
   */
  assert( aa->info != NULL );

  area = (PAllocArea *)aa;

  if (!OBJ_ISA_PTR(item))
    {
      /* silently ignore requests to copy non-ptrs into the store */
      return item;
    }

  h = PTR_TO_HDRPTR(item);

  m = mode_for_object(PTR_TO_PHH(item));

  if (m != SWIZ_MODE_ALLOC_AREA)
    new_item = parea_alloc( (AllocArea *)area, h->pob_class, h->pob_size );
  else
    new_item = FALSE_OBJ;

#ifdef GC_TRACE
  printf( "rstore_copy_in {%08lx} --copy--> {%08lx}\n", VAL(item), VAL(new_item) );
#endif

  switch (m)
    {
    case SWIZ_MODE_BVEC:
    case SWIZ_MODE_UINT32:
    case SWIZ_MODE_FLOAT:
    case SWIZ_MODE_PADDR_VEC:
      memcpy( PTR_TO_DATAPTR(new_item), 
              PTR_TO_DATAPTR(item), 
              h->pob_size );
      break;
    case SWIZ_MODE_ALLOC_AREA:  /* quiet compiler */
      scheme_error( "Illegal copy: ~s", 1, item );
    case SWIZ_MODE_GVEC:
    case SWIZ_MODE_PART_DESCR:
    case SWIZ_MODE_TEMPLATE:
      rstore_copy_gvec_proc( new_item, item );
      break;
    default:
      {
	struct swiz_mode_handler *h = get_swiz_mode_handler( area->owner, m );
	h->copy_in( new_item, item );
      }
    }
  return new_item;
}


AllocArea *rstore_get_default_area( RStore *store )
{
  return (AllocArea *)store->default_area;
}

void rstore_set_default_area( RStore *store, AllocArea *area )
{
  struct PAllocArea *a = (struct PAllocArea *)area;

  /*  be nice to have a better way of checking that it is a PAllocArea, and
   *  in the right place, too...
   */
  assert( a->owner == store );
  store->default_area = a;
}

obj rstore_area_owner( AllocArea *area )
{
   if (area->info)
     return ((PAllocArea *)area)->owner->owner;
   else
     return FALSE_OBJ; /* not a PStore area */
}

obj rstore_hash( obj item )
{
  struct IRC_Header *h;
  struct FirstPageHdr *pg;
  /*struct LocationRef loc;*/
  UINT_32 pagenum, offset;

  if (!OBJ_ISA_PTR( item )) {
    /* it's not a heap object */
    return obj_hash( item );
  }

  h = IRCH( PTR_TO_GCPTR( item ) );

  if (!((h->flagBits & IRC_MASK_GEN) == (~0 & IRC_MASK_GEN))) {
    /* it's not in a persitent store */
    return obj_hash( item );
  }

  pg = FIRST_PAGE_HDR_OF( h );

  /* create_LR_on_page( pg->area->owner, item, pg->vmpr ); */

  pagenum = pg->vmpr->ref.base_page_num;
  offset = VAL(item) - (UINT_32)pg->vmpr->mem_address;

  return uint2_hash( pagenum, offset );
}

AllocArea *rstore_alloc_area( obj item )
{
  if (OBJ_ISA_PTR( item ))
    {
      RStore *o;
      struct VMPageRecord *vmpr;
      struct PHeapHdr *p = PTR_TO_PHH(item);

      vmpr = find_owner_and_vmpr( p, &o );

      if (vmpr)
        {
          struct FirstPageHdr *fph = (struct FirstPageHdr *)vmpr->mem_address;
	  return (AllocArea *)fph->area;
        }
      else
        return (AllocArea *)PTR_TO_DATAPTR( default_alloc_area );
    }
  else
    return (AllocArea *)PTR_TO_DATAPTR( default_alloc_area );
}

int rstore_count_dirty( RStore *store )
{
  return store->num_dirty;
}

/* an AllocArea is a heap-allocated mixvec, so just plop
 * back in the PTR tags and we've got an OBJ
 */

obj alloc_area_to_obj( AllocArea *area )
{
  return OBJ( ((UINT_32)area) + POINTER_TAG );
}

void rstore_set_compression( struct RStore *store, const char *str )
{
  zip_algorithm *z;

  z = lss_find_zip_algorithm( str );
  if (z)
    {
      store->data_zipper = z;
    }
  else
    {
      scheme_error( "zip algorithm undefined: ~s", 1, make_string( str ) );
    }
}

int rstore_get_write_protect( struct RStore *store )
{
  return store->write_protect;
}

void rstore_set_write_protect( struct RStore *store, int flag )
{
  store->write_protect = flag;
}

void rstore_gc_set_tracking( RStore *owner, int level )
{
  assert( (level >= 0) && (level <= 2) );
  irc_pstore_gen_set_tracking( &owner->the_gen, level );
}

unsigned rstore_gc_flagbits( obj item )
{
  gc_obj_addr p = PTR_TO_GCPTR( item );
  IRC_Header *h = IRCH( p );
  return h->flagBits;
}

void rstore_page_has_gray( RStore *ps, struct VMPageRecord *vmpr, int is_live )
{
  if (!vmpr->ref.has_gray) {

    vmpr->ref.has_gray = 1;
    ps->num_pages_w_gray++;
    RS_LVerbose( 463, 3510, "vmp <%08lx> @%p marked gray (now %u)", 
                 vmpr->ref.base_page_num,
                 vmpr->mem_address,
                 ps->num_pages_w_gray );

  } else {
    RS_LVerbose( 463, 3511, "vmp <%08lx> @%p was already gray",
                 vmpr->ref.base_page_num,
                 vmpr->mem_address );
  }

  if (is_live && !vmpr->ref.has_livetbl) {
    obj q;

    RS_LVerbose( 463, 3512, "vmp <%08lx> @%p has new live entries", 
                 vmpr->ref.base_page_num,
                 vmpr->mem_address );
    vmpr->ref.has_livetbl = 1;

    q = gvec_ref( ps->owner, SLOT(6) );
    if (OBJ_ISA_PTR_OF_CLASS( q, dequeue_class )) {
      dequeue_push_back( q,  uint_32_compact( vmpr->ref.base_page_num ) );
    }
  }
}


int rstore_gc_mark_gray( RStore *ps, gc_obj_addr p, rs_bool is_live )
{
  IRC_Header *h = IRCH( p );
  struct PHeapHdr *phh = (void *)h;

  if (!ps->is_persistent_traversal) {
    RS_LVerbose( 463, 3599, "object %p mark_gray ignored; not doing traversal",
                 p );
    return 0;
  }
    
  if ((phh->gc_flag_bits & IRC_MASK_COLOR) == 0) {    /* WHITE */
    struct FirstPageHdr *pg;
    struct VMPageRecord *vmpr;

    pg = FIRST_PAGE_HDR_OF( h );
    vmpr = pg->vmpr;

    RS_LVerbose( 463, 3501, "object %p is now gray", p );

    phh->gc_flag_bits |= IRC_MASK_COLOR;
    
    if (is_live) {
      phh->gc_flag_bits |= IRC_MASK_MARKEDQ;
    }
    rstore_page_has_gray( ps, vmpr, is_live );
    return 1;
  } else {                                            /* BLACK (GRAY) */
    RS_LVerbose( 463, 3502, "object %p is already gray", p );
    return 0;
  }
}

obj rstore_gc_grayify( obj item )
{
  gc_obj_addr p = PTR_TO_GCPTR( item );
  IRC_Header *h = IRCH( p );
  RStore *ps;

  ps = rstore_of_object( item );

  if (ps) {
    rstore_gc_mark_gray( ps, p, NO );
    return ps->owner;   /* persistent object */
  } else if (irc_grayify( PTR_TO_GCPTR( item ) )) {
    return TRUE_OBJ;    /* did some transient work */
  } else {
    return FALSE_OBJ;   /* transient object already gray */
  }
}

struct cleaning_report {
  unsigned num_visit;
  unsigned num_cleaned;
  obj accum;
  void *mem_base;
};

static int clean1live( void *info, struct PHeapHdr *ptr )
{
  struct cleaning_report *r = info;
  unsigned offset = VAL(PHH_TO_PTR(ptr)) - (UINT_32)r->mem_base;
  r->num_visit++;

  if (ptr->gc_flag_bits & IRC_MASK_MARKEDQ) {
    RS_LVerbose( 463, 3122, "LIVE [%08lx] -- noted +%04x", ptr, offset );

    /* this had better be a gray object, too! */
    assert( ptr->gc_flag_bits & IRC_MASK_COLOR );

    ptr->gc_flag_bits &= ~IRC_MASK_MARKEDQ;
    r->num_cleaned++;
    r->accum = cons( int2fx( offset ), r->accum );
  } else {
    RS_LVerbose( 463, 3123, "NOTLIVE [%08lx] (+%04x)", ptr, offset );
  }
  return 0;
}

static int clean1( void *info, struct PHeapHdr *ptr )
{
  struct cleaning_report *r = info;
  r->num_visit++;

  if (ptr->gc_flag_bits & IRC_MASK_COLOR) {
    RS_LVerbose( 463, 3261, "GRAY [%08lx] -- cleaned", ptr );
    ptr->gc_flag_bits &= ~IRC_MASK_COLOR;
    r->num_cleaned++;
  } else {
    RS_LVerbose( 463, 3262, "WHITE [%08lx]", ptr );
  }
  return 0;
}

static obj rstore_clean_livetbl( RStore *ps, struct VMPageRecord *p )
{
  struct cleaning_report info;

  info.num_visit = 0;
  info.num_cleaned = 0;
  info.accum = NIL_OBJ;
  info.mem_base = p->mem_address;

  rstore_foreach_object( p, clean1live, &info );
  RS_LVerbose( 463, 3125, "vmp <%08lx> noted %u out of %u (new) live objects",
               p->ref.base_page_num,
               info.num_cleaned,
               info.num_visit );

  if (p->ref.has_livetbl) {
    p->ref.has_livetbl = 0;
  }
  return info.accum;
}

static void rstore_clean_gray( RStore *ps, struct VMPageRecord *p )
{
  struct cleaning_report info;

  info.num_visit = 0;
  info.num_cleaned = 0;
  info.accum = FALSE_OBJ;
  info.mem_base = p->mem_address;

  if (!p->ref.dirty) {
    /* the page is not dirty (this is the usual case here, because
       we are normally executing in the context of the PGC's eager
       cleaning phase, and which execution happens just after a commit
       succeeds, in which case all pages are clean), so twiddle it's
       protection bits to prevent a write fault which would cause it
       to be spuriously marked dirty
    */
    mm_set_prot( p->mem_address, MM_PAGE_SIZE, MM_MODE_READ_WRITE );
  }

  rstore_foreach_object( p, clean1, &info );

  if (!p->ref.dirty) {
    mm_set_prot( p->mem_address, MM_PAGE_SIZE, MM_MODE_READ_ONLY );
  }

  RS_LVerbose( 463, 3263, "vmp <%08lx> cleaned %u out of %u objects",
               p->ref.base_page_num,
               info.num_cleaned,
               info.num_visit );
  if (p->ref.has_gray) {
    p->ref.has_gray = 0;
    assert( ps->num_pages_w_gray > 0 );
    ps->num_pages_w_gray--;
    RS_LVerbose( 463, 3264, "%u pages w/gray objects left", 
                 ps->num_pages_w_gray );
  }
}

static const char *(phase_str[]) = { "IDLE",
                                     "PREP",
                                     "PENDING",
                                     "TSCAN",
                                     "PSCAN",
                                     "RECLAIM",
                                     "PACKING" };

void rstore_tgc_igp( void *info,
                     void *lvalue, UINT_32 offset,
                     void *rvalue )
{
  RStore *ps = info;
  IRC_Header *r_h = IRCH( rvalue );
  struct PHeapHdr *r_phh = (void *)r_h;

  assert( (r_phh->pstore_flags == PFLAGS_NORMAL_OBJ)
          || (r_phh->pstore_flags == PFLAGS_LARGE_OBJ)
          || (r_phh->pstore_flags == PFLAGS_ALLOC_AREA) );

  if (lvalue) {
    RS_LVerbose( 463, 3211, "WB tripped on rvalue {%p}", rvalue );
    rstore_gc_mark_gray( ps, rvalue, YES );
  } else {
    if (ps->is_twb_enabled) {
      /* only need to do this if we are only in PENDING phase with the 
         persistent write barrier enabled 
      */
      RS_LVerbose( 463, 3212, "traversal reached {%p}", rvalue );
      rstore_gc_mark_gray( ps, rvalue, YES );
    }
  }
}

void rstore_tgc_flipped( void *info, int state )
{
  RStore *ps = info;

  if (state) {
    RS_LVerbose( 463, 3203, "transient GC finished" );
  } else {
    RS_LVerbose( 463, 3202, "transient GC is starting" );
  }

  switch (ps->pgc_phase) {

  case PGC_PENDING:
    if (state == 0) {
      /* a TGC cycle is starting.  That means we can enter
         the TSCAN phase and turn on the write barrier
      */
      rstore_gc_set_phase( ps, PGC_TSCAN );
    }
    break;
    
  case PGC_TSCAN:
    if (state == 1) {
      /* a TGC cycle just finished, so we can enter the PSCAN phase */
      rstore_gc_set_phase( ps, PGC_PSCAN );
    }
    break;

  case PGC_IDLE:
  case PGC_PACKING:
  case PGC_PREP:
  case PGC_PSCAN:
  case PGC_RECLAIM:
    /* we shouldn't really be called in this case... */
    break;
  }
}


void rstore_gc_set_phase( RStore *ps, enum PGC_Phase phase )
{
  RS_LVerbose( 463, 3200, "phase change %s -> %s", 
               phase_str[ ps->pgc_phase ],
               phase_str[ phase ] );

  RS_LPGC( 463, 3201, "phase %s %s",   
           phase_str[ ps->pgc_phase ],
           phase_str[ phase ] );

  /* clobber the live object queue when it doesn't make sense */

  switch (phase) {

  case PGC_IDLE:
  case PGC_PACKING:
  case PGC_RECLAIM:
  case PGC_PREP:
    gvec_write( ps->owner, SLOT(6), FALSE_OBJ );
    break;

  default:
    break;
  }

  switch (phase) {

  case PGC_RECLAIM:
  case PGC_IDLE:
  case PGC_PACKING:
    ps->new_flag_bits = 0xE;
    ps->is_lazy_cleaning = 1;
    ps->is_eager_cleaning = 0;
    ps->is_persistent_traversal = 0;
    ps->is_twb_enabled = 0;
    ps->is_pwb_enabled = 0;
    ps->is_alloc_black = 0;
    ps->the_gen.flip_hook = NULL;
    ps->the_gen.igp_hook = NULL;
    break;

  case PGC_PREP:
    ps->new_flag_bits = 0xE;
    ps->is_lazy_cleaning = 0;
    ps->is_eager_cleaning = 1;
    ps->is_persistent_traversal = 0;
    ps->is_twb_enabled = 0;
    ps->is_pwb_enabled = 0;
    ps->is_alloc_black = 0;
    ps->the_gen.flip_hook = NULL;
    ps->the_gen.igp_hook = NULL;
    break;

  case PGC_PENDING:
    ps->new_flag_bits = 0xF;    /* alloc black */
    ps->is_lazy_cleaning = 0;
    ps->is_eager_cleaning = 0;
    ps->is_persistent_traversal = 1;
    ps->is_twb_enabled = 0;
    ps->is_pwb_enabled = 1;
    ps->is_alloc_black = 1;
    ps->the_gen.flip_hook = rstore_tgc_flipped;
    ps->the_gen.igp_hook = rstore_tgc_igp;
    break;

  case PGC_TSCAN:
    ps->new_flag_bits = 0xF;    /* alloc black */
    ps->is_lazy_cleaning = 0;
    ps->is_eager_cleaning = 0;
    ps->is_persistent_traversal = 1;
    ps->is_twb_enabled = 1;
    ps->is_pwb_enabled = 1;
    ps->is_alloc_black = 1;
    ps->the_gen.flip_hook = rstore_tgc_flipped;
    ps->the_gen.igp_hook = rstore_tgc_igp;
    break;

  case PGC_PSCAN:
    ps->new_flag_bits = 0xE;    /* back to allocating white */
    ps->is_lazy_cleaning = 1;
    ps->is_eager_cleaning = 0;
    ps->is_persistent_traversal = 1;
    ps->is_twb_enabled = 0;
    ps->is_pwb_enabled = 0;
    ps->is_alloc_black = 0;
    ps->the_gen.flip_hook = NULL;
    ps->the_gen.igp_hook = NULL;
    break;
    
  }
  ps->pgc_phase = phase;

  if (ps->is_twb_enabled) {
    irc_pstore_gen_set_tracking( &ps->the_gen, 2 );
  } else if (ps->is_pwb_enabled) {
    irc_pstore_gen_set_tracking( &ps->the_gen, 1 );
  } else {
    irc_pstore_gen_set_tracking( &ps->the_gen, 0 );
  }
}

static void rstore_reclaim_p0( RStore *ps, obj x )
{
  enum SwizzleMode m = mode_for_object( PTR_TO_PHH( x ) );

  RS_LVerbose( 463, 3889, "Clearing pointers from {%08lx} (mode %d)", VAL(x), m );
               
  switch (m) {

  default:              /* XXX what about all those app swizmodes? */
  case SWIZ_MODE_PADDR_VEC:
  case SWIZ_MODE_FLOAT:
  case SWIZ_MODE_UINT32:
  case SWIZ_MODE_BVEC:
    /* nothing to do... XXX if class pointers are
     * ever to be stored in the store, we may have issues with that...
     */
    return;

  case SWIZ_MODE_TEMPLATE:
  case SWIZ_MODE_GVEC:
    {
      UINT_32 i;

      for (i=0; i<SIZEOF_PTR(x); i+=SLOT(1)) {
        gvec_write_non_ptr( x, i, UNMAPPED_OBJ );
      }
    }
    return;

  case SWIZ_MODE_ALLOC_AREA:
    {
      PAllocArea *aa = PTR_TO_DATAPTR( x );

      aa->entry = UNMAPPED_OBJ;
      aa->reserved = UNMAPPED_OBJ;
      aa->free_list_vec = UNMAPPED_OBJ;
    }
    return;
  }
}

static void rstore_reclaim_p1( RStore *ps, obj x )
{
  parea_dealloc( x );
}

static obj rstore_reclaim( RStore *ps,
                           UINT_32 pagenum,
                           unsigned flags,
                           obj offset_list,
                           int submode )
{
  unsigned offset;
  struct LocationRef lr;
  obj x;
  obj n = ZERO;

  while (!EQ( offset_list, NIL_OBJ )) {
    offset = fx2int( pair_car( offset_list ) );

    lr.first = (flags & 1) ? 1 : 0;
    lr.indirect = (flags & 2) ? 1 : 0;
    lr.nth_page = flags >> 2;
    lr.offset = offset;
    lr.base_page_num = pagenum;
  
    assert( lr.first );  /* nothing else is permitted! */

    /* XXX ideally, we would reuse the VMP lookup across all the offsets... */

    x = translate_LR( ps, lr );

    switch (submode) {
    case 0:
      rstore_reclaim_p0( ps, x );
      break;
    case 1:
      rstore_reclaim_p1( ps, x );
      break;
    }
    n = ADD1( n );
    offset_list = pair_cdr( offset_list );
  }
  return n;
}

obj rstore_gc_work( RStore *ps, int mode, obj arg )
{
  obj rc = FALSE_OBJ;

  RS_LVerbose( 463, 3101, "work #%d, arg {%08lx}", mode, VAL(arg) );

  switch (mode) {

  case 1:       /* prep... find a VMP with gray objects to clean */
    {
      unsigned n = 0;
      struct VMPageRecord *p;

      rc = FALSE_OBJ;
      for (p=ps->first_loaded; p; p=p->next_loaded) {

        RS_LVerbose( 463, 3110, "vmp scan[%u] <%08lx> : f=%u i=%u d=%u g=%u l=%u",
                     n,
                     p->ref.base_page_num,
                     p->ref.first,
                     p->ref.indirect,
                     p->ref.dirty,
                     p->ref.has_gray,
                     p->ref.has_livetbl );

        n++;

        if (p->ref.indirect || !p->ref.first) {
          continue;
        }

        if (p->ref.has_gray) {
          RS_LVerbose( 463, 3111, "vmp <%08lx> has gray objects (scanned %u)",
                       p->ref.base_page_num,
                       n );

          if (truish( arg )) {
            rstore_clean_gray( ps, p );
          }

          rc = uint_32_compact( p->ref.base_page_num );
          break;
        }
      }
      if (EQ( rc, FALSE_OBJ )) {
        RS_LVerbose( 463, 3118, "no pages with gray objects (scanned %u)",
                     n );
      }
    }
    break;

  case 2:       /* persistent traversal... find a VMP with new live objects */
    {
      unsigned n = 0;
      struct VMPageRecord *p;

      rc = FALSE_OBJ;
      for (p=ps->first_loaded; p; p=p->next_loaded) {

        RS_LVerbose( 463, 3120, "vmp scan[%u] <%08lx> : f=%u i=%u d=%u g=%u l=%u",
                     n,
                     p->ref.base_page_num,
                     p->ref.first,
                     p->ref.indirect,
                     p->ref.dirty,
                     p->ref.has_gray,
                     p->ref.has_livetbl );

        n++;

        if (p->ref.indirect || !p->ref.first) {
          continue;
        }

        if (p->ref.has_livetbl) {
          RS_LVerbose( 463, 3121, "vmp <%08lx> has live objects (scanned %u)",
                       p->ref.base_page_num,
                       n );

          if (truish( arg )) {
            rc = rstore_clean_livetbl( ps, p );
          }
          break;
        }
      }
    }
    break;

  case 3:       /* persistent traversal... look up VMP by number,
                   and return it's live objects (and also a GC-compatible
                   page_ref for the VMP) */
    {
      UINT_32 pagen = basic_raw_uint( arg );
      struct htent *e;
      struct VMPageRecord *p;

      e = htable_lookup( &ps->reserved_base_pages, (void *)pagen );
      if (!e) {
        scheme_error( "page <~08x> is not even reserved", 1, arg );
      }
      
      p = (struct VMPageRecord *)e->value;
      if (p->ref.indirect) {
        scheme_error( "page <~08x> is not direct!?", 1, arg );
      }
      if (!p->ref.first) {
        scheme_error( "page <~08x> is not a first-page!?", 1, arg );
      }
      
      RS_LVerbose( 463, 3130, "vmp <%08lx> : f=%u i=%u d=%u g=%u l=%u",
                   p->ref.base_page_num,
                   p->ref.first,
                   p->ref.indirect,
                   p->ref.dirty,
                   p->ref.has_gray,
                   p->ref.has_livetbl );
      
      if (p->ref.has_livetbl) {
        obj page_ref_info = int2fx( (p->ref.nth_page << 2)
                                    + (p->ref.indirect ? 2 : 0)
                                    + (p->ref.first ? 1 : 0) );
        rc = cons( page_ref_info, rstore_clean_livetbl( ps, p ) );
      } else {
        rc = NIL_OBJ;
      }
    }
    break;

  case 6:       /* zap pointers in some objects */
  case 7:       /* deallocate some objects */
    {
      assert( VECTOR_P( arg ) );
      rstore_reclaim( ps,
                      basic_raw_uint( gvec_ref( arg, SLOT(0) ) ),
                      fx2int( gvec_ref( arg, SLOT(1) ) ),
                      gvec_ref( arg, SLOT(2) ),
                      mode-6 );
    }
    break;

  case 100:     /* switch to a new phase: IDLE */
  case 101:     /* switch to a new phase: PREP */
  case 102:     /* switch to a new phase: PENDING */
  case 103:     /* switch to a new phase: TSCAN */
  case 104:     /* switch to a new phase: PSCAN */
  case 105:     /* switch to a new phase: RECLAIM */
  case 106:     /* switch to a new phase: PACKING */
    rstore_gc_set_phase( ps, mode - 100 );
    break;


  case 6000:    /* test: dump page status */
    {
      struct VMPageRecord *p;
      unsigned num_real = 0, num_indir = 0, num_load = 0, num_dirty = 0;
      unsigned num_hgray = 0, num_livetbl = 0;

      for (p=ps->first_reserved; p; p=p->next_reserved) {
        if (p->ref.indirect) {
          num_indir++;
        } else {
          num_real++;
          if (p->ref.loaded) {
            num_load++;
            if (p->ref.dirty) {
              num_dirty++;
            }
            if (p->ref.has_gray) {
              num_hgray++;
            }
            if (p->ref.has_livetbl) {
              num_livetbl++;
            }
          }
        }
      }
      RS_LVerbose( 463, 3901, "summary: %u real pages, %u indirects,",
                   num_real,
                   num_indir );
      RS_LVerbose( 463, 3902, "         %u loaded, %u dirty",
                   num_load,
                   num_dirty );
      RS_LVerbose( 463, 3903, "         %u w/gray (vs %u), %u w/livetbl",
                   num_hgray,
                   ps->num_pages_w_gray,
                   num_livetbl );
      rc = make6( vector_class,
                  int2fx( num_real ),
                  int2fx( num_indir ),
                  int2fx( num_load ),
                  int2fx( num_dirty ),
                  int2fx( num_hgray ),
                  int2fx( num_livetbl ) );
    }
  break;

  case 6003:    /* test: mark an object gray, and add to live set */
  case 6001:    /* test: mark an object gray, but not in live set */
    {
      struct PHeapHdr *phh = PTR_TO_PHH( arg );

      assert( OBJ_ISA_PTR( arg ) );

      if ((phh->gc_flag_bits & IRC_MASK_COLOR) == 0) {

        RStore *o;
        struct VMPageRecord *vmpr;
        vmpr = find_owner_and_vmpr( phh, &o );
        assert( o == ps );
        
        phh->gc_flag_bits |= IRC_MASK_COLOR;
        RS_LVerbose( 463, 3910, "phh [%p] is now marked gray", phh );
        rstore_page_has_gray( ps, vmpr, (mode == 6003) );
      } else {
        RS_LVerbose( 463, 3911, "phh [%p] was already marked gray", phh );
      }
      rc = FALSE_OBJ;
    }
  break;

  case 0:       /* query: return phase and activity bits */
  case 6002:    /* test: return phase and activity bits */
    {
      rc = make8( vector_class,
                  lookup_symbol( phase_str[ ps->pgc_phase ] ),
                  int2fx( ps->new_flag_bits ),
                  (ps->is_lazy_cleaning ? TRUE_OBJ : FALSE_OBJ),
                  (ps->is_eager_cleaning ? TRUE_OBJ : FALSE_OBJ),
                  (ps->is_persistent_traversal ? TRUE_OBJ : FALSE_OBJ),
                  (ps->is_twb_enabled ? TRUE_OBJ : FALSE_OBJ),
                  (ps->is_pwb_enabled ? TRUE_OBJ : FALSE_OBJ),
                  int2fx( ps->num_pages_w_gray ) );
    }
  break;
  }
  return rc;
}

