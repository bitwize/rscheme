#include <stdio.h>
#include <string.h>
#include "rstoret.h"
#include "alloc.h"
#include "mmglue.h"
#include <rscheme/scheme.h>
#include "indirect.h"

#define P_NUM_SIZE_CLASS (27)

/*
 *  A table of the size class member sizes
 *  (mem_size, i.e., including all headers)
 *  by size class.
 *
 *  Larger size classes are constructed to minimize external
 *  fragmentation in the case where only objects of that size
 *  are on a given page.
 */

static int psizeclass_l[P_NUM_SIZE_CLASS] = 
                            { 0, 16*2, 16*3, 16*4, 16*5, 16*6, 16*7,
                              32*4, 32*5, 32*6, 32*7,
                              64*4, 64*5, 64*6, 64*7,
                              128*4, 128*5, 128*6, 128*7,
                              256*4,
                              1168,             /* (8176/7) & ~7 */
                              1360,             /* (8176/6) & ~7 */
                              1632,             /* (8176/5) & ~7 */
                              2040,             /* (8176/4) & ~7 */
                              2720,             /* (8176/3) & ~7 */
                              4088,             /* (8176/2) & ~7 */
                              MM_PAGE_SIZE - sizeof( struct FirstPageHdr ) };

/*
 *  A table to map actual mem_sizes to size classes.  Note that this
 *  mapping necessarily rounds "down" because we won't
 *  be able to support any larger objects
 */

static UINT_8 psizeclass_a[MM_PAGE_SIZE+1];

/*
 *  A table to map request sizes to size classes.  This mapping
 *  necessarily rounds "up"
 */
static UINT_8 psizeclass[MM_PAGE_SIZE+1];


rs_bool loading_image = NO; /* hack */

static UINT_32 round_up( UINT_32 n )
{
  return n + ((8-n) & 7);
}


AllocArea *make_sub_alloc_area( AllocArea *aa )
{
  PAllocArea *pa = (PAllocArea *)aa;

  if (!pa->owner)
    {
      scheme_error( "Not a pstore area", 0 );
    }
  return (AllocArea *)make_alloc_area( pa->owner, alloc_area_to_obj( aa ) );
}

static void palloc_inithdr( RStore *owner,
                            struct PHeapHdr *phh,
                            obj the_class,
                            UINT_32 the_size )
{
  phh->gc_flag_bits = owner->new_flag_bits;

  phh->rs_header.pob_class = the_class;
  phh->rs_header.pob_size = the_size;

  if (owner->is_alloc_black) {
    RStore *ignore;
    struct VMPageRecord *vmpr = owner_and_vmpr( phh, &ignore );

    RS_LVerbose( 463, 3801, "Allocated {%08lx} (GRAY): %lu bytes for a %s", 
                 VAL( PHH_TO_PTR( phh ) ),
                 the_size,
                 symbol_text( class_name( the_class ) ) );
    assert( owner == ignore );
    rstore_page_has_gray( owner, vmpr, 0 );
  } else {
    RS_LVerbose( 463, 3800, "Allocated {%08lx} (WHITE): %lu bytes for a %s", 
                 VAL( PHH_TO_PTR( phh ) ),
                 the_size,
                 symbol_text( class_name( the_class ) ) );
  }
}


PAllocArea *make_alloc_area( RStore *owner, obj parent )
{
  struct VMPageRecord *vmpr;
  struct PHeapHdr *phh;
  struct FirstPageHdr *fph;
  PAllocArea *aa;

  vmpr = alloc_ppages( owner, 1 );

  /* the <allocation-area> is allocated in itself */

  fph = (struct FirstPageHdr *)vmpr->mem_address;

  phh = (struct PHeapHdr *)(fph + 1);

  phh->mem_size = round_up( sizeof(PAllocArea) + sizeof(struct PHeapHdr) );
  phh->pstore_flags = PFLAGS_ALLOC_AREA;
  phh->size_class = &owner->the_size_class;
  palloc_inithdr( owner, phh, allocation_area_class, sizeof(PAllocArea) );

  aa = PTR_TO_DATAPTR( PHH_TO_PTR(phh) );

  aa->entry = FALSE_OBJ;
  aa->reserved = FALSE_OBJ;
  aa->allocfn = parea_alloc;

  aa->current_LR.base_page_num = vmpr->ref.base_page_num;
  aa->current_LR.offset = sizeof(struct FirstPageHdr) + phh->mem_size;
  aa->current_LR.nth_page = 1;
  aa->current_LR.first = 1;
  aa->current_LR.indirect = 0;
  aa->current = aa->current_LR.offset + (char *)vmpr->mem_address;

  /* install the 00000000 end-of-page marker */

  ((struct PHeapHdr *)aa->current)->mem_size = 0;

  aa->free_list_vec = FALSE_OBJ;
  aa->reserved2 = 0;
  aa->owner = owner;
  
  aa->accum_bytes = sizeof( PAllocArea );
  aa->accum_objects = 1;
  aa->accum_pages = 1;

  /* go back and fill in the FirstPageHdr */

  fph->area = aa;
  fph->vmpr = vmpr;
  fph->spare1 = fph->spare2 = 0;

  /* install our parent pointer */

  if (OBJ_ISA_PTR(parent))
    aa->parent_LR = create_LR_first( owner, parent );
  else
    aa->parent_LR = create_immob_LR( parent );

  return aa;
}

struct VMPageRecord *owner_and_vmpr( void *addr, RStore **s )
{
  struct FirstPageHdr *fph = FIRST_PAGE_HDR_OF(addr);

  *s = fph->area->owner;
  return fph->vmpr;
}

static void initFirstPage( struct VMPageRecord *vmpr, PAllocArea *area )
{
  struct FirstPageHdr *fph = vmpr->mem_address;

  fph->area = area;
  fph->vmpr = vmpr;
  fph->spare2 = fph->spare1 = 0;
}

static obj finish_alloc_area( obj the_class, struct PHeapHdr *item )
{
  obj mo = PHH_TO_PTR( item );

#ifndef NDEBUG
  /*  If this is not a GVEC, fill the object with
   *  a scrambled fill pattern (except the last word,
   *  which will get zero'd out shortly.  This is to support
   *  the funky bvec <string> protocol, to wit, a <string>
   *  is a bvec of length N+1 with ALL the bytes of the
   *  last word filled with 0 (so we can do word-wide string compares)
   */
  if (!CLASS_GVEC_P( the_class )) {
    memset( PTR_TO_DATAPTR(mo), 0xE3, item->rs_header.pob_size );
  }
#endif

  if (item->rs_header.pob_size)
    {
      UINT_32 *lastw;

      lastw = (UINT_32 *)((char *)PTR_TO_DATAPTR(mo)
                          + ((item->rs_header.pob_size-1) 
                             & ~(sizeof(UINT_32)-1)));
      *lastw = 0;
    }

#ifndef NDEBUG

  /* If this is a GVEC, fill the object with a particular bit pattern
   * which will allow the write barriers to make sure they are being
   * used right (i.e., distinguishing the `initializing write' case
   * from other writes)
   */
  if (CLASS_GVEC_P( the_class )) {
    obj *p = (obj *)((char *)PTR_TO_DATAPTR( mo ));
    unsigned i;

    assert( (item->rs_header.pob_size % SLOT(1)) == 0 );

    for (i=0; i<item->rs_header.pob_size; i+=SLOT(1)) {
      *p++ = DEBUG_TRAP_OBJ;
    }
  }
#endif

  return mo;
}

static obj reusing_alloc( PAllocArea *area,
                          UINT_32 space_req,
                          obj the_class,
                          UINT_32 the_size )
{
  struct LocationRef lr;
  struct PHeapHdr *x, *limit;
  UINT_32 *vec, on_page;
  UINT_8 need_sizeclass = psizeclass[ space_req ];
  RStore *sto;
  struct VMPageRecord *vmpr;

  vec = ((UINT_32 *)PTR_TO_DATAPTR( area->free_list_vec ));
  on_page = vec[ need_sizeclass ];
  if (on_page == 0) {
#ifdef GC_TRACE
      printf( "    no storage to reuse for <SC %d>\n", need_sizeclass );
#endif
      return ZERO;
  }

#ifdef GC_TRACE
  printf( "<*> reusing storage from <SC %d> on page 0x%08lx",
          need_sizeclass,
          on_page );
#endif

  lr.indirect = 0;
  lr.nth_page = 1;
  lr.first = 1;
  lr.base_page_num = on_page;
  lr.offset = sizeof(struct FirstPageHdr);

  x = (struct PHeapHdr *)VAL(translate_LR( area->owner, lr ));
  limit = (struct PHeapHdr *)((char *)x + MM_PAGE_SIZE - sizeof(struct FirstPageHdr));

  vmpr = owner_and_vmpr( x, &sto );
  assert( sto == area->owner );

  /*
   * XXX<TODO>  Make this more efficient!
   *
   *  In this implementation, for every single object we alloc,
   *  we do a linear scan of a single page looking for free objects
   *  of the right size.  I think we could make a cache in another
   *  reserved field of PAllocArea...
   */

  while ((x < limit) && x->mem_size)
    {
      if (x->pstore_flags == PFLAGS_FREE_OBJ)
        {
          struct PFreeBlock *fb = (struct PFreeBlock *)(x+1);

          if (fb->in_sizeclass == need_sizeclass)
            {
#ifdef GC_TRACE
              printf( " @ 0x%08x", x );
#endif
              if (fb->last_on_this_page)
                {
#ifdef GC_TRACE
                  printf( " (LIP; next page = %08x)", fb->next_page );
#endif
                  /* we are using the last one on the page */
                  vec[need_sizeclass] = fb->next_page;
                }
#ifdef GC_TRACE
              printf( "\n" );
#endif

              /*
               *  Reformulate for the new purpose of this storage space
               */
              x->pstore_flags = PFLAGS_NORMAL_OBJ;
              palloc_inithdr( area->owner, x, the_class, the_size );
              return finish_alloc_area( the_class, x );
            }
        }
      /* 
       *  on to the next one
       */
      x = (struct PHeapHdr *)((char *)x + x->mem_size);
    }
  /* reached the end and didn't find it... bail */
  /*
   *  which means, in particular, that we have no idea
   *  where the next page might be!
   */

  scheme_error( "broken freelist starting at page ~d", 1,
                int2fx( on_page ) );
  vec[need_sizeclass] = 0;
  return ZERO;
}

obj parea_alloc_internal( AllocArea *base_area, 
                          obj the_class, 
                          UINT_32 the_size )
{
  PAllocArea  *area = (PAllocArea *)base_area;
  UINT_32 space_req, lo_uses;
  struct PHeapHdr *item;
  RStore *store;

#ifdef GC_TRACE
  printf( "[*] parea_alloc( %lu bytes, class {%08lx} )\n", 
          the_size, VAL(the_class) );
#endif

  store = area->owner;

  assert( store ); /* better not get called for a base (transient)
		      allocation! */

  /* figure out how much space we need */

  space_req = round_up( the_size ) + sizeof(struct PHeapHdr);

  if (space_req >= MM_PAGE_SIZE - sizeof(struct FirstPageHdr))
    {
      struct VMPageRecord *vmp;
      struct PHeapHdr *item;

      unsigned n = (space_req + 
                    sizeof(struct FirstPageHdr) 
                    + (MM_PAGE_SIZE - 1)) >> MM_PAGE_BITS;

      /* it's a LARGE OBJECT */
      
      vmp = alloc_ppages( store, n );
      initFirstPage( vmp, area );

      item = (struct PHeapHdr *)((char *)vmp->mem_address 
				         + sizeof(struct FirstPageHdr));

      item->mem_size = (n << MM_PAGE_BITS) - sizeof( struct FirstPageHdr );
      item->pstore_flags = PFLAGS_LARGE_OBJ;
      item->size_class = &store->the_size_class;

      palloc_inithdr( store, item, the_class, the_size );

      /* update the area stats */

      area->accum_pages += n;
      area->accum_bytes += the_size;
      area->accum_objects++;

#ifdef GC_TRACE
      printf( "<*> large object on page %08lx (%u pages) @ %p\n",
              vmp->ref.base_page_num,
              n,
              item );
#endif
      
      return finish_alloc_area( the_class, item );
    }

  /* see if there is interior free space we can reuse */

  if (!EQ( area->free_list_vec, FALSE_OBJ ))
    {
      obj x;

      x = reusing_alloc( area, space_req, the_class, the_size );
      if (!EQ( x, ZERO ))
        {
          return x;
        }
    }

  /*
   *  Round up the space_req to an even size class, for more
   *  efficient use later on
   */
  space_req = psizeclass_l[ psizeclass[ space_req ] ];

  /* see if we can fit it on the current page */

  if (area->current_LR.offset + space_req > MM_PAGE_SIZE)
    {
      /* doesn't fit, create a new page */
      struct VMPageRecord *vmp;

      vmp = alloc_ppages( store, 1 );
      initFirstPage( vmp, area );

      area->current_LR.base_page_num = vmp->ref.base_page_num;
      area->accum_pages++;
      area->current_LR.offset = sizeof(struct FirstPageHdr);
      item = area->current = (char *)vmp->mem_address + area->current_LR.offset;
#ifndef NDEBUG
      ((struct PHeapHdr *)area->current)->mem_size = 0;
#endif
#ifdef GC_TRACE
      printf( "<*> created new page 0x%08lx @ %p\n",
              vmp->ref.base_page_num,
              item );
#endif
    }
  else if (area->current)
    {
      item = area->current;
#ifdef GC_TRACE
      printf( "<*> using area->current @ %p\n", item );
#endif
    }
  else
    {
      /* the end page hasn't been used before... resolve it */
      struct VMPageRecord *vmp;
      struct PageRef pr;

      assert( area->current_LR.first && area->current_LR.nth_page == 1 );

      pr.base_page_num = area->current_LR.base_page_num;
      pr.first = 1;
      pr.indirect = 0;
      pr.dirty = 0;
      pr.loaded = 0;
      pr.nth_page = 1;

      vmp = get_vmpr( store, &pr );
      assert( vmp );
      item = area->current = (char *)vmp->mem_address 
	+ area->current_LR.offset;
#ifdef GC_TRACE
      printf( "<*> extending end page 0x%08lx @ %p\n", 
              pr.base_page_num,
              item );
#endif
    }

  assert( ((struct PHeapHdr *)area->current)->mem_size == 0 );

  area->current_LR.offset += space_req;
  area->current = (char *)item + space_req;
  
  /* mark the new end of the page */
  
  if (area->current_LR.offset < MM_PAGE_SIZE)
    {
      ((struct PHeapHdr *)area->current)->mem_size = 0;
    }

  item->mem_size = space_req;
  item->pstore_flags = PFLAGS_NORMAL_OBJ;
  item->size_class = &store->the_size_class;

  palloc_inithdr( store, item, the_class, the_size );

  /* update the area stats */

  area->accum_bytes += the_size;
  area->accum_objects++;
  return finish_alloc_area( the_class, item );
}


obj parea_alloc( AllocArea *base_area, obj the_class, UINT_32 the_size )
{
  obj a;

  a = parea_alloc_internal( base_area, the_class, the_size );

  /* see if we just allocated a live object... */

#if 0 /* way too expensive... */
  obj refs;

  refs = all_pointers_to( a );
  assert( SIZEOF_PTR(refs) == 0 );
#endif

  return a;
}

int parea_dealloc_lr( RStore *ps, 
                      unsigned page, 
                      unsigned flags, 
                      unsigned offset )
{
  struct LocationRef lr;

  /* XXX If this is a large object we're deleting, is
   *     there any reason to map it into memory (i.e., reserve
   *     storage?)
   */

  lr.first = (flags & 1) ? 1 : 0;
  lr.indirect = (flags & 2) ? 1 : 0;
  lr.nth_page = flags >> 2;
  lr.offset = offset;
  lr.base_page_num = page;
  
  assert( lr.first );  /* nothing else is permitted! */

  return parea_dealloc( translate_LR( ps, lr ) );
}

static int parea_dealloc_large( struct FirstPageHdr *fph )
{
  RStore *s = fph->area->owner;

  lss_delete_recs( s->lss, 
                   fph->vmpr->ref.base_page_num,
                   fph->vmpr->ref.nth_page );
  dealloc_ppages( s, fph->vmpr, fph->vmpr->ref.nth_page );
  /* by the time dealloc_ppages() returns, `fph->vmpr' has been
     freed, and, in fact, `fph' points to unmapped memory! */
  return 0;
}

static int remove_page_from_free_list_or_next( struct PAllocArea *area,
                                               unsigned sizeclass,
                                               UINT_32 *on_page,
                                               UINT_32 page_to_remove,
                                               UINT_32 next_page )
{
  struct LocationRef lr;
  struct PHeapHdr *x, *limit;

  lr.indirect = 0;
  lr.nth_page = 1;
  lr.first = 1;
  lr.base_page_num = *on_page;
  lr.offset = sizeof(struct FirstPageHdr);

  x = (struct PHeapHdr *)VAL(translate_LR( area->owner, lr ));
  limit = (struct PHeapHdr *)((char *)x + MM_PAGE_SIZE - sizeof(struct FirstPageHdr));
  
  /*
   *  We only need to update the last on the page...
   */
  while ((x < limit) && x->mem_size)
    {
      if (x->pstore_flags == PFLAGS_FREE_OBJ)
        {
          struct PFreeBlock *fb = (struct PFreeBlock *)(x+1);

          if (fb->in_sizeclass == sizeclass)
            {
              if (fb->last_on_this_page)
                {
                  if (fb->next_page == page_to_remove) {
                    fb->next_page = next_page;
                    return 1;
                  } else {
                    *on_page = fb->next_page;
                    return 0;
                  }
                }
            }
        }
      /* 
       *  on to the next one
       */
      x = (struct PHeapHdr *)((char *)x + x->mem_size);
    }
  /*
   *  whoa! We didn't find a last_on_page one here!
   */
  scheme_error( "broken freelist; no last_on_page for page: 0x~08x sizeclass: ~d",
                2,
                int2fx( *on_page ),
                int2fx( sizeclass ) );
  return -1;
}

static void remove_page_from_free_list( struct PAllocArea *area,
                                        unsigned sizeclass,
                                        UINT_32 page_to_remove,
                                        UINT_32 next_page )
{
  UINT_32 *vec = ((UINT_32 *)PTR_TO_DATAPTR( area->free_list_vec ));
  UINT_32 p;

#ifdef GC_TRACE
  printf( "remove page <%08x> from sc %d (next is <%08x>)\n", 
          page_to_remove,
          sizeclass,
          next_page );
#endif

  /*
   *  First, the easy case -- the head of the free list is
   *  exactly the page we want to remove
   */
  if (vec[sizeclass] == page_to_remove) {
    vec[sizeclass] = next_page;
#ifdef GC_TRACE
    printf( "  updated vec[]\n" );
#endif
    return;
  }

  p = vec[sizeclass];
  while (p) {
    int rc;

#ifdef GC_TRACE
    printf( "  checking page <%08x> for prev\n", p );
#endif
    rc = remove_page_from_free_list_or_next( area, sizeclass, 
                                             &p, 
                                             page_to_remove,
                                             next_page );
    if (rc) {
#ifdef GC_TRACE
      printf( "  got it\n" );
#endif
      return;
    }
  }
  scheme_error( "broken freelist; page 0x~08x not on freelist for sizeclass ~d",
                2,
                int2fx( page_to_remove ),
                int2fx( sizeclass ) );
}



/*
 *  This is called when the last (small) object on a page
 *  is being deleted.  The plan is to go through and remove
 *  all of the (other) dead objects from their respective free lists,
 *  and dealloc the persistent page
 */

static int freed_last_object_on_page( struct FirstPageHdr *fph )
{
  UINT_32 nextpg[P_NUM_SIZE_CLASS];
  char hasfree[P_NUM_SIZE_CLASS];

  RStore *s;
  struct PHeapHdr *x, *limit;
  unsigned i;
  UINT_32 this_page = fph->vmpr->ref.base_page_num;

#ifdef GC_TRACE
  printf( "freed_last_object_on_page( 0x%08x )\n", this_page );
#endif

  memset( &hasfree[0], 0, sizeof( hasfree ) );

  x = (struct PHeapHdr *)(fph+1);
  limit = (struct PHeapHdr *)((char *)x 
                              + MM_PAGE_SIZE 
                              - sizeof(struct FirstPageHdr));

  /* 
   *  a `mem_size' of 0 indicates the end of the page, so keep
   *  looking at objects until we get to the end of the page
   *  or we find something with mem_size==0
   */

  while ((x < limit) && x->mem_size) {
    if (x->pstore_flags == PFLAGS_FREE_OBJ) {
      struct PFreeBlock *fb = (struct PFreeBlock *)(x+1);

#ifdef GC_TRACE
      printf( "  x %08x free in sizeclass %d (last=%d)\n",
              x, fb->in_sizeclass, fb->last_on_this_page );
#endif

      if (fb->last_on_this_page) {
        nextpg[ fb->in_sizeclass ] = fb->next_page;
        hasfree[ fb->in_sizeclass ] = 1;
      }
#ifdef GC_TRACE
    } else {
      printf( "  x %08x not free\n", x );
#endif
    }
    x = (struct PHeapHdr *)((char *)x + x->mem_size);
  }

  /*
   *  Now we have, as a function of size class, the pointers to the
   *  next page that contains a free object in that size class.
   */

  /*
   *  This is the expensive part.  For *EACH* size class in which
   *  this page has a free object, we have to scan up to the *ENTIRE*
   *  (singly-linked!) list of free slots to find the entry that
   *  points to this page, and then update it to bypass this page.
   */
  for (i=0; i<P_NUM_SIZE_CLASS; i++) {
    if (hasfree[i]) {
#ifdef GC_TRACE
      printf( "sizeclass[%d] next page => 0x%08x\n", i, nextpg[i] );
#endif
      remove_page_from_free_list( fph->area, i, this_page, nextpg[i] );
    }
  }

  /*
   *  Okay, having removed ourself from the various free lists,
   *  we also need to make sure this page isn't the current
   *  allocation point, or if it is, to update it to force a
   *  new page next time something is allocated in this AA.
   */
  if (fph->area->current_LR.base_page_num == this_page) {
    fph->area->current_LR.base_page_num = 0;
    fph->area->current_LR.offset = MM_PAGE_SIZE-1;
    fph->area->current = NULL;
  }

  /*
   *  Now, delete this page from memory and from the LSS
   */
  assert( fph->vmpr->ref.nth_page == 1 );

  s = fph->area->owner;
  lss_delete_recs( s->lss,
                   fph->vmpr->ref.base_page_num,
                   1 );
  dealloc_ppages( s, fph->vmpr, 1 );
  /*
   *  At this point, `fph->vmpr' has been free()'d
   *  and `fph' points to unmapped memory
   */
  return 0;     /* 0: a job well done */
}


static int parea_dealloc_small( struct FirstPageHdr *fph, obj item,
                                int free_if_last )
{
  PAllocArea *a = fph->area;
  struct PHeapHdr *x, *limit, *last_is;
  UINT_32 *vec;
  UINT_8 sc;
  struct PFreeBlock *fb;
  int any_alloced;

#if 0 /* way too expensive... */
  obj refs;

  refs = all_pointers_to( item );
  assert( SIZEOF_PTR(refs) == 0 );
#endif

  if (EQ( a->free_list_vec, FALSE_OBJ ))
    {
      UINT_32 nb = sizeof(UINT_32) * P_NUM_SIZE_CLASS;
      a->free_list_vec = parea_alloc( (AllocArea *)a, byte_vector_class, nb );
      memset( PTR_TO_DATAPTR( a->free_list_vec ), 0, nb );
    }
  vec = ((UINT_32 *)PTR_TO_DATAPTR( a->free_list_vec ));

  /*
   *  Find the size class (`sc') that this object belongs to
   */
  x = PTR_TO_PHH( item );
  sc = psizeclass_a[ x->mem_size ];
  
  /*
   *  Scan this page for other free blocks that belong to
   *  this size class; in particular, we are looking for
   *  the last one on the page.
   */
  x = (struct PHeapHdr *)(fph+1);
  limit = (struct PHeapHdr *)((char *)x 
                              + MM_PAGE_SIZE 
                              - sizeof(struct FirstPageHdr));
  last_is = NULL;

  /* 
   *  a `mem_size' of 0 indicates the end of the page, so keep
   *  looking at objects until we get to the end of the page
   *  or we find something with mem_size==0
   */
  any_alloced = 0;

  while ((x < limit) && x->mem_size)
    {
      if (x->pstore_flags == PFLAGS_FREE_OBJ)
        {
          fb = (struct PFreeBlock *)(x+1);

          if (fb->in_sizeclass == sc)
            {
              last_is = x;
            }
        }
      else if (x != PTR_TO_PHH(item))
        {
#ifdef GC_TRACE
          if (!any_alloced) {
            printf( "free(%08x): still allocated: page %08x(=%08x)+%04x\n",
                    VAL(item),
                    fph->vmpr->ref.base_page_num,
                    fph,
                    (char *)x - (char *)fph );
          }
#endif
          any_alloced = 1;
        }
      x = (struct PHeapHdr *)((char *)x + x->mem_size);
    }

  /*
   *  If there are no allocated objects on this page
   *  (besides the one we're deleting), then free up the
   *  whole page.  However, this behavior is optional
   *  because it can be so expensive (see freed_last_object_on_page(), 
   *  above, for an explanation of why it can be so expensive)
   */
  if (!any_alloced && free_if_last) {
#ifdef GC_TRACE
    printf( "free(%08x): nothing left on page page %08x\n", 
            VAL(item),
            fph->vmpr->ref.base_page_num );
#endif
    return freed_last_object_on_page( fph );
  }

  /*
   *  Populate this memory location with a PFreeBlock
   */

  x = PTR_TO_PHH( item );

  /*
   *   Large objects (pstore_flags == PFLAGS_LARGE_OBJ) should 
   *   be handled by parea_dealloc_large()
   */
  assert( x->pstore_flags == PFLAGS_NORMAL_OBJ );

  x->pstore_flags = PFLAGS_FREE_OBJ;

  fb = (struct PFreeBlock *)(x+1);
  /*
   *  Fill the entire block (including the POBHeader) with 0xE5's,
   *  so if anybody comes and tries to read something useful from
   *  here, they will fail miserably and we'll detect it.
   */
  memset( (char *)(x+1) - sizeof(POBHeader),  /* XXX debug */
          0xE5, 
          x->mem_size - sizeof( struct PHeapHdr ) + sizeof(POBHeader) );

  fb->in_sizeclass = sc;

  if (last_is)
    {
      /*
       *  There was already a free object on this page that
       *  is in this size class.  Hence, this page must already
       *  be in list of pages in free_list_vec[sc]
       */
      if (last_is > x)
        {
          fb->last_on_this_page = 0;
        }
      else
        {
          struct PFreeBlock *prev_fb = ((struct PFreeBlock *)(last_is+1));
          /*
           *   we have a new `last-on-page'
           */
          prev_fb->last_on_this_page = 0;
          fb->last_on_this_page = 1;
          fb->next_page = prev_fb->next_page;
        }
    }
  else
    {
      /*
       *  This is the first free object in this size class on
       *  this page.  Hence, we need to add us to the free_list_vec[sc]
       */
      fb->last_on_this_page = 1;
      fb->next_page = vec[sc];
      vec[sc] = fph->vmpr->ref.base_page_num;
#ifdef GC_TRACE
      printf( "<*> pushed page 0x%08lx onto free(page)list for <SC %d>\n",
              vec[sc], sc );
#endif
    }
  return 0;     /* job done -- there were no pointers to it */
}

int parea_dealloc( obj item )
{
  struct FirstPageHdr *fph;
  
  assert( OBJ_ISA_PTR( item ) );

#ifdef GC_TRACE
  printf( "--- parea_dealloc( %08lx )\n", VAL(item) );
#endif
  RS_LVerbose( 463, 3899, "Deallocating {%08lx} : %lu bytes of %s", 
               VAL(item),
               SIZEOF_PTR( item ),
               symbol_text( class_name( CLASSOF_PTR( item ) ) ) );

  fph = FIRST_PAGE_HDR_OF( PTR_TO_HDRPTR( item ));

  assert( (((UINT_32)fph) & (MM_PAGE_SIZE-1)) == 0 );
  assert( fph->vmpr->ref.first );

  if (fph->vmpr->ref.nth_page > 1) {
    return parea_dealloc_large( fph );
    /*  by the time `parea_dealloc_large' returns, `item'
     *  refers to unmapped memory, so BE CAREFUL!
     */
  } else {
    return parea_dealloc_small( fph, item, 1 );
    /*  by the time `parea_dealloc_small' returns, `item'
     *  may refer to unmapped memory
     */
  }
}



void rstore_init_psizeclass( void )
{
  int i, j;

  /*
   *  Build the requested size class table
   */
  psizeclass[ MM_PAGE_SIZE ] = 0xE5;
  j = 0;
  for (i=0; i<sizeof(psizeclass_l)/sizeof(int); i++)
    {
      memset( &psizeclass[j], i, (psizeclass_l[i] + 1) - j );
      j = psizeclass_l[i] + 1;
    }
  memset( &psizeclass[j], P_NUM_SIZE_CLASS, MM_PAGE_SIZE - j );
  assert( psizeclass[ MM_PAGE_SIZE ] == 0xE5 );

  /*
   *  Build the satisfies (actual) size class table
   */
  psizeclass_a[ MM_PAGE_SIZE ] = 0xE5;
  j = 0;
  for (i=0; i<(sizeof(psizeclass_l)/sizeof(int)-1); i++)
    {
      memset( &psizeclass_a[j], i, psizeclass_l[i+1] - j );
      j = psizeclass_l[i + 1];
    }
  memset( &psizeclass_a[j], i, MM_PAGE_SIZE - j );
  assert( psizeclass_a[ MM_PAGE_SIZE ] == 0xE5 );
}


