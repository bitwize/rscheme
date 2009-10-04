#include "rstoret.h"
#include "swizzler.h"
#include "scan.h"
#include "pagemgr.h"

/*  Pass I of writing is independent of the rich-model
 *  being employed.
 *
 *  However, it communicates with the output method for
 *  the rich model by looking up the appropriate 
 *  (struct VMPageRecord *) for each heap-object reference
 *  that is located.
 *  
 */

struct Scanning {
  struct RStore      *store;
#ifdef FAILURE_VECTORS
  obj                *failures, *f_limit;
  jmpbuf              f_overflow;   /* exceeded bound on failures */
#else /* now for reality... */
  obj                 failures;
#endif

  /*  note that we could overflow the scans buffer when scanning
   *  an object directly -- therefore, since that method has
   *  no need for scannedptr's, we don't fill them in at all
   */
  struct ScannedPtr  *pscan;
  struct ScannedPtr  *scan_limit;
  obj                 source;
};

/* 
 * save the translated pointer (or reference) in the page's context
 * so we don't have to go through all this work again
 * if we decide to write the page out after all
 */

static void an_xlated_ptr( struct Scanning *ctx, struct VMPageRecord *vmp )
{
  /* (skip the saving if we're not going to write,
   * ie, if we're just noticing an _object_'s refs
   * as opposed to a _page_'s
   */
  if (ctx->pscan)
    {
      assert( ctx->pscan < ctx->scan_limit );
      ctx->pscan->ref.in_page = vmp;
      ctx->pscan->local_id    = -1;
      ctx->pscan->indirect_q  = 0;
      ctx->pscan++;
    }
}

static void an_xlated_indir( struct Scanning *ctx, obj refnum )
{
  if (ctx->pscan)
    {
      assert( ctx->pscan < ctx->scan_limit );
      ctx->pscan->ref.refnum = refnum;
      ctx->pscan->local_id    = -1;
      ctx->pscan->indirect_q  = 1;
      ctx->pscan++;
    }
}

/*-------------------------------------------------------------------*/


/* failing_unit is the object to add to the failure list
 * in case the lookup fails
 */
   
static void table_lookup_or_fail( struct Scanning *ctx,
				  obj lookup_table,
				  obj failing_unit,
				  obj key_value )
{
  obj refnum;

  refnum = objecttable_lookup( lookup_table,
			       obj_hash(key_value),
			       key_value );

  if (EQ(refnum,FALSE_OBJ))
    {
      RS_LVerbose( 463, 3682, "from {%lx} found escaping {%lx}", 
                   VAL(ctx->source) ,
                   VAL(failing_unit) );
      ctx->failures = cons( failing_unit, ctx->failures );
    }
  else
    {
      an_xlated_indir( ctx, refnum );
    }
}

/* find the object referenced by an `obj'.  (trivial if the
   obj is not a PTR).

   records the result in the scanning table of the context,
   to avoid the lookup on the second pass.

   if the object references a stale copy of a relocated
   object (ie, an object which has been seen already and
   copied into the store), then this procedure updates
   the pointer to point to the new (in-store) object.
*/

static void notice_obj( struct Scanning *ctx, obj *pitem )
{
  struct VMPageRecord *vmp;
  obj item;
  
  item = *pitem;

again:
  if (!OBJ_ISA_PTR(item))
    return;
  
  /* check for an within-the-store ptr */

  /*  might want to quickly check a small cache to
   *  speed up resolution, instead of probing the Big Cache
   */

  vmp = addr_to_vm_page_record( ctx->store, PTR_TO_PHH(item) );

  if (vmp)
    {
      an_xlated_ptr( ctx, vmp );
      return;
    }
  else
    {
      obj reloc = objecttable_lookup( ctx->store->reloc_table,
				      obj_hash(item),
				      item );
      if (EQ(reloc,FALSE_OBJ))
	{
	  /* it had better be a pivot! */
	  
	  table_lookup_or_fail( ctx,
				ctx->store->pivot_table,
				item, 
				item );
	  return;
	}
      else
	{
	  /*  it was found in the relocation table
	   *  -- store the new pointer into the object
	   *  making the reference, and try again
	   */
	  item = *pitem = reloc;
	  /*  note that we loop back to BEFORE the PTR check;
	   *  -- this allows a relocation to a non-ptr
	   */
	  goto again;
	}
    }
  /* failed! */
  RS_LVerbose( 463, 3681, "from {%lx} found escaping {%lx}", 
               VAL(ctx->source) ,
               VAL(item) );
  ctx->failures = cons( item, ctx->failures );
  return;
}


/*  specialized functions for the first two slots in a template
 *  analagous to notice_page_ref, but treat object references
 *  specially, as via the local_code_ptrs and local_fn_descrs
 *  tables
 */

static void notice_code_ptr( struct Scanning *ctx, obj inside, obj *pitem )
{
  table_lookup_or_fail( ctx, 
		        ctx->store->local_code_ptrs,
		        inside,
		        *pitem );
}

static void notice_fn_descr_ptr( struct Scanning *ctx, obj inside, obj *pitem )
{
  table_lookup_or_fail( ctx, 
		        ctx->store->local_fn_descrs,
		        inside,
		        *pitem );
}

static void scan_mem( struct Scanning *ctx, 
		      struct PHeapHdr *hdr,
		      void *src, UINT_32 from_start, 
		      UINT_32 len )
{
  UINT_32 i = 0;
  obj *gvecp = src;
  enum SwizzleMode m = mode_for_object(hdr);

  switch (m)
    {
    case SWIZ_MODE_TEMPLATE:

      if (from_start == 0)
	{
	  assert( len >= SLOT(2) );
	  notice_code_ptr( ctx, PHH_TO_PTR(hdr), gvecp++ );
	  notice_fn_descr_ptr( ctx, PHH_TO_PTR(hdr), gvecp++ );
	  i = SLOT(2);
	}
      /* fall through into gvec. */

    case SWIZ_MODE_GVEC:
      for (; i<len; i+=SLOT(1), gvecp++)
	{
	  notice_obj( ctx, gvecp );
	}
      break;

    case SWIZ_MODE_ALLOC_AREA:
      { 
	PAllocArea *aa = src;
	assert( from_start == 0 );
	
	notice_obj( ctx, &aa->entry );
	notice_obj( ctx, &aa->reserved );
        if (!EQ( aa->free_list_vec, FALSE_OBJ ))
          notice_obj( ctx, &aa->free_list_vec );
      }
      break;

      
    case SWIZ_MODE_PADDR_VEC:
      /* just verify that the owner is this pstore, and
       * skip the first two words 
       */
      if (from_start == 0)
	{
	  if (((struct PAddrVec *)src)->owner != ctx->store)
	    {
	      scheme_error( "<persistent-addr> in different store: ~s",
			    1, PHH_TO_PTR(hdr) );
	    }
	  i = SLOT(2);
	  src = (char *)src + SLOT(2);
	}

    case SWIZ_MODE_FLOAT:
    case SWIZ_MODE_UINT32:
    case SWIZ_MODE_BVEC:
      /* nothing to do in `scan' procedure */
      break;

    default:
      {
	struct swiz_mode_handler *h = get_swiz_mode_handler( ctx->store, m );
	/* pass I -- notice pointers */
	h->trav_notice( h, hdr, src, from_start, len, 
			ctx, notice_obj );
	break;
      }
    }
}

obj rstore_scan_pob( struct RStore *store, obj ptr, obj other_failures,
		     obj with_reloc )
{
  struct Scanning ctx;
  struct PHeapHdr *p;
  UINT_32 N;

  store->reloc_table = with_reloc;

  assert( OBJ_ISA_PTR(ptr) );

  ctx.failures = other_failures;
  ctx.store = store;
  ctx.source = ptr;

  /* set the save pointer to NULL to tell us not to bother
   * remembering the translated pointers.  If we didn't
   * do this, we would have to make a bigger buffer, because
   * ctx.xlated_ptrs is only big enough for a page's worth of
   * pointers, and an object may be quite a bit larger and full
   * of pointers
   */
  ctx.pscan = NULL;

  p = PTR_TO_PHH( ptr );

  notice_obj( &ctx, &p->rs_header.pob_class );
  N = p->rs_header.pob_size;
  
  scan_mem( &ctx, p, p+1, 0, N );

  store->reloc_table = FALSE_OBJ;
  return ctx.failures;
}

int rstore_foreach_object( struct VMPageRecord *page,
                           int (*proc)( void *info, 
                                        struct PHeapHdr *ptr ),
                           void *info )
{
  struct PHeapHdr *ptr;
  assert( page->ref.first );    /* no object headers on non-first pages */
  
  ptr = first_on_first( page );

  if (page->ref.nth_page > 1) {
    /* multi-page object: only one object on this (the first) page */
    return proc( info, ptr );
  } else {
    int rc = 0;
    struct PHeapHdr *limit;

    /* possibly more than one object on this page */

    limit = (struct PHeapHdr *)((char *)page->mem_address + MM_PAGE_SIZE);
    while (ptr < limit) {
      UINT_32 N, i;

      /* check for an early end of the page */
      if (ptr->mem_size == 0) {
        break;
      }

      if (ptr->pstore_flags != PFLAGS_FREE_OBJ) {
        rc = proc( info, ptr );
        if (rc) {
          return rc;    /* early exit if proc returns non-zero */
        }
      } 
      /* go on to the next object */
      ptr = (struct PHeapHdr *)((char *)ptr + ptr->mem_size);
    }
    return 0;
  }
}

static void scan_page( struct Scanning *ctx,
		       struct VMPageRecord *page )
{
  struct PHeapHdr *p;
  unsigned i;
  
  if (page->ref.first)
    {
      struct PHeapHdr *limit;
      struct FirstPageHdr *fph = (struct FirstPageHdr *)page->mem_address; 
      obj tmp;

      /*
       *  write out the page's allocation-area pointer
       */

      tmp = DATAPTR_TO_PTR(fph->area);

      ctx->source = tmp;
      notice_obj( ctx, &tmp );

      /*
       *  traverse / write out the objects on the page
       */

      p = first_on_first( page );
      limit = (struct PHeapHdr *)((char *)page->mem_address + MM_PAGE_SIZE);
      while (p < limit)
	{
	  UINT_32 N, i;

	  /* check for an early end of the page */
	  if (p->mem_size == 0)
	    {
	      break;
	    }

	  /* p points to the PHeapHdr of an object on this page */
          if (p->pstore_flags != PFLAGS_FREE_OBJ)
            {
              ctx->source = PHH_TO_PTR( p );
              notice_obj( ctx, &p->rs_header.pob_class );
              
              N = p->rs_header.pob_size;
              
              if (page->ref.nth_page > 1)
                {
                  /* clip the length to go only to the end of the page */
                  N = MM_PAGE_SIZE - sizeof(struct PHeapHdr) 
                    - sizeof(struct FirstPageHdr);
                }
              
              scan_mem( ctx, p, p+1, 0, N );
            }
	  /* go on to the next object */
	  p = (struct PHeapHdr *)((char *)p + p->mem_size);
	}
    }
  else
    {
      UINT_32 i, M, N;
      struct PHeapHdr *p;
      
      p = large_object_hdr( page ); 
      ctx->source = PHH_TO_PTR( p );
      
      /* figure out how many bytes to decode */

      M = page->ref.nth_page * MM_PAGE_SIZE;
      N = p->rs_header.pob_size 
	  + sizeof(struct PHeapHdr)
	  + sizeof(struct FirstPageHdr);

/*      printf( "save interior page %08x: at %08x\n",
	     page->ref.base_page_num + page->ref.nth_page,
	     page->mem_address );
  */    
      if (N > M)
	{
	  N -= M;
	  
	  /* there is at least SOMETHING to do */
	  
	  if (N >= MM_PAGE_SIZE)
	    {
	      /*  we're completely inside, so do only this page worth */
	      N = MM_PAGE_SIZE;
	    }
	  scan_mem( ctx, p, 
		    page->mem_address, 
		    (char *)page->mem_address - (char *)(p+1),
		    N );
	}
    }
}

/**
 *  scan a page in preparation for writing it out
 */

obj rstore_scan_page( struct RStore *store, 
		      struct VMPageRecord *page,
		      struct ScannedPtr *scans,
		      int *num_scanned )
{
  struct Scanning s;

  s.store = store;
  s.failures = NIL_OBJ;
  s.pscan = scans;
  s.scan_limit = scans + MAX_PAGE_PTRS;
  s.source = ZERO;

  scan_page( &s, page );
  *num_scanned = s.pscan - scans;
  return s.failures;
}

