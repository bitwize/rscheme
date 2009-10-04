#include "rstoret.h"
#include "scan.h"
#include "pagemgr.h"

struct PHeapHdr *large_object_hdr( struct VMPageRecord *page )
{
    return (struct PHeapHdr *)((char *)page->mem_address
			        + sizeof(struct FirstPageHdr)
    				- MM_PAGE_SIZE * page->ref.nth_page);
}

struct PHeapHdr *first_on_first( struct VMPageRecord *page )
{
  assert( page->ref.first );

  return (struct PHeapHdr *)((char *)page->mem_address 
			     + sizeof(struct FirstPageHdr) );
}

void load_page( RStore *store, struct VMPageRecord *vmp )
{
  struct VMPageRecord *fvmp;
  
  assert( !vmp->ref.loaded );
  
  /* make sure that the first page is loaded, unless it's us */
  
  if (vmp->ref.first)
    {
      /* we are the first, and we're not loaded */
      fvmp = NULL;
    }
  else
    {
      struct PageRef fpr;
      
      fpr = vmp->ref;
      fpr.first = 1;
      
      fvmp = get_vmpr( store, &fpr );
      if (!fvmp->ref.loaded)
	load_page( store, fvmp );
    }
  switch (store->using_rich_model)
    {
    case LRU_RICH_MODEL:
      lru_model_load_page( store, vmp, fvmp );
      break;
    case OLD_RICH_MODEL:
      old_model_load_page( store, vmp, fvmp );
      break;
    }
}

obj write_page( RStore *store, struct VMPageRecord *page )
{
  obj fails;
  struct ScannedPtr scans[MAX_PAGE_PTRS];
  int num_scanned;

  /* first, scan the page */
  fails = rstore_scan_page( store, page, scans, &num_scanned );

  if (EQ(fails,NIL_OBJ))
    {
      /*
        printf( "writing page %08x %c %u (at %p)\n", 
              page->ref.base_page_num,
              page->ref.first ? '#' : '+',
              page->ref.nth_page,
              page->mem_address );
      */
      /* OK to write */
      switch (store->using_rich_model)
	{
	case LRU_RICH_MODEL:
	  lru_model_write_page( store, page, scans, num_scanned );
	  break;
	case OLD_RICH_MODEL:
	  old_model_write_page( store, page, scans, num_scanned );
	  break;
	}
    }
  else
    {
      /* can't write -- there are unresolved pointers */
    }
  return fails;
}

/*
 *  Non-rstore page scanning (used by garbage collector and
 *  other meta-level rstore tools)
 */
 
obj meta_scan_page( int type, LSS *lss, unsigned page, int mode, obj arg )
{
  switch (type)
    {
    case LRU_RICH_MODEL:
      return lru_model_scan_page( lss, page, mode, arg );

    case OLD_RICH_MODEL:
      return old_model_scan_page( lss, page, mode, arg );
    }
  scheme_error( "pstore-scan-page: unrecognized model type %d",
                1, int2fx(type) );
  return FALSE_OBJ;
}
