#include "rstoret.h"
#include "scan.h"


static void unload_page( struct RStore *store, struct VMPageRecord *page )
{
  struct VMPageRecord *l, *p;

#if 0  
  printf( "*** Unloading {%p} page <%08lx[%s%d]> at %p\n",
          page,
          (unsigned long)page->ref.base_page_num,
          (page->ref.first ? "n=" : "#"),
          (int)page->ref.nth_page,
          page->mem_address );
#endif

  assert( page->ref.loaded );

  for (p=NULL,l=store->first_loaded; l; p=l,l=l->next_loaded) {
    if (l == page) {
      if (p) {
        p->next_loaded = page->next_loaded;
      } else {
        store->first_loaded = page->next_loaded;
      }
      page->ref.loaded = 0;
      page->next_loaded = NULL;
      mm_set_prot( page->mem_address, MM_PAGE_SIZE, MM_MODE_NO_ACCESS );
      break;
    }
  }
  assert( l );
}

int rstore_rollback_dirty( struct RStore *store )
{
  /* this is crude... */
  /* XXX [ ] check to make sure this doesn't leave holes
   *         if pages are dirtied during allocation and then rolled back
   *            -> in fact, there appears to be some leakage 
   *               related to the use of `next_page' in RStore
   * XXX [ ] check to make sure this interacts correctly with
   *         the semi-automatic allocation of indirect pages (e.g.,
   *         <symbol> reference pages)
   */

  struct VMPageRecord *page;
  int n = 0;

#if 0
  if (1) {
    unsigned i;
    struct VMPageRecord *l;

    printf( "Loaded pages: (%d dirty)\n", store->num_dirty );

    for (i=0,l=store->first_loaded; l; i++,l=l->next_loaded) {
      printf( "  %u: {%p}  <%08lx[%s%d]> at packages/rstore/writer.cs\n",
              i,
              l,
              (unsigned long)l->ref.base_page_num,
              (l->ref.first ? "n=" : "#"),
              (int)l->ref.nth_page,
              l->mem_address,
              (l->ref.dirty ? " (+DIRTY)" : "") );
    }
  }
#endif

  for (page=store->first_dirty; page;) {
    struct VMPageRecord *next_page = page->next_dirty;
    page->ref.dirty = 0;
    page->next_dirty = NULL;
    unload_page( store, page );
    n++;
    page = next_page;
  }
  assert( n == store->num_dirty );
  store->num_dirty = 0;
  store->first_dirty = NULL;
  store->last_dirty = NULL;
  return n;
}


obj write_dirty_pages( struct RStore *store )
{
  struct VMPageRecord *page;
  
  while ((page = store->first_dirty))
    {
      obj unresolved = write_page( store, page );
      if (!EQ(unresolved,NIL_OBJ))
	{
	  /* note that if there was anything unresolved,
	     we do NOT mark this page as clean, because it
	     wasn't written
	     */
	  return unresolved;
	}
      page->ref.dirty = 0;
      store->num_dirty--;
      if (!(store->first_dirty = page->next_dirty))
	{
	  store->last_dirty = NULL;
	}
      page->next_dirty = NULL;
      mm_set_prot( page->mem_address, MM_PAGE_SIZE, MM_MODE_READ_ONLY );
    }
  return NIL_OBJ;
}
