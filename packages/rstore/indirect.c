#include "rscheme/pkgs/rstore/rstore.h"
#include "indirect.h"
#include <rscheme/hashmain.h>
#include <rscheme/hashfn.h>

struct VMPageRecord *build_indirect_page( struct RStore *store, 
					  struct PageRef *pr )
{
  LSSAccess *a;
  UINT_32  hdr[2], size;
  char *data;
  obj itemv;
  zipbuf bufvec[3];

  /* printf( "Faulting in indirect page: %u\n", pr->base_page_num ); */

  a = lss_read_access( store->lss, pr->base_page_num );
  if (!a) {
    scheme_error( "rstore error: missing indirect page ~d", 
                  1, int2fx( pr->base_page_num ) );
    abort();
  }

  size = lss_access_bytes( a );

  assert( size >= 2 * sizeof( UINT_32 ) );

  size -= 2 * sizeof( UINT_32 );

  bufvec[0].ptr = &hdr[0];
  bufvec[0].limit = &hdr[2];

  bufvec[1].ptr = data = malloc( size );
  bufvec[1].limit = (char *)bufvec[1].ptr + size;

  bufvec[2].ptr = NULL;

  lss_readv( store->lss, bufvec, a );
 
  /*   hdr[0] = type_id       */
  /*   hdr[1] = instance_id   */
     
  switch (hdr[0])
    {
    case SYMBOL_INDIRECT_PAGE_CONSTRUCTOR:
      itemv = construct_symbols( store, hdr[1], data, size );
      break;
    default:
      fprintf( stderr, "invalid indirect page type: %lu\n", 
	       (unsigned long)hdr[0] );
      abort();
    }
  free( data );
  lss_read_release( store->lss, a );
  return register_indirect_page( store, pr->base_page_num, itemv );
}

  /* this function was ported from the scheme side, because we
     need to be able to call it when demand-loading pages
   */

  /* itemv is a vector of the indirect page items.
     install the entries in the pivot table for later unswizzling
   */

struct VMPageRecord *register_indirect_page( RStore *store,
					     UINT_32 page_num,
					     obj itemv )
{
  UINT_32 id, i;
  struct VMPageRecord *vmpr = alloc_vmpr();

  id = page_num * 64;
  for (i=0; i<SIZEOF_PTR(itemv); i+=SLOT(1))
    {
      obj item;

      item = gvec_read( itemv, i );
      objecttable_insert( store->pivot_table,
			  obj_hash( item ),
			  item,
			  int2fx(id) );
      id++;
    }

  vmpr->mem_address = PTR_TO_DATAPTR(itemv);
  vmpr->ref.base_page_num = page_num;
  vmpr->ref.first = 1;
  vmpr->ref.indirect = 1;
  vmpr->ref.dirty = 0;
  vmpr->ref.loaded = 1;
  vmpr->ref.nth_page = 1;
  install_new_vmpr( store, vmpr );

  /* we need to tuck the itemv away somewhere so the GC doesn't
     come around and release it... we are keeping a pointer 
     into it (as an array of `obj'!) in the vmpr
  */

  objecttable_insert( gvec_ref( store->owner, SLOT(4) ),
		      rehash_fixnum( int2fx( page_num ) ),  /*  Hash  */
		      int2fx( page_num ),                   /*  Key   */
		      itemv );                              /*  Value */
  return vmpr;
}

void write_indirect_page_data( RStore *store,
			       int rec_num,
			       int type_id,
			       int instance_id,
			       obj data )
{
  UINT_32 header[2];
  zipbuf vec[3];

  header[0] = type_id;
  header[1] = instance_id;

  vec[0].ptr = &header[0];
  vec[0].limit = &header[2];

  vec[1].ptr = PTR_TO_DATAPTR( data );
  vec[1].limit = (char *)vec[1].ptr + SIZEOF_PTR( data );

  if (STRING_P( data ))
    {
      vec[1].limit = ((char *)vec[1].limit) - 1;
    }
  vec[2].ptr = NULL;
  
  lss_writev( store->lss, rec_num, vec, NULL ); /* don't compress */
}

int alloc_indir_pages( RStore *store, int n )
{
  int fp = store->next_indirect_page;
  store->next_indirect_page += n;
  return fp;
}
