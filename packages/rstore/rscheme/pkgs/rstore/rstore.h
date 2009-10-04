#ifndef _H_RSTORE_RSTORE
#define _H_RSTORE_RSTORE

#include <rscheme.h>
#include <rscheme/pkgs/lss/lss.h>

struct VMPageRecord; /* forward decl */

typedef struct RStore RStore;

RStore *rstore_open( obj owner, LSS *lss );
obj rstore_root( RStore *store );
void set_rstore_root( RStore *store, obj item );

void rstore_close( RStore *sto );

obj rstore_commit( RStore *store, obj root, obj with_reloc, obj live_tbl );
void rstore_set_compression( RStore *store, const char *str );

obj rstore_scan_pob( RStore *store, obj ptr, obj other_failures, 
		     obj with_reloc );

/* set up an indirect page, either a new one or one that's being
   faulted in, by doing:
   (1)  adding each element of [itemv] to the store's pivot table,
         mapping to the appropriate PERS ID
   (2) creating a VMPR to represent the mapping of data at
       the indicated persistent address (page_num)
   (3) installing the VMPR in the store's tables
   (4) adding itemv to the scheme-side <persistent-store>
       to keep it around as long as the pstore's around
       (since the VMPR's mem_address is an (obj *) pointer to the
        contents of itemv)
*/

struct VMPageRecord *register_indirect_page( RStore *store,
					     UINT_32 page_num,
					     obj itemv );

void write_indirect_page_data( RStore *store,
			       int rec_num,
			       int type_id,
			       int instance_id,
			       obj data );

obj unswizzle_symbol_itemv( obj symbol_vec );

obj alloc_area_to_obj( AllocArea *area );
AllocArea *rstore_get_default_area( RStore *store );
AllocArea *rstore_alloc_area( obj item );
AllocArea *make_sub_alloc_area( AllocArea *aa );

void rstore_set_default_area( RStore *store, AllocArea *area );

obj rstore_copy_in( AllocArea *into, obj item );
int alloc_indir_pages( RStore *store, int num_pages );
obj rstore_area_owner( AllocArea *area );

int rstore_count_dirty( RStore *store );
int rstore_rollback_dirty( RStore *store );
int rstore_did_rollback( RStore *store );

enum MetaScanMode {
  RSTORE_SCAN_STARTS,           /* identify object starts on "normal" page */
  RSTORE_SCAN_OBJECTS,          /* find ptrs from objects in an N page */
  RSTORE_SCAN_FIRST,            /* find ptrs from objects in first pg of LO */
  RSTORE_SCAN_INTERIOR,         /* find ptrs from objects in middle pg of LO */
  RSTORE_SCAN_PAGETABLE,        /* find referenced pages */
  RSTORE_SCAN_PAGECOUNT         /* find out how many pages are occupied */
};

obj meta_scan_page( int model, LSS *lss, unsigned page, int mode, obj arg );
obj meta_root_info( LSS *lss );

int parea_dealloc( obj item );
int parea_dealloc_lr( RStore *store, 
                      unsigned page, unsigned flags, unsigned offset );

/* a PageRef plus an offset, w/o VM page rec bits, but DOES
   work for indirect and large objects.
   there is also a  hack for "literal" values (immobs)
   where first == 0 && indirect == 1
 */

struct LocationRef {
  UINT_32         base_page_num;
  UINT_16         nth_page;
  unsigned        first : 1;
  unsigned        indirect : 1;
  unsigned        offset : 14;
};

struct PAddrVec {
  RStore               *owner;
  UINT_32               spare;  /* align to 64-bit boundary */
  struct LocationRef    vec[1];
};

int init_paddr( struct PAddrVec *pv, obj item );

obj rstore_get_scheme_object( RStore *owner );
obj paddr_get( struct PAddrVec *pv, int index );

obj rstore_get_live_objects( RStore *owner, obj otbl );

void rstore_set_write_protect( struct RStore *store, int flag );
int rstore_get_write_protect( struct RStore *store );

obj rstore_hash( obj item );    /* hash an object which might be in rstore */

/*
 *  Return NULL if `item' is not in any store
 */
RStore *rstore_of_object( obj item );

void rstore_gc_set_tracking( RStore *owner, int level );
unsigned rstore_gc_flagbits( obj item );
obj rstore_gc_grayify( obj item );
obj rstore_gc_work( RStore *ps, int mode, obj arg );

#endif /* _H_RSTORE_RSTORE */
