#ifndef _H_RSTORE_SWIZZLER
#define _H_RSTORE_SWIZZLER

#include "rstoret.h"

enum SwizzleMode {
  SWIZ_MODE_GVEC = 0,
  SWIZ_MODE_BVEC = 1,
  SWIZ_MODE_PART_DESCR = 3, 
  SWIZ_MODE_TEMPLATE = 4,
  SWIZ_MODE_FLOAT = 6,
  SWIZ_MODE_UINT32 = 7,
  SWIZ_MODE_ALLOC_AREA = 8,
  SWIZ_MODE_PADDR_VEC = 20,
  SWIZ_MODE_APP_0 = 80,
  SWIZ_MODE_APP_1 = 81,
  SWIZ_MODE_APP_2 = 82,
  SWIZ_MODE_APP_3 = 83,
  SWIZ_MODE_APP_4 = 84,
  SWIZ_MODE_APP_5 = 85,
  SWIZ_MODE_APP_6 = 86,
  SWIZ_MODE_APP_7 = 87,
  SWIZ_MODE_APP_8 = 88,
  SWIZ_MODE_APP_9 = 89
};

enum SwizzleMode mode_for_object( struct PHeapHdr *hdr );
struct swiz_mode_handler *get_swiz_mode_handler( RStore *sto,
						 enum SwizzleMode mode );

/* functions for use by application swizzle-mode handlers */

#define QUICK_NREFS (200)

struct ref_tbl {
    unsigned	 	num_refs;
    unsigned	 	cap_refs;
    struct PageRef   	*refs;
    struct PageRef   	temp[QUICK_NREFS];
};

/*-------------------------------------------------------------------*/

struct Scanning; /* forward decl */

struct swiz_mode_handler {

  /* Pass I on the write side */
  void (*trav_notice)( struct swiz_mode_handler *self,
		       struct PHeapHdr *hdr,
		       void *src, UINT_32 from_start, 
		       UINT_32 len,
		       struct Scanning *info,
		       void (*notice_obj)( struct Scanning *info, 
					   obj *ref ) );

  /* Pass II on the write side */
  void (*trav_write)( struct swiz_mode_handler *self,
		      struct PHeapHdr *hdr,
		      void *src, UINT_32 from_start, 
		      UINT_32 len,
		      void *info,
		      void (*unswiz_and_compress)( void *info, obj ref ) );

  /* load side */
  void (*swizzle)( struct swiz_mode_handler *self,
		   RStore *store,
		   struct PHeapHdr *hdr,
		   UINT_32 *dst,
		   UINT_32 from_start,
		   UINT_32 N,
		   void *info,
		   UINT_32 (*get_word)( void *info ),
		   obj (*get_obj)( void *info ) );

  /* misc. */

  void (*copy_in)( obj dst_item, obj src_item );
  void *app_info;
  enum SwizzleMode handles;
};

/*--void unswiz_and_compress( write_ctx_t *ctx, obj thing );
  --void notice_page_ref( write_ctx_t *ctx, obj *pitem );
  --obj decompress_and_swiz( struct Decompressor *s, struct VMPageRecord **prt );
  --*/

extern struct swiz_mode_handler SWM_transient_cell;

#endif /* _H_RSTORE_SWIZZLER */
