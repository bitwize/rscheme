#ifndef _H_RSTORE_SCAN
#define _H_RSTORE_SCAN

/*  each scanned pointer gets one of these
 *  the current implementation packs these DENSELY, 
 *  so the output routine *MUST* extract them in the
 *  same order ("canonical" traversal order).  This is not normally
 *  a problem, because "page_storer.ci" provides the actual
 *  traversal machinery.
 *
 *  coming out of Pass I, the ScannedPtr entries have `local_id' 
 *  set to -1
 */

struct ScannedPtr {
  union {
    struct VMPageRecord  *in_page;    /* referenced page in current memory */
    obj                   refnum;     /* indirect reference number */
  } ref;
  short                   local_id;   /* id in the output page */
  short                   indirect_q;
};

/*  construct/extract an indirect reference number from
 *  an indirect page number and an entry number within that page
 */

#define MAKE_INDIR_CODE(page,entry) int2fx( ((page) << 6) + entry )
#define INDIR_CODE_PAGE(code) (VAL(code) >> 8)
#define INDIR_CODE_ENTRY(code) (fx2int(code) & 63)

#define MAX_PAGE_PTRS  (MM_PAGE_SIZE/sizeof(obj))

obj rstore_scan_page( struct RStore *store, 
		      struct VMPageRecord *page,
		      struct ScannedPtr *scans,
		      int *num_scanned );

#endif /* _H_RSTORE_SCAN */
