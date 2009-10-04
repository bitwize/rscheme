#ifndef _H_RSTORET
#define _H_RSTORET

#include <rscheme/irc.h>
#include <rscheme.h>
#include <rscheme/platform.h>
#include <rscheme/smemory.h>

/*#define GC_TRACE        (1)*/

#define NATIVE_ONLY

#ifdef NATIVE_ONLY
#define LOAD_32(n)   (n)
#define STORE_32(n)  (n)

#define LOAD_OBJ(n)  (n)
#define STORE_OBJ(n) (n)

#define LOAD_16(n)   (n)
#define STORE_16(n)  (n)

#define LOAD_IEEE_64(n) (n)
#define STORE_IEEE_64(n) (n)
#else
#error non-NATIVE_ONLY not supported yet
#endif

/* only compatible with IRC */

#include "rscheme/pkgs/rstore/rstore.h"
#include "htable.h"
#include "mmglue.h"

enum RichModel {
  OLD_RICH_MODEL = 0,
  LRU_RICH_MODEL
};


enum PGC_Phase {        /* see also `char *(phase_str[])' in util.c */
  PGC_IDLE,
  PGC_PREP,
  PGC_PENDING,
  PGC_TSCAN,
  PGC_PSCAN,
  PGC_RECLAIM,
  PGC_PACKING           /* this is PGC_IDLE from our point of view, but logs differently */
};

struct RStore {
    /*
     *  the scheme-level object which runs us
     */
    obj                         owner;

    /*
     *  table mapping local objects to pivot #'s
     *  (which are 24 bit reference page #s and 6-bit indexes)
     */
    obj  			pivot_table;

    /*
     *  tables to map local code pointers and function pointers
     *  to persistent IDs.  Misses in these tables when commiting
     *  generate unresolved page references for the <template>'s
     *  containing the pointers
     */
    obj                         local_code_ptrs;
    obj                         local_fn_descrs;
    
    /*
     *  relocation table -- records objects that have been
     *  copied into the pstore for the purpose of the current
     *  commit operation
     */
    obj                         reloc_table;

    /*
     *  dirty cards table -- records places in the store
     *  (indexed by transient address "cards" of 16 words each)
     *  that have been scribbled on by the application and may
     *  point to objects in the transient store (and hence have
     *  been placed in the extraHeapPointers list, and act as
     *  roots for the transient GC)
     */
    obj                         dirty_cards;

    /*
     *  lists of dirty & loaded (VM) pages
     */
    struct VMPageRecord 	*first_dirty;
    struct VMPageRecord         *last_dirty;
    unsigned                     num_dirty;
    int                          write_protect;
    struct VMPageRecord 	*first_loaded;
    struct VMPageRecord 	*first_reserved;
    /*
     *  This table maps VM page addrs to their VM page records
     */
    struct htable 		vm_page_records;
    /*
     *  This table maps base (pers.) page numbers to their VM page records
     */
    struct htable 		reserved_base_pages;
    LSS				*lss;

    /*
     *  where to allocate new objects & pages
     *
     *  NOTE: the "root" object is the entry point 
     *  of the default_entry
     */
    struct PAllocArea           *default_area;
    UINT_32                     next_page;
    UINT_32                     next_indirect_page;
    /*
     *  Which rich-model we're using
     *  (currently implemented as an enum instead of vtbl
     *  for convenience)
     */
    enum RichModel using_rich_model;
    /*
     *  Are page records self-identifying (i.e., have a leading type word)?
     */
    int id_pages;
    /*
     *  The low-level compression algorithm to use when writing
     *  new data records
     */
    zip_algorithm  *data_zipper;
    /*
     *  The gc_flag_bits for newly allocated objects
     *  Usually this is 0xE, indicating a WHITE GEN=7 object,
     *  but during the PGC_PENDING and PGC_TSCAN phases, we allocate black,
     *  and so this becomes 0xF { BLACK GEN=7 }
     */
    unsigned                    new_flag_bits;
    /*
     *  Other flags describing the current state of persistent GC
     */
    unsigned                    is_lazy_cleaning : 1;
    unsigned                    is_eager_cleaning : 1;
    unsigned                    is_persistent_traversal : 1;
    unsigned                    is_twb_enabled : 1;     /* G2 */
    unsigned                    is_pwb_enabled : 1;     /* G1 */
    unsigned                    is_alloc_black : 1;
    enum PGC_Phase              pgc_phase;
    unsigned                    num_pages_w_gray;

    /*
     *  The (dummy) generation used for objects that belong to us
     */
    struct IRC_Gen              the_gen;
    /*
     *  The (dummy) size class used for objects that belong to us
     */
    struct IRC_SizeClass        the_size_class;
    /*
     *  The set of application-specific image (swizzle) mode handlers
     */
    struct swiz_mode_handler *(swiz_mode_handlers[11]);
};

void rstore_add_swiz_mode_handler( RStore *sto, struct swiz_mode_handler *h );

struct PageRef {
  UINT_32		base_page_num;
  unsigned		first : 1;
  unsigned		indirect : 1;
  
  /* These bit fields are only used in VM page records */
  unsigned		dirty : 1;
  unsigned		loaded : 1;
  unsigned		has_gray : 1;		/* any gray objects? */
  unsigned              has_livetbl : 1;        /* anything new for livetbl? */

  UINT_16		nth_page;  /* if FIRST, then # pages */
};

struct VMPageRecord {
    struct VMPageRecord *next_dirty;
    struct VMPageRecord *next_loaded;
    struct VMPageRecord *next_reserved;
    void		*mem_address;
    struct PageRef	 ref;
};
 
/* this is a mixvec, 2 objs + N bytes ... */

typedef struct PAllocArea {
  obj                   entry;      /* each AllocArea get's its own entry */
  obj                   reserved;   /* a translated reserved object */
  allocator_fn         *allocfn;    /* allocation implementation */
  RStore               *owner;
  void                 *current;    /* NULL for lazy translation */
  struct LocationRef    current_LR;
  struct LocationRef    parent_LR;
  UINT_32               accum_bytes;
  UINT_32               accum_objects;
  UINT_32               accum_pages;
  obj                   free_list_vec;  /* free lists (FALSE_OBJ if none) */
  UINT_32               reserved2;  /* reserved for future use (0 for now) */
} PAllocArea;

/* there is one of these at the beginning of every first page
   (which is most pages, but only the first of large objects)
*/

struct FirstPageHdr {
  PAllocArea           *area;
  struct VMPageRecord  *vmpr;
  UINT_32 spare1, spare2;
};

#define FIRST_PAGE_HDR_OF(ptr) ((struct FirstPageHdr *)\
				 (~MM_PAGE_MASK & ((UINT_32)(ptr))))

/*  get the vmpr for a PageRef, reserving the page if necessary
    only accessing the base_page_num, first, and nth_page fields
    of the PageRef pr
 */

struct VMPageRecord *get_vmpr( struct RStore *store, struct PageRef *pr );

/*
   looks through the list of open pstores and finds
   the owner of a particular address.  Returns the
   VMPageRecord for that address, and the owner through *s
*/

struct VMPageRecord *find_owner_and_vmpr( void *addr, RStore **s );

/*
    similar in result to find_owner_and_vmpr(), but faster and
    only works for pointers that ARE into a pstore, and are onto
    a FirstPage
*/

struct VMPageRecord *owner_and_vmpr( void *addr, RStore **s );


void install_new_vmpr( RStore *store, struct VMPageRecord *vmpr );

/*  allocate some new (marked dirty) pages in the store, already
    knowing the base page num to alloc them at.  Returns the VMPR
    of the first page
 */

struct VMPageRecord *alloc_ppages( RStore *store, UINT_32 num_pages );

/*
 *  translates a single persistent object reference to
 *  a local object reference.  Works for PTRs and IMMOBs.
 *  page references are interpreted relative to the Page
 *  Reference Table, prt
 */
/*  map a single (immob or ptr) scheme object reference from
    persistent representation to local representation, with
    pointers interpreted relative to the PageReferenceTable
*/

obj map_pers_to_local( struct VMPageRecord **prt, obj item );



/*  single-pointer object translation
*/

obj translate_LR( RStore *in_store, struct LocationRef lr );

/*  the reverse of translate_ptr
    signals an error if the pointer is not into the given rstore
 */

struct LocationRef create_LR( RStore *in_store, obj item );

struct LocationRef create_immob_LR( obj item );
struct LocationRef create_LR_on_page( RStore *in_store, 
				      obj item, 
				      struct VMPageRecord *rec );

/* same as untranslate_ptr, but only works, for PTRs,
   for pointers to first pages, and will probably crash if the
   pointer is not into the given pstore
*/

struct LocationRef create_LR_first( RStore *in_store, obj item );

/*  find the vmpr that controls a particular VM address,
    or return NULL if the addr is not owned by the given store
*/

struct VMPageRecord *addr_to_vm_page_record( struct RStore *store, 
					     void *addr );

/*
 *  try to write a page out to the underlying storage device,
 *  returning a list of unresolved references on failure, or
 *  the empty list on success
 */
 
obj write_page( struct RStore *store, struct VMPageRecord *page );

/* keep writing pages as long as possible.
   returns the empty list iff all of the store's pages are clean
*/

obj write_dirty_pages( struct RStore *store );

/* transition a page from the RESERVED to the LOADED state */

void load_page( RStore *store, struct VMPageRecord *vmp );

/* transition a page from the LOADED to the DIRTY state */

void dirty_page( RStore *store, struct VMPageRecord *vmp );

/* currently, mem_size has to be at the beginning, because the
   loader & writer use it to notice the end of the page
*/

struct PHeapHdr {
    UINT_32		  mem_size;		/* bytes taken up */
    UINT_32		  pstore_flags;		/* our flags */
    struct IRC_SizeClass *size_class;
    UINT_32		  gc_flag_bits;		/* set to GEN=7, WHITE or BLACK */
    POBHeader	  	  rs_header;
};

struct PFreeBlock {
  UINT_8    spare1, spare2;
  UINT_8    last_on_this_page;          /* are we the last on this page */
  UINT_8    in_sizeclass;               /* the size class we are in */
  UINT_32   next_page;                  /* what is the next page? */
};

/*  valid values for pstore_flags...  */

/*  hmmm.. I wonder if pstore_flags would be a good place to 
 *  store the heap type of the object...?
 */

#define PFLAGS_ALLOC_AREA       (0x12121212)
#define PFLAGS_LARGE_OBJ        (0x69699696)
#define PFLAGS_NORMAL_OBJ       (0x76766767)
#define PFLAGS_FREE_OBJ         (0x0000F100)    /* a free obj on a page */
#define PFLAGS_FREE_OBJ_LAST    (0x0000F104)    /* only used in extern rep */

#define PHH_TO_PTR(p) OBJ(((UINT_32)(p))+sizeof(struct PHeapHdr)+POINTER_TAG)
#define PTR_TO_PHH(p) ((struct PHeapHdr *)((char *)PTR_TO_DATAPTR(p) \
					   - sizeof(struct PHeapHdr)))

obj all_modules( void );

PAllocArea *make_alloc_area( RStore *owner, obj parent );

obj parea_alloc( AllocArea *area, obj class, UINT_32 size );
obj copy_into_pstore( PAllocArea *area, obj item );
void rstore_copy_gvec_proc( obj new_item, obj old_item );

/* extend the list of failures (other_failures) by references from `ptr' */

obj notice_object_refs( RStore *store, obj ptr, obj other_failures );

/*
 *  the info we store in the LSS's commit area
*/

struct RStoreCommitInfo {
  char                 tag[16];
  UINT_32              next_page_num;
  UINT_32              next_indirect_page;
  struct LocationRef   default_area;
};

void plain_lss_error_handler( LSS *lss, void *info, int code, char *msg );

void rstore_init_psizeclass( void );

void dealloc_ppages( RStore *store, 
                     struct VMPageRecord *first,
                     UINT_32 num_pages );

int rstore_foreach_object( struct VMPageRecord *page,
                           int (*proc)( void *info, 
                                        struct PHeapHdr *ptr ),
                           void *info );
void rstore_gc_set_phase( RStore *ps, enum PGC_Phase phase );

void rstore_page_has_gray( RStore *store, struct VMPageRecord *page, int liveq );
struct VMPageRecord *alloc_vmpr( void );

#endif /* _H_RSTORET */
