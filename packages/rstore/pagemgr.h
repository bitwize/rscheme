#ifndef _H_RSTORE_PAGEMGR
#define _H_RSTORE_PAGEMGR

#include "scan.h"

struct PHeapHdr *large_object_hdr( struct VMPageRecord *page );
struct PHeapHdr *first_on_first( struct VMPageRecord *page );

void load_page( RStore *store, struct VMPageRecord *vmp );
obj write_page( RStore *store, struct VMPageRecord *page );


void lru_model_load_page( RStore *store, 
			  struct VMPageRecord *vmp,
			  struct VMPageRecord *first_vmp );

void lru_model_write_page( struct RStore *store, 
			   struct VMPageRecord *page,
			   struct ScannedPtr *scans,
			   int num_scanned );

void old_model_load_page( RStore *store, 
			  struct VMPageRecord *vmp,
			  struct VMPageRecord *first_vmp );

void old_model_write_page( struct RStore *store, 
			   struct VMPageRecord *page,
			   struct ScannedPtr *scans,
			   int num_scanned );

obj lru_model_scan_page( LSS *lss, 
                         unsigned page, 
                         enum MetaScanMode mode, 
                         obj arg );
obj old_model_scan_page( LSS *lss, 
                         unsigned page, 
                         enum MetaScanMode mode, 
                         obj arg );

#endif /* _H_RSTORE_PAGEMGR */
