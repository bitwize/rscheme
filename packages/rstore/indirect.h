#include "rstoret.h"
#include <stdio.h>
#include <rscheme/scheme.h>
#include "alloc.h"

enum {
  SYMBOL_INDIRECT_PAGE_CONSTRUCTOR      =  (0),
  CODE_PTR_INDIRECT_PAGE_CONSTRUCTOR    =  (1),
  TABLE_LOOKUP_INDIRECT_PAGE_CONSTRUCTOR = (2)
};

obj construct_symbols( struct RStore *store, UINT_32 inst_id, 
 		       char *data, UINT_32 len );

obj unswizzle_symbol_itemv( obj symbol_list );

struct VMPageRecord *build_indirect_page( RStore *store, struct PageRef *pr );
