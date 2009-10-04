#include "swizzler.h"

/*
 *  This file serves as both an example and a sometimes useful
 *  practical utility.  It is an example of how to define 
 *  application-specific image mode operators for the rstore
 *  package.
 *
 *  Specifically, the behavior it implements for SWIZ_MODE_APP_9
 *  is that of an object whose first slot is transient -- ie, doesn't
 *  get written out to the store.  On load, the first slot is set
 *  to FALSE (#f).
 */

static void notice_proc( struct swiz_mode_handler *self,
			 struct PHeapHdr *hdr,
			 void *src, UINT_32 from_start, 
			 UINT_32 len,
			 struct Scanning *info,
			 void (*notice_obj)( struct Scanning *info, 
					     obj *ref ) )
{
  obj *s = (obj *)src;
  UINT_32 i;

  s++;
  for (i=SLOT(1); i<len; i+=SLOT(1))
    notice_obj( info, s++ );
}

static void write_proc( struct swiz_mode_handler *self,
			struct PHeapHdr *hdr,
			void *src, UINT_32 from_start, 
			UINT_32 len,
			void *info,
			void (*unswiz_and_compress)( void *info, obj ref ) )
{
  obj *s = (obj *)src;
  UINT_32 i;

  s++;
  for (i=SLOT(1); i<len; i+=SLOT(1))
    unswiz_and_compress( info, *s++ );
}

static void load_proc( struct swiz_mode_handler *self,
		       RStore *store,
		       struct PHeapHdr *hdr,
		       UINT_32 *dst,
		       UINT_32 from_start,
		       UINT_32 N,
		       void *info,
		       UINT_32 (*get_word)( void *info ),
		       obj (*get_obj)( void *info ) )
{
  obj *d = (obj *)dst;
  UINT_32 i;

  *d++ = FALSE_OBJ;

  for (i=SLOT(1); i<N; i+=SLOT(1))
    {
      *d++ = get_obj( info );
    }
}

struct swiz_mode_handler SWM_transient_cell = {
  notice_proc,
  write_proc,
  load_proc,
  rstore_copy_gvec_proc,
  NULL,
  SWIZ_MODE_APP_9    /* image mode = 89 */
};
