/*-----------------------------------------------------------------*-C-*---
 * File:    packages/rstore/xlation.c
 *
 *          Copyright (C)1997 Donovan Kolbly <d.kolbly@rosette.com>
 *          as part of the RScheme project, licensed for free use
 *
 * Version: 1.4
 * Date:    2003-10-13 13:53:05
 * Build:   v0.7.3.4-b7u, 2007-05-30
 *
 * Purpose: RStore pointer translation
 *------------------------------------------------------------------------*/

#include "rstoret.h"
#include "alloc.h"
#include "mmglue.h"
#include <errno.h>
#include <stdio.h>
#include <rscheme/scheme.h>
#include "indirect.h"
#include "scan.h"

struct VMPageRecord *addr_to_vm_page_record( struct RStore *store, 
					     void *addr )
{
struct htent *e;

    e = htable_lookup( &store->vm_page_records, MM_PAGE_BASE_ADDR(addr) );
    return e ? e->value : (struct VMPageRecord *)0;
}

struct LocationRef create_LR_on_page( RStore *in_store, 
				      obj item,
				      struct VMPageRecord *vmpr )
{
  struct LocationRef lr;

  lr.base_page_num = vmpr->ref.base_page_num;
  lr.nth_page = vmpr->ref.nth_page;
  lr.first = vmpr->ref.first;
  lr.offset = VAL(item) - (UINT_32)vmpr->mem_address;
  lr.indirect = 0;
  return lr;
}

struct LocationRef create_immob_LR( obj item )
{
  struct LocationRef lr;

  lr.base_page_num = VAL(item);
  lr.nth_page = 0;
  lr.first = 0;
  lr.indirect = 1;
  lr.offset = 0;

  return lr;
}

struct LocationRef create_LR( RStore *in_store, obj item )
{
  if (OBJ_ISA_PTR(item))
    {
      struct VMPageRecord *vmpr;

      vmpr = addr_to_vm_page_record( in_store, PTR_TO_DATAPTR(item) );
      if (vmpr)
	{
	  return create_LR_on_page( in_store, item, vmpr );
	}
      else
	{
	  /* could check for an indirect object, but currently
	     indirect objects are often created on-demand, and 
	     we're not really in a position to do that here just yet
	     */
	  scheme_error( "create_LR(~s): not in pstore ~s",
		        2, item, in_store->owner );
	}
    }
  return create_immob_LR( item );
}

obj paddr_get( struct PAddrVec *pv, int k )
{
  return translate_LR( pv->owner, pv->vec[k] );
}

int init_paddr( struct PAddrVec *pv, obj item )
{
  struct VMPageRecord *vmpr;
  RStore *owner;

  vmpr = find_owner_and_vmpr( PTR_TO_DATAPTR(item), &owner );
  if (!vmpr) {
    return -EINVAL;
  }
  pv->owner = owner;
  pv->spare = 0;
  pv->vec[0] = create_LR( owner, item );
  return 0;
}

struct LocationRef create_LR_first( RStore *in_store, obj item )
{
  struct LocationRef lr;
  struct VMPageRecord *vmpr;
  struct FirstPageHdr *fph = FIRST_PAGE_HDR_OF(VAL(item));

  assert( OBJ_ISA_PTR(item) );

  assert( in_store == fph->area->owner );

  vmpr = fph->vmpr;

  lr.base_page_num = vmpr->ref.base_page_num;
  lr.nth_page = vmpr->ref.nth_page;
  lr.first = vmpr->ref.first;
  lr.offset = VAL(item) - (UINT_32)vmpr->mem_address;
  lr.indirect = 0;
  return lr;
}

obj translate_LR( RStore *in_store, struct LocationRef lr )
{
  struct PageRef ref;

  if (lr.indirect)
    {
      if (lr.first)
	{
	  struct VMPageRecord *vmpr;

	  ref.base_page_num = lr.base_page_num;
	  ref.first = 1;
	  ref.indirect = 1;
	  ref.dirty = 0;
	  ref.loaded = 0;
	  ref.nth_page = lr.nth_page;

	  vmpr = get_vmpr( in_store, &ref );

	  if (!vmpr || lr.offset > 63 || lr.nth_page != 1)
	    {
	      scheme_error( "translate_ptr(~d[~d]): illegal",
			    2, 
			    int2fx( lr.base_page_num ),
			    int2fx( lr.offset ) );
	    }
	  return ((obj *)vmpr->mem_address)[ lr.offset ];
	}
      else
	{
	  /* special hack for immobs */
	  return OBJ(lr.base_page_num);
	}
    }
  else
    {
      struct VMPageRecord *vmpr;

      ref.base_page_num = lr.base_page_num;
      ref.first = lr.first;
      ref.indirect = 0;
      ref.dirty = 0;
      ref.loaded = 0;
      ref.nth_page = lr.nth_page;
      vmpr = get_vmpr( in_store, &ref );
      if (!vmpr)
	{
	  scheme_error( "translate_ptr(~x.~04x+~x): illegal",
		        3,
		       int2fx( lr.base_page_num >> 16 ),
		       int2fx( lr.base_page_num & 0xFFFF ),
		       int2fx( lr.offset ) );

	}
      return OBJ( (UINT_32)vmpr->mem_address + lr.offset );
    }
}

obj map_pers_to_local( struct VMPageRecord **prt, obj item )
{
    if (OBJ_ISA_PTR(item))
    {
    struct VMPageRecord *pr;
    
	pr = prt[VAL(item) >> 16];
	if (pr->ref.indirect)
	{
	    return ((obj *)(pr->mem_address))[ INDIR_CODE_ENTRY(item) ];
	}
	else
	{
	  UINT_32 val;
	  UINT_16 offset = VAL(item) & 0xFFFF;

	  /* [CR 600] special hack to read rstores corrupted with 
	   * page-mask'ed offsets when zero-length object is at end
	   * of page
	   */
	  if (offset == 0x3)
	    offset = 0x2003;

	  val = (UINT_32)pr->mem_address + offset;
	  return OBJ(val);
	}
    }
    else
    {
	return item;
    }
}

#if 0
void mk_persistent_addr( RStore *store,
			 struct VMPageRecord *vmpr,
			 obj item,
			 struct PersistentAddr *pa )
{
  if (vmpr)
    {
      assert( OBJ_ISA_PTR(item) );
      if (!vmpr->ref.first)
	{
	  scheme_error( "rstore internal error: 3", 0 );
	}
      else if (vmpr->ref.indirect)
	{
	  scheme_error( "transient->persistent: indirect pointers not supported: ~s", 1, item );
	}
      pa->base_page_num = vmpr->ref.base_page_num;
      pa->offset = OBJ(VAL(item)-(UINT_32)vmpr->mem_address);
      pa->nth_page = vmpr->ref.nth_page;
    }
  else
    {
      assert( !OBJ_ISA_PTR(item) );
      pa->base_page_num = VAL(item);
      pa->offset = 0;
      pa->nth_page = 0;
    }
}
#endif
