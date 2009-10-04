#|------------------------------------------------------------*-Scheme-*--|
 | %Z%1.3 %W% %G% 10:26:31
 |
 | Purpose:	Interface w/PAllocArea's
 | 
 |------------------------------------------------------------------------|
 | Notes:
 |
 `------------------------------------------------------------------------|#

(define-rstore-glue (default-allocation-area (ps <persistent-store>))
{
   REG0 = DATAPTR_TO_PTR( ps_store->default_area );
   RETURN1();
})

(define-rstore-glue (allocation-area->store (area <allocation-area>))
{
  if (area->owner)
    REG0 = area->owner->owner;
  else
    REG0 = FALSE_OBJ;
  RETURN1();
})

(define-rstore-glue (allocation-area-stats (area <allocation-area>))
{
   REG0 = int2fx( area->accum_bytes );
   REG1 = int2fx( area->accum_objects );
   REG2 = int2fx( area->accum_pages );
   RETURN(3);
})

(define-rstore-glue (allocation-area-parent (area <allocation-area>))
{
  REG0 = translate_LR( area->owner, area->parent_LR );
  RETURN1();
})


;;
;; this function is unsafe...
;;  for a safe version, use object-allocation-area

(define-safe-glue (ptr-allocation-area item)
{
  RStore *o;
  struct PHeapHdr *p = PTR_TO_PHH(item);
  struct FirstPageHdr *fph = FIRST_PAGE_HDR_OF(p);

  assert( OBJ_ISA_PTR(item) );
  assert( find_owner_and_vmpr( p, &o ) );

  REG0 = DATAPTR_TO_PTR(fph->area);
  RETURN1();
})


;; returns #f if the given item is not in a persistent store
;; (note that it returns #t for immobs)

(define-safe-glue (persistent? item)
{
  if (OBJ_ISA_PTR(item))
    {
      RStore *o;
      struct PHeapHdr *p = PTR_TO_PHH(item);
      
      if (find_owner_and_vmpr( p, &o ))
	REG0 = TRUE_OBJ;
      else
	REG0 = FALSE_OBJ;
    }
  else
    REG0 = TRUE_OBJ;
  RETURN1();
})

(define-safe-glue (object-allocation-area item)
 literals: ((& *default-allocation-area*))
{
  if (OBJ_ISA_PTR(item))
    {
      RStore *o;
      struct VMPageRecord *vmpr;
      struct PHeapHdr *p = PTR_TO_PHH(item);

      vmpr = find_owner_and_vmpr( p, &o );
      if (vmpr)
	{
	  struct FirstPageHdr *fph = (struct FirstPageHdr *)vmpr->mem_address;

	  REG0 = DATAPTR_TO_PTR(fph->area);
	}
      else
	REG0 = TLREF(0);
    }
  else
    REG0 = TLREF(0);
  RETURN1();
})

(define-rstore-glue (make-allocation-area (in <allocation-area>))
{
  PAllocArea *area;

  area = make_alloc_area( in->owner, raw_in );
  REG0 = DATAPTR_TO_PTR(area);
  RETURN1();
})

