
(define-rstore-glue (alloc-indirect-pages (ps <persistent-store>)
					  (n <raw-int>))
{
  REG0 = int2fx( alloc_indir_pages( ps, n ) );
  RETURN1();
})

(define (alloc-indirect-page (self <persistent-store>))
  (let ((n (alloc-indirect-pages self 1)))
    (dm 434 "allocating for use indirect page[~d]\n" n)
    n))

;;;  register_indirect_page() installs the page_name->itemv 
;;;  mapping into the `indirect-pages' table.  We do that
;;;  in C instead of in Scheme because sometimes (ie, during
;;;  loading) we will elaborate and register indirect pages

(define-rstore-glue (register-indirect-page (ps <persistent-store>)
					    (page_num <raw-int>)
					    (itemv <vector>))
{
  register_indirect_page( ps, page_num, itemv );
  RETURN0();
})

#|
;;; what's the diff between `register...' and `install...'?

(define-rstore-glue (install-indirect-page (ps <persistent-store>) 
					   (page_num <raw-int>)
					   (vec <vector>))
{
  struct VMPageRecord *vmpr = ALLOC(struct VMPageRecord);

  vmpr->mem_address = PTR_TO_DATAPTR(vec);
  vmpr->ref.base_page_num = page_num;
  vmpr->ref.first = 1;
  vmpr->ref.indirect = 1;
  vmpr->ref.dirty = 0;
  vmpr->ref.loaded = 1;
  vmpr->ref.nth_page = 1;
  install_new_vmpr( ps_store, vmpr );
  RETURN0();
})
|#

;;
;;  returns three values:
;;    [0] the page data as a <string>
;;    [1] the type id
;;    [2] the instance id
;;

#|
(define-rstore-glue (read-indirect-page-data (ps <persistent-store>)
					     (rec_num <raw-int>))
 literals: ((& <string>))
{
  access_t a;
  UINT_32 hdr[2];

  lss_access( ps_store->lss, rec_num, &a );

  memcpy( hdr, a.addr, sizeof hdr );
  REG1 = int2fx( hdr[0] );
  REG2 = int2fx( hdr[1] );
  REG0 = alloc( a.bytes - sizeof hdr + 1, TLREF(0) );
  memcpy( PTR_TO_DATAPTR(REG0), 
	  ((char *)a.addr) + sizeof hdr, 
	  a.bytes - sizeof hdr );
  lss_release( ps_store->lss, &a );
  RETURN(3);
})
|#

(define-rstore-glue (write-indirect-page-data (ps <persistent-store>)
					      (rec_num <raw-int>)
					      (type_id <raw-int>)
					      (instance_id <raw-int>)
					      data)
{
  write_indirect_page_data( ps, rec_num, type_id, instance_id, data );
  RETURN0();			  
})

(define-glue (symbol-list->indirect-page symbols)
{
  REG0 = unswizzle_symbol_itemv( symbols );
  RETURN1();
})
