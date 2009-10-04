;; note: <persistent-addr> and <persistent-addr-vector>
;; use identical underlying representations.  The former
;; simply appears to be an immutable, single-element 
;; instantiation of the former

(define-class <persistent-addr> (<object>) :bvec 
 image-mode: 20)

(define-class <persistent-addr-vector> (<object>) :bvec 
 image-mode: 20)

(define-class <persistent-store> (<object>)
  (c-rstore-ptr type: <fixnum> init-value: 0)
  ;; ** note: the following three slots must be SLOT(1)..SLOT(3),
  ;;    and the slots must be immutable, because rstore_open() in
  ;;    util.c extracts them at open time and saves them in the
  ;;    RStore
  (pivots init-function: make-object-table)
  (local-code-ptrs init-function: make-object-table)
  (local-fn-descrs init-function: make-object-table)
  ;; ** note: the following slot must be SLOT(4) because
  ;;    register_indirect_page() in indirect.c installs
  ;;    page-num --> vector mappings in this table
  (indirect-pages init-function: make-fixnum-table)
  ;; ** note: likewise the following SLOT(5) and the dirty_cards
  (dirty-cards-table init-function: make-fixnum-table)
  ;;
  ;; ** note: likewise the following SLOT(6)
  ;;
  ;; During PENDING and TSCAN, a queue of pages 
  ;; which are live (when initialized, a vector with the snapshot commit
  ;; id is pushed in as the first entry)
  (live-queue init-value: #f)
  ;;------------------------------------------------------------
  ;;   these slots are used only by Scheme-level code
  ;;------------------------------------------------------------
  ;;
  ;; user-overridable hook for copying objects into the
  ;; pstore.
  ;;
  ;; the default implementation will copy all objects
  ;; except <<class>> instances, which it will signal an
  ;; error for (since the most common error seems to be
  ;; to forget to specify a pivot class, which often winds up
  ;; sucking in the Universe)
  ;;
  ;; the procedure, if supplied, is called with two
  ;; arguments: the persistent store and the current
  ;; relocation table
  ;;
  ;; it should return two procedures -- see make-std-copy-iner
  ;; (the default) for more details
  ;;
  (make-copy-iner-proc init-value: '#uninit)
  (underlying-lss type: <lss>)
  (name init-value: #f)
  (compaction-hook init-value: #f))


#|
    /pivots/ are objects which are referred to directly from
    inside the persistent image.  The most notable examples
    are class objects, when the classes aren't to be in the
    image itself (the usual case)

    The /pivot-table/ is a hash table (object-table) mapping the
    local copies of these things to their persistent identifiers.

    The persistent identifier of a pivot consists of an indirect
    page number and an offset in that page.  The reverse mapping
    of pivots -- from persistent identifiers to local objects --
    are via /indirect pages/.  The local application makes available,
    for each indirect page, a vector of the local objects which
    are the pivots.


    Almost always, the /pivots/ are classes or symbols.

    Unfortunately, because indirect pages have to be available
    when a page is swizzled -- which could occur at essentially
    any memory instruction -- arbitrary scheme code cannot be
    responsible for providing the contents of indirect pages.

    Instead, specially crafted C functions called /indirect page
    constructors/ are responsible for creating the indirect pages
    when necessary.  Note that arbitrary objects can be pivots,
    but the indirect pages have to be available BEFORE the page
    requiring it is unswizzled.

    The following /indirect page constructors/ are available:
        SYMBOL()
        CODE-PTR()
        TABLE-LOOKUP(symbol-table)

     Currently, only SYMBOL is implemented.

     Each indirect page records the /page constructor type/ and
     /instantiation id/ which is to be used to load the indirect page.  
     Since SYMBOL and CODE-PTR have no data, there is only one
     unique instance of each constructor, so their instantiation id
     is always 0.

        SYMBOL-INDIRECT-PAGE-CONSTRUCTOR       (0)
        CODE-PTR-INDIRECT-PAGE-CONSTRUCTOR     (1)
        TABLE-LOOKUP-INDIRECT-PAGE-CONSTRUCTOR (2)

     The client program is responsible for filling the <persistent-store>'s
     indirect-page-constructor-data vector with appropriate information
     for the corresponding instantiation ids.  There is a common
     instantiation data id space, shared by all page constructor types.
|#


(define-method initialize ((self <persistent-store>))
  ;;
  ;; install default make-copy-iner if they didn't
  ;; specify one, which is the std-copy-iner (which handles symbols)
  ;; parameterized with the default-copy-in-obj proc (which
  ;; copies everything except <<class>>'s, which it signals 
  ;; an error on)
  ;;
  (if (eq? (make-copy-iner-proc self) '#uninit)
      (set-make-copy-iner-proc! 
       self 
       (make-make-std-copy-iner default-copy-in-obj))))

(define (setup-std-page-0! (self <persistent-store>))
  (bind ((unit-ptr (find-linked-module "bci"))
	 (part-ptr (find-part-in-linked-module unit-ptr 8902))
	 (code-ptr fn-desc (find-code-ptr-in-part part-ptr 0))
	 (p0 (clone *std-pivots*))
         (p1 (clone *named-pivot-objects*)))
    ;; load-side mapping
    (vector-set! p0 3 code-ptr)
    (vector-set! p0 4 fn-desc)
    ;; store-side mapping
    (table-insert! (local-code-ptrs self) code-ptr 3)
    (table-insert! (local-fn-descrs self) fn-desc 4)
    ;; add the indirect page (puts it into the `indirect-pages' table, too)
    (register-indirect-page self 0 p0)
    (register-indirect-page self 1 p1)
    p0))

;;;   This is very non-scalable -- we need to be able to organize our
;;;   pivots by a hierarchical numbering (or even better -- symbolic)
;;;   scheme, so, e.g., we can say:
;;;       (module rs.util.quantity <nquantity>) == #{<<class>> <nquantity>}
;;;   for now, we will simply manage pivot ids by hand

(define *named-pivot-index* '#((module rs.util.quantity <nquantity>)
                               (module rs.util.quantity <derived-unit>)
                               (module rs.util.quantity <base-unit>)
                               (module rs.util.relation <universe>)
                               (module rs.util.relation <relation-extent>)))

(define *named-pivot-objects* '#())

(define (get-named-pivots)
  (clone *named-pivot-objects*))

;;;   This should only be used during module development, 
;;;   to avoid having to recompile this module

(define (extend-named-pivot-index! namelist)
  (set! *named-pivot-index* (vector-append *named-pivot-index* 
                                           (vector namelist)))
  (vector-length *named-pivot-index*))

(define (find-named-pivot namelist)
  (let loop ((i 0))
    (if (< i (vector-length *named-pivot-index*))
        (if (equal? (vector-ref *named-pivot-index* i) namelist)
            i
            (loop (+ i 1)))
        #f)))

(define (register-pivot! namelist pivot)
  (let ((k (find-named-pivot namelist)))
    (if k
        (let ((l (vector-length *named-pivot-objects*)))
          (if (< k l)
              (vector-set! *named-pivot-objects* k pivot)
              (set! *named-pivot-objects* (vector-append
                                           *named-pivot-objects*
                                           (make-vector (- k l))
                                           (vector pivot))))
          k)
        (error "unknown named pivot: ~s" namelist))))

(define *std-pivots*
  (with-module syscalls
    (with-module paths
      (vector <vector>
	      <pair>
	      <string>
	      0    ;; BCI code ptr [3]
	      0    ;; BCI fn descr [4]
	      <double-float>
	      <byte-vector>
	      <string-table>
	      <string-ci-table>
	      <eq-table>
	      <integer-table>
	      <hash-integer-table>
	      <generic-table>
	      <symbol-table>
	      <table-bucket>
	      <closure>
	      <function>
	      <template>
	      <byte-coded>
	      <file-name>
	      <directory-name>
	      <root-dir>
	      <time>
	      <interval>
	      <allocation-area>
	      ;<persistent-addr>
	      ;<persistent-addr-vector>
	      ;<persistent-object-table>
	      ;<persistent-object-table-bucket>
	      ;<long-int>
	      ))))

(define (open-pstore-on-lss (lss <lss>))
  (let (((sto <persistent-store>) (make <persistent-store>
					underlying-lss: lss)))
    (set-c-rstore-ptr! sto (open-pstore* sto lss))
    (setup-std-page-0! sto)
    sto))

(define (open-persistent-store (path <string>))
  (open-pstore-on-lss (lss-open path)))

(define (create-persistent-store (path <string>))
  (open-pstore-on-lss (lss-create path)))

(define (read-persistent-store (path <string>))
  (open-pstore-on-lss (lss-open path #f #t)))

(define (pstore-file (self <persistent-store>))
  (lss-file (underlying-lss self)))

(define (close-persistent-store (ps <persistent-store>))
  (if (eq? (c-rstore-ptr ps) 0)
      (em 202 "~a: persistent store already closed!" (pstore-file ps)))
  (close-pstore ps)
  (values))

(define-rstore-glue (close-pstore (ps <persistent-store>))
{
 /*
  *   make sure we zero out the (RStore *) pointer before
  *   any chance to switch threads
  */
   if (ps)
     {
       rstore_close( ps );
       gvec_set( raw_ps, SLOT(0), ZERO );
     }
   RETURN0();
})

(define-rstore-glue (root-object (ps <persistent-store>))
{
  REG0 = rstore_root( ps );
  RETURN1();
})

(define-rstore-glue (set-root-object! (ps <persistent-store>) new_root)
{
  obj old_root = rstore_root( ps );
  set_rstore_root( ps, new_root );
  REG0 = old_root;
  RETURN1();
})

(define-rstore-glue (open-pstore* owner (lss <lss>))
{
  RStore *sto = rstore_open( owner, lss );
  REG0 = C_PTR_TO_OBJ( RStore *, sto );
  RETURN1();
})

(define-rstore-glue (pstore-commit* (store <persistent-store>) 
                                    root
                                    reloc
                                    livetbl)
{
  REG0 = rstore_commit( store, root, reloc, livetbl );
  RETURN1();
})

#|
;; returns 3 values:
;;  [0] commit version
;;  [1] commit time
;;  [2] creation time

(define-rstore-glue (commit-info (ps <persistent-store>))
  literals: ((& <time>))
{
  commit_info_t ci;

  lss_get_lss_commit_info( ps_store->lss, &ci );

  REG0 = int2fx( ci.commit_version );
  REG1 = make_time_sec( ci.commit_time, TLREF(0) );
  REG2 = make_time_sec( ci.create_time, TLREF(0) );
  REG3 = int2fx( ci.prev_commit_at >> 16 );
  REG4 = int2fx( ci.prev_commit_at & 0xFFFF );
  RETURN(5);
})

(define-rstore-glue (commit-record-locator (ps <persistent-store>))
{
  off_t spare_cr = ps_store->lss->spare_commit_at;

  REG0 = cons( int2fx( spare_cr >> 16 ), int2fx( spare_cr & 0xFFFF ) );
  RETURN1();
})

|#

(define-rstore-glue (scan-object (ps <persistent-store>) item more reloc)
{
  if (OBJ_ISA_PTR( item ))
    REG0 = rstore_scan_pob( ps, item, more, reloc );
  else
    REG0 = more;
  RETURN1();
})

#|
(define-rstore-glue (add-image-mode-handler! (ps <persistent-store>)
					     (h <raw-ptr>))
{
  rstore_add_swiz_mode_handler( ps_store, (struct swiz_mode_handler *)h );
  RETURN0();
})

(define-rstore-glue (transient-cell-mode-handler)
{
  REG0 = RAW_PTR_TO_OBJ( &SWM_transient_cell );
  RETURN1();
})
|#

(define-rstore-glue (copy-into-pstore (area <allocation-area>) item)
{
  REG0 = rstore_copy_in( area, item );
  arg_count_reg = 1;    /* set up a safe point */
  assert( OBJ_ISA_PTR( REG0 ) );
  gc_work( SIZEOF_PTR( REG0 ) );
  RETURN1();
})

(define-rstore-glue (allocation-area->store (area <allocation-area>))
{
  REG0 = rstore_area_owner( area );
  RETURN1();
})

(define-rstore-glue (num-dirty-pages (ps <persistent-store>))
{
  REG0 = int2fx( rstore_count_dirty( ps ) );
  RETURN1();
})

(define-rstore-glue (did-rollback-lss* (ps <persistent-store>))
{
  rstore_did_rollback( ps );
  RETURN0();
})

(define-rstore-glue (rollback-dirty-pages (ps <persistent-store>))
{
  REG0 = int2fx( rstore_rollback_dirty( ps ) );
  RETURN1();
})

(define-rstore-glue (default-allocation-area (ps <persistent-store>))
{
  AllocArea *aa = rstore_get_default_area( ps );
  REG0 = alloc_area_to_obj( aa );
  RETURN1();
})

(define-rstore-glue (set-default-allocation-area! (ps <persistent-store>) 
						  (aa <allocation-area>))
{
  rstore_set_default_area( ps, aa );
  RETURN0();
})

(define-rstore-glue (object->allocation-area item)
{
  REG0 = alloc_area_to_obj( rstore_alloc_area( item ) );
  RETURN1();
})

(define-rstore-glue (make-allocation-area (in <allocation-area>))
{
  REG0 = alloc_area_to_obj( make_sub_alloc_area( in ) );
  RETURN1();
})

;;; take a pstore arg even though its not used now,
;;; to forward-compat API to when we make it per-PS

(define-rstore-glue (set-compression-method! (ps <persistent-store>)
					     (method <raw-string>))
{
  rstore_set_compression( ps, method );
  RETURN0();
})

(define-lss-glue (pstore-meta-root-info (lss <lss>))
{
  REG0 = meta_root_info( lss );
  RETURN1();
})

(define-lss-glue (pstore-meta-scan-pagetable (model <raw-int>)
                                             (lss <lss>) 
                                             (page <raw-int>))
{
  REG0 = meta_scan_page( model,
                         lss,
                         page,
                         RSTORE_SCAN_PAGETABLE,
                         FALSE_OBJ );
  RETURN1();
})

(define-lss-glue (pstore-meta-scan-starts (model <raw-int>)
                                          (lss <lss>) 
                                          (page <raw-int>)
                                          (swizmodes <hash-integer-table>))
{
  REG0 = meta_scan_page( model,
                         lss,
                         page,
                         RSTORE_SCAN_STARTS,
                         swizmodes );
  RETURN1();
})

;;;  Figure out what kind of page it is by looking
;;;  at the first-page header (which has the same
;;;  structure for both first-of-large-object pages
;;;  and first-and-only pages)

(define-lss-glue (pstore-meta-scan-npages (model <raw-int>)
                                          (lss <lss>) 
                                          (page <raw-int>)
                                          (swizmodes <hash-integer-table>))
{
  REG0 = meta_scan_page( model,
                         lss,
                         page,
                         RSTORE_SCAN_PAGECOUNT,
                         swizmodes );
  RETURN1();
})


;;;  `pstore-meta-scan-first-pp', `pstore-meta-scan-objects', and
;;;  `pstore-meta-scan-interior-pp' all return the same basic
;;;  information structure.  They just apply to different kinds
;;;  of pages in the store.  pstore-meta-scan-{first,interior}-pp
;;;  return a value which has one less level of nesting (since there
;;;  is by construction only one object)
;;;
;;;  scanresult ::= (pglist . value)
;;;
;;;  pglist ::= (pagref ...)
;;;
;;;  pageref ::= #(base_page flags #f)          ; cf page_loader.ci:946
;;;
;;;  value ::= (((pageref . offset) ...))
;;;
;;;  where each member of the `value' list
;;;  corresponds 1-1 with a member of the `obj' list,
;;;  which is a list of offsets into this page
;;;

(define-lss-glue (pstore-meta-scan-objects (model <raw-int>)
                                           (lss <lss>) 
                                           (page <raw-int>)
                                           (swizmodes <hash-integer-table>)
                                           objs)
{
  REG0 = meta_scan_page( model,
                         lss,
                         page,
                         RSTORE_SCAN_OBJECTS,
                         cons( swizmodes, objs ) );
  RETURN1();
})

(define-lss-glue (pstore-meta-scan-first-pp (model <raw-int>)
                                            (lss <lss>) 
                                            (page <raw-int>)
                                            (swizmodes <hash-integer-table>))
{
  REG0 = meta_scan_page( model,
                         lss,
                         page,
                         RSTORE_SCAN_FIRST,
                         cons( swizmodes, NIL_OBJ ) );
  RETURN1();
})

(define-lss-glue (pstore-meta-scan-interior-pp (model <raw-int>)
                                               (lss <lss>) 
                                               (page <raw-int>)
                                               (swizmode <fixnum>)
                                               (length <fixnum>))
{
  REG0 = meta_scan_page( model,
                         lss,
                         page,
                         RSTORE_SCAN_INTERIOR,
                         cons( length, swizmode ) );
  RETURN1();
})

(define (pstore-meta-insert-indirects tbl rec data)
  (for-each
   (lambda (i)
     (let ((pivot (vector-ref data i)))
       (if (class? pivot)
           (table-insert! tbl
                          (+ (* rec 64) i)
                          (or (image-mode pivot) 0)))))
   (range (vector-length data))))

(define (pstore-meta-std-indirects)
  (let ((tbl (make-fixnum-table)))
    (pstore-meta-insert-indirects tbl 0 *std-pivots*)
    tbl))

(define-rstore-glue (object-deallocate item)
{
  int n;
 
  n = parea_dealloc( item );
  if (n == 0)
    {
      REG0 = TRUE_OBJ;
    }
  else
    {
      REG0 = FALSE_OBJ;
    }
  RETURN1();
})


(define-rstore-glue (location-deallocate (ps <persistent-store>) 
                                         (page <raw-int>)
                                         (flags <raw-int>)
                                         (offset <raw-int>))
{
  parea_dealloc_lr( ps, page, flags, offset );
  RETURN0();
})

(define-rstore-glue (pstore-write-protect? (ps <persistent-store>))
{
  REG0 = rstore_get_write_protect( ps ) ? TRUE_OBJ : FALSE_OBJ;
  RETURN1();
})

(define-rstore-glue (pstore-set-write-protect! (ps <persistent-store>) flag)
{
  rstore_set_write_protect( ps, truish(flag) );
  RETURN0();
})


(define-rstore-glue (%pstore-live-objects (ps <persistent-store>) otbl)
{
  REG0 = rstore_get_live_objects( ps, otbl );
  RETURN1();
})

(define-rstore-glue (in-persistent-store? item)
{
  if (rstore_of_object( item )) {
    REG0 = TRUE_OBJ;
  } else {
    REG0 = FALSE_OBJ;
  }
  RETURN1();
})


(define-rstore-glue (transient->persistent item)
  literals: ((& <persistent-addr>))
{
  struct PAddrVec *pv;
  obj pp = ZERO;

  pp = bvec_alloc( sizeof( struct PAddrVec ), TLREF(0) );
  init_paddr( (struct PAddrVec *)PTR_TO_DATAPTR(pp), item );
  REG0 = pp;
  RETURN1();
})

(define-rstore-glue (persistent->transient (pa <persistent-addr>))
{
  REG0 = paddr_get( pa, 0 );
  RETURN1();
})

(define-rstore-glue (persistent->parts (pa <persistent-addr>))
{
  REG0 = rstore_get_scheme_object( pa->owner );
  REG1 = int2fx( pa->vec[0].base_page_num );
  REG2 = int2fx( (pa->vec[0].first ? 1 : 0)
                 + (pa->vec[0].indirect ? 2 : 0)
                 + (pa->vec[0].nth_page << 2) );
  REG3 = int2fx( pa->vec[0].offset );
  RETURN(4);
})

(define-rstore-glue (parts->persistent (ps <persistent-store>)
                                       (base <raw-int>)
                                       (flags <raw-int>)
                                       (offset <raw-int>))
  literals: ((& <persistent-addr>))
{
  struct PAddrVec *pa;
  obj pp = ZERO;

  pp = bvec_alloc( sizeof( struct PAddrVec ), TLREF(0) );
  pa = PTR_TO_DATAPTR(pp);

  pa->owner = ps;
  pa->spare = 0;

  pa->vec[0].base_page_num = base;
  pa->vec[0].first = (flags & 1) ? 1 : 0;
  pa->vec[0].indirect = (flags & 2) ? 1 : 0;
  pa->vec[0].nth_page = flags >> 2;
  pa->vec[0].offset = offset;

  REG0 = pp;
  RETURN1();
})


(define-method to-string ((self <persistent-addr>))
  (bind ((s page (flags <fixnum>) offset (persistent->parts self))
         (n (name s))
         (ch (case (bitwise-and flags #b11)
               ((#b01) #\+)
               ((#b00) #\.)
               ((#b10) #\/)
               ((#b11) #\?)))
         (npages (logical-shift-right flags 2))
         (plain (if n
                    (format #f "~a ~08x~c~04x" n page ch offset)
                    (format #f "~08x~c~04x" page ch offset))))
    (if (eq? npages 1)
        plain
        (string-append plain "*" (number->string npages)))))
  
(define-method write-object ((self <persistent-addr>) port)
  (format port "#[<paddr> ~a]" (to-string self)))

(define-method lss-tune ((self <persistent-store>) key value)
  (lss-tune (underlying-lss self) key value))

(define (pstore-last-commit-id (self <persistent-store>))
  (lss-get-generation (underlying-lss self)))

(define (pstore-next-commit-id (self <persistent-store>))
  (add1 (lss-get-generation (underlying-lss self))))

;;;  Works for any object, and is consistent 
;;;  across loadings for persistent objects

(define-glue (persistent-hash item)
{
  REG0 = rstore_hash( item );
  RETURN1();
})


(define (pstore-set-commit-id (self <persistent-store>) gen)
  (let ((app-indir '()))
    ;;
    (table-for-each
     (indirect-pages self)
     (lambda (h k v)
       (if (and (> k 1) (< k 256))
           (set! app-indir (cons (cons k v) app-indir)))))
    ;;
    (if gen     ; #f means don't set it on the LSS
        (lss-set-generation (underlying-lss self) gen))
    ;;
    ;;  Reset various mapping tables
    ;;
    (set-local-code-ptrs! self (make-object-table))
    (set-local-fn-descrs! self (make-object-table))
    (set-indirect-pages! self (make-fixnum-table))
    ;;
    (did-rollback-lss* self)
    ;; repopulate the standard (page 0 and 1) indirects
    (setup-std-page-0! self)
    ;; repopulate the application's indirect pages
    (for-each
     (lambda (i)
       (register-indirect-page self (car i) (cdr i)))
     app-indir)
    ;;
    self))

(define-rstore-glue (gc:set-tracking (ps <persistent-store>) (level <raw-int>))
{
  rstore_gc_set_tracking( ps, level );
  RETURN0();
})

(define-glue (gc:set-gray! item)
{
  if (OBJ_ISA_PTR( item )) {
    REG0 = rstore_gc_grayify( item );
    RETURN1();
  } else {
    RETURN0();
  }
})

(define-glue (gc:color item)
{
  if (OBJ_ISA_PTR( item )) {
    unsigned f = rstore_gc_flagbits( item );
    REG0 = int2fx( f & 1 );
    REG1 = int2fx( (f >> 1) & 7 );
    REG2 = int2fx( (f >> 8) & 15 );
    RETURN(3);
  } else {
    RETURN0();
  }
})

(define-rstore-glue (gc:work (ps <persistent-store>) (mode <raw-int>) arg)
{
  REG0 = rstore_gc_work( ps, mode, arg );
  RETURN1();
})
