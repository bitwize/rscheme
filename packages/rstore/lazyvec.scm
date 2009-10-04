
;;
;;  this function operates on <persistent-addr> objects only,
;;  and
;;     (persistent->hash x)
;;  is equivalent to
;;     (location->hash (persistent->transient x))
;;

(define-glue (persistent->hash pp)
 literals: ((& <persistent-addr>))
{
  struct PAddrVec *pv = PTR_TO_DATAPTR(pp);

  COUNT_ARGS(1);
  if (OBJ_ISA_PTR(pp) && EQ(CLASSOF_PTR(pp),TLREF(0)))
    {
      /* for now, we can just hash the whole thing
       * that may not always remain true (e.g., if
       * we put a cached translation in there, we
       * can't include it in the hash)
       */
      REG0 = OBJ( crc_hash( pv->vec, 
			   sizeof( struct LocationRef ), 0 )
		 & ~PRIMARY_TAG_MASK );
    }
  else
    {
      scheme_error( "persistent->hash: not a persistent addr: ~s", 
		    1, pp );
    }
  RETURN1();
})

;;
;;  this function is an object-identity hash function like 
;;  transient->hash, except it "does the right thing" for
;;  pointers into the pstore (ie, it hashes the persistent
;;  address rather than the transient one)
;;
;;  Note that sometimes objects appear to move (when they are
;;  copied implicitly into the pstore on commit), so for example:
;;
;;    (set-some-value! (root-object p) (compute-some-value))
;;
;;    (location->hash (some-value (root-object p)))  ==> some number
;;
;;    (commit p)
;;
;;    (location->hash (some-value (root-object p)))  ==> some OTHER number
;;
;;     -- because the value was copied into the pstore
;;

(define-safe-glue (location->hash item)
 literals: ((& <persistent-addr>))
{
  if (OBJ_ISA_PTR(item))
    {
      struct VMPageRecord *vmpr;
      RStore *owner;

      vmpr = find_owner_and_vmpr( (void *)VAL(item), &owner );
      if (vmpr)
	{
	  struct LocationRef lr;
	  lr = create_LR_on_page( owner, item, vmpr );
	  REG0 = OBJ( crc_hash( &lr, 
			        sizeof( struct LocationRef ), 
			        0 )
		      & ~PRIMARY_TAG_MASK );
	}
      else
	{
	  REG0 = obj_hash(item);
	}
    }
  else
    REG0 = obj_hash(item);
  RETURN1();
})

#|
  Note that even though we store the owner of the
  persistent address in the <persistent-addr>, nothing
  prevents pstores from storing pointers to other
  pstores (with a little extra work on our part, that
  is).

  It's simply a matter of creating & swizzling an indirect
  pointer for the pstore, which will "name" the other pstore
  in some appropriate way.  At load (or translation) time,
  the indirect pointer can be resolved to the actual other
  pstore

  For now, however, we'll trap if trying to save a foreign
  pers-addr, and load it to just point to ourself.
|#

(define (persistent-addr-vector-ref pv i)
  (persistent->transient pv i))

(define (persistent-addr-vector-length pv)
 (quotient (fixnum- (bvec-length pv) 8) 8))

(define-glue (persistent->transient pp ix)
{
  struct PAddrVec *pv = PTR_TO_DATAPTR(pp);
  struct VMPageRecord *vmpr;
  struct PageRef ref;
  unsigned n;

  if (arg_count_reg == 2)
    n = fx2int(ix);
  else
    n = 0;

  if ((char *)&pv->vec[n+1] - (char *)pv > SIZEOF_PTR(pp))
    scheme_error( "persistent-vector-ref: index ~d out of range 0..~d",
		  2, 
		  int2fx(n),
		  int2fx( (struct LocationRef *)
			     ((char *)pv + SIZEOF_PTR(pp)) - &pv->vec[0] ) );

  if (pv->vec[n].offset == 0)
    {
      REG0 = OBJ(pv->vec[n].base_page_num);
    }
  else
    {
      ref.base_page_num = pv->vec[n].base_page_num;
      ref.first = 1;
      ref.indirect = 0;
      ref.dirty = 0;
      ref.loaded = 0;
      ref.nth_page = pv->vec[n].nth_page;

      vmpr = get_vmpr( pv->owner, &ref );
      REG0 = map_pers_to_local( &vmpr, OBJ(pv->vec[n].offset) );
    }
  RETURN1();
})

(define-rstore-glue (make-persistent-addr-vector (area <allocation-area>) 
		     len)
 literals: ((& <persistent-addr-vector>))
{
  struct PAddrVec *pv;
  obj pp;
  int i, n = fx2int(len);

  pp = area->allocfn( (AllocArea *)area, 
		      TLREF(0), 
		      sizeof( struct PAddrVec )
		      + (n-1) * sizeof( struct LocationRef ) );

  pv = PTR_TO_DATAPTR(pp);
  pv->owner = area->owner;
  pv->spare = 0;

  for (i=0; i<n; i++)
    {
      pv->vec[i].base_page_num = VAL(FALSE_OBJ);
      pv->vec[i].offset = 0;
      pv->vec[i].nth_page = 0;
    }
  REG0 = pp;
  RETURN1();
})

(define-glue (persistent-addr-vector-set! pa_vec x tr)
 literals: ((& <persistent-addr>))
{
  struct PAddrVec *pv = PTR_TO_DATAPTR(pa_vec);
  int n;

  COUNT_ARGS(3);
  
  n = fx2int(x);

  if (OBJ_ISA_PTR(tr))
    {
      void *ptr;
      struct VMPageRecord *vmpr;
      
      ptr = (void *)VAL(tr);
      
      vmpr = addr_to_vm_page_record( pv->owner, (void *)VAL(tr) );
      if (vmpr)
	{
	  pv->vec[n] = create_LR_on_page( pv->owner, tr, vmpr );
	}
      else
	{
	  scheme_error( "persistent-addr-vector-set!: not a pointer into this persistent store: ~s",
		       1, tr );
	}
    }
  else
    {
     pv->vec[n] = create_immob_LR(tr);
    }
  RETURN0();
})

(define-glue (transient->persistent tr)
 literals: ((& <persistent-addr>))
{
  struct PAddrVec *pv;
  obj pp;

  pp = bvec_alloc( sizeof( struct PAddrVec ), TLREF(0) );
  pv = PTR_TO_DATAPTR(pp);

  if (OBJ_ISA_PTR(tr))
    {
      void *ptr;
      struct VMPageRecord *vmpr;

      ptr = (void *)VAL(tr);
      vmpr = find_owner_and_vmpr( ptr, &pv->owner );
      if (vmpr)
	{
	   pv->vec[0] = create_LR_on_page( pv->owner, tr, vmpr );
	}
      else
	{
	  scheme_error( "transient->persistent: not a transient pointer into a persistent store: ~s",
		       1, tr );
	}
    }
  else
    {
      scheme_error( "transient->persistent: not a pointer: ~s", 1, tr );
      pp = ZERO;
    }
  
  REG0 = pp;
  RETURN1();
})
