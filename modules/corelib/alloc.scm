#|------------------------------------------------------------*-Scheme-*--|
 | File:    modules/corelib/alloc.scm
 |
 |          Copyright (C)1997 Donovan Kolbly <d.kolbly@rscheme.org>
 |          as part of the RScheme project, licensed for free use.
 |          See <http://www.rscheme.org/> for the latest information.
 |
 | File version:     1.15
 | File mod date:    2003-10-22 12:53:54
 | System build:     v0.7.3.4-b7u, 2007-05-30
 | Owned by module:  corelib
 |
 | Purpose:          Heap-object allocation and manipulation functions
 `------------------------------------------------------------------------|#

(define-class <allocation-area> (<object>)
  heap-type: 5  ;; a mixvec (slot slot . uint-32)
  image-mode: 8)

(define-glue (make-transient-allocation-area)
  literals: ((& <allocation-area>))
{
  AllocArea *aa;

  REG0 = bvec_alloc( sizeof( AllocArea ), TLREF(0) );

  aa = (AllocArea *)PTR_TO_DATAPTR(REG0);
  aa->entry = FALSE_OBJ;
  aa->reserved = FALSE_OBJ;
  aa->allocfn = default_alloc_obj;
  aa->info = NULL;  /* transients are guaranteed to have NULL info */

  RETURN1();
})

(define-safe-glue (allocation-area-op (area <allocation-area>)
				      (opcode <raw-int>)
				      data)
{
  obj old = FALSE_OBJ;

  switch (opcode)
    {
    case 0: /* get-entry */
      old = area->entry;
      break;
    case 1: /* set-entry */
      old = area->entry;
      gvec_set( raw_area, SLOT(0), data );
      break;
    case 2: /* get-reserved */
      old = area->reserved;
      break;
    case 3: /* set-reserved */
      old = area->reserved;
      gvec_set( raw_area, SLOT(1), data );
      break;
    }
  REG0 = old;
  RETURN1();
})


(define (allocation-area-entry area)
 (allocation-area-op area 0 #f))

(define (set-allocation-area-entry! area entry)
 (allocation-area-op area 1 entry))

(define (allocation-area-reserved area)
 (allocation-area-op area 2 #f))

(define (set-allocation-area-reserved! area rsvd)
 (allocation-area-op area 3 rsvd))


(define-fluid *default-allocation-area*)

(%early-once-only
  (set! *default-allocation-area* (make-transient-allocation-area))
  (vector-set! (vector-ref (rscheme-global-ref 0) 0)
	       31
	       *default-allocation-area*))

(define-safe-glue (make-gvec (the_class <<class>>) #rest)
{
unsigned i;

    REG0 = alloc( SLOT(arg_count_reg-1), the_class );

    for (i=1; i<arg_count_reg; i++)
	gvec_write_init( REG0, SLOT(i-1), reg_ref(i) );
    
    RETURN(1);
})

(define-safe-glue (area-make-gvec (area <allocation-area>)
		                  (the_class <<class>>) #rest)
{
unsigned i;

    REG0 = alloc_in_area( area, the_class, SLOT(arg_count_reg-2) );

    for (i=2; i<arg_count_reg; i++)
      {
	gvec_write_non_ptr( REG0, SLOT(i-2), ZERO );
	/* note we can't use gvec_write_init here, because that's
	   only valid for the default alloc area */
	gvec_write( REG0, SLOT(i-2), reg_ref(i) );
      }
    
    RETURN(1);
})

;; like make-gvec, except the last argument is a list
;; which gets expanded (a la apply):
;;
;;   (define (make-gvec* class . args)
;;      (apply apply (cons* make-gvec class args)))
;;

(define-safe-glue (make-gvec* (the_class <<class>>) maybe_last #rest)
{
unsigned i, n;

    n = expand_last();
    REG0 = alloc( SLOT(n-1), the_class );
    
    for (i=1; i<n; i++)
	gvec_write_init( REG0, SLOT(i-1), reg_ref(i) );
    
    RETURN(1);
})

(define-safe-glue (area-make-gvec* (area <allocation-area>)
		                   (the_class <<class>>) maybe_last #rest)
{
unsigned i, n;

    n = expand_last();
    REG0 = alloc( SLOT(n-2), the_class );
    
    for (i=2; i<n; i++)
      {
	gvec_write_non_ptr( REG0, SLOT(i-2), FALSE_OBJ );
	gvec_write( REG0, SLOT(i-2), reg_ref(i) );
      }
    
    RETURN(1);
})


(define-safe-glue (area-clone (area <allocation-area>)
		              (new_class <<class>>)
		              source)
 literals: ("area-clone: ~s cannot be cloned into a ~s"
	    "area-clone: ~s cannot be cloned")
{
  obj newobj = FALSE_OBJ;

  if (!OBJ_ISA_PTR(source))
    {
      if (EQ(new_class,object_class(source)))
	newobj = source;
      else
	scheme_error( string_text(LITERAL(0)), 2, source, new_class );
    }
  else
    {
      int ht1 = fx2int( gvec_ref( CLASSOF_PTR(source), SLOT(1) ) );
      int ht2 = fx2int( gvec_ref( new_class, SLOT(1) ) );
      UINT_32 i, len = SIZEOF_PTR(source);

      if (ht1 != ht2)
	scheme_error( string_text(LITERAL(0)), 2, source, new_class );
	
      newobj = alloc_in_area( area, new_class, len );
      
      switch (ht1)
	{
	case 0:
	  /* gvec */
	  for (i=0; i<len; i+=SLOT(1))
	    {
	      gvec_write_non_ptr( newobj, i, FALSE_OBJ );
	      gvec_set( newobj, i, gvec_ref( source, i ) );
	    }
	  break;

	case 1:
	  /* bvec */
	  /* copy the bytes, including the whole last word */

	  memcpy( PTR_TO_DATAPTR(newobj), 
		  PTR_TO_DATAPTR(source), 
		  ((len - 1) | (sizeof(UINT_32)-1)) + 1 );
	  break;
	
	default:
	  scheme_error( string_text( LITERAL(1) ), 1, source );
	}
    }
  REG0 = newobj;
  RETURN1();
})

;;

(define-safe-glue (register-for-finalization thing)
{
   mark_as_finalizable(thing);
   RETURN0();
})

;;;  Return an integer representing the global GC cycle

(define-glue (gc-cycle-id)
{
   REG0 = get_gc_cycle_id();
   RETURN1();
})

(define-safe-glue (gc-work (amt <raw-int>))
{
  gc_work( (amt < 0) ? 0 : amt );
  RETURN0();
})

(define-glue (gc-now)
{
   gc_now();
   RETURN0();
})

(define-glue (live-object-stats)
{
   scheme_error( "live-stats: not implemented", 0 );
   RETURN0();
})

;; returns a vector of instances

(define-glue (all-instances of_class)
{
  flush_stack_cache();
  if (arg_count_reg < 2)
     gc_now();
  REG0 = all_instances(of_class);
  RETURN1();
})

;; return a vector of (instance . offset) pairs

(define-glue (all-pointers-to item)
{
  flush_stack_cache();
  if (arg_count_reg < 2)
     gc_now();
  REG0 = all_pointers_to(item);
  RETURN1();
})

;;; relocate/replace a collection of objects

(define-glue (relocate-objects tbl)
{
  REG0 = int2fx( rs_relocate_objects(tbl) );
  RETURN1();
})
