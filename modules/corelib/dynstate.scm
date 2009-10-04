#|------------------------------------------------------------*-Scheme-*--|
 | File:    modules/corelib/dynstate.scm
 |
 |          Copyright (C)1997 Donovan Kolbly <d.kolbly@rscheme.org>
 |          as part of the RScheme project, licensed for free use.
 |          See <http://www.rscheme.org/> for the latest information.
 |
 | File version:     1.11
 | File mod date:    2005-06-05 16:19:11
 | System build:     v0.7.3.4-b7u, 2007-05-30
 | Owned by module:  corelib
 |
 | Purpose:          dynamic state manipulation primitives
 `------------------------------------------------------------------------|#

;;;========================================================================

(define-method rewind ((self <object>))
  ;; do nothing... somebody is storing something else in the dynamic state
  )

(define-method unwind ((self <object>))
  ;; do nothing... somebody is storing something else in the dynamic state
  )

;;;========================================================================

(define-class <winding-protect> (<winding-contour>)
  unwinder
  winder
  winding-protected)

(define-method rewind ((self <winding-contour>))
  ((winder self)))

(define-method unwind ((self <winding-contour>))
  ((unwinder self)))

(define-syntax (unwind-protect* on-unwind on-wind protected thunk)
  (dynamic-call-thunk
   on-wind
   on-unwind
   thunk
   (cons (make-gvec <winding-protect>
		    on-unwind
		    on-wind
		    protected)
	 (get-dynamic-state-reg))
   (get-thread-state-reg)))

(define-syntax (unwind-protect protected-form . cleanup-forms)
  (unwind-protect* (lambda () (begin . cleanup-forms))
		   #f ;; no rewind action
		   'unwind-protect
		   (lambda () 
		     protected-form)))

;;; R5RS

(define (dynamic-wind before thunk after)
  (unwind-protect* after
                   before
                   'dynamic-wind
                   thunk))
                   
;;;

;; ATTENTION
;; 
;;  the following function relies on knowledge of the exact structure
;;  of the captured continuation escape procedure defined above
;;

(define (continuation->apply-before-continuing full-continuation)
  (let ((e (environment full-continuation)))
    (make-resumer
     (make <closure>
	   environment: (environment (gvec-ref e 1))
	   template: restore-then-call)
     (gvec-ref e 2))))

(define (apply-before-continuing full-continuation (proc <function>) . args)
  (apply* proc 
	  args
	  (continuation->apply-before-continuing full-continuation)))



(define-glue (continuer) :template
{
    USE_FUNCTION_ENVT();
    continuation_reg = LEXREF0(0);
    if (rsprof_active)
       rsprof_contn_restored(continuation_reg);
    dynamic_state_reg = LEXREF0(1);
    thread_state_reg = LEXREF0(2);
    if (arg_count_reg == 0)
	RETURN0();
    else
	RETURN(arg_count_reg);
})

(define-class <one-shot-recontinued> (<error>)
  continuation)

(define-method display-object ((self <one-shot-recontinued>) port)
  (__format port "Cannot re-continue one-shot continuation ~#@50s\n"
            (continuation self)))

(define (recontinue-error who)
  (error (make <one-shot-recontinued>
               continuation: who)))
                
(define-glue (one-shot-continuer) :template
  literals: ((& recontinue-error))
{
  obj cr;
  obj me = envt_reg;

  USE_FUNCTION_ENVT();

  cr = LEXREF0(0);
  if (EQ( cr, FALSE_OBJ )) {
    REG0 = me;
    APPLY( 1, TLREF(0) );
  } else {
    continuation_reg = cr;
    LEXSET0( 0, FALSE_OBJ );

    if (rsprof_active) {
       rsprof_contn_restored( continuation_reg );
    }
    dynamic_state_reg = LEXREF0( 1 );
    thread_state_reg = LEXREF0( 2 );

    if (arg_count_reg == 0) {
      RETURN0();
    } else {
      RETURN( arg_count_reg );
    }
  }
})

(define-glue (restore-then-call) :template
{
unsigned i, n;
obj proc;

    USE_FUNCTION_ENVT();
    continuation_reg = LEXREF0(0);
    dynamic_state_reg = LEXREF0(1);
    thread_state_reg = LEXREF0(2);

    COUNT_ARGS_AT_LEAST(1);
    proc = REG0;
    n = arg_count_reg - 1;
    
    /*  shift the registers, because we are removing the
     *  'proc' arg from REG0, just like (apply) does
     */
    
    for (i=0; i<n; i++)
	reg_set( i, reg_ref(i+1) );

    APPLY( n, proc );
})

(define-glue (proc->low-level-contn proc)
  literals: ((& continuer))
{
  obj contn = alloc( SLOT(CONT_FIXED+1), partcont_class );
#if CONT_FIXED != 4
#error oops: CONTN_FIXED != 4
#endif
  gvec_write_init( contn, SLOT(0), FALSE_OBJ );    /* envt_reg */
  gvec_write_init( contn, SLOT(1), literals_reg ); /* literals_reg */
  gvec_write_init( contn, SLOT(2), JUMP_ADDR_TO_OBJ(proc2contn) );
  gvec_write_init( contn, SLOT(3), FALSE_OBJ );    /* continuation_reg */
  gvec_write_init( contn, SLOT(CONT_FIXED), proc );

  if (rsprof_active)
     rsprof_contn_captured( continuation_reg );

  BIND3( contn, dynamic_state_reg, thread_state_reg );
  REG0 = make_closure( envt_reg, TLREF(0) /* continuer */ );
  RETURN1();
}
("proc2contn" {
  obj proc;
  proc = PARTCONT_REG(0);
  RESTORE_CONT_REG();
  APPLY(arg_count_reg,proc);
}))

(define-glue (low-level-call/cc proc onesh) literals: ((& continuer)
                                                       (& one-shot-continuer))
{
  obj the_proc = proc;
  obj use_continuer;

  if (arg_count_reg == 2) {
    if (truish( onesh )) {
      use_continuer = TLREF( 1 );   /* one-shot-continuer */
    } else {
      use_continuer = TLREF( 0 );   /* continuer */
    }
  } else {
    COUNT_ARGS(1);
    use_continuer = TLREF( 0 );   /* continuer */
  }

  USE_EMPTY_ENVT();

  flush_stack_cache();	/* flush the stack cache, if any */
  if (rsprof_active) {
    rsprof_contn_captured( continuation_reg );
  }

  BIND3( continuation_reg, dynamic_state_reg, thread_state_reg );
  REG0 = make_closure( envt_reg, use_continuer );
  APPLY( 1, the_proc );
})

(define-glue (dynamic-call-thunk pre_thunk post_thunk body_thunk new_ds new_ts)
{
  USE_EMPTY_ENVT();
  BEGIN_BIND6();
    BIND_ARG(0,body_thunk);
    BIND_ARG(1,post_thunk);
    BIND_ARG(2,new_ds);
    BIND_ARG(3,new_ts);
    BIND_ARG(4,dynamic_state_reg);
    BIND_ARG(5,thread_state_reg);
  END_BIND

  if (truish(pre_thunk))
   {
     SAVE_CONT0(dct_2);
     APPLY(0,pre_thunk);
   }
  else
   {
     SAVE_CONT0(dct_3);
     dynamic_state_reg = new_ds /* new_ds */;
     thread_state_reg = new_ts /* new_ts */;
     APPLY(0,LEXREF0(0) /*body_thunk*/);
   }
}

("dct_2" {
  RESTORE_CONT0();
  SAVE_CONT0(dct_3);
  dynamic_state_reg = LEXREF0(2) /* new_ds */;
  thread_state_reg = LEXREF0(3) /* new_ts */;
  APPLY(0,LEXREF0(0) /*body_thunk*/);
})

("dct_3" {
  obj post;

  RESTORE_CONT0();
  dynamic_state_reg = LEXREF0(4) /* saved_ds */;
  thread_state_reg = LEXREF0(5) /* saved_ts */;
  post = LEXREF0(1) /*post_thunk*/;
  if (EQ(post, FALSE_OBJ))
   {
     /* if arg_count_reg=0, then REG0 was already set to #f
        by whoever returned to the dct_3 continuation */
     RETURN(arg_count_reg);
   }
  else if (arg_count_reg == 1)
   {
    /* special-case returning a single arg from body */
    SAVE_CONT1(dct_5);
    APPLY(0,post);
   }
  else if (arg_count_reg == 0)
   {
    /* special-case returning a *no* values from body */
    SAVE_CONT0(dct_6);
    APPLY(0,post);
   }
  else
   {
     COLLECT0();  /* stores result into REG0 */
     SAVE_CONT1(dct_4);
     APPLY(0,post);
   }
})

("dct_5" {
  RESTORE_CONT1();
  RETURN1();
})

("dct_6" {
  RESTORE_CONT0();
  RETURN0();
})

("dct_4" {
  unsigned n;

  RESTORE_CONT1();
  arg_count_reg = 1;
  n = expand_last();

  RETURN(n);    /* above logic and use of dct_5, dct_5 ensures that n>=2 */
}))


;;;


(define-glue (find-common-ancestor from to)
{
  extern obj find_common_ancestor( obj f, obj t );
  REG0 = find_common_ancestor( from, to );
  RETURN1();
})

(define-glue (wind-fluid-tlv-contour ftlc)
{
  extern void wind_fluid_tl_contour( obj item );
  wind_fluid_tl_contour( ftlc );
  RETURN0();
})

(define-glue (unwind-fluid-tlv-contour ftlc)
{
  extern void unwind_fluid_tl_contour( obj item );
  unwind_fluid_tl_contour( ftlc );
  RETURN0();
})

(define-glue (do-unwind oldstate common)
  literals: ((& <fluid-tl-contour>)
	     (& unwind))
{
  /* extract the common cell from the ancestor descriptor */
  common = gvec_ref(common,SLOT(0));
  JUMP(2,do_unwind_1);
}

("do_unwind_1" {
  extern void unwind_fluid_tl_contour( obj ftlc );
  obj s = oldstate;
again:
  if (EQ(s,common))
    {
      RETURN0();
    }
  else
    {
      obj item = pair_car(s);
      s = pair_cdr(s);

      if (OBJ_ISA_PTR_OF_CLASS(item,TLREF(0)))
	{
	  unwind_fluid_tl_contour(item);
	  goto again;
	}
      else if (VECTOR_P(item))
	{
          /* ignore <vector> nodes -- they are debugging info */
          goto again;
        }
      else
	{
	  oldstate = s;
	  SAVE_CONT2(do_unwind_cont);
	  REG0 = item;
	  APPLY(1,TLREF(1));
	}
    }
})

("do_unwind_cont" {
  RESTORE_CONT2();
  BJUMP(2,do_unwind_1);
}))

;;;
;;;   "forward" dynamic state processor.
;;;
;;;   executes the (re)winding functions necessary
;;;   to get from the common ancestor of `from' and `to' to the
;;;   `to' cell, in the order *from* the common ancestor
;;;
;;;   those points along the path are stored in the ancestor descriptor
;;;   vector
;;;

(define-glue (do-rewind ancestor_descr)
  literals: ((& <fluid-tl-contour>)
	     (& rewind))
{
  /* figure out how many points along the path we have to traverse */
  REG1 = SUB1(RIBYTES_TO_FXWORDS(SIZEOF_PTR(ancestor_descr)));
  assert(OBJ_ISA_FIXNUM(REG1));
  JUMP(2,do_rewind_1);
}

("do_rewind_1" {
  extern void wind_fluid_tl_contour( obj ftlc );
  obj ix = REG1;
  obj ftlcc = TLREF(0);

again:
  assert(OBJ_ISA_FIXNUM(ix) && FX_GE(ix,ZERO));
  if (EQ(ix,ZERO))
    {
      RETURN0();
    }
  else
    {
      obj item = gvec_ref( ancestor_descr, FXWORDS_TO_RIBYTES(ix) );
      ix = SUB1(ix);

      if (OBJ_ISA_PTR_OF_CLASS(item,ftlcc))
	{
	  wind_fluid_tl_contour(item);
	  goto again;
	}
      else
	{
	  REG1 = ix;
	  SAVE_CONT2(do_rewind_cont);
	  REG0 = item;
	  APPLY(1,TLREF(1));
	}
    }
})

("do_rewind_cont" {
  RESTORE_CONT2();
  BJUMP(2,do_rewind_1);
}))

(%strategy ccode

(define (make-resumer saved-contn saved-dynamic-envt)
  (lambda 'saved-continuation args
    (let ((ds (get-dynamic-state-reg)))
      (if (eq? ds saved-dynamic-envt)
	  (apply* args saved-contn)
	  (let ((ancestor-descr (find-common-ancestor ds saved-dynamic-envt)))
	    (do-unwind ds ancestor-descr)
	    (do-rewind ancestor-descr)
	    (apply* args saved-contn))))))

(define (call-with-current-continuation proc)
  (low-level-call/cc
   (lambda (cc)
     (proc (make-resumer cc (get-dynamic-state-reg))))))

(define (call-with-current-continuation/one-shot proc)
  (low-level-call/cc
   (lambda (cc)
     (proc (make-resumer cc (get-dynamic-state-reg))))
   #t))

(define (close-over-dynamic-state proc)
  (let ((saved-ds (get-dynamic-state-reg))
	(ll-proc (proc->low-level-contn proc)))
    (lambda args
      (let ((ds (get-dynamic-state-reg)))
	(if (not (eq? ds saved-ds))
	    (let ((ancestor-descr (find-common-ancestor ds saved-ds)))
	      (do-unwind ds ancestor-descr)
	      (do-rewind ancestor-descr)))
	(apply* args ll-proc)))))
)
