#|------------------------------------------------------------*-Scheme-*--|
 | File:    modules/corelib/apply.scm
 |
 |          Copyright (C)1997 Donovan Kolbly <d.kolbly@rscheme.org>
 |          as part of the RScheme project, licensed for free use.
 |          See <http://www.rscheme.org/> for the latest information.
 |
 | File version:     1.12
 | File mod date:    2005-02-25 13:27:18
 | System build:     v0.7.3.4-b7u, 2007-05-30
 | Owned by module:  corelib
 |
 | Purpose:          apply and friends
 `------------------------------------------------------------------------|#

;; 
;;
;;  apply and similar functionality
;;

;; %%Documentation: apply
;; %%Usage: (apply func (optional: (sequence: arg)) arg-list)
;; 
;; %%Description
;; apply applies a function (procedure) to some arguments.
;; The arguments passed to the function are the optional
;; args followed by the contents of arg-list.

(define-glue (apply)
{
unsigned i, n;
obj proc;

    COUNT_ARGS_AT_LEAST(2);
    proc = REG0;
    
    n = expand_last() - 1;
    
    /* shift the registers, because we are removing the
       'proc' arg from REG0
       (might want to optimize this for smaller n's...) */
    
    for (i=0; i<n; i++)
	reg_set( i, reg_ref(i+1) );

    APPLY( n, proc );
})

;;;
;;;  (funcall proc a b ...) == (proc a b ...)
;;;
;;;  (first-class combination)

(define-glue (funcall)
{
unsigned i, n;
obj proc;

    COUNT_ARGS_AT_LEAST(1);
    proc = REG0;
    
    n = arg_count_reg - 1;
    
    /* shift the registers, because we are removing the
       'proc' arg from REG0
       (might want to optimize this for smaller n's...) */
    
    for (i=0; i<n; i++)
	reg_set( i, reg_ref(i+1) );

    APPLY( n, proc );
})

(define-glue (apply-template)
{
unsigned i, n;
obj proc, tmpl;

    COUNT_ARGS_AT_LEAST(3);
    tmpl = REG0;
    proc = REG1;
    
    n = expand_last() - 2;
    
    /* shift the registers, because we are removing the
       'proc' arg from REG0
       (might want to optimize this for smaller n's...) */
    
    for (i=0; i<n; i++)
	reg_set( i, reg_ref(i+2) );

    APPLY_TMPL( n, proc, tmpl );
})

;; `standalone-template' is expecting to get clone'd and to have
;; it's first literal clobbered with a procedure, which this template
;; will call with two arguments: first, the function it was invoked
;; on behalf of, and second, a list of the arguments passed to it

(define-glue (standalone-template) :template
  literals: (#f)
{
obj caller = envt_reg;

   COLLECT0();
   REG1 = REG0;
   REG0 = caller;
   APPLY( 2, LITERAL(0) );
})

;;;  Yet another take on <template> hacking...
;;;
;;;  This is to support lazy code generation.
;;;
;;;    A lazy-flush-stub template looks like:
;;;
;;;
;;;      +------------+
;;;   [0]|         *--+------> lazy_flush_trampoline()
;;;      +------------+
;;;    1 | {fn_descr} |
;;;      +------------+
;;;    2 |         *--+------> ((function-scope lazy-flush-trampoline))
;;;      +------------+
;;;    3 |         *--+------> (& flush-and-call)
;;;      +------------+
;;;    4 | moi     *--+---\
;;;      +------------+    \   <template>
;;;   			    ->+------------+
;;;   			      |		 N |
;;;   			      +------------+
;;;   			      |		*--+---> #[<part-descr>]
;;;   			      +------------+
;;;   			      :		   :
;;;
;;;  The way it works is to call (flush-and-call) with the arguments
;;;  to this procedure (and this procedure itself, available in
;;;  `envt_reg').  Unless, that is, the template has already BEEN
;;;  flushed, which it can tell by looking at the real template's
;;;  code-pointer/fn-descr.
;;;
;;;  n.b., this may interact poorly with debugging...

(%early-once-only
 (define flush-and-call (lambda ())))

(define (set-flush-and-call-proc! proc)
  (set! flush-and-call proc))

(define-glue (lazy-flush-trampoline)
  :template
  literals: ((& flush-and-call) #t)
{
  obj moi = LITERAL(1);

  /*  check if the shadowed template has
   *  already been loaded (it may be visible
   *  through multiple closures, although
   *  on deeper thought I'm not sure exactly
   *  how that could happen; anyway,
   *  someone else may have triggered the flush)
   */

  if (!OBJ_ISA_PTR( gvec_ref( moi, SLOT(1) ) ) )
    {
      /*  update our procedure to point to the
       *  real template
       */
      gvec_set( envt_reg, SLOT(0), moi );
      APPLYF( arg_count_reg, envt_reg ); /* call our patched self */
    }
  else
    {
      /* flush all the code */
      /*   call the flusher with enough info to
       *   resume _this_ call, ie, our procedure
       *   and arguments
       */
      COLLECT0();
      REG1 = envt_reg;
      APPLYF( 2, TLREFB(0) );
    }
})

; APPLY* is analagous to apply, but is
; more efficient because the arguments don't have to be shifted
; (it takes the procedure argument as it's last argument,
;  rather than it's first)

(define-glue (apply*)
{
unsigned n;
obj proc;

    COUNT_ARGS_AT_LEAST(2);

    proc = reg_ref( --arg_count_reg );
    n = expand_last();
    
    APPLY( n, proc );
})

;; take a list and return the elements as multiple values

(define-glue (list->values)
{
unsigned n;

    n = expand_last();
    if (n == 0)
      RETURN0();
    else
      RETURN(n);
})

;;; R5RS


(define (full-values . lst)
  (list->values lst))

(define-syntax values
  (syntax-form args (%values . args))
  (else full-values))

(define-safe-glue (vector->values (v <vector>))
{
  unsigned n;

  n = SIZEOF_PTR(v) / SLOT(1);
  switch (n)
   {
     case 0: RETURN0();
             break;
     case 1: REG0 = gvec_ref( v, SLOT(0) );
             RETURN1();
             break;
     case 2: REG0 = gvec_ref( v, SLOT(0) );
             REG1 = gvec_ref( v, SLOT(1) );
             RETURN(2);
             break;
     case 3: REG0 = gvec_ref( v, SLOT(0) );
             REG1 = gvec_ref( v, SLOT(1) );
             REG2 = gvec_ref( v, SLOT(2) );
             RETURN(3);
             break;
    default: REG0 = gvec_ref( v, SLOT(0) );
             REG1 = gvec_ref( v, SLOT(1) );
             REG2 = gvec_ref( v, SLOT(2) );
	     {
	       unsigned i;

	       for (i=3; i<n; i++)
	        {
	          reg_set( i, gvec_ref( v, SLOT( i ) ) );
	        }
               RETURN(n);
             }
             break;
   }
})

(define-glue (backstop mark thunk)
{
  SAVE_CONT1( backstop_cont );
  APPLY( 0, thunk );
}
("backstop_cont" {
  /* This is a little evil; we saved 1 register
   * for marking purposes, but we're not restoring
   * any because we don't want to rearrange registers
   * to return whatever the thunk returned.
   */
  RESTORE_CONT0();
  if (arg_count_reg == 0) {
    RETURN0();
  } else {
    RETURN(arg_count_reg);
  }
}))

(define-syntax (values->list expr)
  (bind ((#rest rest expr)) rest))

;;

#| Documentation: (function identity)
 | Short: return it's only argument
 | Long: <<-+---
The identity function returns it's argument.
-+---
 |
 |#

(define (identity x)
  x)
