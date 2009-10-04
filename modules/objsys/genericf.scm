#|------------------------------------------------------------*-Scheme-*--|
 | File:    modules/objsys/genericf.scm
 |
 |          Copyright (C)1997 Donovan Kolbly <d.kolbly@rscheme.org>
 |          as part of the RScheme project, licensed for free use.
 |          See <http://www.rscheme.org/> for the latest information.
 |
 | File version:     1.18
 | File mod date:    2003-06-22 18:15:04
 | System build:     v0.7.3.4-b7u, 2007-05-30
 | Owned by module:  objsys
 |
 | Purpose:          Generic function dispatcher and find-method
 `------------------------------------------------------------------------|#

#|
   This is the default generic function dispatcher

   It's what gets called when a <single-dispatch-gf> procedure
   is called.
|#

(define-glue (generic-function-dispatch) :template
{
  return rs_gf_dispatch( envt_reg );
})

(define-safe-glue (get-gf-lru-histogram resetq)
{
  REG0 = collect_gf_cache_histogram( truish(resetq) );
  RETURN1();
})

(define-safe-glue (pre-compute-dispatch (gf <single-dispatch-gf>) rcvr #rest)
  literals: ((& load-cache))
{
  obj m;

  m = rs_gf_find_method( gf, rcvr );
  if (EQ(m,FALSE_OBJ))
    {
      COLLECT1();
      APPLYF( 2, TLREF(0) );
    }
  else
    {
      REG0 = m;
      RETURN1();
    }
})

(define-method call-gf-with-zero-args ((gf <generic-function>))
  (error "~s: generic function called with no arguments"
	 (generic-function-name gf)))

(define-method does-not-understand ((gf <object>) arg)
  ;; this should never be invoked, because there is another
  ;; handler for <generic-function>, which is the most generic
  ;; object that people should be trying to invoke generic functions
  ;; against.  However, just in case...
  (error "illegal does-not-understand"))

(define-method does-not-understand ((gf <generic-function>) (args <list>))
  (error "~s: generic function doesn't understand ~s\n(rest are ~s)"
	 (generic-function-name gf)
	 (car args) (cdr args)))

(%strategy ccode

(define-syntax (method-key-class m)
  (car (gvec-ref m 2)))

(define-syntax (method-function-specializers m)
  (gvec-ref m 2))

(define (find-method-by-class (gf <single-dispatch-gf>) class)
  (let loop ((i (generic-function-methods gf)))
    (if (pair? i)
	(if (subclass? class (method-key-class (car i)))
	    (car i)
	    (loop (cdr i)))
	#f)))

(define (find-method (gf <single-dispatch-gf>) (args <pair>))
  (find-method-by-class gf (object-class (car args))))

(define-syntax (fill-cache* gf args m withit)
  (let ((m (find-method-by-class gf (object-class (car args)))))
    (if m
	;;
	;; store it in the cache..
	;;
	(let* (((c <<class>>) (object-class (car args)))
	       ((ix <fixnum>) (fixnum+ 4 (bitwise-and (class-hash c) #b110))))
	  (let-syntax ((do-overflow (syntax-form ()
				      (set-gf-cache-overflow! 
				       gf
				       (%make <vector> 
					      (gf-cache-overflow gf)
					      (gf-cache-V-k gf)
					      (gf-cache-V-v gf)))))
		       (do-victim (syntax-form ()
				    (set-gf-cache-V-v! gf 
						       (gvec-ref gf (add1 ix)))
				    (set-gf-cache-V-k! gf 
						       (gvec-ref gf ix))))
		       (do-primary (syntax-form ()
				     (gvec-set! gf ix c)
				     (gvec-set! gf (add1 ix) m)
				     withit)))
	    (if (gvec-ref gf ix)
		(if (gf-cache-V-k gf)
		    (begin
		      (do-overflow)
		      (do-victim)
		      (do-primary))
		    (begin
		      (do-victim)
		      (do-primary)))
		(do-primary))))
        ;; NOTE: `does-not-understand' is itself a generic function,
        ;;       but we avoid the potential loop by ENSURING that
        ;;       does-not-understand has a method on <object>
	(does-not-understand gf args))))

(define (load-cache (gf <single-dispatch-gf>) (args <pair>))
  (fill-cache* gf args m m))

(define (load-cache-and-call (gf <single-dispatch-gf>) (args <pair>))
  (set-miss-count! gf (add1 (miss-count gf)))
  (fill-cache* gf args m (apply* args m)))
)

;;

(define (clear-gf-cache! (gf <single-dispatch-gf>))
  (set-gf-cache-0-k! gf #f) (set-gf-cache-0-v! gf #f)
  (set-gf-cache-1-k! gf #f) (set-gf-cache-1-v! gf #f)
  (set-gf-cache-2-k! gf #f) (set-gf-cache-2-v! gf #f)
  (set-gf-cache-3-k! gf #f) (set-gf-cache-3-v! gf #f)
  (set-gf-cache-V-k! gf #f) (set-gf-cache-V-v! gf #f)
  (set-gf-cache-overflow! gf #f))
