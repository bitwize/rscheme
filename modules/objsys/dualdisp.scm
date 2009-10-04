#|------------------------------------------------------------*-Scheme-*--|
 | File:    modules/objsys/dualdisp.scm
 |
 |          Copyright (C)1997 Donovan Kolbly <d.kolbly@rscheme.org>
 |          as part of the RScheme project, licensed for free use.
 |          See <http://www.rscheme.org/> for the latest information.
 |
 | File version:     1.5
 | File mod date:    1997-11-29 23:10:38
 | System build:     v0.7.3.4-b7u, 2007-05-30
 |
 | Purpose:          multi-method dispatch (NOT IMPLEMENTED YET)
 `------------------------------------------------------------------------|#

(define-class <multi-generic-function> (<generic-function>))

(define-class <cached-dual> (<object>)
  class-1
  class-2
  next-cached
  method-impl)
#|
(define-glue (generic-function-dual-dispatch)
  :template
  literals: ((& dual-cache-miss))
{
  obj c1, c2, m, impl;

  COUNT_ARGS(2);
  
  c1 = class_of(REG0);
  c2 = class_of(REG1);
  
  for (m=LEXREF0(1); !NULL_P(m); m=gvec_read(m,SLOT(2)))
    {
      if (EQ(gvec_read(m,SLOT(0)),c1)
	  && EQ(gvec_read(m,SLOT(1)),c2))
	{
	  APPLY( arg_count_reg, gvec_read(m,SLOT(3)) );
	}
    }
  REG2 = REG1;
  REG1 = REG0;
  REG0 = LEXREF0(0);
  APPLY( 3, TLREF(0) );
})
|#

(define dual-gf-proc 
  (let ((owner-gf #f)
	(cache '()))
    (lambda (a b)
      (let ((c1 (object-class a))
	    (c2 (object-class b)))
	(let loop ((i cache))
	  (if (null? i)
	      (dual-cache-miss owner-gf a b)
	      (if (and (eq? (class-1 i) c1)
		       (eq? (class-2 i) c2))
		  ((method-impl i) a b)
		  (loop (next-cached i)))))))))
      
(define generic-function-dual-dispatch (template dual-gf-proc))
  
(define (dual-cache-miss (gf <generic-function>) arg1 arg2)
  (let ((m (find-method gf (list arg1 arg2))))
    (if m
	(let ((gf (gvec-ref gf 0))
	      (c (make <cached-dual>
		       class-1: (object-class arg1)
		       class-2: (object-class arg2)
		       next-cached: (gvec-ref gfe 2)
		       method-impl: m)))
	  (gvec-set! gfe 2 c)
	  (m arg1 arg2))
	(does-not-understand gf (list arg1 arg2)))))

	    
