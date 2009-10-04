#|------------------------------------------------------------*-Scheme-*--|
 | File:    modules/lowscm/vectors.scm
 |
 |          Copyright (C)1997 Donovan Kolbly <d.kolbly@rscheme.org>
 |          as part of the RScheme project, licensed for free use.
 |          See <http://www.rscheme.org/> for the latest information.
 |
 | File version:     1.10
 | File mod date:    1998-12-19 22:54:01
 | System build:     v0.7.3.4-b7u, 2007-05-30
 | Owned by module:  low-scheme
 |
 | Purpose:          general vector operations
 `------------------------------------------------------------------------|#


#|
   in 0.6.0/56, these are placed into primops

(define (vector-set! (vector <vector>) (index <fixnum>) item)
  (if (and (fixnum>=? index 0)
	   (fixnum<? index (gvec-length vector)))
      (gvec-set! vector index item)
      (range-error vector-set! 0 (gvec-length vector) item)))

(define (vector-ref (v <vector>) (i <fixnum>))
  (if (and (fixnum>=? i 0)
	   (fixnum<? i (gvec-length v)))
      (gvec-ref v i)
      (range-error vector-ref 0 (gvec-length v) i)))
|#

(define-glue (make-vector vec_length vec_fill) 
    literals: ("make-vector: argument `vec_length' is not a fixnum: ~s")
{
UINT_32 i, len;
obj fill = FALSE_OBJ;

    if (arg_count_reg != 1)
    {
	if (arg_count_reg == 2)
	    fill = vec_fill;
	else
	    wrong_num_args_range( FUNCTION, 1, 2 );
    }

    if (!OBJ_ISA_FIXNUM(vec_length))
	scheme_error( string_text(LITERAL(0)), 1, vec_length );

    len = FXWORDS_TO_RIBYTES(vec_length);
    REG0 = make_gvec( vector_class, len, fill );
    RETURN(1);
})

;;; if the list is small, this does it fast -- otherwise,
;;; it punts to a slower but safer implementation
;;; (`big-list->vector'); in particular, if the given
;;; list is cyclic, the safe implementation will not lock
;;; the system indefinitely.

(define-glue (list->vector list)
  literals: ((& big-list->vector)
	     (& signal-improper-list))
{
UINT_32 len;
obj lp;

    len = 0;
    for (lp=list; (len < SLOT(1000)) && PAIR_P( lp ); lp=pair_cdr( lp ))
      {
	len += sizeof(obj);
      }
    if (PAIR_P( lp ))
      {
        APPLY( 1, TLREFB( 0 ) ); /* (big-list->vector list) */
      }
    else if (NULL_P(lp))
      {
        obj vec = alloc( len, vector_class );
        UINT_32 i = 0;

        for (lp=list; !NULL_P(lp); lp=pair_cdr( lp ), i += SLOT(1))
	  {
            gvec_write_init( vec, i, pair_car( lp ) );
          }
        REG0 = vec;
        RETURN1();
      }
    else
      {
        REG0 = list;
	REG1 = lp;
        APPLY( 2, TLREFB( 1 ) );  /* (signal-improper-list list at) */
      }
})

;;; notice there may still be significant latency here --
;;;
;;;    (list->vector (range 2300000))
;;;
;;; (assuming you had enough memory!!) will lock a 200MHz PPro
;;;  for about a second.  However, we have little choice -- we can't
;;;  have a safe point until we've initialized the entire vector!)

(define-glue (big-list->vector* list n)
{
  obj lp, vec = alloc( FXWORDS_TO_RIBYTES( n ), vector_class );
  UINT_32 i = 0;

  for (lp=list; !NULL_P(lp); lp=pair_cdr( lp ), i += SLOT(1))
    {
      gvec_write_init( vec, i, pair_car( lp ) );
    }
  REG0 = vec;
  RETURN1();
})

(define (big-list->vector list)
  (big-list->vector* list (length list)))

(define-glue (vector-fill! vector fill)
{
UINT_32 i, len;

    COUNT_ARGS(2);
    len = SIZEOF_PTR(vector);
    
    if (OBJ_ISA_PTR(fill))
    {
	for (i=0; i<len; i+=SLOT(1))
	    gvec_write_ptr( vector, i, fill );
    }
    else
    {
	for (i=0; i<len; i+=SLOT(1))
	    gvec_write_non_ptr( vector, i, fill );
    }
    RETURN1();
})


(define-glue (vector)
{
unsigned i;
obj old0;

    old0 = REG0;
    REG0 = alloc( SLOT(arg_count_reg), vector_class );
    switch (arg_count_reg)
    {
	default:
	         for (i=10; i<arg_count_reg; i++)
		  gvec_write_init( REG0, SLOT(i), REG(i) );
	case 10: gvec_write_init( REG0, SLOT(9), REG9 );
	case  9: gvec_write_init( REG0, SLOT(8), REG8 );
	case  8: gvec_write_init( REG0, SLOT(7), REG7 );
	case  7: gvec_write_init( REG0, SLOT(6), REG6 );
	case  6: gvec_write_init( REG0, SLOT(5), REG5 );
	case  5: gvec_write_init( REG0, SLOT(4), REG4 );
	case  4: gvec_write_init( REG0, SLOT(3), REG3 );
	case  3: gvec_write_init( REG0, SLOT(2), REG2 );
	case  2: gvec_write_init( REG0, SLOT(1), REG1 );
	case  1: gvec_write_init( REG0, SLOT(0), old0 );
	case  0: /* the empty vector */;
    }
    RETURN1();
})

(define-syntax vector-length
;;not working in current compiler...
;;  (syntax-form ((x :: <vector>))
;;    (gvec-length x))
  (syntax-form (x)
    (gvec-length (check-vector x)))
  (else
   full-vector-length))

(define (full-vector-length (v <vector>))
  (gvec-length v))

(define (vector->list (v <vector>))
  (let loop (((i <fixnum>) (vector-length v))
	     (r '()))
    (if (eq? i 0)
	r
	(loop (sub1 i) (cons (gvec-ref v (sub1 i)) r)))))


(define-glue (vector-append)
{
  obj r, v, totlen = ZERO;
  UINT_32 i, j, k;

  for (i=0; i<arg_count_reg; i++)
    {
      v = reg_ref(i);
      if (VECTOR_P(v))
	totlen = FX_ADD( totlen, RIBYTES_TO_FXWORDS(SIZEOF_PTR(v)) );
      else
	scheme_error( "vector-append: arg ~d is not a vector: ~s",
		      2, int2fx(i), v );
    }
  r = alloc( FXWORDS_TO_RIBYTES(totlen), vector_class );
  k = 0;
  for (i=0; i<arg_count_reg; i++)
    {
      v = reg_ref(i);
      for (j=0; j<SIZEOF_PTR(v); j+=SLOT(1))
	{
	  gvec_write_init( r, k, gvec_ref( v, j ) );
	  k += SLOT(1);
	}
    }
  REG0 = r;
  RETURN1();
})


(define-syntax subvector
  (syntax-form (vec offset)
    (let (((temp <vector>) vec))
      (subvector* temp offset (vector-length temp))))
  (syntax-form (vec offset limit)
    (subvector* vec offset limit))
  (else
   full-subvector))

(define (full-subvector (v <vector>) (off <fixnum>) . opt)
  (if (null? opt)
      (subvector* v off (vector-length v))
      (subvector* v off (car opt))))

(define-syntax vector-slice
  (syntax-form (vec offset)
    (subvector vec offset))
  (syntax-form (vec offset len)
    (let (((temp <fixnum>) offset))
      (subvector* vec temp (fixnum+ temp len))))
  (else
   full-vector-slice))

(define (full-vector-slice (v <vector>) (off <fixnum>) . opt)
  (if (null? opt)
      (subvector* v off (vector-length v))
      (subvector* v off (fixnum+ off (car opt)))))

(define-method to-string ((self <vector>))
  (string-append "#("
		 (string-join " " 
			      (vector->list
			       (vector-map to-string self)))
		 ")"))
