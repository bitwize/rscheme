#|------------------------------------------------------------*-Scheme-*--|
 | File:    modules/mathlib/random.scm
 |
 |          Copyright (C)1997 Donovan Kolbly <d.kolbly@rscheme.org>
 |          as part of the RScheme project, licensed for free use.
 |          See <http://www.rscheme.org/> for the latest information.
 |
 | File version:     1.10
 | File mod date:    2007-05-30 06:51:57
 | System build:     v0.7.3.4-b7u, 2007-05-30
 | Owned by module:  mathlib
 |
 | Purpose:          (random) implementation
 `------------------------------------------------------------------------|#

(define-class <random-state> (<object>))

(define-glue (make-random-state seed)
 literals: ((& <random-state>))
{
  UINT_32 s;
  unsigned i;
  obj result;

  COUNT_ARGS(1);
  if (!OBJ_ISA_FIXNUM(seed))
    {
      scheme_error( "(make-random-state ~s): bad arg", 1, seed );
    }
  s = VAL(rehash_fixnum(seed));

  REG0 = result = alloc( SLOT(12), TLREF(0) );
  gvec_write_init_non_ptr( result, SLOT(0), int2fx(1) );
  for (i=0; i<11; i++)
    {
      gvec_write_init_non_ptr( result, 
			        SLOT(i+1), 
			        OBJ((s & ~PRIMARY_TAG_MASK) + FIXNUM_TAG) );
      s = ((s >> 1) & ~3) + (((s+4) & 8) ? 0x80000000 : 0);
      s = ((s >> 1) & ~3) + (((s+4) & 8) ? 0x80000000 : 0);
      s = ((s >> 1) & ~3) + (((s+4) & 8) ? 0x80000000 : 0);
    }
  RETURN1();
})
  
(define-glue (next-random state range)
 literals: ((& <random-state>))
{
  obj index, next_index, z;

  if (arg_count_reg == 2)
    {
      if (!OBJ_ISA_FIXNUM(range))
	scheme_error( "next-random: range arg ~s invalid", 1, range );
      else if (fx2int(range) < 1)
	scheme_error( "next-random: range arg ~d out of range", 1, range );
    }
  else if (arg_count_reg != 1)
    scheme_error( "next-random: ~d args, expected 1 or 2", 
		  1, int2fx(arg_count_reg) );

  if (!OBJ_ISA_PTR_OF_CLASS(state,TLREF(0)))
    {
      scheme_error( "next-random: state arg ~s invalid", 1, state );
    }

  index = gvec_read( state, SLOT(0) );
  assert( OBJ_ISA_FIXNUM(index) );

  next_index = ADD1(index);
  if (FXWORDS_TO_RIBYTES(next_index) >= SIZEOF_PTR(state))
    next_index = int2fx(1);
  z = FX_ADD( gvec_read( state, FXWORDS_TO_RIBYTES(index) ),
	      gvec_read( state, FXWORDS_TO_RIBYTES(next_index) ) );
  z = OBJ(VAL(z) & 0x7FFFFFFF);
  assert( OBJ_ISA_FIXNUM(z) );

  assert( OBJ_ISA_FIXNUM(next_index) );
  gvec_write_non_ptr( state, FXWORDS_TO_RIBYTES(index), z );
  gvec_write_non_ptr( state, SLOT(0), next_index );

  if (arg_count_reg == 2)
    z = OBJ(VAL(z) % VAL(range));
  REG0 = z;
  RETURN1();
})

#|
  (let* (((i1 <fixnum>) (gvec-ref state 0))
	 ((i2 <fixnum>) (add1 i1))
	 ((i2 <fixnum>) (if (eq? i2 (gvec-length state))
			    1
			    i2)))
    (let ((z (bitwise-and (fixnum+ (gvec-ref state i1)
				   (gvec-ref state i2))
			  #x1fffffff)))
      (gvec-set! state i1 z)
      (gvec-set! state 0 i2)
      z))
|#

(define-class <random-float-state> (<object>) :bvec)

(define-safe-glue (make-random-float-state seed)
  literals: ((& <random-float-state>))
{
  obj s = alloc( sizeof(unsigned short) * 3, TLREF(0) );
  unsigned short *uss = PTR_TO_DATAPTR(s);

  uss[0] = crc_hash_int2( 0x12348765, VAL(seed) );
  uss[1] = crc_hash_int2( VAL(seed), 0xFACEF00D );
  uss[2] = crc_hash_int2( 0xb5c0fbcf, (uss[0] << 16) + uss[1] );
  REG0 = s;
  RETURN1();
})

(define-safe-glue (next-random-float (state <random-float-state>))
{
  double f;

#if HAVE_ERAND48
  f = erand48( (unsigned short *)PTR_TO_DATAPTR( state ) );
#else
#if HAVE_RANDOM
  f = (double)random() / (double)LONG_MAX;
#else
  f = (double)rand() / (double)RAND_MAX;
#endif
#endif
  REG0 = make_float( f );
  RETURN1();
})

;;;

(%early-once-only
 (define *default-random-state* (make-random-state 8901))
 (define *default-random-float-state* (make-random-float-state 462267161)))

;;;

(define-syntax random
  (syntax-form ()
    (next-random *default-random-state*))
  (syntax-form (range)
    (next-random *default-random-state* range))
  (else full-random))

(define (full-random . args)
  (if (null? args)
      (random)
      (next-random *default-random-state* (car args))))

;;;

(define (frandom #optional (state default: *default-random-float-state*))
  (next-random-float state))
