/*-----------------------------------------------------------------*-C-*---
 * File:    handc/runtime/dequeue.c
 *
 *          Copyright (C)1997 Donovan Kolbly <d.kolbly@rscheme.org>
 *          as part of the RScheme project, licensed for free use.
 *          See <http://www.rscheme.org/> for the latest information.
 *
 * File version:     1.12
 * File mod date:    2005-01-20 20:26:53
 * System build:     v0.7.3.4-b7u, 2007-05-30
 *
 *------------------------------------------------------------------------*/

#include <rscheme/scheme.h>
#include <rscheme/smemory.h>
#include <rscheme/allocns.h>

/*
 * example state:
 *
 *               0     1     2     3     4     5     6
 *            +-----+-----+-----+-----+-----+-----+-----+
 *            |     |     |  a  |  b  |  c  |     |     |
 *            +-----+-----+-----+-----+-----+-----+-----+
 *                        ^                 ^
 *                       front             back
 *                        =2                =5
 *   pop-front => a
 *   pop-back => c
 */

#define DEQ_STATE(deq)  gvec_ref((deq),SLOT(0))
#define DEQ_FRONT(deq)  gvec_ref((deq),SLOT(1))
#define DEQ_BACK(deq)  gvec_ref((deq),SLOT(2))

#define SET_DEQ_STATE(deq,s) gvec_set((deq),SLOT(0),(s))
#define SET_DEQ_FRONT(deq,f) gvec_write_non_ptr((deq),SLOT(1),(f))
#define SET_DEQ_BACK(deq,b) gvec_write_non_ptr((deq),SLOT(2),(b))

obj make_dequeue( void )
{
  return make3( dequeue_class,
		make_gvec( vector_class, SLOT(5), FALSE_OBJ ),
		ZERO,
		ZERO );
}

rs_bool dequeue_empty( obj deq )
{
  return EQ(DEQ_FRONT(deq),DEQ_BACK(deq));
}

/*
 * two cases to consider; wrapped, and unwrapped.
 *
 * in the (simpler) unwrapped case:
 *
 *               0     1     2     3     4     5     6
 *            +-----+-----+-----+-----+-----+-----+-----+
 *            |     |     |  a  |  b  |  c  |     |     |
 *            +-----+-----+-----+-----+-----+-----+-----+
 *                        ^ [0]   [1]   [2] ^
 *                       front             back
 *                        =2                =5
 *
 *               0     1     2     3     4     5     6
 *            +-----+-----+-----+-----+-----+-----+-----+
 *            |  c  |     |     |     |     |  a  |  b  |
 *            +-----+-----+-----+-----+-----+-----+-----+
 *              [2] ^                       ^ [0]   [1]
 *                 back                    front
 *                  =1                      =5
 *
 *  in general, the desired slot is at ((front + index) % length),
 *  if the slot is valid.
 *
 *  Note that these functions don't do scheme-level error checking;
 *  preconditions are intended to be checked at a higher level
 *  (however, we encode them in assertions for system debugging)
 *
 */

static UINT_32 offset_for_index( obj deq, obj fx_index )
{
  UINT_32 index = FXWORDS_TO_RIBYTES( fx_index );
  UINT_32 front = FXWORDS_TO_RIBYTES( DEQ_FRONT( deq ) );
  UINT_32 len = SIZEOF_PTR( DEQ_STATE( deq ) );

  assert( FIXNUM_P( fx_index ) );
  assert( FX_LT( fx_index, dequeue_count( deq ) ) );

  return (front + index) % len;
}

obj dequeue_ref( obj deq, obj index )
{
  UINT_32 offset = offset_for_index( deq, index );
  return gvec_ref( DEQ_STATE( deq ), offset );
}

obj dequeue_set( obj deq, obj index, obj item )
{
  UINT_32 offset = offset_for_index( deq, index );
  obj old = gvec_ref( DEQ_STATE( deq ), offset );

  gvec_set( DEQ_STATE( deq ), offset, item );
  return old;
}


obj dequeue_count( obj deq )
{
  obj len;

  len = FX_SUB( DEQ_BACK(deq), DEQ_FRONT(deq) );
  if (FX_LT(len,ZERO))
    return FX_ADD( len, RIBYTES_TO_FXWORDS(SIZEOF_PTR(DEQ_STATE(deq))) );
  else
    return len;
}

static obj expanded_state( obj deq, UINT_32 expand_bytes )
{
  obj state, len = dequeue_count(deq);
  UINT_32 i, j, end, lim;
  obj result;

  result = alloc( expand_bytes + FXWORDS_TO_RIBYTES(len), vector_class );

  state = DEQ_STATE(deq);
  j = 0;
  i = FXWORDS_TO_RIBYTES(DEQ_FRONT(deq));
  end = FXWORDS_TO_RIBYTES(DEQ_BACK(deq));
  lim = SIZEOF_PTR(state);

  while (i != end)
    {
      gvec_write_init( result, j, gvec_ref( state, i ) );
      j += SLOT(1);
      i += SLOT(1);
      if (i >= lim)
	i = 0;
    }
  while (expand_bytes > 0)
    {
      gvec_write_init_non_ptr( result, j, FALSE_OBJ );
      j += SLOT(1);
      expand_bytes -= SLOT(1);
    }
  return result;
}

obj dequeue_state( obj deq )
{
  return expanded_state( deq, 0 );
}

static void expand( obj deq )
{
  obj len = dequeue_count(deq);
  unsigned slen = SIZEOF_PTR( DEQ_STATE( deq ) );

  if (slen <= SLOT(10)) {
    SET_DEQ_STATE( deq, expanded_state( deq, SLOT(10) ) );
  } else {
    SET_DEQ_STATE( deq, expanded_state( deq, slen ) );
  }
  SET_DEQ_FRONT( deq, ZERO );
  SET_DEQ_BACK( deq, len );
}

static obj succ( obj deq, obj i )
{
  i = ADD1(i);
  if (EQ(i,RIBYTES_TO_FXWORDS(SIZEOF_PTR(DEQ_STATE(deq)))))
    return ZERO;
  else
    return i;
}

static obj pred( obj deq, obj i )
{
  if (EQ(i,ZERO))
    i = RIBYTES_TO_FXWORDS(SIZEOF_PTR(DEQ_STATE(deq)));
  return SUB1(i);
}

static void dequeue_is_empty( char *op )
{
  scheme_error( "~a: <dequeue> is empty", 1, make_string(op) );
}

#define assert_ne(deq,op) if (dequeue_empty(deq)) dequeue_is_empty( op )

void dequeue_push_back( obj deq, obj item )
{
  obj oldb = DEQ_BACK(deq);
  obj newb;
  newb = succ( deq, oldb );
  if (EQ(newb,DEQ_FRONT(deq)))
    {
      expand(deq);
      dequeue_push_back( deq, item );
    }
  else
    {
      gvec_set( DEQ_STATE(deq), FXWORDS_TO_RIBYTES(oldb), item );
      SET_DEQ_BACK( deq, newb );
    }
}

void dequeue_push_front( obj deq, obj item )
{
  obj oldf = DEQ_FRONT(deq);
  obj newf;
  newf = pred( deq, oldf );
  if (EQ(newf, DEQ_BACK(deq)))
    {
      expand(deq);
      dequeue_push_front( deq, item );
    }
  else
    {
      gvec_set( DEQ_STATE(deq), FXWORDS_TO_RIBYTES(newf), item );
      SET_DEQ_FRONT( deq, newf );
    }
}

obj dequeue_pop_back( obj deq )
{
  obj item, newb, oldb = DEQ_BACK(deq);
  assert_ne(deq, "dequeue-pop-back!");
  newb = pred( deq, oldb );
  SET_DEQ_BACK( deq, newb );
  item = gvec_ref( DEQ_STATE(deq), FXWORDS_TO_RIBYTES(newb) );
  /* clear the now-unused position in case there is a GC liveness issue
   * (ie, I was seeing annoying latencies finalizing threads in the
   * new threads system)
   */
  gvec_write_non_ptr( DEQ_STATE(deq), FXWORDS_TO_RIBYTES(newb), FALSE_OBJ );
  return item;
}

obj dequeue_pop_front( obj deq )
{
  obj item, newf, oldf = DEQ_FRONT(deq);
  assert_ne(deq, "dequeue-pop-front!");
  newf = succ( deq, oldf );
  SET_DEQ_FRONT( deq, newf );
  item = gvec_ref( DEQ_STATE(deq), FXWORDS_TO_RIBYTES(oldf) );
  gvec_write_non_ptr( DEQ_STATE(deq), FXWORDS_TO_RIBYTES(oldf), FALSE_OBJ );
  return item;
}

obj dequeue_memq( obj deq, obj item )
{
  int j = 0;
  UINT_32 i = FXWORDS_TO_RIBYTES( DEQ_FRONT( deq ) );
  UINT_32 b = FXWORDS_TO_RIBYTES( DEQ_BACK( deq ) );
  obj state = DEQ_STATE( deq );
  UINT_32 lim = SIZEOF_PTR(state);

  while (i != b) {
    if (EQ(gvec_read( state, i ),item)) {
      return int2fx( j );
    }
    i += SLOT(1);
    if (i >= lim) {
      i = 0;
    }
    j++;
  }
  return FALSE_OBJ;
}

int dequeue_delq( obj deq, obj item )
{
  int n = 0;
  UINT_32 f = FXWORDS_TO_RIBYTES( DEQ_FRONT( deq ) );
  UINT_32 b = FXWORDS_TO_RIBYTES( DEQ_BACK( deq ) );
  UINT_32 i, newb;
  obj state = DEQ_STATE( deq );
  UINT_32 lim = SIZEOF_PTR(state);

  newb = f;
  for (i=f; i != b;) {
    obj x;
    x = gvec_read( state, i );
    /*printf( "i %lu  f %lu  b %lu  lim %lu  x 0x%x item 0x%x\n", 
            i, f, b, lim, x, item );*/
    if (EQ( x, item )) {
      n++;
    } else {
      gvec_write( state, newb, x );
      newb += SLOT(1);
      if (newb >= lim) {
        newb = 0;
      }
    }
    i += SLOT(1);
    if (i >= lim) {
      i = 0;
    }
  }
                        
  SET_DEQ_BACK( deq, RIBYTES_TO_FXWORDS(newb) );
  while (newb != b) {
    gvec_write_non_ptr( state, newb, FALSE_OBJ );
    newb += SLOT(1);
    if (newb >= lim) {
      newb = 0;
    }
  }
  return n;
}
