/*-----------------------------------------------------------------*-C-*---
 * File:    handc/runtime/fluid.c
 *
 *          Copyright (C)1997 Donovan Kolbly <d.kolbly@rscheme.org>
 *          as part of the RScheme project, licensed for free use.
 *          See <http://www.rscheme.org/> for the latest information.
 *
 * File version:     1.3
 * File mod date:    1997-11-29 23:10:50
 * System build:     v0.7.3.4-b7u, 2007-05-30
 *
 *------------------------------------------------------------------------*/

#include <rscheme/scheme.h>
#include <rscheme/smemory.h>
#include <rscheme/allocns.h>

/* support for fluid variables 
   (this style of fluid variable is deprecated in 0.9, but we
   need some context-switch performance improvement in the meantime...)
 */

/* winding a fluid TL contour;
   store TL bindings in `saved' values
   and load `inside' values into TL bindings
*/

#define FTLC_BINDINGS        SLOT(0)
#define FTLC_INSIDE_VALUES   SLOT(1)
#define FTLC_SAVED_VALUES    SLOT(2)

static void shift_tl_values( obj var_vec, obj save_into, obj load_from )
{
  UINT_32 i, sz;
  obj tlv;

  sz = SIZEOF_PTR(var_vec);
  for (i=0; i<sz; i+=SLOT(1))
    {
      tlv = gvec_ref( var_vec, i );
#if 0
      fprintf( stdout, "updating %s ", symbol_text( gvec_ref(tlv,SLOT(0)) ) );
      fprinto( stdout, gvec_ref(tlv,SLOT(1)) );
      fprintf( stdout, " ==> " );
      fprinto( stdout, gvec_ref(load_from,i) );
      fprintf( stdout, "\n" );
#endif
      gvec_write( save_into, i, gvec_ref(tlv,SLOT(1)) );
      gvec_write( tlv, SLOT(1), gvec_ref(load_from,i) );
    }
}

void wind_fluid_tl_contour( obj ftlc )
{
  shift_tl_values( gvec_ref( ftlc, FTLC_BINDINGS ),
		   gvec_ref( ftlc, FTLC_SAVED_VALUES ),
		   gvec_ref( ftlc, FTLC_INSIDE_VALUES ) );
}

void unwind_fluid_tl_contour( obj ftlc )
{
  shift_tl_values( gvec_ref( ftlc, FTLC_BINDINGS ),
		   gvec_ref( ftlc, FTLC_INSIDE_VALUES ),
		   gvec_ref( ftlc, FTLC_SAVED_VALUES ) );
}

/* returns an "ancestor descriptor", which is a vector
 * whose first element is the common cell and whose remaining
 * elements are the car's of the chain from the common cell
 * to the `to' cell (not including the common cell).
 *
 *  this technique costs an allocation at context switch time,
 *  but is MUCH easier than avoiding the mass of continuations
 *  that would be allocated in the rewinder
 */

obj find_common_ancestor( obj from, obj to )
{
  obj f, t, v, k;
  unsigned n;

  if (EQ(from,to))
    return make1( vector_class, from );

  /* this is not MP safe! */

  /* mark the cells in the `from' list */

  for (f=from; PAIR_P(f); f=pair_cdr(f))
    {
      PTR_TO_HDRPTR(f)->pob_size += 1;
    }

  /* crawl up the `to' list looking for something with the bit set */

  n = SLOT(1);
  for (t=to; PAIR_P(t); t=pair_cdr(t))
    {
      if (PTR_TO_HDRPTR(t)->pob_size & 1)
	break;
      if (!VECTOR_P(pair_car(t)))
	n += SLOT(1);
    }

/* found a common ancestor or ran out of `to' list; 
 * in either case, `n' is the number of slots we need
 * in the ancestor vector (initially one for the common
 * cell, and incremented once for each value we'll put
 * in it)
 */

 /*
  * remove the marks from the `from' list...
  */
  for (f=from; PAIR_P(f); f=pair_cdr(f))
    {
      PTR_TO_HDRPTR(f)->pob_size -= 1;
    }

  /* construct the ancestor descriptor */
  
  v = alloc( n, vector_class );
  gvec_write_init( v, SLOT(0), t );
  
  for (n=SLOT(1), k=to; !EQ(k,t); k=pair_cdr(k))
    {
      if (!VECTOR_P(pair_car(k)))
	{
	  gvec_write_init( v, n, pair_car(k) );
	  n += SLOT(1);
	}
    }
  return v;
}
