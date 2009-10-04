/*-----------------------------------------------------------------*-C-*---
 * File:    handc/runtime/applyg.c
 *
 *          Copyright (C)1997 Donovan Kolbly <d.kolbly@rscheme.org>
 *          as part of the RScheme project, licensed for free use.
 *          See <http://www.rscheme.org/> for the latest information.
 *
 * File version:     1.5
 * File mod date:    1997-11-29 23:10:51
 * System build:     v0.7.3.4-b7u, 2007-05-30
 *
 * Purpose:          Generic function dispatch code (using a tiny hash table cache)
 *------------------------------------------------------------------------*/

#include <rscheme/vinsns.h>
#include <rscheme/runtime.h>
#include <rscheme/linktype.h>
#include <rscheme/scheme.h>
#include <rscheme/allocns.h>

#define CLASS_HASH_CODE  SLOT(5)
#define GF_CACHE_0_K     SLOT(4)
#define GF_VICTIM_K      SLOT(12)
#define GF_VICTIM_V       SLOT(13)
#define GF_CACHE_OVERFLOW SLOT(14)

#ifdef GF_DEBUG
FILE *foo = NULL;

static void gf_stat( obj gf )
{
  int i;
  obj p, k;

  fprintf( foo, "%s status: ",
	   symbol_text( gvec_ref( gf, SLOT(3) ) ) );
  for (i=0; i<4; i++)
    {
      k = gvec_ref( gf, SLOT(i*2) + GF_CACHE_0_K );
      if (truish(k))
	fprintf( foo, " %s", symbol_text(class_name(k)) );
      else
	fprintf( foo, " #f" );
    }
  fprintf( foo, " |" );
  k = gvec_ref( gf, GF_VICTIM_K );
  if (truish(k))
    fprintf( foo, " %s", symbol_text(class_name(k)) );
  else
    fprintf( foo, " #f" );

  fprintf( foo, " |" );
  for (p=gvec_ref(gf,GF_CACHE_OVERFLOW);truish(p);p=gvec_ref(p,0))
    {
      k = gvec_ref(p,SLOT(1));
      fprintf( foo, " %s", symbol_text(class_name(k)) );
    }
  fprintf( foo, "\n" );
}
#endif /* GF_DEBUG */

obj rs_gf_find_method( obj gf, obj rcvr )
{
  obj c, m, impl, h, k;
  UINT_32 k_ix, v_ix;

#ifdef GF_DEBUG
  if (!foo)
    foo = fopen( "/tmp/gf.trc", "w" );
#endif /* GF_DEBUG */

  c = object_class(rcvr);
  h = FX_AND(gvec_ref(c, CLASS_HASH_CODE),int2fx(3*2));
  
  k_ix = GF_CACHE_0_K + FXWORDS_TO_RIBYTES(h);
  v_ix = k_ix + SLOT(1);


  k = gvec_ref( gf, k_ix );

#ifdef GF_DEBUG
  gf_stat(gf);
#endif /* GF_DEBUG */
  if (EQ(k,c))
    return gvec_ref( gf, v_ix );
  else
    {
      /* check the victim entry */
      k = gvec_ref( gf, GF_VICTIM_K );
      if (EQ(k,c))
	{
	  m = gvec_ref( gf, GF_VICTIM_V );
	  
	  /* a hit -- victimize the primary entry
	   * (note: there is no way you can hit in the victim cache
	   *        if your primary entry is #f)
	   */
	  gvec_write_ptr( gf, GF_VICTIM_K, gvec_ref( gf, k_ix ) );
	  gvec_write_ptr( gf, GF_VICTIM_V, gvec_ref( gf, v_ix ) );
	  gvec_write_ptr( gf, k_ix, k );
	  gvec_write_ptr( gf, v_ix, m );
	  return m;
	}
      else
	{
	  obj ov, new_ov, prev = ZERO;
	 
#ifdef GF_DEBUG
	  fprintf( foo, "%s: check overflow for key = %s\n",
		   symbol_text( gvec_ref( gf, SLOT(3) ) ),
		   symbol_text( class_name( c ) ) );

	  if (truish( gvec_ref( gf, k_ix ) ))
	    fprintf( foo, "  primary[%d] => %s\n", 
		     fx2int(h),
		     symbol_text( class_name( gvec_ref( gf, k_ix ) ) ) );
	  if (truish(k))
	    fprintf( foo, "  victim => %s\n",
		     symbol_text( class_name( k ) ) );
#endif /* GF_DEBUG */
	  /* a primary miss -- check the overflow list */
	  for (ov = gvec_ref( gf, GF_CACHE_OVERFLOW );
	       !EQ(ov,FALSE_OBJ);
	       ov = gvec_ref( ov, SLOT(0) ) )
	    {
	      k = gvec_ref( ov, SLOT(1) );
#ifdef GF_DEBUG
	      fprintf( foo, "  overflow {%#x} => %s\n", ov, 
		       symbol_text( class_name( k ) ) );
#endif /* GF_DEBUG */
	      if (EQ(k,c))
		{
#ifdef GF_DEBUG
		  fprintf( foo, "  HIT (prev = {%#x})\n", prev );
#endif /* GF_DEBUG */
		  m = gvec_ref( ov, SLOT(2) );
		  /* found it in the overflow list... move this entry 
		   * to the primary cache area and spill the victim cache
		   */
		  new_ov = make3( vector_class,
				  (EQ(prev,ZERO)
				   ? gvec_ref( ov, SLOT(0) )
				   : gvec_ref( gf, GF_CACHE_OVERFLOW )),
				  gvec_ref( gf, GF_VICTIM_K ),
				  gvec_ref( gf, GF_VICTIM_V ) );
		  gvec_write_ptr( gf, GF_VICTIM_K, gvec_ref( gf, k_ix ) );
		  gvec_write_ptr( gf, GF_VICTIM_V, gvec_ref( gf, v_ix ) );
		  if (!EQ(prev,ZERO))
		    gvec_write( prev, SLOT(0), gvec_ref( ov, SLOT(0) ) );
		  gvec_write_ptr( gf, GF_CACHE_OVERFLOW, new_ov );
		  gvec_write_ptr( gf, k_ix, k );
		  gvec_write_ptr( gf, v_ix, m );
#ifdef GF_DEBUG
		  gf_stat(gf);
#endif /* GF_DEBUG */
		  return m;
		}
	      prev = ov;
	    }
#ifdef GF_DEBUG
	  fprintf( foo, "  MISS\n" );
#endif /* GF_DEBUG */
	  return FALSE_OBJ;
	}
    }
}

jump_addr rs_gf_dispatch( obj gf )
{
  obj m;

  if (arg_count_reg < 1)
    {
      scheme_error( "GF ~s called with no arguments", 1, gf );
    }

  m = rs_gf_find_method( gf, REG0 );
  if (EQ(m,FALSE_OBJ))
    {
      /* a miss -- call the fallback function  */
      COLLECT0();
      REG1 = REG0;
      REG0 = gf;
      arg_count_reg = 2;
      return apply( load_cache_and_call_proc );
    }
  else
    {
      return apply(m);
    }
}

jump_addr applyg( obj gf )
{
  /* if the template has been changed, call the full procedure
   * (this is for debugging, tracing, and introspection purposes...
   * we want to keep the protocol that pretends like the template
   * is always called, even if we optimize away that call in certain
   * cases
   */

  if (!EQ(gvec_ref(gf,SLOT(0)),gf_dispatch_template))
    return apply(gf);
  else
    return rs_gf_dispatch(gf);
}
