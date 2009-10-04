/*-----------------------------------------------------------------*-C-*---
 * File:    handc/runtime/stakcach.c
 *
 *          Copyright (C)1997 Donovan Kolbly <d.kolbly@rscheme.org>
 *          as part of the RScheme project, licensed for free use.
 *          See <http://www.rscheme.org/> for the latest information.
 *
 * File version:     1.9
 * File mod date:    2005-03-04 21:01:27
 * System build:     v0.7.3.4-b7u, 2007-05-30
 *
 * Purpose:          stack-cache stack implementation
 *------------------------------------------------------------------------*/

#include <rscheme/vinsns.h>
#include <rscheme/scheme.h>

#ifdef STACK_CACHE

obj the_stack_cache[CACHE_SIZE];
obj *cache_iteration;

#ifndef INLINES
#include "stakcach.ci"
#endif /* INLINES */

void init_stack_cache( void )
{
}

/*  FLUSH_STACK_CACHE() -- Does the obvious thing, which is flush the stack
    cache.  In effect, it guarantees that the current continuation
    is a REAL object, flushing the cache sufficiently to ensure the
    truth of this.
*/

void flush_stack_cache( void )
{
UINT_32 cr = VAL(continuation_reg);
#define x  ((obj *)(cr-POINTER_TAG))
obj n, prev, next, first;
rs_bool more;
UINT_32 i, size_in_bytes;

    if (!in_stack_cache(continuation_reg))
	return; /* nothing to do... already a real object */

#ifdef STEP_DUMP
    {
    extern FILE *step_dump_file;
    extern int do_step_dump;
    extern void touch_step_dump_file( void );
    
	if (do_step_dump && step_dump_file)
	{
	    touch_step_dump_file();
	    fprintf( step_dump_file, "** Flushing stack cache **\n" );
	}
    }
#endif /* STEP_DUMP */

    more = YES;
    prev = ZERO;
    first = ZERO;	/* suppress warning */
    do {
	next = x[3];
	if (in_stack_cache(next))
	{
	    size_in_bytes = VAL(next) - cr;
	}
	else
	{
	    size_in_bytes = cache_upper_limit - cr;
	    more = NO;
	}
	n = alloc( size_in_bytes, partcont_class );
	if (EQ(prev,ZERO))
	    first = n;
	else
	  {
	    /* it's not obvious, but this *is* an initializing write,
	       because we only fill in SLOT(3) of an allocated
	       <part-cont> here or outside of the loop 
	       */
	    gvec_write_init_ptr( prev, SLOT(3), n );
	  }
	gvec_write_init( n, SLOT(0), x[0] );   /* saved envt_reg */
	gvec_write_init( n, SLOT(1), x[1] );   /* saved literals_reg */
	gvec_write_init( n, SLOT(2), x[2] );   /* saved jump_addr */
	for (i=SLOT(4); i<size_in_bytes; i+=SLOT(1))
	{
	    gvec_write_init( n, i, *(obj *)(i+(char *)x) );
	}
	prev = n;
	cr = VAL(next);
    } while (more);
    gvec_write_init( n, SLOT(3), next );

    continuation_reg = first;
#undef x
}



/*
    Restores a dynamically-determined number of
    argument-passing registers (ie, for use in the
    extremely rare cases when it cannot be determined
    statically -- nb, this only happens when compiling
    to closure-threaded code) and the continuation
    register itself
*/

unsigned get_partcont_size( obj cro )
{
#define x ((obj *)(cr - POINTER_TAG))
  UINT_32 cr = VAL(cro), size_in_bytes, next;
  unsigned n;

  if (cr < cache_upper_limit && cr >= cache_lower_limit) {
    next = VAL(x[3]);
    if (next < cache_upper_limit && next >= cache_lower_limit) {
      size_in_bytes = next - cr;
    } else {
      size_in_bytes = cache_upper_limit - cr;
    }
  } else {
    size_in_bytes = SIZEOF_PTR(cro);
  }
  
  return (size_in_bytes / sizeof(obj)) - CONT_FIXED;
#undef x
}

unsigned restore_arb( void )
{
  unsigned n = get_partcont_size( continuation_reg );
  restore_cont( n );
  return n;
}

int process_stack_roots( process_root_fn *fn, void *info )
{
obj *p;
obj item, *stop;
UINT_32 cr = VAL(continuation_reg);
int rc;

    p = (obj *)(cache_upper_limit - POINTER_TAG);

    if (!in_stack_cache(continuation_reg))
	return 0;

    stop = (obj *)(cr - POINTER_TAG);

    while (p > stop)
    {
      rc = fn( --p, info );
      if (rc)
	return rc;
    }
    return 0;
}

#endif /* STACK_CACHE */
