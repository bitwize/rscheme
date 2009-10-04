/*-----------------------------------------------------------------*-C-*---
 * File:    handc/runtime/allocns.c
 *
 *          Copyright (C)1997 Donovan Kolbly <d.kolbly@rscheme.org>
 *          as part of the RScheme project, licensed for free use.
 *          See <http://www.rscheme.org/> for the latest information.
 *
 * File version:     1.6
 * File mod date:    1997-11-29 23:10:48
 * System build:     v0.7.3.4-b7u, 2007-05-30
 *
 * Purpose:          Size-specialized allocators (makeN), and variable-arity maken
 *------------------------------------------------------------------------*/

#include <stdarg.h>
#include <rscheme/smemory.h>
#include <rscheme/allocns.h>

obj make0( obj the_class )
{
obj it = alloc( 0, the_class );

    return it;
}

obj make1( obj the_class, obj value0 )
{
obj it = alloc1( the_class );

    gvec_write_init( it, SLOT(0), value0 );
    return it;
}

obj make2( obj the_class, obj value0, obj value1 )
{
obj it = alloc2( the_class );

    gvec_write_init( it, SLOT(0), value0 );
    gvec_write_init( it, SLOT(1), value1 );
    return it;
}

obj make3( obj the_class, obj value0, obj value1, obj value2 )
{
obj it = alloc3( the_class );

    gvec_write_init( it, SLOT(0), value0 );
    gvec_write_init( it, SLOT(1), value1 );
    gvec_write_init( it, SLOT(2), value2 );
    return it;
}

obj make4( obj the_class, obj value0, obj value1, obj value2,
			  obj value3 )
{
obj it = alloc4( the_class );

    gvec_write_init( it, SLOT(0), value0 );
    gvec_write_init( it, SLOT(1), value1 );
    gvec_write_init( it, SLOT(2), value2 );
    gvec_write_init( it, SLOT(3), value3 );
    return it;
}

obj make5( obj the_class, obj value0, obj value1, obj value2,
			  obj value3, obj value4 )
{
obj it = alloc5( the_class );

    gvec_write_init( it, SLOT(0), value0 );
    gvec_write_init( it, SLOT(1), value1 );
    gvec_write_init( it, SLOT(2), value2 );
    gvec_write_init( it, SLOT(3), value3 );
    gvec_write_init( it, SLOT(4), value4 );
    return it;
}

obj make6( obj the_class, obj value0, obj value1, obj value2,
			  obj value3, obj value4, obj value5 )
{
obj it = alloc6( the_class );

    gvec_write_init( it, SLOT(0), value0 );
    gvec_write_init( it, SLOT(1), value1 );
    gvec_write_init( it, SLOT(2), value2 );
    gvec_write_init( it, SLOT(3), value3 );
    gvec_write_init( it, SLOT(4), value4 );
    gvec_write_init( it, SLOT(5), value5 );
    return it;
}

obj make7( obj the_class, obj value0, obj value1, obj value2,
			  obj value3, obj value4, obj value5,
			  obj value6 )
{
obj it = alloc7( the_class );

    gvec_write_init( it, SLOT(0), value0 );
    gvec_write_init( it, SLOT(1), value1 );
    gvec_write_init( it, SLOT(2), value2 );
    gvec_write_init( it, SLOT(3), value3 );
    gvec_write_init( it, SLOT(4), value4 );
    gvec_write_init( it, SLOT(5), value5 );
    gvec_write_init( it, SLOT(6), value6 );
    return it;
}

obj make8( obj the_class, obj value0, obj value1, obj value2,
			  obj value3, obj value4, obj value5,
			  obj value6, obj value7 )
{
obj it = alloc8( the_class );

    gvec_write_init( it, SLOT(0), value0 );
    gvec_write_init( it, SLOT(1), value1 );
    gvec_write_init( it, SLOT(2), value2 );
    gvec_write_init( it, SLOT(3), value3 );
    gvec_write_init( it, SLOT(4), value4 );
    gvec_write_init( it, SLOT(5), value5 );
    gvec_write_init( it, SLOT(6), value6 );
    gvec_write_init( it, SLOT(7), value7 );
    return it;
}

obj make9( obj the_class, obj value0, obj value1, obj value2,
			  obj value3, obj value4, obj value5,
			  obj value6, obj value7, obj value8 )
{
obj it = alloc9( the_class );

    gvec_write_init( it, SLOT(0), value0 );
    gvec_write_init( it, SLOT(1), value1 );
    gvec_write_init( it, SLOT(2), value2 );
    gvec_write_init( it, SLOT(3), value3 );
    gvec_write_init( it, SLOT(4), value4 );
    gvec_write_init( it, SLOT(5), value5 );
    gvec_write_init( it, SLOT(6), value6 );
    gvec_write_init( it, SLOT(7), value7 );
    gvec_write_init( it, SLOT(8), value8 );
    return it;
}

obj maken( obj the_class, unsigned n, ... )
{
  obj it = alloc( SLOT(n), the_class );
  unsigned i;
  va_list ap;
  
  va_start( ap, n );
  for (i=0; i<n; i++)
    gvec_write_init( it, SLOT(i), va_arg(ap, obj) );
  va_end( ap );

  return it;
}

