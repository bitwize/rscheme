/*-----------------------------------------------------------------*-C-*---
 * File:    handc/runtime/allocns.h
 *
 *          Copyright (C)1997 Donovan Kolbly <d.kolbly@rscheme.org>
 *          as part of the RScheme project, licensed for free use.
 *          See <http://www.rscheme.org/> for the latest information.
 *
 * File version:     1.6
 * File mod date:    1997-11-29 23:10:48
 * System build:     v0.7.3.4-b7u, 2007-05-30
 *
 * Purpose:          maken() functions
 *------------------------------------------------------------------------*/

#ifndef _H_RSCHEME_ALLOCNS
#define _H_RSCHEME_ALLOCNS

obj make0( obj the_class );
obj make1( obj the_class, obj value0 );
obj make2( obj the_class, obj value0, obj value1 );
obj make3( obj the_class, obj value0, obj value1, obj value2 );
obj make4( obj the_class, obj value0, obj value1, obj value2,
			  obj value3 );
obj make5( obj the_class, obj value0, obj value1, obj value2,
			  obj value3, obj value4 );
obj make6( obj the_class, obj value0, obj value1, obj value2,
			  obj value3, obj value4, obj value5 );
obj make7( obj the_class, obj value0, obj value1, obj value2,
			  obj value3, obj value4, obj value5,
			  obj value6 );
obj make8( obj the_class, obj value0, obj value1, obj value2,
			  obj value3, obj value4, obj value5,
			  obj value6, obj value7 );
obj make9( obj the_class, obj value0, obj value1, obj value2,
			  obj value3, obj value4, obj value5,
			  obj value6, obj value7, obj value8 );
obj maken( obj the_class, unsigned n, ... );

#endif /* _H_RSCHEME_ALLOCNS */
