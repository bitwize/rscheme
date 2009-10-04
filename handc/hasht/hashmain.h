/*-----------------------------------------------------------------*-C-*---
 * File:    handc/hasht/hashmain.h
 *
 *          Copyright (C)1997 Donovan Kolbly <d.kolbly@rscheme.org>
 *          as part of the RScheme project, licensed for free use.
 *          See <http://www.rscheme.org/> for the latest information.
 *
 * File version:     1.7
 * File mod date:    2003-12-15 09:43:26
 * System build:     v0.7.3.4-b7u, 2007-05-30
 *
 *------------------------------------------------------------------------*/

#ifndef _H_HASHMAIN
#define _H_HASHMAIN

#include <rscheme/obj.h>


obj stringtable_lookup( obj table, obj hash, obj str );
obj stringtable_insert( obj table, obj hash, obj str, obj value );
obj stringtable_remove( obj table, obj hash, obj str );
rs_bool stringtable_probe( obj table, obj hash, obj str );

obj cistringtable_lookup( obj table, obj hash, obj str );
obj cistringtable_insert( obj table, obj hash, obj str, obj value );
obj cistringtable_remove( obj table, obj hash, obj str );
rs_bool cistringtable_probe( obj table, obj hash, obj str );

obj objecttable_lookup( obj table, obj hash, obj key );
obj objecttable_insert( obj table, obj hash, obj key, obj value );
obj objecttable_remove( obj table, obj hash, obj key );
rs_bool objecttable_probe( obj table, obj hash, obj key );

/* bool hashtable_probe( obj table, obj hash ); */
void hashtable_install( obj table, obj hash, obj key, obj value );
void hashtable_foreach( obj table, void *info, 
			void (*fn)( void *info, obj h, obj key, obj value ) );
obj hashtable_chains( obj table );
obj hashtable_copy( obj tbl );

obj hashtable_keys_to_list( obj table );
obj hashtable_values_to_list( obj table );
UINT_32 hashtable_size( obj table );

void hashtable_clear( obj table );

#endif /* _H_HASHMAIN */
