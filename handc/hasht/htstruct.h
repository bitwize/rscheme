/*-----------------------------------------------------------------*-C-*---
 * File:    handc/hasht/htstruct.h
 *
 *          Copyright (C)1997 Donovan Kolbly <d.kolbly@rscheme.org>
 *          as part of the RScheme project, licensed for free use.
 *          See <http://www.rscheme.org/> for the latest information.
 *
 * File version:     1.4
 * File mod date:    1997-11-29 23:10:43
 * System build:     v0.7.3.4-b7u, 2007-05-30
 *
 *------------------------------------------------------------------------*/

#ifndef _H_HTSTRUCT
#define _H_HTSTRUCT

#include <rscheme/smemory.h>

#define BUCKET_CAPACITY		(8)

#define HASHTABLE_DIRECTORY	SLOT(0)
#define HASHTABLE_DIR_BITS	SLOT(1)
#define HASHTABLE_COUNT		SLOT(2)
#define HASHTABLE_BUCKET_CLASS  SLOT(3)
/*      HASHTABLE_HASH_PROC     SLOT(4) */
#define HASHTABLE_EQ_PROC	SLOT(5)

#define BUCKET_BITS		SLOT(0)
#define BUCKET_OVERFLOW		SLOT(1)

void split_bucket( obj table, obj bucket, obj h, obj k, obj v );
obj make_bucket( obj bucket_class, int bucket_bits );

#define DIR_SLOT(dir,hash) (FXWORDS_TO_RIBYTES(hash)&(SIZEOF_PTR(dir)-SLOT(1)))

#define read_dir(dir,hash) gvec_read(dir,DIR_SLOT(dir,hash))
#define write_dir(dir,hash,v) gvec_write(dir,DIR_SLOT(dir,hash),v)

#define HASH_SLOT(base_offset) ((base_offset))
#define KEY_SLOT(base_offset) ((base_offset)+SLOT(BUCKET_CAPACITY))
#define VALUE_SLOT(base_offset) ((base_offset)+2*SLOT(BUCKET_CAPACITY))

#define read_bucket_hash(bucket,index) gvec_read(bucket,HASH_SLOT(index))
#define read_bucket_key(bucket,index) gvec_read(bucket,KEY_SLOT(index))
#define read_bucket_value(bucket,index) gvec_read(bucket,VALUE_SLOT(index))

#define write_bucket_hash(bucket,index,h) gvec_write_non_ptr(bucket,\
							     HASH_SLOT(index),\
							     h)
#define write_bucket_key(bucket,index,k) gvec_write(bucket,KEY_SLOT(index),k)
#define write_bucket_value(bucket,i,v) gvec_write(bucket,VALUE_SLOT(i),v)

/* incr/decr the table-size field */

#define change_count(ht,fn) gvec_write_non_ptr(ht,\
					       HASHTABLE_COUNT,\
					       fn(gvec_read(ht,\
							    HASHTABLE_COUNT)))
#define inserting_one(ht) change_count(ht,ADD1)
#define removing_one(ht) change_count(ht,SUB1)

#endif /* _H_HTSTRUCT */
