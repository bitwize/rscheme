/*-----------------------------------------------------------------*-C-*---
 * File:    handc/hasht/generic0.c
 *
 *          Copyright (C)1997 Donovan Kolbly <d.kolbly@rscheme.org>
 *          as part of the RScheme project, licensed for free use.
 *          See <http://www.rscheme.org/> for the latest information.
 *
 * File version:     1.7
 * File mod date:    1997-11-29 23:10:44
 * System build:     v0.7.3.4-b7u, 2007-05-30
 *
 * Purpose:          fully generic hash table manipulation
 *------------------------------------------------------------------------*/

/********************* generic-hash-table *********************/

/*-------------------- lookup --------------------*/

#define FUNCTION "generic-hash-table-lookup"
#define GENERIC_SCAN_FN generic_lookup
#ifdef __STDC__
#define GENERIC_SCAN_LABEL(x) generic_lookup_ ## x
#else
#define GENERIC_SCAN_LABEL(x) generic_lookup_/**/x
#endif
#define EXTRA_ARGS (0)

PROLOGUE(GENERIC_SCAN_FN)

#include "generic.c1"

/*
             regs on entry ---> +----------------+
    				|     table      |  REG0
				+----------------+
    				|     hash       |  REG1
				+----------------+
				|      key       |  REG2
				+----------------+
    				|     bucket     |  REG3
				+----------------+
    				|     index      |  REG4
				+----------------+
*/

GENERIC_SCAN(2) /* success */
{
    REG0 = read_bucket_value( REG3, FXWORDS_TO_RIBYTES(REG4) );
    RETURN(1);
}

/*
             regs on entry ---> +----------------+
    				|     table      |  REG0
				+----------------+
    				|     hash       |  REG1
				+----------------+
				|      key       |  REG2
				+----------------+
*/

GENERIC_SCAN(3) /* failure */
{
    REG0 = FALSE_OBJ;
    RETURN(0);
}

EPILOGUE(GENERIC_SCAN_FN)

BEGIN_BACK(GENERIC_SCAN_FN)
  BACK_MONOTONE(GENERIC_SCAN_LABEL(0))
  BACK_MONOTONE(GENERIC_SCAN_LABEL(1))
  BACK_MONOTONE(GENERIC_SCAN_LABEL(2))
  BACK_MONOTONE(GENERIC_SCAN_LABEL(3))
END_BACK(GENERIC_SCAN_FN)

static struct function_descr generic_lookup_descr = {
	&rscheme_hasht_part,
	JUMP_TABLE(GENERIC_SCAN_FN),
	FUNCTION };

#undef GENERIC_SCAN_FN
#undef GENERIC_SCAN_LABEL
#undef GENERIC_SCAN
#undef FUNCTION
#undef EXTRA_ARGS

/*-------------------- probe --------------------*/

#define FUNCTION "generic-hash-table-probe"
#define GENERIC_SCAN_FN generic_probe
#ifdef __STDC__
#define GENERIC_SCAN_LABEL(x) generic_probe_ ## x
#else
#define GENERIC_SCAN_LABEL(x) generic_probe_/**/x
#endif
#define EXTRA_ARGS (0)

PROLOGUE(GENERIC_SCAN_FN)

#include "generic.c1"

/*
             regs on entry ---> +----------------+
    				|     table      |  REG0
				+----------------+
    				|     hash       |  REG1
				+----------------+
				|      key       |  REG2
				+----------------+
    				|     bucket     |  REG3
				+----------------+
    				|     index      |  REG4
				+----------------+
*/

GENERIC_SCAN(2) /* success */
{
    REG0 = TRUE_OBJ;
    RETURN(1);
}

/*
             regs on entry ---> +----------------+
    				|     table      |  REG0
				+----------------+
    				|     hash       |  REG1
				+----------------+
				|      key       |  REG2
				+----------------+
*/

GENERIC_SCAN(3) /* failure */
{
    REG0 = FALSE_OBJ;
    RETURN(0);
}

EPILOGUE(GENERIC_SCAN_FN)

BEGIN_BACK(GENERIC_SCAN_FN)
  BACK_MONOTONE(GENERIC_SCAN_LABEL(0))
  BACK_MONOTONE(GENERIC_SCAN_LABEL(1))
  BACK_MONOTONE(GENERIC_SCAN_LABEL(2))
  BACK_MONOTONE(GENERIC_SCAN_LABEL(3))
END_BACK(GENERIC_SCAN_FN)

static struct function_descr generic_probe_descr = {
	&rscheme_hasht_part,
	JUMP_TABLE(GENERIC_SCAN_FN),
	FUNCTION };

#undef GENERIC_SCAN_FN
#undef GENERIC_SCAN_LABEL
#undef GENERIC_SCAN
#undef FUNCTION
#undef EXTRA_ARGS

/*-------------------- remove --------------------*/

#define FUNCTION "generic-hash-table-remove"
#define GENERIC_SCAN_FN generic_remove
#ifdef __STDC__
#define GENERIC_SCAN_LABEL(x) generic_remove_ ## x
#else
#define GENERIC_SCAN_LABEL(x) generic_remove_/**/x
#endif
#define EXTRA_ARGS (0)

PROLOGUE(GENERIC_SCAN_FN)

#include "generic.c1"

/*
             regs on entry ---> +----------------+
    				|     table      |  REG0
				+----------------+
    				|     hash       |  REG1
				+----------------+
				|      key       |  REG2
				+----------------+
    				|     bucket     |  REG3
				+----------------+
    				|     index      |  REG4
				+----------------+
*/

GENERIC_SCAN(2) /* success */
{
obj bucket;
UINT_32 index;

    bucket = REG3;
    index = FXWORDS_TO_RIBYTES(REG4);
    
    removing_one( REG0 );
    write_bucket_hash( bucket, index, FALSE_OBJ );
    write_bucket_key( bucket, index, FALSE_OBJ );
    
    REG0 = read_bucket_value( bucket, index );
    write_bucket_value( bucket, index, FALSE_OBJ );
    RETURN(1);
}

/*
             regs on entry ---> +----------------+
    				|     table      |  REG0
				+----------------+
    				|     hash       |  REG1
				+----------------+
				|      key       |  REG2
				+----------------+
*/

GENERIC_SCAN(3) /* failure */
{
    REG0 = FALSE_OBJ;
    RETURN(0);
}

EPILOGUE(GENERIC_SCAN_FN)

BEGIN_BACK(GENERIC_SCAN_FN)
  BACK_MONOTONE(GENERIC_SCAN_LABEL(0))
  BACK_MONOTONE(GENERIC_SCAN_LABEL(1))
  BACK_MONOTONE(GENERIC_SCAN_LABEL(2))
  BACK_MONOTONE(GENERIC_SCAN_LABEL(3))
END_BACK(GENERIC_SCAN_FN)

static struct function_descr generic_remove_descr = {
	&rscheme_hasht_part,
	JUMP_TABLE(GENERIC_SCAN_FN),
	FUNCTION };


#undef GENERIC_SCAN_FN
#undef GENERIC_SCAN_LABEL
#undef GENERIC_SCAN
#undef FUNCTION
#undef EXTRA_ARGS

/*-------------------- insert --------------------*/

#define FUNCTION "generic-hash-table-insert"
#define GENERIC_SCAN_FN generic_insert
#ifdef __STDC__
#define GENERIC_SCAN_LABEL(x) generic_insert_ ## x
#else
#define GENERIC_SCAN_LABEL(x) generic_insert_/**/x
#endif
#define EXTRA_ARGS (1)

PROLOGUE(GENERIC_SCAN_FN)

#include "generic.c1"

/*
             regs on entry ---> +----------------+
    				|     table      |  REG0
				+----------------+
    				|     hash       |  REG1
				+----------------+
				|      key       |  REG2
				+----------------+
				|    value       |  REG3
				+----------------+
    				|     bucket     |  REG4
				+----------------+
    				|     index      |  REG5
				+----------------+
*/

GENERIC_SCAN(2) /* success */
{
obj bucket;
UINT_32 index;

    bucket = REG4;
    index = FXWORDS_TO_RIBYTES(REG5);

    REG0 = read_bucket_value( bucket, index );
    write_bucket_key( bucket, index, REG2 );
    write_bucket_value( bucket, index, REG3 );
    RETURN(1);
}

/*
             regs on entry ---> +----------------+
    				|     table      |  REG0
				+----------------+
    				|     hash       |  REG1
				+----------------+
				|      key       |  REG2
				+----------------+
				|    value       |  REG3
				+----------------+
*/

GENERIC_SCAN(3) /* failure */
{
    hashtable_install( REG0, REG1, REG2, REG3 );
    REG0 = FALSE_OBJ;
    RETURN(1);
}

EPILOGUE(GENERIC_SCAN_FN)

BEGIN_BACK(GENERIC_SCAN_FN)
  BACK_MONOTONE(GENERIC_SCAN_LABEL(0))
  BACK_MONOTONE(GENERIC_SCAN_LABEL(1))
  BACK_MONOTONE(GENERIC_SCAN_LABEL(2))
  BACK_MONOTONE(GENERIC_SCAN_LABEL(3))
END_BACK(GENERIC_SCAN_FN)

static struct function_descr generic_insert_descr = {
	&rscheme_hasht_part,
	JUMP_TABLE(GENERIC_SCAN_FN),
	FUNCTION };

#undef GENERIC_SCAN_FN
#undef GENERIC_SCAN_LABEL
#undef GENERIC_SCAN
#undef FUNCTION
#undef EXTRA_ARGS
