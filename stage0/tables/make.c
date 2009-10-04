/**********************************************
THIS FILE WAS AUTOMATICALLY GENERATED, AND MAY
BE AUTOMATICALLY RE-GENERATED WHEN THE COMPILER
OR SOURCE CHANGES.  DO NOT MODIFY THIS FILE BY HAND!
RScheme Build (v0.7.3.4-b7u, 2007-05-30)
**********************************************/

/******************************** Preamble ********************************/

#define _MODULE_TABLES
#define _SCM_MAKE
#define _C_MAKE
#include "tables_p.h"
#include <rscheme/vinsns.h>
extern struct module_descr module_tables;
extern struct part_descr tables_part_make;
static char sccsid[] = "@(#)tables modules/tables/make.scm [411336704] (RS v0.7.3.4-b7u, 2007-05-30)";

/************************** Function Definitions **************************/


/*********************** Raw glue `hash-table-copy' ***********************/
#define raw_table REG0

static char rsfn_hash_table_copy_name[] = "hash-table-copy";
#define FUNCTION rsfn_hash_table_copy_name

PROLOGUE(hash_table_copy)

BEGIN_FWD(hash_table_copy)
  FWD_MONOTONE(hash_table_copy_0)
END_FWD(hash_table_copy)

#define FPLACE_CODE (1000+0)
MONOTONE(hash_table_copy_0)
{  obj table;
  COUNT_ARGS(1);
  if (!instance_p(raw_table,TLREFB(0)))
    {
      obj c;
      c = make5( TLREFB(1),
                 NIL_OBJ,  /* properties */
                 lookup_symbol( FUNCTION ),
                 cons( raw_table, NIL_OBJ ),
                 lookup_symbol( "table" ),
                 TLREFB(0) );
      raise_error( c );
    }
  table = raw_table;


{
  REG0 = hashtable_copy(table);
  RETURN1();
}}
#undef FPLACE_CODE

EPILOGUE(hash_table_copy)

BEGIN_BACK(hash_table_copy)
  BACK_MONOTONE(hash_table_copy_0)
END_BACK(hash_table_copy)

static struct function_descr hash_table_copy_descr = {
	&tables_part_make,
	JUMP_TABLE( hash_table_copy ),
	rsfn_hash_table_copy_name };
#undef FUNCTION

#undef raw_table

/********************** Raw glue `hash-table-clear!' **********************/
#define raw_table REG0

static char rsfn_hash_table_clear_name[] = "hash-table-clear!";
#define FUNCTION rsfn_hash_table_clear_name

PROLOGUE(hash_table_clear)

BEGIN_FWD(hash_table_clear)
  FWD_MONOTONE(hash_table_clear_0)
END_FWD(hash_table_clear)

#define FPLACE_CODE (1000+0)
MONOTONE(hash_table_clear_0)
{  obj table;
  COUNT_ARGS(1);
  if (!instance_p(raw_table,TLREFB(0)))
    {
      obj c;
      c = make5( TLREFB(1),
                 NIL_OBJ,  /* properties */
                 lookup_symbol( FUNCTION ),
                 cons( raw_table, NIL_OBJ ),
                 lookup_symbol( "table" ),
                 TLREFB(0) );
      raise_error( c );
    }
  table = raw_table;


{
  hashtable_clear( table );
  RETURN0();
}}
#undef FPLACE_CODE

EPILOGUE(hash_table_clear)

BEGIN_BACK(hash_table_clear)
  BACK_MONOTONE(hash_table_clear_0)
END_BACK(hash_table_clear)

static struct function_descr hash_table_clear_descr = {
	&tables_part_make,
	JUMP_TABLE( hash_table_clear ),
	rsfn_hash_table_clear_name };
#undef FUNCTION

#undef raw_table
/******************************* Postamble *******************************/
/**************************** Part Link Table ****************************/


static struct function_descr *(part_make_tab[]) = {
    &hash_table_copy_descr,
    &hash_table_clear_descr,
    NULL };
struct part_descr tables_part_make = {
    411336704,
    &module_tables,
    part_make_tab,
    "make",
    0, sccsid };
#undef _MODULE_TABLES
#undef _SCM_MAKE
#undef _C_MAKE
