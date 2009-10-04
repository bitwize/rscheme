/**********************************************
THIS FILE WAS AUTOMATICALLY GENERATED, AND MAY
BE AUTOMATICALLY RE-GENERATED WHEN THE COMPILER
OR SOURCE CHANGES.  DO NOT MODIFY THIS FILE BY HAND!
RScheme Build (v0.7.3.4-b7u, 2007-05-30)
**********************************************/

#include "tables_p.h"

/******************* Link file for the `tables' module *******************/


extern struct part_descr tables_part_make;

static struct part_descr *(parts_table[]) = {
    &tables_part_make,
    (struct part_descr *)0 };

struct module_descr module_tables = { "tables", parts_table,
	0 /* num roots */,
	(obj *)0,
	(struct root_info *)0,	(struct bcx_descr *)0, /* bc_extensions */
	0 /* num_bc_extensions */
    };

struct module_descr *RS_module_tables = &module_tables;
struct module_descr *RS_fm_tables( void )
{ return &module_tables; }
