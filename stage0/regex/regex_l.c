/**********************************************
THIS FILE WAS AUTOMATICALLY GENERATED, AND MAY
BE AUTOMATICALLY RE-GENERATED WHEN THE COMPILER
OR SOURCE CHANGES.  DO NOT MODIFY THIS FILE BY HAND!
RScheme Build (v0.7.3.4-b7u, 2007-05-30)
**********************************************/

#include "regex_p.h"

/******************** Link file for the `regex' module ********************/


extern struct part_descr regex_part_rxinterp;

static struct part_descr *(parts_table[]) = {
    &regex_part_rxinterp,
    (struct part_descr *)0 };

struct module_descr module_regex = { "regex", parts_table,
	0 /* num roots */,
	(obj *)0,
	(struct root_info *)0,	(struct bcx_descr *)0, /* bc_extensions */
	0 /* num_bc_extensions */
    };

struct module_descr *RS_module_regex = &module_regex;
struct module_descr *RS_fm_regex( void )
{ return &module_regex; }
