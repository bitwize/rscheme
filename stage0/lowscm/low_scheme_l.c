/**********************************************
THIS FILE WAS AUTOMATICALLY GENERATED, AND MAY
BE AUTOMATICALLY RE-GENERATED WHEN THE COMPILER
OR SOURCE CHANGES.  DO NOT MODIFY THIS FILE BY HAND!
RScheme Build (v0.7.3.4-b7u, 2007-05-30)
**********************************************/

#include "low_scheme_p.h"

/***************** Link file for the `low_scheme' module *****************/


extern struct part_descr low_scheme_part_callwval;
extern struct part_descr low_scheme_part_vectops;
extern struct part_descr low_scheme_part_chars;
extern struct part_descr low_scheme_part_finding;
extern struct part_descr low_scheme_part_mapping;
extern struct part_descr low_scheme_part_append;
extern struct part_descr low_scheme_part_vectors;
extern struct part_descr low_scheme_part_lists;

static struct part_descr *(parts_table[]) = {
    &low_scheme_part_callwval,
    &low_scheme_part_vectops,
    &low_scheme_part_chars,
    &low_scheme_part_finding,
    &low_scheme_part_mapping,
    &low_scheme_part_append,
    &low_scheme_part_vectors,
    &low_scheme_part_lists,
    (struct part_descr *)0 };

struct module_descr module_low_scheme = { "low_scheme", parts_table,
	0 /* num roots */,
	(obj *)0,
	(struct root_info *)0,	(struct bcx_descr *)0, /* bc_extensions */
	0 /* num_bc_extensions */
    };

struct module_descr *RS_module_low_scheme = &module_low_scheme;
struct module_descr *RS_fm_low_scheme( void )
{ return &module_low_scheme; }
