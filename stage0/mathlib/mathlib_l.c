/**********************************************
THIS FILE WAS AUTOMATICALLY GENERATED, AND MAY
BE AUTOMATICALLY RE-GENERATED WHEN THE COMPILER
OR SOURCE CHANGES.  DO NOT MODIFY THIS FILE BY HAND!
RScheme Build (v0.7.3.4-b7u, 2007-05-30)
**********************************************/

#include "mathlib_p.h"

/******************* Link file for the `mathlib' module *******************/


extern struct part_descr mathlib_part_random;
extern struct part_descr mathlib_part_arith;

static struct part_descr *(parts_table[]) = {
    &mathlib_part_random,
    &mathlib_part_arith,
    (struct part_descr *)0 };

struct module_descr module_mathlib = { "mathlib", parts_table,
	0 /* num roots */,
	(obj *)0,
	(struct root_info *)0,	(struct bcx_descr *)0, /* bc_extensions */
	0 /* num_bc_extensions */
    };

struct module_descr *RS_module_mathlib = &module_mathlib;
struct module_descr *RS_fm_mathlib( void )
{ return &module_mathlib; }
