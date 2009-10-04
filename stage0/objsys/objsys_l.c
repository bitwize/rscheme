/**********************************************
THIS FILE WAS AUTOMATICALLY GENERATED, AND MAY
BE AUTOMATICALLY RE-GENERATED WHEN THE COMPILER
OR SOURCE CHANGES.  DO NOT MODIFY THIS FILE BY HAND!
RScheme Build (v0.7.3.4-b7u, 2007-05-30)
**********************************************/

#include "objsys_p.h"

/******************* Link file for the `objsys' module *******************/


extern struct part_descr objsys_part_getnset;
extern struct part_descr objsys_part_makeinst;
extern struct part_descr objsys_part_genericf;

static struct part_descr *(parts_table[]) = {
    &objsys_part_getnset,
    &objsys_part_makeinst,
    &objsys_part_genericf,
    (struct part_descr *)0 };

struct module_descr module_objsys = { "objsys", parts_table,
	0 /* num roots */,
	(obj *)0,
	(struct root_info *)0,	(struct bcx_descr *)0, /* bc_extensions */
	0 /* num_bc_extensions */
    };

struct module_descr *RS_module_objsys = &module_objsys;
struct module_descr *RS_fm_objsys( void )
{ return &module_objsys; }
