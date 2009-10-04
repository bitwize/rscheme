/**********************************************
THIS FILE WAS AUTOMATICALLY GENERATED, AND MAY
BE AUTOMATICALLY RE-GENERATED WHEN THE COMPILER
OR SOURCE CHANGES.  DO NOT MODIFY THIS FILE BY HAND!
RScheme Build (v0.7.3.4-b7u, 2007-05-30)
**********************************************/

#include "iolib_p.h"

/******************** Link file for the `iolib' module ********************/

static struct bcx_descr bcx_tab[] = {
  { 10, bc_stdio_extension, "stdio", &module_iolib },
};

extern struct part_descr iolib_part_writers;
extern struct part_descr iolib_part_format;
extern struct part_descr iolib_part_bstrout;
extern struct part_descr iolib_part_strout;

static struct part_descr *(parts_table[]) = {
    &iolib_part_writers,
    &iolib_part_format,
    &iolib_part_bstrout,
    &iolib_part_strout,
    (struct part_descr *)0 };

struct module_descr module_iolib = { "iolib", parts_table,
	0 /* num roots */,
	(obj *)0,
	(struct root_info *)0,	bcx_tab, /* bc_extensions */
	1 /* num_bc_extensions */
    };

struct module_descr *RS_module_iolib = &module_iolib;
struct module_descr *RS_fm_iolib( void )
{ return &module_iolib; }
