/**********************************************
THIS FILE WAS AUTOMATICALLY GENERATED, AND MAY
BE AUTOMATICALLY RE-GENERATED WHEN THE COMPILER
OR SOURCE CHANGES.  DO NOT MODIFY THIS FILE BY HAND!
RScheme Build (v0.7.3.4-b7u, 2007-05-30)
**********************************************/

#include "corelib_p.h"

/******************* Link file for the `corelib' module *******************/


extern struct part_descr corelib_part_basicnum;
extern struct part_descr corelib_part_rational;
extern struct part_descr corelib_part_complex;
extern struct part_descr corelib_part_keywords;
extern struct part_descr corelib_part_linkinfo;
extern struct part_descr corelib_part_intrglue;
extern struct part_descr corelib_part_imageio;
extern struct part_descr corelib_part_num2str;
extern struct part_descr corelib_part_str2num;
extern struct part_descr corelib_part_string;
extern struct part_descr corelib_part_dynstate;
extern struct part_descr corelib_part_process;
extern struct part_descr corelib_part_alloc;
extern struct part_descr corelib_part_apply;

static struct part_descr *(parts_table[]) = {
    &corelib_part_basicnum,
    &corelib_part_rational,
    &corelib_part_complex,
    &corelib_part_keywords,
    &corelib_part_linkinfo,
    &corelib_part_intrglue,
    &corelib_part_imageio,
    &corelib_part_num2str,
    &corelib_part_str2num,
    &corelib_part_string,
    &corelib_part_dynstate,
    &corelib_part_process,
    &corelib_part_alloc,
    &corelib_part_apply,
    (struct part_descr *)0 };

struct module_descr module_corelib = { "corelib", parts_table,
	0 /* num roots */,
	(obj *)0,
	(struct root_info *)0,	(struct bcx_descr *)0, /* bc_extensions */
	0 /* num_bc_extensions */
    };

struct module_descr *RS_module_corelib = &module_corelib;
struct module_descr *RS_fm_corelib( void )
{ return &module_corelib; }
