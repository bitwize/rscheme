/**********************************************
THIS FILE WAS AUTOMATICALLY GENERATED, AND MAY
BE AUTOMATICALLY RE-GENERATED WHEN THE COMPILER
OR SOURCE CHANGES.  DO NOT MODIFY THIS FILE BY HAND!
RScheme Build (v0.7.3.4-b7u, 2007-05-30)
**********************************************/

#include "imageio_p.h"

/******************* Link file for the `imageio' module *******************/


extern struct part_descr imageio_part_glue;

static struct part_descr *(parts_table[]) = {
    &imageio_part_glue,
    (struct part_descr *)0 };

struct module_descr module_imageio = { "imageio", parts_table,
	0 /* num roots */,
	(obj *)0,
	(struct root_info *)0,	(struct bcx_descr *)0, /* bc_extensions */
	0 /* num_bc_extensions */
    };

struct module_descr *RS_module_imageio = &module_imageio;
struct module_descr *RS_fm_imageio( void )
{ return &module_imageio; }
