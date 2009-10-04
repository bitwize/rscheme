/*-----------------------------------------------------------------*-C-*---
 * File:    handc/heapi/imagfile.h
 *
 *          Copyright (C)1997 Donovan Kolbly <d.kolbly@rscheme.org>
 *          as part of the RScheme project, licensed for free use.
 *          See <http://www.rscheme.org/> for the latest information.
 *
 * File version:     1.8
 * File mod date:    2003-10-13 13:02:27
 * System build:     v0.7.3.4-b7u, 2007-05-30
 *
 *------------------------------------------------------------------------*/

#ifndef _H_IMAGFILE
#define _H_IMAGFILE

#include <rscheme/obj.h>

/*
    The heap image file looks like:

	+-----------------+
	|   blank space   |
	+-----------------+  FILE_HDR_OFFSET
	|       file      |
	|      header     |
	+-----------------+  FILE_DATA_OFFSET
	|                 |
	:      object     :
	:   descriptions  :
	|                 |
	|                 |
	+-----------------+  load2_offset[LOAD2_CLASS_ONLY]
	|                 |
	:   objects that  :
	: only need their :
	|classes swizzled |
	+-----------------+  load2_offset[LOAD2_GVEC]
	|                 |
	:   objects that  :
	:    need their   :
	|  slots swizzled |
	+-----------------+  load2_offset[LOAD2_BIGNUM]
	|                 |
	:   bignums       :
	+-----------------+  load2_offset[LOAD2_TEMPLATE]
	|                 |
	:   templates     :
	+-----------------+  load2_offset[LOAD2_PARTCONT]
	|                 |
	:   part.conts    :
	+-----------------+

*/

#define FILE_HDR_OFFSET		(128)
#define FILE_DATA_OFFSET	(FILE_HDR_OFFSET + sizeof(struct file_header))

enum load1_mode {
    LOAD1_REF,
    LOAD1_SYMBOL,
    LOAD1_PART,
    LOAD1_ARRAY8,
    LOAD1_ARRAY16,
    LOAD1_ARRAY32,
    LOAD1_ARRAY64,
    LOAD1_BIGNUM
};

enum load2_mode {
    LOAD2_NOP,
    LOAD2_CLASS_ONLY,
    LOAD2_GVEC,
    LOAD2_TEMPLATE,
    LOAD2_PARTCONT,
    LOAD2_MIXVEC_2,
    LOAD2_BIGNUM
};

#define NUM_LOAD2_MODES  	(10)
#define IMAGE_MAGIC_NUMBER	(0x455A646B)	/* EZdk */
#define IMAGE_VERSION_NUMBER	(2)
#define IMAGE_VERSION_06        (3)
#define IMAGE_VERSION_06_BOOT   (4)

struct file_header {
    XUINT_32		magic;
    				/* IMAGE_MAGIC_NUMBER */
    XUINT_32		version;
    				/* IMAGE_VERSION_NUMBER */

    XUINT_32		load2_offset[NUM_LOAD2_MODES];
    				/* offset to the list of objects
				   to be swizzled in the given mode */

    XUINT_32		load2_count[NUM_LOAD2_MODES];
    				/* number of objects
				   to be swizzled in the given mode */

    XUINT_32		entry_object_offt;
    				/* entry point */
    XUINT_32		num_objects;
    				/* number of object names in file */
    XUINT_32		data_area_length;
				/* bytes taken up by data section */
};

#endif /* _H_IMAGFILE */
